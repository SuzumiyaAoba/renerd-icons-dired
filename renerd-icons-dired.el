;;; renerd-icons-dired.el --- Fast Dired icons using Nerd Icons -*- lexical-binding: t; -*-

;; Author: SuzumiyaAoba
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (nerd-icons "0.0.1"))
;; Keywords: convenience, files, icons
;; URL: https://github.com/SuzumiyaAoba/renerd-icons-dired
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Display Nerd Icons in Dired without doing whole-buffer refreshes on the hot
;; path.  Enabling the mode annotates only displayed windows plus a small
;; look-ahead region; scrolling schedules more work in bounded idle chunks.
;; Existing overlays are reused, icon strings are cached, and ordinary refreshes
;; never delete/recreate every icon in a large Dired buffer.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'nerd-icons)

(defgroup renerd-icons-dired nil
  "Fast Nerd Icons integration for Dired."
  :group 'dired
  :prefix "renerd-icons-dired-")

(defcustom renerd-icons-dired-infix-string "\t"
  "String inserted after ordinary file and directory icons."
  :type 'string
  :group 'renerd-icons-dired)

(defcustom renerd-icons-dired-special-prefix-string "  \t"
  "Prefix used for `.' and `..' entries."
  :type 'string
  :group 'renerd-icons-dired)

(defcustom renerd-icons-dired-file-icon-function #'nerd-icons-icon-for-file
  "Function returning an icon for a file name."
  :type 'function
  :group 'renerd-icons-dired)

(defcustom renerd-icons-dired-dir-icon-function #'nerd-icons-icon-for-dir
  "Function returning an icon for a directory name."
  :type 'function
  :group 'renerd-icons-dired)

(defcustom renerd-icons-dired-icon-size 1.0
  "Icon height passed to Nerd Icons functions."
  :type 'number
  :group 'renerd-icons-dired)

(defcustom renerd-icons-dired-prefetch-lines 80
  "Number of lines before and after visible windows to annotate opportunistically."
  :type 'natnum
  :group 'renerd-icons-dired)

(defcustom renerd-icons-dired-chunk-size 300
  "Maximum number of entries annotated by one idle callback."
  :type 'natnum
  :group 'renerd-icons-dired)

(defcustom renerd-icons-dired-idle-delay 0.03
  "Idle delay in seconds before processing queued off-screen annotations."
  :type 'number
  :group 'renerd-icons-dired)

(defface renerd-icons-dired-overlay-face
  '((t :inherit default))
  "Base face for inserted icon prefixes."
  :group 'renerd-icons-dired)

(defvar renerd-icons-dired-mode nil)

(defvar renerd-icons-dired--file-cache (make-hash-table :test #'equal))
(defvar renerd-icons-dired--dir-cache (make-hash-table :test #'equal))

(defvar-local renerd-icons-dired--queue nil)
(defvar-local renerd-icons-dired--queued nil)
(defvar-local renerd-icons-dired--timer nil)
(defvar-local renerd-icons-dired--generation 0)
(defvar-local renerd-icons-dired--scheduled-windows nil)

(defun renerd-icons-dired-clear-cache ()
  "Clear global icon string caches.
Call this after changing icon functions, icon size, theme-dependent icon faces,
or file-type rules."
  (interactive)
  (clrhash renerd-icons-dired--file-cache)
  (clrhash renerd-icons-dired--dir-cache)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p renerd-icons-dired-mode)
        (renerd-icons-dired-refresh t)))))

(defun renerd-icons-dired--key (kind name function)
  "Return an icon cache key for KIND, NAME and FUNCTION."
  (list kind name function renerd-icons-dired-icon-size
        renerd-icons-dired-infix-string renerd-icons-dired-special-prefix-string))

(defun renerd-icons-dired--cached-icon (file directory-p special-p)
  "Return the prefix string for FILE.
DIRECTORY-P selects the directory icon function.  SPECIAL-P is non-nil for
`.` and `..', whose visible prefix intentionally matches nerd-icons-dired."
  (if special-p
      renerd-icons-dired-special-prefix-string
    (let* ((function (if directory-p
                         renerd-icons-dired-dir-icon-function
                       renerd-icons-dired-file-icon-function))
           ;; Directory icons can depend on absolute path state (.git,
           ;; symlink, remote).  File icons in nerd-icons depend on basename
           ;; and extension; custom functions may still inspect the full name,
           ;; so cache by the provided Dired name rather than only extension.
           (name (if directory-p (expand-file-name file default-directory) file))
           (table (if directory-p renerd-icons-dired--dir-cache
                    renerd-icons-dired--file-cache))
           (key (renerd-icons-dired--key (if directory-p 'dir 'file) name function)))
      (or (gethash key table)
          (puthash key
                   (concat (funcall function file :height renerd-icons-dired-icon-size)
                           renerd-icons-dired-infix-string)
                   table)))))

(defun renerd-icons-dired--overlay-at (pos)
  "Return the existing renerd icon overlay at POS, if any."
  (catch 'found
    (dolist (overlay (overlays-in (max (point-min) (1- pos)) pos))
      (when (overlay-get overlay 'renerd-icons-dired-overlay)
        (throw 'found overlay)))))

(defun renerd-icons-dired--put-prefix (pos prefix generation)
  "Show PREFIX before POS, reusing the overlay for GENERATION."
  (let* ((beg (max (point-min) (1- pos)))
         (overlay (or (renerd-icons-dired--overlay-at pos)
                      (make-overlay beg pos (current-buffer) nil t))))
    (move-overlay overlay beg pos (current-buffer))
    (overlay-put overlay 'renerd-icons-dired-overlay t)
    (overlay-put overlay 'renerd-icons-dired-generation generation)
    (overlay-put overlay 'after-string
                 (propertize prefix 'face 'renerd-icons-dired-overlay-face))
    overlay))

(defun renerd-icons-dired--line-entry ()
  "Return (POS FILE DIRECTORY-P SPECIAL-P) for the current Dired line.
Return nil for non-file lines."
  (let ((pos (dired-move-to-filename nil)))
    (when pos
      (let ((file (dired-get-filename 'relative 'noerror)))
        (when file
          (let ((special (member file '("." ".."))))
            ;; Avoid file-directory-p for . and ..: they use the special blank
            ;; prefix.  For ordinary entries, Dired buffers contain local file
            ;; names here; this intentionally follows nerd-icons-dired's public
            ;; function contract and supports custom icon functions.
            (list pos file (and (not special) (file-directory-p file)) special)))))))

(defun renerd-icons-dired--annotate-line (generation)
  "Annotate the Dired entry on the current line for GENERATION."
  (pcase-let ((`(,pos ,file ,directory-p ,special-p)
               (renerd-icons-dired--line-entry)))
    (when pos
      (renerd-icons-dired--put-prefix
       pos (renerd-icons-dired--cached-icon file directory-p special-p)
       generation))))

(defun renerd-icons-dired--collect-positions (start end generation)
  "Collect filename positions from START to END for GENERATION.
Already-current overlays are skipped.  The returned list is in buffer order."
  (let (positions)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char start)
        (beginning-of-line)
        (while (< (point) end)
          (when-let* ((pos (dired-move-to-filename nil)))
            (unless (eq (when-let* ((overlay (renerd-icons-dired--overlay-at pos)))
                          (overlay-get overlay 'renerd-icons-dired-generation))
                        generation)
              (push (copy-marker pos) positions)))
          (forward-line 1))))
    (nreverse positions)))

(defun renerd-icons-dired--visible-region (window)
  "Return (START . END) to annotate around WINDOW." 
  (with-current-buffer (window-buffer window)
    (save-excursion
      (let ((start (window-start window))
            (end (or (window-end window) (point-max))))
        (goto-char start)
        (forward-line (- renerd-icons-dired-prefetch-lines))
        (setq start (point))
        (goto-char end)
        (forward-line renerd-icons-dired-prefetch-lines)
        (cons start (point))))))

(defun renerd-icons-dired--enqueue-region (start end)
  "Queue annotations between START and END."
  (let ((positions (renerd-icons-dired--collect-positions
                    start end renerd-icons-dired--generation)))
    (when positions
      (dolist (marker positions)
        (let ((pos (marker-position marker)))
          (unless (gethash pos renerd-icons-dired--queued)
            (puthash pos t renerd-icons-dired--queued)
            (push marker renerd-icons-dired--queue))))
      (setq renerd-icons-dired--queue (nreverse renerd-icons-dired--queue))
      (renerd-icons-dired--schedule))))

(defun renerd-icons-dired--annotate-window-now (window)
  "Synchronously annotate the exact visible portion of WINDOW.
This is bounded by the number of displayed lines, not the Dired buffer size."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (save-excursion
      (save-restriction
        (widen)
        (let ((end (or (window-end window) (point-max))))
          (goto-char (window-start window))
          (beginning-of-line)
          (while (< (point) end)
            (renerd-icons-dired--annotate-line renerd-icons-dired--generation)
            (forward-line 1)))))))

(defun renerd-icons-dired--process-queue (buffer generation)
  "Annotate queued entries in BUFFER for GENERATION." 
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq renerd-icons-dired--timer nil)
      (when (and renerd-icons-dired-mode
                 (= generation renerd-icons-dired--generation))
        (let ((count 0))
          (while (and renerd-icons-dired--queue
                      (< count renerd-icons-dired-chunk-size)
                      (not (input-pending-p)))
            (let ((marker (pop renerd-icons-dired--queue)))
              (remhash (and (markerp marker) (marker-position marker))
                       renerd-icons-dired--queued)
              (when (and (markerp marker) (marker-buffer marker))
                (goto-char marker)
                (renerd-icons-dired--annotate-line generation)
                (set-marker marker nil))
              (cl-incf count)))
        (when renerd-icons-dired--queue
          (renerd-icons-dired--schedule)))))))

(defun renerd-icons-dired--schedule ()
  "Schedule queued annotation work for the current buffer." 
  (unless (timerp renerd-icons-dired--timer)
    (setq renerd-icons-dired--timer
          (run-with-idle-timer
           renerd-icons-dired-idle-delay nil
           #'renerd-icons-dired--process-queue
           (current-buffer) renerd-icons-dired--generation))))

(defun renerd-icons-dired--schedule-window (window)
  "Ensure WINDOW and nearby lines are annotated." 
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             renerd-icons-dired-mode)
    ;; Visible lines are cheap and should appear immediately.  The prefetch
    ;; range is queued and bounded by `renerd-icons-dired-chunk-size'.
    (renerd-icons-dired--annotate-window-now window)
    (pcase-let ((`(,start . ,end) (renerd-icons-dired--visible-region window)))
      (renerd-icons-dired--enqueue-region start end))))

(defun renerd-icons-dired--schedule-visible-windows ()
  "Schedule annotation for every live window displaying the current buffer." 
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (renerd-icons-dired--schedule-window window)))

(defun renerd-icons-dired--post-command ()
  "Post-command hook that fills icons after scroll/navigation commands." 
  (when renerd-icons-dired-mode
    (renerd-icons-dired--schedule-visible-windows)))

(defun renerd-icons-dired--window-scroll (window _start)
  "Window scroll hook for WINDOW."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when renerd-icons-dired-mode
        (renerd-icons-dired--schedule-window window)))))

(defun renerd-icons-dired--remove-overlays (&optional all)
  "Remove stale renerd overlays.
When ALL is nil, remove overlays from generations older than the current one."
  (if all
      (remove-overlays (point-min) (point-max)
                       'renerd-icons-dired-overlay t)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (and (overlay-get overlay 'renerd-icons-dired-overlay)
                 (/= (or (overlay-get overlay 'renerd-icons-dired-generation) -1)
                     renerd-icons-dired--generation))
        (delete-overlay overlay)))))

(defun renerd-icons-dired--reset (&optional clear-cache)
  "Reset buffer-local state and reschedule icons.
When CLEAR-CACHE is non-nil, also clear global icon caches."
  (when clear-cache (renerd-icons-dired-clear-cache))
  (when (timerp renerd-icons-dired--timer)
    (cancel-timer renerd-icons-dired--timer))
  (setq renerd-icons-dired--timer nil
        renerd-icons-dired--queue nil
        renerd-icons-dired--queued (make-hash-table :test #'eql))
  (cl-incf renerd-icons-dired--generation)
  (renerd-icons-dired--remove-overlays t)
  (renerd-icons-dired--schedule-visible-windows))

;;;###autoload
(defun renerd-icons-dired-refresh (&optional all)
  "Refresh icons in the current Dired buffer.
Without prefix, reschedule displayed windows and reuse existing overlays.  With
ALL, clear all icon overlays in this buffer first."
  (interactive "P")
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not a Dired buffer"))
  (if all
      (renerd-icons-dired--reset)
    (renerd-icons-dired--schedule-visible-windows)))

(defun renerd-icons-dired--after-readin ()
  "Handle Dired buffer replacement after revert/readin." 
  (when renerd-icons-dired-mode
    (renerd-icons-dired--reset)))

(defun renerd-icons-dired--teardown ()
  "Disable all buffer-local resources." 
  (when (timerp renerd-icons-dired--timer)
    (cancel-timer renerd-icons-dired--timer))
  (setq renerd-icons-dired--timer nil
        renerd-icons-dired--queue nil
        renerd-icons-dired--queued nil)
  (remove-hook 'post-command-hook #'renerd-icons-dired--post-command t)
  (remove-hook 'window-scroll-functions #'renerd-icons-dired--window-scroll t)
  (remove-hook 'dired-after-readin-hook #'renerd-icons-dired--after-readin t)
  (renerd-icons-dired--remove-overlays t))

;;;###autoload
(define-minor-mode renerd-icons-dired-mode
  "Display Nerd Icons in Dired with visible-range-first updates."
  :lighter " RIcons"
  :group 'renerd-icons-dired
  (if renerd-icons-dired-mode
      (if (not (derived-mode-p 'dired-mode))
          (setq renerd-icons-dired-mode nil)
        (setq-local tab-width 1)
        (unless (hash-table-p renerd-icons-dired--queued)
          (setq renerd-icons-dired--queued (make-hash-table :test #'eql)))
        (add-hook 'post-command-hook #'renerd-icons-dired--post-command nil t)
        (add-hook 'window-scroll-functions #'renerd-icons-dired--window-scroll nil t)
        (add-hook 'dired-after-readin-hook #'renerd-icons-dired--after-readin nil t)
        (renerd-icons-dired--reset))
    (renerd-icons-dired--teardown)))

;;;###autoload
(defun renerd-icons-dired-enable ()
  "Enable `renerd-icons-dired-mode' in Dired buffers." 
  (when (derived-mode-p 'dired-mode)
    (renerd-icons-dired-mode 1)))

(provide 'renerd-icons-dired)
;;; renerd-icons-dired.el ends here
