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
;;
;; Filename positions are discovered through the `dired-filename' text
;; property, directory detection reuses the ls type column, and edits
;; invalidate affected icons through `after-change-functions', so steady-state
;; post-command and scroll handling boils down to a few integer comparisons
;; per window.

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

(defcustom renerd-icons-dired-fast-file-icons t
  "Resolve file icons through a fast path when possible.
When non-nil and `renerd-icons-dired-file-icon-function' is
`nerd-icons-icon-for-file', file names that cannot match
`nerd-icons-regexp-icon-alist' are resolved through a per-extension cache
instead of calling the icon function for every file name.  Names matching
the combined regexp still go through the real function, so results are
identical to plain `nerd-icons-icon-for-file'."
  :type 'boolean
  :group 'renerd-icons-dired)

(defface renerd-icons-dired-overlay-face
  '((t :inherit default))
  "Base face for inserted icon prefixes."
  :group 'renerd-icons-dired)

(defvar renerd-icons-dired-mode nil)

(defvar renerd-icons-dired--file-cache (make-hash-table :test #'equal))
(defvar renerd-icons-dired--dir-cache (make-hash-table :test #'equal))

;; Fast path state for the default `nerd-icons-icon-for-file': two combined
;; regexps deciding whether a name can be special (an anchored one tried only
;; at position 0, and a cheaper unanchored one), plus a per-extension hash of
;; the resulting icon string.
(defvar renerd-icons-dired--special-anchored nil)
(defvar renerd-icons-dired--special-unanchored nil)
(defvar renerd-icons-dired--special-regexp-source :none)
(defvar renerd-icons-dired--ext-cache nil)
(defvar renerd-icons-dired--ext-cache-source nil)

(defun renerd-icons-dired--special-regexps ()
  "Refresh the combined regexps for special file names when needed.
Sets `renerd-icons-dired--special-anchored' and
`renerd-icons-dired--special-unanchored'.  A name can match
`nerd-icons-regexp-icon-alist' only if it matches one of them; a nil regexp
means no name can match that group (e.g. when a key is not a regexp string)."
  (unless (eq renerd-icons-dired--special-regexp-source
              nerd-icons-regexp-icon-alist)
    (let ((split (condition-case nil
                     (let (anchored unanchored)
                       (dolist (entry nerd-icons-regexp-icon-alist)
                         (let ((regexp (car entry)))
                           (cond
                            ((string-prefix-p "^" regexp)
                             (push (substring regexp 1) anchored))
                            ((string-prefix-p "\\`" regexp)
                             ;; `\` matches only at position 0 while `^'
                             ;; also matches after newlines, so keeping it
                             ;; in the anchored group is safe: the verdict
                             ;; can only be a false positive, which merely
                             ;; delegates to the real function.
                             (push (substring regexp 2) anchored))
                            (t (push regexp unanchored)))))
                       (cons anchored unanchored))
                   (error 'error))))
      (setq renerd-icons-dired--special-anchored
            (cond
             ((eq split 'error)
              ;; Unusable table: match every name so all lookups delegate
              ;; to the real function.
              "")
             ((car split)
              (concat "^\\(?:"
                      (mapconcat #'identity (car split) "\\|")
                      "\\)")))
            renerd-icons-dired--special-unanchored
            (and (consp split)
                 (cdr split)
                 (mapconcat (lambda (regexp)
                              (concat "\\(?:" regexp "\\)"))
                            (cdr split) "\\|"))
            renerd-icons-dired--special-regexp-source
            nerd-icons-regexp-icon-alist)
      ;; The extension table may have changed together with the regexp table.
      (unless (eq renerd-icons-dired--ext-cache-source
                  nerd-icons-extension-icon-alist)
        (setq renerd-icons-dired--ext-cache nil
              renerd-icons-dired--ext-cache-source
              nerd-icons-extension-icon-alist)))))

(defun renerd-icons-dired--ext-cache ()
  "Return the per-extension icon cache for `nerd-icons-icon-for-file'."
  (unless (and renerd-icons-dired--ext-cache
               (eq renerd-icons-dired--ext-cache-source
                   nerd-icons-extension-icon-alist))
    (setq renerd-icons-dired--ext-cache (make-hash-table :test #'equal)
          renerd-icons-dired--ext-cache-source
          nerd-icons-extension-icon-alist))
  renerd-icons-dired--ext-cache)

(defun renerd-icons-dired--nerd-file-icon (name)
  "Return the `nerd-icons-icon-for-file' icon string for NAME.
This mirrors `nerd-icons-icon-for-file' with only the `:height' override:
names that might match `nerd-icons-regexp-icon-alist' are delegated to the
real function; the rest resolve through the per-extension cache."
  (let ((base (file-name-nondirectory name)))
    (renerd-icons-dired--special-regexps)
    (if (let ((case-fold-search nil))
          (condition-case nil
              (or (and renerd-icons-dired--special-anchored
                       (string-match-p renerd-icons-dired--special-anchored
                                       base))
                  (and renerd-icons-dired--special-unanchored
                       (string-match-p renerd-icons-dired--special-unanchored
                                       base)))
            ;; A malformed regexp would fail inside the real function too;
            ;; delegate instead of guessing.
            (error t)))
        ;; Possibly special: exact behavior lives in the real function.
        (nerd-icons-icon-for-file name :height renerd-icons-dired-icon-size)
      (let* ((ext (file-name-extension base))
             (key (and ext (downcase ext)))
             (cache (renerd-icons-dired--ext-cache))
             (entry (and key (gethash key cache))))
        (if (and entry (eql (car entry) renerd-icons-dired-icon-size))
            (cdr entry)
          (let* ((icon (or (and key
                                (cdr (assoc key
                                            nerd-icons-extension-icon-alist)))
                           nerd-icons-default-file-icon))
                 ;; (apply (car icon) (append (list (car args)) overrides
                 ;;                           (cdr args))), like the original.
                 (args (cdr icon))
                 (string (apply (car icon)
                                (append (list (car args))
                                        (list :height
                                              renerd-icons-dired-icon-size)
                                        (cdr args)))))
            (when key
              (puthash key
                       (cons renerd-icons-dired-icon-size string)
                       cache))
            string))))))

(defvar-local renerd-icons-dired--timer nil)
(defvar-local renerd-icons-dired--generation 0)
;; (GENERATION TICK START END): the contiguous buffer range already fully
;; annotated at that generation and `buffer-modified-tick'.  The range grows
;; as windows scroll, so work done once is never rediscovered while the
;; buffer text stays unchanged.
(defvar-local renerd-icons-dired--coverage nil)

(defun renerd-icons-dired-clear-cache ()
  "Clear global icon string caches.
Call this after changing icon functions, icon size, theme-dependent icon faces,
or file-type rules."
  (interactive)
  (clrhash renerd-icons-dired--file-cache)
  (clrhash renerd-icons-dired--dir-cache)
  (setq renerd-icons-dired--special-anchored nil
        renerd-icons-dired--special-unanchored nil
        renerd-icons-dired--special-regexp-source :none
        renerd-icons-dired--ext-cache nil
        renerd-icons-dired--ext-cache-source nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p renerd-icons-dired-mode)
        (renerd-icons-dired-refresh t)))))

(defun renerd-icons-dired--icon (name directory-p special-p)
  "Return the propertized prefix string for NAME.
DIRECTORY-P selects the directory icon function.  SPECIAL-P is non-nil for
`.' and `..', whose visible prefix intentionally matches nerd-icons-dired."
  (if special-p
      (propertize renerd-icons-dired-special-prefix-string
                  'face 'renerd-icons-dired-overlay-face)
    (let* ((function (if directory-p
                         renerd-icons-dired-dir-icon-function
                       renerd-icons-dired-file-icon-function))
           ;; Directory icons can depend on absolute path state (.git,
           ;; symlink, remote).  File icons in nerd-icons depend on basename
           ;; and extension; custom functions may still inspect the full name,
           ;; so cache by the provided Dired name rather than only extension.
           (key (if directory-p (expand-file-name name default-directory) name))
           (table (if directory-p renerd-icons-dired--dir-cache
                    renerd-icons-dired--file-cache))
           (entry (gethash key table)))
      ;; Entries carry (FUNCTION SIZE INFIX) so that rebinding the relevant
      ;; options, even dynamically, invalidates stale icons without a
      ;; generation bump.
      (if (and entry
               (let ((sig (car entry)))
                 (and (eq (car sig) function)
                      (eql (cadr sig) renerd-icons-dired-icon-size)
                      (equal (caddr sig)
                             renerd-icons-dired-infix-string))))
          (cdr entry)
        (let ((string (propertize
                       (concat (if (and renerd-icons-dired-fast-file-icons
                                        (not directory-p)
                                        (eq function #'nerd-icons-icon-for-file))
                                   (renerd-icons-dired--nerd-file-icon name)
                                 (funcall function name
                                          :height renerd-icons-dired-icon-size))
                               renerd-icons-dired-infix-string)
                       'face 'renerd-icons-dired-overlay-face)))
          (puthash key
                   (cons (list function
                               renerd-icons-dired-icon-size
                               renerd-icons-dired-infix-string)
                         string)
                   table)
          string)))))

(defun renerd-icons-dired--dir-p (name)
  "Return non-nil when the entry on the current line is a directory.
Point must be on the file's line.  Uses the ls type column and falls back to
`file-directory-p' for symlinks and unrecognized line formats; NAME is the
Dired-relative file name used for that fallback."
  ;; Dired inserts a two-character "<mark><space>" prefix before the ls
  ;; mode string, so the type char is column 2.  Listings without the
  ;; prefix (e.g. ls-lisp) have a mode char in column 1, never a space.
  (let* ((bol (line-beginning-position))
         (type (char-after (+ bol (if (eq (char-after (1+ bol)) ?\s)
                                      2
                                    0)))))
    (cond
     ((eq type ?d) t)
     ((memq type '(?- ?b ?c ?p ?s)) nil)
     (t (file-directory-p name)))))

(defun renerd-icons-dired--resolve-name (pos end flat)
  "Return the Dired-relative file name between POS and END.
FLAT means the buffer has a single subdirectory listing.  Quoted or escaped
names are resolved through `dired-get-filename' instead."
  (let ((raw (buffer-substring-no-properties pos (min end (point-max)))))
    (if (string-match-p "[\\\"]" raw)
        (save-excursion
          (goto-char pos)
          (dired-get-filename 'relative 'noerror))
      (if (or flat (file-name-absolute-p raw))
          raw
        (concat (or (dired-current-directory t) "") raw)))))

(defun renerd-icons-dired--overlay-map (start end)
  "Return a hash table mapping filename position to renerd overlay."
  (let ((map (make-hash-table :test #'eql)))
    (dolist (overlay (overlays-in start end))
      (when (overlay-get overlay 'renerd-icons-dired-overlay)
        (puthash (overlay-end overlay) overlay map)))
    map))

(defun renerd-icons-dired--annotate-at (pos name map generation)
  "Place the icon overlay for NAME at POS.
MAP maps positions to existing overlays; GENERATION marks fresh overlays.
Point must already be at POS."
  (let* ((special (member name '("." "..")))
         (directory-p (and (not special)
                           (renerd-icons-dired--dir-p name)))
         (icon (renerd-icons-dired--icon name directory-p special))
         (beg (max (point-min) (1- pos)))
         (overlay (gethash pos map)))
    (if overlay
        (move-overlay overlay beg pos (current-buffer))
      (setq overlay (make-overlay beg pos (current-buffer) nil t)))
    (overlay-put overlay 'renerd-icons-dired-overlay t)
    (overlay-put overlay 'renerd-icons-dired-generation generation)
    (overlay-put overlay 'after-string icon)
    overlay))

(defun renerd-icons-dired--annotate-region (start end generation limit)
  "Annotate file entries between START and END for GENERATION.
At most LIMIT entries are annotated; nil LIMIT means no bound.
Returns (COUNT . COMPLETE), where COMPLETE is non-nil when the scan reached
END without hitting LIMIT or pending input."
  (save-excursion
    (setq start (max start (point-min))
          end (min end (point-max)))
    (let ((map (renerd-icons-dired--overlay-map start end))
          (flat (null (cdr dired-subdir-alist)))
          (count 0)
          (pos start)
          seen)
      (while (and (< pos end)
                  (or (null limit) (< count limit))
                  (or (eql 0 (logand count 15))
                      (not (input-pending-p))))
        (if (not (get-text-property pos 'dired-filename))
            (setq pos (or (next-single-property-change
                           pos 'dired-filename nil end)
                          end))
          (setq seen t)
          (let ((fend (or (next-single-property-change pos 'dired-filename)
                          (point-max))))
            (when (or (= pos (point-min))
                      (not (get-text-property (1- pos) 'dired-filename)))
              ;; POS is the start of a file name.
              (let ((overlay (gethash pos map)))
                (unless (and overlay
                             (eql (overlay-get
                                   overlay 'renerd-icons-dired-generation)
                                  generation))
                  (goto-char pos)
                  (when-let* ((name (renerd-icons-dired--resolve-name
                                     pos fend flat)))
                    (renerd-icons-dired--annotate-at pos name map generation)
                    (cl-incf count)))))
            (setq pos fend))))
      (if seen
          (cons count (>= pos end))
        ;; No `dired-filename' properties: nonstandard buffer, use the
        ;; line-oriented fallback.
        (renerd-icons-dired--annotate-region-fallback
         start end generation limit)))))

(defun renerd-icons-dired--annotate-region-fallback (start end generation limit)
  "Annotate entries between START and END without `dired-filename' properties.
See `renerd-icons-dired--annotate-region' for the return value."
  (save-excursion
    (let ((map (renerd-icons-dired--overlay-map start end))
          (count 0))
      (goto-char start)
      (beginning-of-line)
      (while (and (< (point) end)
                  (or (null limit) (< count limit))
                  (not (input-pending-p)))
        (when-let* ((pos (dired-move-to-filename nil)))
          (let ((overlay (gethash pos map)))
            (unless (and overlay
                         (eql (overlay-get
                               overlay 'renerd-icons-dired-generation)
                              generation))
              (when-let* ((name (dired-get-filename 'relative 'noerror)))
                (renerd-icons-dired--annotate-at pos name map generation)
                (cl-incf count)))))
        (forward-line 1))
      (cons count (>= (point) end)))))

(defun renerd-icons-dired--prefetch-bounds (window)
  "Return (START . END) of the look-ahead region around WINDOW."
  (let ((start (window-start window))
        (end (or (window-end window) (point-max))))
    (save-excursion
      (goto-char start)
      (forward-line (- renerd-icons-dired-prefetch-lines))
      (setq start (point))
      (goto-char end)
      (forward-line renerd-icons-dired-prefetch-lines)
      (cons start (point)))))

(defun renerd-icons-dired--coverage-fresh-p (coverage generation tick)
  "Return non-nil when COVERAGE is valid for GENERATION and TICK."
  (and coverage
       (eql (car coverage) generation)
       (eql (cadr coverage) tick)))

(defun renerd-icons-dired--covered-p (generation tick start end)
  "Return non-nil when START..END is already fully annotated."
  (and (renerd-icons-dired--coverage-fresh-p
        renerd-icons-dired--coverage generation tick)
       (>= start (nth 2 renerd-icons-dired--coverage))
       (<= end (nth 3 renerd-icons-dired--coverage))))

(defun renerd-icons-dired--cover (generation tick start end)
  "Extend coverage to include START..END at GENERATION and TICK."
  (let ((coverage renerd-icons-dired--coverage))
    (if (and (renerd-icons-dired--coverage-fresh-p coverage generation tick)
             (<= start (nth 3 coverage))
             (>= end (nth 2 coverage)))
        ;; Same epoch and the ranges touch or overlap: coverage stays
        ;; contiguous, so it can be merged.
        (setf (nth 2 coverage) (min start (nth 2 coverage))
              (nth 3 coverage) (max end (nth 3 coverage)))
      (setq renerd-icons-dired--coverage (list generation tick start end)))))

(defun renerd-icons-dired--annotate-uncovered (start end generation tick limit)
  "Annotate the parts of START..END outside the current coverage.
TICK is the current `buffer-modified-tick'.  Returns (COUNT . COMPLETE)
like `renerd-icons-dired--annotate-region'."
  (if (not (renerd-icons-dired--coverage-fresh-p
            renerd-icons-dired--coverage generation tick))
      (renerd-icons-dired--annotate-region start end generation limit)
    (let* ((coverage renerd-icons-dired--coverage)
           (cstart (nth 2 coverage))
           (cend (nth 3 coverage))
           (lo (if (< start cstart)
                   (renerd-icons-dired--annotate-region
                    start (min end cstart) generation limit)
                 (cons 0 t)))
           (count (car lo)))
      (if (or (not (cdr lo))
              (and limit (>= count limit)))
          (cons count (cdr lo))
        (let ((hi (if (> end cend)
                      (renerd-icons-dired--annotate-region
                       (max start cend) end generation
                       (and limit (- limit count)))
                    (cons 0 t))))
          (cons (+ count (car hi)) (cdr hi)))))))

(defun renerd-icons-dired--update-window (window)
  "Annotate WINDOW now and arrange look-ahead annotation at idle time."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             renerd-icons-dired-mode)
    (let* ((generation renerd-icons-dired--generation)
           (tick (buffer-modified-tick))
           (start (window-start window))
           (end (or (window-end window) (point-max))))
      (unless (renerd-icons-dired--covered-p generation tick start end)
        ;; Visible lines are cheap and should appear immediately.  Only the
        ;; parts outside the covered range need work.
        (renerd-icons-dired--annotate-uncovered start end generation tick nil)
        (renerd-icons-dired--cover generation tick start end))
      (pcase-let ((`(,pstart . ,pend)
                   (renerd-icons-dired--prefetch-bounds window)))
        (unless (renerd-icons-dired--covered-p generation tick pstart pend)
          (renerd-icons-dired--schedule))))))

(defun renerd-icons-dired--idle (buffer generation)
  "Annotate look-ahead regions of BUFFER's windows for GENERATION."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq renerd-icons-dired--timer nil)
      (when (and renerd-icons-dired-mode
                 (= generation renerd-icons-dired--generation))
        (let ((budget renerd-icons-dired-chunk-size)
              (tick (buffer-modified-tick))
              pending)
          (dolist (window (get-buffer-window-list nil nil t))
            (when (and (window-live-p window) (> budget 0))
              (pcase-let ((`(,pstart . ,pend)
                           (renerd-icons-dired--prefetch-bounds window)))
                (unless (renerd-icons-dired--covered-p
                         generation tick pstart pend)
                  (let ((result (renerd-icons-dired--annotate-uncovered
                                 pstart pend generation tick budget)))
                    (cl-decf budget (car result))
                    (if (cdr result)
                        (renerd-icons-dired--cover
                         generation tick pstart pend)
                      (setq pending t)))))))
          (when pending
            (renerd-icons-dired--schedule)))))))

(defun renerd-icons-dired--schedule ()
  "Schedule idle look-ahead annotation for the current buffer."
  (unless (timerp renerd-icons-dired--timer)
    (setq renerd-icons-dired--timer
          (run-with-idle-timer
           renerd-icons-dired-idle-delay nil
           #'renerd-icons-dired--idle
           (current-buffer) renerd-icons-dired--generation))))

(defun renerd-icons-dired--update-windows ()
  "Update every live window displaying the current buffer."
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (renerd-icons-dired--update-window window)))

(defun renerd-icons-dired--post-command ()
  "Post-command hook that fills icons after scroll/navigation commands."
  (when renerd-icons-dired-mode
    (renerd-icons-dired--update-windows)))

(defun renerd-icons-dired--window-scroll (window _start)
  "Window scroll hook for WINDOW."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when renerd-icons-dired-mode
        (renerd-icons-dired--update-window window)))))

(defun renerd-icons-dired--after-change (beg end _length)
  "Drop icon overlays touched by the text change at BEG..END.
Overlay positions alone cannot detect file-name edits, so any change inside
or right next to an overlay forces re-annotation on the next window update."
  (dolist (overlay (overlays-in (max (point-min) (- beg 2))
                                (min (point-max) (+ end 1))))
    (when (overlay-get overlay 'renerd-icons-dired-overlay)
      (delete-overlay overlay))))

(defun renerd-icons-dired--remove-overlays (&optional all)
  "Remove stale renerd overlays.
When ALL is nil, remove overlays from generations older than the current one."
  (if all
      (remove-overlays (point-min) (point-max)
                       'renerd-icons-dired-overlay t)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (and (overlay-get overlay 'renerd-icons-dired-overlay)
                 (/= (or (overlay-get overlay 'renerd-icons-dired-generation)
                         -1)
                     renerd-icons-dired--generation))
        (delete-overlay overlay)))))

(defun renerd-icons-dired--reset ()
  "Reset buffer-local state and reschedule icons."
  (when (timerp renerd-icons-dired--timer)
    (cancel-timer renerd-icons-dired--timer))
  (setq renerd-icons-dired--timer nil
        renerd-icons-dired--coverage nil)
  (cl-incf renerd-icons-dired--generation)
  (renerd-icons-dired--remove-overlays t)
  (renerd-icons-dired--update-windows))

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
    (renerd-icons-dired--update-windows)))

(defun renerd-icons-dired--after-readin ()
  "Handle Dired buffer replacement after revert/readin."
  (when renerd-icons-dired-mode
    (renerd-icons-dired--reset)))

(defun renerd-icons-dired--teardown ()
  "Disable all buffer-local resources."
  (when (timerp renerd-icons-dired--timer)
    (cancel-timer renerd-icons-dired--timer))
  (setq renerd-icons-dired--timer nil
        renerd-icons-dired--coverage nil)
  (remove-hook 'post-command-hook #'renerd-icons-dired--post-command t)
  (remove-hook 'window-scroll-functions #'renerd-icons-dired--window-scroll t)
  (remove-hook 'dired-after-readin-hook
               #'renerd-icons-dired--after-readin t)
  (remove-hook 'after-change-functions
               #'renerd-icons-dired--after-change t)
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
        (add-hook 'post-command-hook #'renerd-icons-dired--post-command nil t)
        (add-hook 'window-scroll-functions
                  #'renerd-icons-dired--window-scroll nil t)
        (add-hook 'dired-after-readin-hook
                  #'renerd-icons-dired--after-readin nil t)
        (add-hook 'after-change-functions
                  #'renerd-icons-dired--after-change nil t)
        (renerd-icons-dired--reset))
    (renerd-icons-dired--teardown)))

;;;###autoload
(defun renerd-icons-dired-enable ()
  "Enable `renerd-icons-dired-mode' in Dired buffers."
  (when (derived-mode-p 'dired-mode)
    (renerd-icons-dired-mode 1)))

(provide 'renerd-icons-dired)
;;; renerd-icons-dired.el ends here
