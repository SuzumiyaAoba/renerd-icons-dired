;;; bench-renerd-icons-dired.el --- Synthetic Dired icon benchmark -*- lexical-binding: t; -*-

(setq native-comp-jit-compilation nil)
(require 'cl-lib)
(require 'benchmark)
(require 'dired)
(require 'renerd-icons-dired)
;; Compare Dired overlay work, not first Nerd Icons data loading.
(nerd-icons-icon-for-file "warmup.el" :height renerd-icons-dired-icon-size)
(nerd-icons-icon-for-dir default-directory :height renerd-icons-dired-icon-size)

(defvar renerd-benchmark-files '(1000 10000))
(defvar renerd-benchmark-old-library "nerd-icons-dired")

(defun renerd-benchmark--make-directory (files)
  (let ((directory (make-temp-file "renerd-bench-" t)))
    (dotimes (i files)
      (write-region "" nil (expand-file-name (format "file-%05d.el" i) directory)
                    nil 'silent))
    directory))

(defun renerd-benchmark--overlay-count (property)
  (cl-count-if (lambda (overlay) (overlay-get overlay property))
               (overlays-in (point-min) (point-max))))

(defun renerd-benchmark--visible-end ()
  (save-excursion
    (goto-char (point-min))
    (forward-line 40)
    (point)))

(defun renerd-benchmark--measure-renerd (directory files)
  (let (buffer enable refresh overlays queue)
    (unwind-protect
        (save-window-excursion
          (let ((dired-mode-hook nil))
            (setq buffer (dired-noselect directory)))
          (switch-to-buffer buffer)
          (let ((visible-end (renerd-benchmark--visible-end))
                (renerd-icons-dired-prefetch-lines 80)
                (renerd-icons-dired-chunk-size 300))
            (garbage-collect)
            (setq enable
                  (benchmark-run 1
                    (cl-letf (((symbol-function 'window-end)
                               (lambda (&rest _) visible-end)))
                      (renerd-icons-dired-mode 1))))
            (setq overlays (renerd-benchmark--overlay-count
                            'renerd-icons-dired-overlay)
                  queue (length renerd-icons-dired--queue))
            (garbage-collect)
            (setq refresh
                  (benchmark-run 10
                    (cl-letf (((symbol-function 'window-end)
                               (lambda (&rest _) visible-end)))
                      (renerd-icons-dired-refresh))))
            `((backend . "renerd-icons-dired")
              (files . ,files)
              (enable_seconds . ,(car enable))
              (refresh10_seconds . ,(car refresh))
              (overlays_after_enable . ,overlays)
              (queued_after_enable . ,queue))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun renerd-benchmark--measure-old (directory files)
  (let (buffer enable refresh overlays)
    (if (not (require (intern renerd-benchmark-old-library) nil t))
        `((backend . "nerd-icons-dired") (files . ,files) (missing . t))
      (unwind-protect
          (save-window-excursion
            (let ((dired-mode-hook nil))
              (setq buffer (dired-noselect directory)))
            (switch-to-buffer buffer)
            (garbage-collect)
            (setq enable (benchmark-run 1 (nerd-icons-dired-mode 1)))
            (setq overlays (renerd-benchmark--overlay-count
                            'nerd-icons-dired-overlay))
            (garbage-collect)
            (setq refresh (benchmark-run 10 (nerd-icons-dired--refresh)))
            `((backend . "nerd-icons-dired")
              (files . ,files)
              (enable_seconds . ,(car enable))
              (refresh10_seconds . ,(car refresh))
              (overlays_after_enable . ,overlays)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun renerd-benchmark-run ()
  (let (results)
    (dolist (files renerd-benchmark-files)
      (let ((directory (renerd-benchmark--make-directory files)))
        (unwind-protect
            (progn
              (push (renerd-benchmark--measure-renerd directory files) results)
              (push (renerd-benchmark--measure-old directory files) results))
          (delete-directory directory t))))
    (require 'json)
    (princ (json-encode (nreverse results)))
    (terpri)))

(renerd-benchmark-run)
;;; bench-renerd-icons-dired.el ends here
