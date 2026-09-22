;;; renerd-icons-dired-test.el --- Tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'dired)
(require 'subr-x)
(require 'renerd-icons-dired)

(defvar renerd-test--icon-calls 0)

(defun renerd-test--counted-icon (&rest _)
  (cl-incf renerd-test--icon-calls)
  "F")

(defmacro renerd-test--dired (files &rest body)
  "Create a Dired buffer with FILES and run BODY there."
  (declare (indent 1) (debug t))
  `(let ((directory (make-temp-file "renerd-dired-" t)))
     (unwind-protect
         (progn
           (dolist (file ,files)
             (if (string-suffix-p "/" file)
                 (make-directory (expand-file-name (string-remove-suffix "/" file) directory))
               (write-region "" nil (expand-file-name file directory) nil 'silent)))
           (save-window-excursion
             (let ((buffer (dired-noselect directory)))
               (unwind-protect
                   (with-current-buffer buffer
                     (switch-to-buffer buffer)
                     ,@body)
                 (when (buffer-live-p buffer) (kill-buffer buffer))))))
       (delete-directory directory t))))

(defun renerd-test--overlays ()
  (cl-remove-if-not (lambda (overlay)
                      (overlay-get overlay 'renerd-icons-dired-overlay))
                    (overlays-in (point-min) (point-max))))

(defun renerd-test--drain ()
  "Run the idle worker until no look-ahead work remains."
  (let ((limit 100))
    (while (and renerd-icons-dired--timer (> limit 0))
      (setq limit (1- limit))
      (renerd-icons-dired--idle (current-buffer)
                                renerd-icons-dired--generation))
    (should-not renerd-icons-dired--timer)))

(ert-deftest renerd-icons-dired-visible-first-and-queue ()
  (renerd-test--dired (cl-loop for i below 500 collect (format "file-%04d.el" i))
    (let ((renerd-icons-dired-prefetch-lines 5)
          (renerd-icons-dired-chunk-size 20)
          (renerd-icons-dired-file-icon-function (lambda (&rest _) "F"))
          (renerd-icons-dired-dir-icon-function (lambda (&rest _) "D")))
      (goto-char (point-min))
      (forward-line 30)
      (let ((visible-end (point)))
        (goto-char (point-min))
        (cl-letf (((symbol-function 'window-end) (lambda (&rest _) visible-end)))
          (renerd-icons-dired-mode 1)
          (let ((initial (length (renerd-test--overlays))))
            (should (> initial 0))
            (should (< initial 500))
            ;; Look-ahead work remains scheduled.
            (should renerd-icons-dired--timer)
            (renerd-test--drain)
            ;; Only visible + prefetch region should have been annotated,
            ;; not all 500.
            (should (< (length (renerd-test--overlays)) 500))))))))

(ert-deftest renerd-icons-dired-refresh-reuses-overlays ()
  (renerd-test--dired '("a.el" "b.el" "dir/")
    (let ((renerd-icons-dired-file-icon-function (lambda (&rest _) "F"))
          (renerd-icons-dired-dir-icon-function (lambda (&rest _) "D")))
      (renerd-icons-dired-mode 1)
      (renerd-test--drain)
      (let ((before (renerd-test--overlays)))
        (should before)
        (renerd-icons-dired-refresh)
        (renerd-test--drain)
        (should (equal before (renerd-test--overlays)))))))

(ert-deftest renerd-icons-dired-special-and-directory-prefixes ()
  (renerd-test--dired '("a.el" "dir/")
    (let ((renerd-icons-dired-file-icon-function (lambda (&rest _) "F"))
          (renerd-icons-dired-dir-icon-function (lambda (&rest _) "D"))
          (renerd-icons-dired-special-prefix-string "S\t")
          (renerd-icons-dired-infix-string "|"))
      (renerd-icons-dired-mode 1)
      (renerd-test--drain)
      (goto-char (point-min))
      (dired-goto-file default-directory)
      (let ((prefixes (mapcar (lambda (overlay)
                                (substring-no-properties
                                 (overlay-get overlay 'after-string)))
                              (renerd-test--overlays))))
        (should (member "S\t" prefixes))
        (should (member "F|" prefixes))
        (should (member "D|" prefixes))))))

(ert-deftest renerd-icons-dired-cache-and-clear ()
  (renerd-test--dired '("a.el" "b.el")
    (let ((renerd-test--icon-calls 0)
          (renerd-icons-dired-file-icon-function #'renerd-test--counted-icon)
          (renerd-icons-dired-dir-icon-function (lambda (&rest _) "D")))
      (clrhash renerd-icons-dired--file-cache)
      (renerd-icons-dired-mode 1)
      (renerd-test--drain)
      (let ((first renerd-test--icon-calls))
        (should (> first 0))
        (renerd-icons-dired-refresh t)
        (renerd-test--drain)
        (should (= renerd-test--icon-calls first))
        (renerd-icons-dired-clear-cache)
        (renerd-test--drain)
        (should (> renerd-test--icon-calls first))))))

(ert-deftest renerd-icons-dired-icon-face-preserved ()
  (renerd-test--dired '("a.el")
    ;; The icon function's `face' carries the icon color and font family;
    ;; the overlay face must apply only to the padding we insert, and the
    ;; `display' spec keeps those faces effective on the overlay string.
    (let ((renerd-icons-dired-file-icon-function
           (lambda (&rest _)
             (propertize "I" 'face 'error 'font-lock-face 'error)))
          (renerd-icons-dired-infix-string "|"))
      (renerd-icons-dired-mode 1)
      (renerd-test--drain)
      (goto-char (point-min))
      (dired-goto-file (expand-file-name "a.el" default-directory))
      (let* ((overlay (car (overlays-in (1- (point)) (point))))
             (string (overlay-get overlay 'after-string))
             (shown (get-text-property 0 'display string)))
        (should (equal (substring-no-properties shown) "I|"))
        (should (eq (get-text-property 0 'face shown) 'error))
        (should (eq (get-text-property 1 'face shown)
                    'renerd-icons-dired-overlay-face))))))

(ert-deftest renerd-icons-dired-teardown-removes-resources ()
  (renerd-test--dired '("a.el" "b.el")
    (renerd-icons-dired-mode 1)
    (should (renerd-test--overlays))
    (should (memq #'renerd-icons-dired--post-command post-command-hook))
    (renerd-icons-dired-mode -1)
    (should-not (renerd-test--overlays))
    (should-not renerd-icons-dired--timer)
    (should-not renerd-icons-dired--coverage)
    (should-not (memq #'renerd-icons-dired--post-command post-command-hook))))

(ert-deftest renerd-icons-dired-no-whole-buffer-work-on-enable ()
  (renerd-test--dired (cl-loop for i below 10000 collect (format "file-%05d.el" i))
    (let ((renerd-icons-dired-prefetch-lines 20)
          (renerd-icons-dired-file-icon-function (lambda (&rest _) "F")))
      (goto-char (point-min))
      (forward-line 30)
      (let ((visible-end (point)))
        (goto-char (point-min))
        (cl-letf (((symbol-function 'window-end) (lambda (&rest _) visible-end)))
          (renerd-icons-dired-mode 1)))
      (let ((seen (length (renerd-test--overlays))))
        ;; Batch window is 24-ish lines; only the visible slice is annotated
        ;; synchronously, so this must stay far below the directory size.
        (should (< seen 200))))))

(ert-deftest renerd-icons-dired-edit-invalidates-icon ()
  (renerd-test--dired '("a.el" "b.el")
    (let ((renerd-icons-dired-file-icon-function (lambda (&rest _) "F"))
          (renerd-icons-dired-dir-icon-function (lambda (&rest _) "D")))
      (renerd-icons-dired-mode 1)
      (renerd-test--drain)
      (let ((before (length (renerd-test--overlays))))
        ;; Editing a line drops its overlay; the next update re-annotates it.
        (goto-char (point-min))
        (forward-line 3)
        (let ((inhibit-read-only t))
          (insert " ")
          (delete-char -1))
        (renerd-icons-dired--update-windows)
        (renerd-test--drain)
        (should (= before (length (renerd-test--overlays))))))))

(ert-deftest renerd-icons-dired-fast-path-matches-real-function ()
  ;; The dispatch tables must produce exactly the same icons as plain
  ;; `nerd-icons-icon-for-file': special names delegate to it, ordinary
  ;; names take the extension cache.  The corpus exercises every bucket
  ;; class: literal prefixes, literal suffixes, the anchored fallback
  ;; regexp (group/optional prefixes), and plain names.
  (let ((names '("file.el" "TAGS" "TODO" "LICENSE" "LICENSE.txt" "readme"
                 "readme.md" "README" "COPYING" "NEWS" "ChangeLog" "INSTALL"
                 "Makefile.am" "Makefile.in" "Makefile" "GNUmakefile"
                 "configure" "configure.ac" "config.guess" "code-of-conduct"
                 "MAINTAINERS" "CONTRIBUTE" "BUGS" "ar-lib" "depmond"
                 "install-sh" "missing" "mkdep" "mkinstalldirs"
                 "move-if-change" "symlink-tree" "test-driver" "ylwrap"
                 ".editorconfig" ".env" ".env.local" ".env.dev.local"
                 ".envrc" ".envx" ".environment"
                 "nginx" "xnginx" "nginx.conf" "apache" "myapache"
                 "backup~" "~" "x~"
                 "test.rb" "_test.rb" "xtest.rb" "test_helper.rb"
                 "xtest_helper.rb" "spec.rb" "_spec.rb" "spec_helper.rb"
                 "spec.ts" "-spec.ts" "xspec.ts" "test.ts" "-test.ts"
                 "spec.js" "-spec.js" "test.js" "-test.js"
                 "spec.jsx" "-spec.jsx" "test.jsx" "-test.jsx"
                 ".npmignore" "x.npmignore" "npmignore"
                 "Jenkinsfile" "xJenkinsfile" "Cask" "xCask" "Eask" "xEask"
                 "babel.config.js" "xbabel.config.js"
                 "CMakeLists.txt" "CMakeCache.txt" "meson.build"
                 "meson_options.txt" "stack.yaml.json" "serverless.yml"
                 "mix.lock" "Gemfile" "Gemfile.lock" "xGemfile.lock"
                 "Podfile" "Dangerfile" "Appfile" "Matchfile"
                 ".dockerignore" "Dockerfile" "Containerfile" ".Dockerfile"
                 "xDockerfile" "docker-compose.yml" "compose.yml"
                 "compose.yaml" "docker-compose.gitlab.yml"
                 "Brewfile" "PKGBUILD" ".SRCINFO" "go.mod" "go.work"
                 "xgo.mod" "Cargo.toml" "Cargo.lock" "flake.lock"
                 "MERGE_HEAD" "COMMIT_EDITMSG" ".gitlab-ci.yml"
                 ".gitlab-ci.yaml" "stylelint" "stylelint.config.js"
                 "package.json" "package.lock.json" "yarn.lock"
                 "bower.json" "gulpfile" "gulpfile.js" "gruntfile"
                 "webpack" "webpack.config.js"
                 ".eslint" "eslint" "eslint.config.js" ".eslintrc" "xeslint"
                 ".prettier" "prettier" ".prettierrc" ".jest" "jest"
                 "jest.config.js" "vite.config" "vite.config.ts" "vitest"
                 "bookmark" "bookmarks" "xbookmark"
                 "*scratch*" "*scratchpad*" "*new-tab*" "*new-tab*x"
                 "normal.txt" "foo.c" "archive.tar.gz" "script" "noext"
                 ".hidden" "file." "a.b.c" ".x" "..x" "trailing~x")))
    (dolist (name names)
      (should
       (equal (renerd-icons-dired--nerd-file-icon name)
              (nerd-icons-icon-for-file
               name :height renerd-icons-dired-icon-size))))))

(ert-deftest renerd-icons-dired-incomplete-scan-not-covered ()
  ;; If pending input aborts the visible scan, coverage must not claim the
  ;; range: the rest has to be annotated later.
  (renerd-test--dired (cl-loop for i below 100 collect (format "file-%03d.el" i))
    (let ((renerd-icons-dired-file-icon-function (lambda (&rest _) "F"))
          (renerd-icons-dired-dir-icon-function (lambda (&rest _) "D")))
      (renerd-icons-dired-mode 1)
      (renerd-icons-dired--remove-overlays t)
      (setq renerd-icons-dired--coverage nil)
      (cl-letf (((symbol-function 'input-pending-p) (lambda () t)))
        (renerd-icons-dired--update-windows))
      (should-not renerd-icons-dired--coverage)
      ;; The incomplete scan left a rescheduled idle timer that finishes
      ;; the remaining entries once input settles.
      (should renerd-icons-dired--timer)
      (renerd-test--drain)
      (should renerd-icons-dired--coverage))))

(ert-deftest renerd-icons-dired-symlink-uses-target-type ()
  (renerd-test--dired '("real-dir/" "a.el")
    (make-symbolic-link "real-dir"
                        (expand-file-name "dir-link" default-directory) t)
    (revert-buffer)
    (let ((renerd-icons-dired-file-icon-function (lambda (&rest _) "F"))
          (renerd-icons-dired-dir-icon-function (lambda (&rest _) "D"))
          (renerd-icons-dired-infix-string "|"))
      (renerd-icons-dired-mode 1)
      (renerd-test--drain)
      ;; A symlink to a directory resolves through file-directory-p and must
      ;; get the directory icon.
      (goto-char (point-min))
      (let ((prefix nil))
        (while (< (point) (point-max))
          (when (equal (and (dired-move-to-filename nil)
                            (dired-get-filename 'relative 'noerror))
                       "dir-link")
            (dolist (overlay (overlays-in (1- (point)) (point)))
              (when (overlay-get overlay 'renerd-icons-dired-overlay)
                (setq prefix (substring-no-properties
                              (overlay-get overlay 'after-string))))))
          (forward-line 1))
        (should (equal prefix "D|"))))))

;;; renerd-icons-dired-test.el ends here
