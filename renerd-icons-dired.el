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
instead of calling the icon function for every file name.  Names that may
match - decided by literal prefix/suffix dispatch tables built from the
alist - still go through the real function, so results are identical to
plain `nerd-icons-icon-for-file'."
  :type 'boolean
  :group 'renerd-icons-dired)

(defface renerd-icons-dired-overlay-face
  '((t :inherit default))
  "Base face for inserted icon prefixes."
  :group 'renerd-icons-dired)

(defvar renerd-icons-dired-mode nil)

(defvar renerd-icons-dired--file-cache (make-hash-table :test #'equal))
(defvar renerd-icons-dired--dir-cache (make-hash-table :test #'equal))

;; Fast path state for the default `nerd-icons-icon-for-file': dispatch
;; tables deciding whether a name can be special, plus a per-extension hash
;; of the resulting icon string.
;;
;; Every entry of `nerd-icons-regexp-icon-alist' is classified once:
;;
;; - `^'- or `\`'-anchored regexps contribute their required literal PREFIX,
;;   bucketed by its first character in `renerd-icons-dired--prefix-table'.
;; - Other `$'- or `\''-terminated regexps contribute their required literal
;;   SUFFIX, bucketed by its last character in
;;   `renerd-icons-dired--suffix-table'.
;; - Anything that yields no usable prefix/suffix (empty literal run, a
;;   top-level `\|', a non-string key, ...) goes into one of two fallback
;;   regexps, `renerd-icons-dired--anchored-rest' and
;;   `renerd-icons-dired--unanchored-rest', which are still far smaller than
;;   the original alist.
;;
;; A file name can match the alist only if it starts with a prefix from its
;; first-character bucket, ends with a suffix from its last-character
;; bucket, or matches a fallback regexp.  Extraction is deliberately
;; conservative: any construct that cannot be resolved shortens the literal
;; run instead of guessing, so the gates can only produce false positives
;; (a wasted delegation to `nerd-icons-icon-for-file'), never false
;; negatives.
(defvar renerd-icons-dired--prefix-table nil)
(defvar renerd-icons-dired--anchored-rest nil)
(defvar renerd-icons-dired--suffix-table nil)
(defvar renerd-icons-dired--unanchored-rest nil)
(defvar renerd-icons-dired--special-regexp-source :none)
(defvar renerd-icons-dired--ext-cache nil)
(defvar renerd-icons-dired--ext-cache-source nil)
(defvar renerd-icons-dired--ext-alist-hash nil)
(defvar renerd-icons-dired--ext-alist-hash-source nil)

(defun renerd-icons-dired--escaped-literal-p (char)
  "Return non-nil when the regexp escape \\CHAR denotes literal CHAR.
Structural escapes (groups, alternation, assertions, syntax classes,
back-references) use letters, digits, or a small set of punctuation, so
any other escaped character is an ordinary literal."
  (not (or (<= ?a char ?z)
           (<= ?A char ?Z)
           (<= ?0 char ?9)
           (memq char '(?\( ?\) ?\{ ?\} ?\| ?= ?< ?> ?` ?' ?_)))))

(defun renerd-icons-dired--literal-prefix (regexp)
  "Return the longest string every match of REGEXP must begin with.
Returns nil when no required literal prefix can be determined.  The scan
stops at the first construct that may match something other than one
fixed character; postfix operators retract the character they modify
when it is optional.  The result is always a sound necessary prefix."
  (let ((i 0) (len (length regexp)) chars done)
    (while (and (< i len) (not done))
      (let ((c (aref regexp i)))
        (cond
         ((eq c ?\\)
          (if (or (>= (1+ i) len)
                  (not (renerd-icons-dired--escaped-literal-p
                        (aref regexp (1+ i)))))
              (setq done t)
            (push (aref regexp (1+ i)) chars)
            (setq i (+ i 2))))
         ((memq c '(?* ??))
          ;; Optional or repeated: the modified char is not required.
          (when chars (pop chars))
          (setq done t))
         ((eq c ?+)
          ;; At least one occurrence required: keep the char, stop there.
          (setq done t))
         ;; `.', `[', `^', `$': variable or context-dependent constructs.
         ;; `^'/`$' mid-pattern are literals in Emacs, but stopping is the
         ;; conservative choice and barely weakens the gate.
         ((memq c '(?. ?\[ ?^ ?$))
          (setq done t))
         (t (push c chars) (setq i (1+ i))))))
    (and chars (apply #'string (nreverse chars)))))

(defun renerd-icons-dired--end-anchor-pos (regexp)
  "Return the index where REGEXP's end anchor begins, or nil.
Only an unescaped trailing `$' or `\\'' anchors the pattern; an escaped
final char (as in \"foo\\$\") is an ordinary literal, so the pattern is
not end-anchored at all."
  (let ((len (length regexp))
        slashes j)
    (cond
     ((and (> len 0) (eq (aref regexp (1- len)) ?$))
      (setq slashes 0 j (- len 2))
      (while (and (>= j 0) (eq (aref regexp j) ?\\))
        (setq slashes (1+ slashes) j (1- j)))
      (and (evenp slashes) (1- len)))
     ((and (> len 1)
           (eq (aref regexp (- len 2)) ?\\)
           (eq (aref regexp (1- len)) ?'))
      (setq slashes 0 j (- len 3))
      (while (and (>= j 0) (eq (aref regexp j) ?\\))
        (setq slashes (1+ slashes) j (1- j)))
      (and (evenp slashes) (- len 2))))))

(defun renerd-icons-dired--literal-suffix (regexp end)
  "Return the longest literal string every match of REGEXP must end with.
END is the index of the end anchor, as returned by
`renerd-icons-dired--end-anchor-pos'.  The scan walks backward from END
collecting provably literal chars; any ambiguous construct - a
metacharacter, a group or character-class boundary, a structural
escape - ends the run instead of being parsed, so the result is always
a sound necessary suffix or nil."
  (let ((i (1- end)) chars)
    (while (>= i 0)
      (let* ((c (aref regexp i))
             (slashes 0)
             (j (1- i)))
        (while (and (>= j 0) (eq (aref regexp j) ?\\))
          (setq slashes (1+ slashes) j (1- j)))
        (if (oddp slashes)
            ;; `\C': literal only when C is an escaped literal char.
            (if (renerd-icons-dired--escaped-literal-p c)
                (progn (push c chars) (setq i (- i 2)))
              (setq i -1))
          ;; Bare char.  `.', `*', `+', `?', `[', `]', `^', `$' may be
          ;; metacharacters and a bare `\\' cannot be classified without
          ;; scanning forward, so all of them end the run.  Bare `(',
          ;; `)', `{', `}', `|' are ordinary literals in Emacs regexps.
          (if (memq c '(?. ?* ?+ ?? ?\[ ?\] ?^ ?$ ?\\))
              (setq i -1)
            (push c chars)
            (setq i (1- i))))))
    (and chars (apply #'string chars))))

(defun renerd-icons-dired--top-level-alternation-p (regexp)
  "Return non-nil when REGEXP contains `\\|' outside any group.
Branches of a top-level alternation may disagree on anchoring, so the
entry cannot be classified by a single prefix or suffix."
  (let ((i 0) (len (length regexp)) (depth 0) found)
    (while (and (< i len) (not found))
      (if (eq (aref regexp i) ?\\)
          (progn
            (when (< (1+ i) len)
              (let ((e (aref regexp (1+ i))))
                (cond ((memq e '(?\( ?\{)) (setq depth (1+ depth)))
                      ((memq e '(?\) ?\})) (setq depth (max 0 (1- depth))))
                      ((and (eq e ?|) (zerop depth)) (setq found t)))))
            (setq i (+ i 2)))
        (setq i (1+ i))))
    found))

(defun renerd-icons-dired--special-regexps ()
  "Refresh the special-name dispatch tables when the alist changed.
Sets `renerd-icons-dired--prefix-table', `--anchored-rest',
`--suffix-table' and `--unanchored-rest'.  See the comment block above the
variables for the classification rules."
  (unless (eq renerd-icons-dired--special-regexp-source
              nerd-icons-regexp-icon-alist)
    (let ((split
           (condition-case nil
               (let (prefix-alist anchored-any suffix-alist unanchored-any)
                 (dolist (entry nerd-icons-regexp-icon-alist)
                   (let ((regexp (car entry)))
                     (cond
                      ((not (stringp regexp))
                       ;; Cannot poison a combined regexp; match everything.
                       (push "" unanchored-any))
                      ((renerd-icons-dired--top-level-alternation-p regexp)
                       (push regexp unanchored-any))
                      ((or (string-prefix-p "^" regexp)
                           (string-prefix-p "\\`" regexp))
                       ;; `\` matches only at position 0 while `^' also
                       ;; matches after newlines; for the gate they are
                       ;; interchangeable.
                       (let* ((off (if (eq (aref regexp 0) ?^) 1 2))
                              (body (substring regexp off))
                              (prefix
                               (renerd-icons-dired--literal-prefix body)))
                         (if prefix
                             (let ((cell (assq (aref prefix 0)
                                               prefix-alist)))
                               (if cell
                                   (push prefix (cdr cell))
                                 (push (cons (aref prefix 0) (list prefix))
                                       prefix-alist)))
                           (push body anchored-any))))
                      ((renerd-icons-dired--end-anchor-pos regexp)
                       ;; Only an end-anchored regexp can impose a
                       ;; required suffix.  `end-anchor-pos' returns a
                       ;; truthy index that `cond' already verified.
                       (let ((suffix
                              (renerd-icons-dired--literal-suffix
                               regexp
                               (renerd-icons-dired--end-anchor-pos
                                regexp))))
                         (if suffix
                             (let ((c (aref suffix (1- (length suffix))))
                                   (cell nil))
                               (setq cell (assq c suffix-alist))
                               (if cell
                                   (push suffix (cdr cell))
                                 (push (cons c (list suffix))
                                       suffix-alist)))
                           (push regexp unanchored-any))))
                      ;; No usable anchor at either end: keep the regexp.
                      (t (push regexp unanchored-any)))))
                 (list prefix-alist anchored-any suffix-alist
                       unanchored-any))
             (error 'error))))
      (if (eq split 'error)
          ;; Unusable table: match every name so all lookups delegate to
          ;; the real function.
          (setq renerd-icons-dired--prefix-table nil
                renerd-icons-dired--anchored-rest nil
                renerd-icons-dired--suffix-table nil
                renerd-icons-dired--unanchored-rest "")
        (setq renerd-icons-dired--prefix-table
              (let ((table (make-char-table nil)))
                (dolist (cell (nth 0 split))
                  (aset table (car cell) (cdr cell)))
                table)
              renerd-icons-dired--anchored-rest
              (and (nth 1 split)
                   (concat "^\\(?:"
                           (mapconcat #'identity (nth 1 split) "\\|")
                           "\\)"))
              renerd-icons-dired--suffix-table
              (let ((table (make-char-table nil)))
                (dolist (cell (nth 2 split))
                  (aset table (car cell) (cdr cell)))
                table)
              renerd-icons-dired--unanchored-rest
              (and (nth 3 split)
                   (mapconcat (lambda (regexp)
                                (concat "\\(?:" regexp "\\)"))
                              (nth 3 split) "\\|")))
        (setq renerd-icons-dired--special-regexp-source
              nerd-icons-regexp-icon-alist))
      ;; The extension table may have changed together with the regexp table.
      (unless (eq renerd-icons-dired--ext-cache-source
                  nerd-icons-extension-icon-alist)
        (setq renerd-icons-dired--ext-cache nil
              renerd-icons-dired--ext-cache-source
              nerd-icons-extension-icon-alist)))))

(defun renerd-icons-dired--maybe-special-p (base)
  "Return non-nil when BASE might match `nerd-icons-regexp-icon-alist'.
This is a necessary-condition gate only: a positive answer always
delegates to `nerd-icons-icon-for-file' for the real verdict, so false
positives merely cost one extra call."
  (let ((case-fold-search nil)
        (len (length base)))
    (or (and (> len 0)
             renerd-icons-dired--prefix-table
             (let (hit)
               (dolist (prefix (aref renerd-icons-dired--prefix-table
                                     (aref base 0))
                               hit)
                 (when (string-prefix-p prefix base)
                   (setq hit t)))))
        (and renerd-icons-dired--anchored-rest
             (string-match-p renerd-icons-dired--anchored-rest base))
        (and (> len 0)
             renerd-icons-dired--suffix-table
             (let (hit)
               (dolist (suffix (aref renerd-icons-dired--suffix-table
                                     (aref base (1- len)))
                               hit)
                 (when (string-suffix-p suffix base)
                   (setq hit t)))))
        (and renerd-icons-dired--unanchored-rest
             (string-match-p renerd-icons-dired--unanchored-rest base))
        ;; `^' also matches after a newline, which the prefix gate cannot
        ;; see; names with an embedded newline always delegate.
        (and (> len 0) (string-match-p "\n" base)))))

(defun renerd-icons-dired--ext-cache ()
  "Return the per-extension icon cache for `nerd-icons-icon-for-file'."
  (unless (and renerd-icons-dired--ext-cache
               (eq renerd-icons-dired--ext-cache-source
                   nerd-icons-extension-icon-alist))
    (setq renerd-icons-dired--ext-cache (make-hash-table :test #'equal)
          renerd-icons-dired--ext-cache-source
          nerd-icons-extension-icon-alist))
  renerd-icons-dired--ext-cache)

(defun renerd-icons-dired--ext-alist ()
  "Return `nerd-icons-extension-icon-alist' as a hash table.
`assoc' would walk the 300+ entry alist linearly for every new
extension; a hash keeps first lookups O(1).  Like `assoc', the first
entry wins when the alist contains duplicate keys."
  (unless (and renerd-icons-dired--ext-alist-hash
               (eq renerd-icons-dired--ext-alist-hash-source
                   nerd-icons-extension-icon-alist))
    (let ((table (make-hash-table :test #'equal)))
      (dolist (entry nerd-icons-extension-icon-alist)
        (when (and (stringp (car entry)) (cdr entry))
          (unless (gethash (car entry) table)
            (puthash (car entry) entry table))))
      (setq renerd-icons-dired--ext-alist-hash table
            renerd-icons-dired--ext-alist-hash-source
            nerd-icons-extension-icon-alist)))
  renerd-icons-dired--ext-alist-hash)

(defun renerd-icons-dired--nerd-file-icon (name)
  "Return the `nerd-icons-icon-for-file' icon string for NAME.
This mirrors `nerd-icons-icon-for-file' with only the `:height' override:
names that might match `nerd-icons-regexp-icon-alist' are delegated to the
real function; the rest resolve through the per-extension cache."
  ;; `file-name-nondirectory'/`file-name-extension' consult
  ;; `file-name-handler-alist' on every call; plain string searches give
  ;; identical results for Dired names at a fraction of the cost.
  (let* ((slash (string-match "/[^/]*\\'" name))
         (base (if slash (substring name (1+ slash)) name)))
    (renerd-icons-dired--special-regexps)
    (if (condition-case nil
            (renerd-icons-dired--maybe-special-p base)
          ;; A malformed regexp would fail inside the real function too;
          ;; delegate instead of guessing.
          (error t))
        ;; Possibly special: exact behavior lives in the real function.
        (nerd-icons-icon-for-file name :height renerd-icons-dired-icon-size)
      (let* ((dot (string-match "\\.[^.]*\\'" base))
             ;; `file-name-extension' semantics: a trailing dot counts but
             ;; a leading dot does not.
             (ext (and dot (> dot 0) (substring base (1+ dot))))
             (cache (renerd-icons-dired--ext-cache))
             ;; Lowercase extensions (the common case) hit without
             ;; allocating a downcased key.
             (entry (and ext (or (gethash ext cache)
                                 (gethash (downcase ext) cache)))))
        (if (and entry (eql (car entry) renerd-icons-dired-icon-size))
            (cdr entry)
          (let* ((key (and ext (downcase ext)))
                 (icon (or (and key
                                (cdr (gethash key
                                              (renerd-icons-dired--ext-alist))))
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
  (setq renerd-icons-dired--prefix-table nil
        renerd-icons-dired--anchored-rest nil
        renerd-icons-dired--suffix-table nil
        renerd-icons-dired--unanchored-rest nil
        renerd-icons-dired--special-regexp-source :none
        renerd-icons-dired--ext-cache nil
        renerd-icons-dired--ext-cache-source nil
        renerd-icons-dired--ext-alist-hash nil
        renerd-icons-dired--ext-alist-hash-source nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p renerd-icons-dired-mode)
        (renerd-icons-dired-refresh t)))))

(defun renerd-icons-dired--icon (name directory-p special-p)
  "Return the ready-to-use `after-string' value for NAME.
The string already carries the `display' spec that makes its own faces
merge with faces active at the overlay position (e.g. `hl-line'), the
same workaround nerd-icons-dired uses for rainstormstudio/nerd-icons-dired#1.
DIRECTORY-P selects the directory icon function.  SPECIAL-P is non-nil for
`.' and `..', whose visible prefix intentionally matches nerd-icons-dired."
  (if special-p
      (let ((string (propertize renerd-icons-dired-special-prefix-string
                                'face 'renerd-icons-dired-overlay-face)))
        (propertize string 'display string))
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
        ;; Only the padding gets `renerd-icons-dired-overlay-face'; the icon
        ;; string must keep the face set by the icon function, which carries
        ;; both the color and the Nerd Font family the glyph needs.
        (let ((string (concat (if (and renerd-icons-dired-fast-file-icons
                                      (not directory-p)
                                      (eq function #'nerd-icons-icon-for-file))
                                  (renerd-icons-dired--nerd-file-icon name)
                                (funcall function name
                                         :height renerd-icons-dired-icon-size))
                              (propertize
                               renerd-icons-dired-infix-string
                               'face 'renerd-icons-dired-overlay-face))))
          ;; Cache the display-wrapped string: annotating an entry then
          ;; needs no further string allocation.
          (let ((wrapped (propertize string 'display string)))
            (puthash key
                     (cons (list function
                                 renerd-icons-dired-icon-size
                                 renerd-icons-dired-infix-string)
                           wrapped)
                     table)
            wrapped))))))

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
    (if (string-match-p "[\\\"']" raw)
        (save-excursion
          (goto-char pos)
          (dired-get-filename 'relative 'noerror))
      (if (or flat (file-name-absolute-p raw))
          raw
        (concat (or (dired-current-directory t) "") raw)))))

(defun renerd-icons-dired--overlay-map (start end)
  "Return a hash table mapping filename position to renerd overlay.
Returns nil when START..END contains no overlays at all, which is the
common case for fresh territory."
  (let ((overlays (overlays-in start end))
        map)
    (when overlays
      (setq map (make-hash-table :test #'eql))
      (dolist (overlay overlays)
        (when (overlay-get overlay 'renerd-icons-dired-overlay)
          (puthash (overlay-end overlay) overlay map))))
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
         (overlay (and map (gethash pos map))))
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
          ;; Non-nil when POS is known to start a file name because it was
          ;; reached through a nil->non-nil property transition; only the
          ;; initial POS and positions after a mid-name skip need the
          ;; explicit (1- pos) boundary check.
          (at-start nil)
          seen)
      (while (and (< pos end)
                  (or (null limit) (< count limit))
                  (or (eql 0 (logand count 15))
                      (not (input-pending-p))))
        (if (not (get-text-property pos 'dired-filename))
            (setq pos (or (next-single-property-change
                           pos 'dired-filename nil end)
                          end)
                  at-start t)
          (setq seen t)
          (let ((fend (or (next-single-property-change pos 'dired-filename)
                          (point-max))))
            (when (or at-start
                      (= pos (point-min))
                      (not (get-text-property (1- pos) 'dired-filename)))
              ;; POS is the start of a file name.
              (let ((overlay (and map (gethash pos map))))
                (unless (and overlay
                             (eql (overlay-get
                                   overlay 'renerd-icons-dired-generation)
                                  generation))
                  (goto-char pos)
                  (when-let* ((name (renerd-icons-dired--resolve-name
                                     pos fend flat)))
                    (renerd-icons-dired--annotate-at pos name map generation)
                    (cl-incf count)))))
            (setq pos fend
                  at-start nil))))
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
          (let ((overlay (and map (gethash pos map))))
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
        ;; parts outside the covered range need work.  Pending input can cut
        ;; the scan short; coverage must only be claimed when it completed,
        ;; otherwise the tail of the window would stay iconless for the
        ;; whole epoch.
        (if (cdr (renerd-icons-dired--annotate-uncovered
                  start end generation tick nil))
            (renerd-icons-dired--cover generation tick start end)
          (renerd-icons-dired--schedule)))
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
