;;; misc-ext.el --- miscellaneous extensions for various standart modes/features
;;
;; Copyright 2011-2012 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;; Code:
;;


;;; grep
;; ----------------------------------------------------------------------
(defvar my-grep-stack nil)

(defun my-grep-stack-buffer-push ()
  (interactive)
  (push (current-buffer) my-grep-stack)
  (rename-uniquely)
  (message "Grep buffer pushed to stack."))

(defun my-grep-stack-buffer-pop ()
  (interactive)
  ;(quit-window)
  (if my-grep-stack
      (let ((buffer (pop my-grep-stack)))
        (switch-to-buffer buffer)
        (message "Grep buffer popped from stack."))))

(defvar grep-find-ext-regexp-function nil
  "Function returning regexp for `grep-find-ext'.
Called wit no arguments.")

(defvar grep-find-ext-command-function 'grep-find-ext-command-std
  "Function returning command for `grep-find-ext'.
Called with one argument: the regexp to be searched for.")

(defun grep-find-ext-command-std (regexp)
  (replace-regexp-in-string "<R>" (replace-regexp-in-string "'" "'\"'\"'" (replace-regexp-in-string "\\\\" "\\\\\\\\" regexp)) grep-find-command))

(defun grep-find-ext(arg)
  "As `grep-find', however dynamically determines default.

`grep-find-ext-regexp-function' is used to determine the regexp
searched for. `grep-find-ext-command-function' is used to
determine the whole shell command used to search."
  (interactive "P")
  (let* ((regexp (cond ((equal arg '(16)) "")
                       (mark-active (regexp-quote (buffer-substring-no-properties (point) (mark))))
                       (t (or (and grep-find-ext-regexp-function (funcall grep-find-ext-regexp-function))
                              (concat "\\b" (regexp-quote (buffer-substring-sexp-no-properties)) "\\b")))))
         (gfc (or (and grep-find-ext-command-function (funcall grep-find-ext-command-function regexp))
                  grep-find-command))) ; grep-find-command
    (if (and (listp arg) (not (null arg)))
        (progn
          (grep-apply-setting 'grep-find-command gfc)
          (call-interactively 'grep-find))
      (grep-find gfc))))


;;; kill, kill-ring, delete
;; ----------------------------------------------------------------------
(defalias 'browse-kill-ring-ext-insert
  (read-kbd-macro "C-x o i C-x o"))

(defalias 'browse-kill-ring-ext-insert-then-goto-next
  (read-kbd-macro "C-x o i n C-x o"))

(defalias 'browse-kill-ring-ext-insert-then-goto-prev
  (read-kbd-macro "C-x o i n C-x o"))

(defun browse-kill-ring-insert-as-separated (items)
  (let ((count 0))
    (while (cdr items)
      (setq count (+ count 1))
      (insert (format "-- %s --" count) "\n")
      (browse-kill-ring-insert-as-separated-1 (car items) t)
      (setq items (cdr items))))
  (when items
    (browse-kill-ring-insert-as-separated-1 (car items) nil)))

(defun eval-last-sexp-to-kill-ring()
  "Similar to `eval-last-sexp', but additionaly adds it to kill ring."
  (interactive)
  (let ((val (format "%s" (eval (preceding-sexp)))))
    (kill-new val)
    (message val)))

(defvar yank-indent-modes '(c++-mode emacs-lisp-mode))

(defun yank-ext()
  "As `yank', but additionaly calls `indent-region'."
  (interactive)
  (call-interactively 'yank)
  (when (member major-mode yank-indent-modes)
    (undo-boundary)
    (call-interactively 'indent-region)))

(put 'yank-ext 'delete-selection 'yank)

(defun yank-push(&optional arg)
  "As `yank-pop', but with inverse meaning of ARG"
  (interactive "*p")
  (yank-pop (- arg)))

(defun yank-pop-ext(&optional arg)
  "As `yank-pop', but additionaly calls `indent-region' and `print-kill-ring'"
  (interactive)
  (call-interactively 'yank-pop)
  (when (member major-mode yank-indent-modes)
    (call-interactively 'indent-region))
  (print-kill-ring))

(defun yank-push-ext(&optional arg)
  "As `yank-pop-ext', but in inverse direction"
  (interactive "*p")
  (yank-pop (- arg))
  (when (member major-mode yank-indent-modes)
    (call-interactively 'indent-region))
  (print-kill-ring))

(defun print-kill-ring ()
  "Prints `kill-ring' near `kill-ring-yank-pointer'."
  (interactive)
  (unless (minibufferp)
    (let* (elt
           (cnt (min (length kill-ring) 7))
           (end (/ cnt 2))
           (i (- end))
           (pointer (nthcdr (mod (- i (length kill-ring-yank-pointer))
                                 (length kill-ring))
                            kill-ring))
           result)
      (while (< i end)
        (setq elt (car pointer)
              elt (if (string= elt "") "<empty string>" elt)
              elt (if (string-match "^[ \t\n]+$" elt) "<only blanks>" elt)
              elt (replace-regexp-in-string "^[ \t]+" "" elt)
              elt (replace-regexp-in-string "\n" "\\\\n" elt)
              elt (if (> (length elt) 50) (concat (substring elt 0 50) "...") elt)
              elt (concat (if (eq i 0) ">" " ") elt))
        (setq result (concat result
                             (if result "\n")
                             (if (eq pointer kill-ring) "----- kill-ring start ------\n")
                             elt)
              pointer (or (cdr pointer) kill-ring)
              i (1+ i)))
      (set-text-properties 0 (length result) nil result)
      (message "%s" result))))

(defun backward-delete-word ()
  ""
  (interactive)
  (let ()
    ))

;;; files & libraries
;; ----------------------------------------------------------------------
(defun save-some-buffers-no-query ()
  "Save all buffers without query"
  (interactive)
  (save-some-buffers t 'save-some-buffers-pred-fun))

;; todo: more answers
;; - never for this buffer: save that in a buffer local variable
;; - patch basic-save-buffer so the prompt tells the buffer name
(defun save-some-buffers-pred-fun ()
  (cond
   ((buffer-file-name) t)
   ((string-match "^\\*.*\\*$" (buffer-name)) nil)
   (t
    (pop-to-buffer (current-buffer))
    (yes-or-no-p (format "save buffer '%s'? " (buffer-name))))))

(defun yank-and-newline-and-indent ()
  "calls yank and insert-new-line"
  (interactive)
  (yank)
  (newline-and-indent))

;; todo: advice load so require would also profit from the feature of
;; 'auto-byte-compiling'
(defun load-library-ext (file)
  "As `load-library', however first byte compiles file if it is
not already done so."
  (let* ((file-raw (file-name-sans-extension (locate-library file)))
         (file-el (concat file-raw ".el"))
         (file-elc (concat file-raw ".elc")))
    (if (file-readable-p file-el)
      (let ((date-el (nth 5 (file-attributes file-el)))
            (date-elc (nth 5 (file-attributes file-elc))))
        (when (or (not (file-readable-p file-elc))
                  (null date-elc)
                  (> (nth 0 date-el) (nth 0 date-elc))
                  (and (eq (nth 0 date-el) (nth 0 date-elc))
                       (> (nth 1 date-el) (nth 1 date-elc))))
          (byte-compile-file file-el t)))
      (load file))))

;; does not really work yet
(defun make-backup()
  (interactive)
  (let ((old-name (buffer-file-name))
        (bak-name (let ((version-control nil))
                    (car (find-backup-file-name (buffer-file-name)))))
        (bak-buffer))
    (write-file bak-name)
    (setq bak-buffer (current-buffer))
    (find-file old-name)
    (kill-buffer bak-buffer)))

(defun toggle-read-only-ext()
  "As `toggle-read-only', but additionaly also calls set-file-modes."
  (interactive)
  (call-interactively 'toggle-read-only)
  (when (and buffer-file-name (y-or-n-p "Also change file mode? "))
    (set-file-modes buffer-file-name
     (file-modes-symbolic-to-number (if buffer-read-only "-w" "+w") (file-modes buffer-file-name)))))


;;; dired & buffer
;; ----------------------------------------------------------------------
(defun dired-mark-extension-dwim (ext-list)
  "Similar to dired-mark-extension.
However, not a 'direct' regex is given, but a pipe (|) separated
list of regexps. A prefix argument unmarks the matching files
instead of marking them. A dot is interpreted as regular
expression meta char. Thus 'cpp' matches files ending in '.cpp',
and '.h' matches files ending in 'ch' where c is any character."
  (interactive "sExtension(s) : ")
  (let* ((inner-regex (replace-regexp-in-string "|" "\\\\|" ext-list))
         (whole-regex (concat "\\.\\(" inner-regex "\\)$")))
    (dired-mark-files-regexp whole-regex (when current-prefix-arg ?  ))))

(defun dired-view-file-other-window ()
  ""
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist)
                 (dired-goto-subdir file))
            (dired file))
      (view-file-other-window file))))

(defun dired-first-file-line ()
  "Moves to the first dired line that have a file or directory name on it."
  (interactive)
  (beginning-of-buffer)
  (if (not (dired-move-to-filename))
      (dired-next-file-line)))

(defun dired-last-file-line ()
  "Moves to the last dired line that have a file or directory name on it."
  (interactive)
  (end-of-buffer)
  (if (not (dired-move-to-filename))
      (dired-previous-file-line)))

(defun dired-next-file-line ()
  "Moves to the next dired line that have a file or directory name on it."
  (interactive)
  (call-interactively 'dired-next-line)
  (if (not (dired-move-to-filename))
      (dired-next-file-line)))

(defun dired-previous-file-line ()
  "Moves to the previous dired line that have a file or directory name on it."
  (interactive)
  (call-interactively 'dired-previous-line)
  (if (not (dired-move-to-filename))
      (dired-previous-file-line)))

(defun dired-ediff-marked-files ()
  "Run ediff on marked ediff files."
  (interactive)
  (set 'marked-files (dired-get-marked-files))
  (when (= (safe-length marked-files) 2)
    (ediff-files (nth 0 marked-files) (nth 1 marked-files)))

  (when (= (safe-length marked-files) 3)
    (ediff3 (buffer-file-name (nth 0 marked-files))
            (buffer-file-name (nth 1 marked-files))
            (buffer-file-name (nth 2 marked-files)))))

(defun dired-do-delete-ext (&optional arg)
  "As `dired-do-delete' but additionaly warns on certain occasions."
  (interactive "P")
  (let ((marked-files (dired-get-marked-files)))
    (when (or (not (and
                    ;; ???
                    (or (cdr marked-files) (not (string= (car marked-files) (dired-get-filename))))
                    ;; current line and line before are both not marked
                    (save-excursion (and (progn (beginning-of-line) (not (looking-at dired-re-mark)))
                                         (progn (forward-line -1) (beginning-of-line) (not (looking-at dired-re-mark)))))))
              (y-or-n-p "Point is on an unmarked files. You want to delete the marked files? "))
      (call-interactively 'dired-do-delete))))

(defun dired-update-file-autoloads ()
  ;; Return nil for success, offending file name else.
  (let ((file (dired-get-filename)) failure)
    (condition-case err
        (update-file-autoloads file)
      (error (setq failure err)))
    (if (not failure)
        nil
      (dired-log "update-file-autoloads error for %s:\n%s\n" file failure)
      (dired-make-relative file))))

(defun dired-do-update-file-autoloads (&optional arg)
  (interactive "P")
  (dired-map-over-marks-check (function dired-update-file-autoloads) arg 'update-file-autoloads t))

(defun dired-c-convert-ifndef-to-pragma-once ()
  ;; Return nil for success, offending file name else.
  (let ((file (dired-get-filename)) failure)
    (condition-case err
        (progn
          (find-file file)
          (c-convert-ifndef-to-pragma-once))
      (error (setq failure err)))
    (if (not failure)
        nil
      (dired-log "c-convert-ifndef-to-pragma-once error for %s:\n%s\n" file failure)
      (dired-make-relative file))))

(defun dired-c-convert-ifndef-to-pragma-once (&optional arg)
  (interactive "P")
  (dired-map-over-marks-check (function c-convert-ifndef-to-pragma-once) arg 'c-convert-ifndef-to-pragma-once t))

(defun dired-open-in-external-app (&optional file)
  "In dired, open file in external app.

The app is chosen from your OS's preference."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((not file) (list (buffer-file-name)))
           (file (list file)))))
    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ")))
    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList))))))

(defun dired-find-file-literally ()
  "In Dired, visit literally the file or directory named on this line."
  (interactive)
  (let ((find-file-run-dired t))
    (find-file-literally (dired-get-file-for-visit))))


;;; fill
;; ----------------------------------------------------------------------
(defun fill-paragraph-dwim (arg)
  "With nil ARG, `fill-paragraph' with nil as arg, else `unfill-paragraph'."
  (interactive "*P")
  (if arg
      (unfill-paragraph)
    (fill-paragraph nil)))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (let ((fill-column (point-max)))
    (fill-paragraph-dwim nil)))


;;; rectangular region
;; ----------------------------------------------------------------------
(defun copy-rectangle-as-kill()
  (interactive)
  (let (deactivate-mark)
    (save-excursion
      (if (<= (mark) (point))
          (exchange-point-and-mark))
      (call-interactively 'kill-rectangle)
      (yank-rectangle))))

(defcustom paragraph-rect-regex "^\\s-*\\sw+(" "")

(defun mark-paragraph-rect ()
  "Marks this paragraph, but excluding preceding and trailing
lines, and point and mark will be in the same column as they
were before. If mark was not active before, it's now in the
same column as point.

This command is useful for quickly making a logical text
column for rectangle commands. "
  (interactive)
  (let ((deactivate-mark)
        (column-left (current-column))
        (saved-point (point))
         column-right)

    ;; remember current columns of point/mark
    (when mark-active
      (exchange-point-and-mark))
    (setq column-right (current-column))
    (when (< column-right column-left)
      (let* ((help column-right)
             (column-right column-left)
             (column-left help))))

    ;; set point at start
    (beginning-of-line)
    (while (looking-at paragraph-rect-regex)
     (fordward-line -1))
    (forward-line)
    (move-to-column column-right)
    (set-mark (point))

    ;; set mark at end
    (goto-char saved-point)
    (while (looking-at paragraph-rect-regex)
     (fordward-line))
    (forward-line -1)
    (move-to-column column-right)
    (set-mark (point))))

(defun delete-blank-rectangle()
  (interactive)
  (let (edge-column edge)
    (beginning-of-line)
    (re-search-forward "\\s-*")
    (setq edge-column (current-column))
    (beginning-of-line)
    (re-search-forward "^[^ \t\n]")
    (forward-line -1)
    (if (looking-at "\\s-*$")
        (insert-char ?\s edge-column))
    (beginning-of-line)
    (forward-char edge-column)
    (setq edge (point))
    (re-search-backward "^[^ \t\n]")
    (forward-line)
    (delete-rectangle (point) edge)))


;;; external tools
;; ----------------------------------------------------------------------
(defun flyspell-ext-buffer ()
  (interactive)
  (progn
    (let ((reg-beg)
          (reg-end))
      (flyspell-buffer)
      (goto-char (point-min))
      (while (<= (point) (point-max))
        ; define region and move to start of it
        ;(setq reg-beg (point))
        ;(end-of-line)
        ;(setq reg-end (point))
        ;(goto-char reg-beg)

        ; flyspell the defined region
        ;(flyspell-region reg-beg reg-end)
        (flyspell-goto-next-error)
        (call-interactively 'flyspell-auto-correct-previous-word)

        ; goto end of defined region
        ;(goto-char reg-end)
        ))))

(defun shell-command-on-region-ext()
  "As shell-command-on-region. If there is currently no region,
the whole buffer is marked. If the major mode is grep-mode,
read only flag is automatically unset."
  (interactive)
  (unless mark-active
    (mark-whole-buffer))
  (when (and (equal major-mode 'grep-mode) buffer-read-only)
    (toggle-read-only))
  (call-interactively 'shell-command-on-region)
  (when (equal major-mode 'grep-mode)
    (grep-mode)))

(defun svn-status-wd ()
  "run svn-status in currents buffer directory"
  (interactive)
  (svn-status (file-name-directory (buffer-file-name))))

;; vc+ uses vc-name, which in its original definition doesn't seem to work.
(defun vc-name (x) x)

(defun vc-ediff (arg)
  (interactive "P")
  (if arg
      (call-interactively 'ediff-revision)
      (ediff-vc-internal "" "" nil)))

(defun woman-goto-option (name)
  ""
  (interactive "sName : " )
  (let ((case-fold-search nil))
    (goto-char 0)
    (if (string-match "perl\\w+\\*$" (buffer-name))
        (re-search-forward (concat "^\\s-*\n\\s-*" name "\\b"))
        (re-search-forward (concat "^\\s-*\\(--?\\(\\w\\|-\\)+,\\s-*\\)*?--?" name "\\b")))))


;;; compilation
;; ----------------------------------------------------------------------

(require 'compile)

;; The default 'gnu' item does not work in from my point of view. That's maybe
;; because gcc is, a as the name says, a compiler collection; it's hard to make
;; a general regexp for gcc's output. The following is a regexp that works for
;; my uses of gcc.
(add-to-list
 'compilation-error-regexp-alist-alist
 '(gnu-sensorflo
   "^\\(?:[^ \t:]+:[ \t]+\\|[ \t]*[a-zA-Z0-9_]+=\\)?\\(.*?[^0-9\n]\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)?\\(?: *error\\|\\( *W:\\| *[a-zA-Z]*[wW]arn\\)\\|\\( *required from\\| *note\\)\\|[^0-9\n]\\)"
   1 2 3 (4 . 5)))

;; The vanila gcc-include does not make the type unconditionally 'warning' as I
;; want it to be.
(add-to-list
 'compilation-error-regexp-alist-alist
 '(gcc-include-sensorflo
   "^\\(?:In file included \\|                 \\|\t\\)from \
\\([0-9]*[^0-9\n]\\(?:[^\n :]\\| [^-/\n]\\|:[^ \n]\\)*?\\):\
\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?"
   1 2 3 0))

(add-to-list
 'compilation-error-regexp-alist-alist
 '(doxygen
   "^\\(.*?[^0-9\n]\\)(\\([0-9]+\\)):\\(?: *error\\|\\( *W:\\| *[a-zA-Z]*[wW]arn\\)\\|\\)"
   1 2 nil (3 . nil)))

(add-to-list
 'compilation-error-regexp-alist-alist
 '(cppunit
   "^[0-9]+) .*? line: \\([0-9]+\\)[ \t]*\\(.+\\)"
   2 1))

(add-to-list
 'compilation-error-regexp-alist-alist
 '(xentis-codegen
   "^[ \t]*in file \\(.+?\\) on line \\([0-9]+\\)"
   1 2))

(add-to-list
 'compilation-error-regexp-alist-alist
 '(xentis-log
   "^.......... ..:..:...[^ ]* <[^>]+> \\[\\(?:ERROR\\|\\(WARN\\)\\|\\(.+\\)\\)?\\] \\(.*?\\):\\([0-9]+\\)"
   3 4 nil (1 . 2)))

;; The original compilation-mode-font-lock-keywords does highlight to much stuff
;; for my taste. That results in wrong/misleading highlights in my makefiles.
(setq compilation-mode-font-lock-keywords
      '(("^Compilation \\(finished\\).*"
         (0 '(face nil message nil help-echo nil mouse-face nil) t)
         (1 compilation-info-face))
        ("^Compilation \\(exited abnormally\\|interrupt\\|killed\\|terminated\\|segmentation fault\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
         (0 '(face nil message nil help-echo nil mouse-face nil) t)
         (1 compilation-error-face)
         (2 compilation-error-face nil t))))

(defun compilation-ext-next-file ()
  (interactive)
  (pop-to-buffer next-error-last-buffer)
  (call-interactively 'compilation-next-file)
  (compile-goto-error))

(defun compilation-ext-next-file-no-select ()
  (interactive)
  (compilation-ext-next-file)
  (pop-to-buffer next-error-last-buffer))

(defun compilation-ext-previous-file ()
  (interactive)
  (pop-to-buffer next-error-last-buffer)
  (call-interactively 'compilation-previous-file)
  (compile-goto-error))

(defun compilation-ext-previous-file-no-select ()
  (interactive)
  (compilation-ext-previous-file)
  (pop-to-buffer previous-error-last-buffer))


;;; comment
;; ----------------------------------------------------------------------

;; Bugs
;; - when point is within comment-start-skip, then the previous comment is selected
;; - when multple comment syntaxes exist (eg. as in C++ \\..\n and /*..*/), then
;;   it is NOT ensured that the end-delimiter matching the start-delimiter is
;;   found - ANY found end delimiter will end the comment
;; - multiple technical comments in a row, which most probably constitue one
;;   logical comment (e.g. in shell scripts, multiple #...) are NOT considered
;;   as one comment
(defun mark-comment-dwim ()
  "Marks 'the' comment in a dwim fashion.
To select a previous/following comment, move point out of comment"
  (interactive)

  (let (point-mark-exchanged)

    ;; canonicalize: point <= mark
    (when (and mark-active (> (point) (mark)))
      (exchange-point-and-mark)
      (setq point-mark-exchanged t))

    (cond
     ;; whole comment incl delimiter -> comment's text
     ((and
       mark-active
       (looking-at comment-start-skip)
       (save-excursion (goto-char (mark)) (looking-back comment-end-skip)))
      (re-search-forward comment-start-skip)
      (exchange-point-and-mark)
      (re-search-backward-greedy comment-end-skip)
      (exchange-point-and-mark))

     ;; comment's text -> whole comment incl delimiter
     ((and
       mark-active
       (looking-back comment-start-skip)
       (save-excursion (goto-char (mark)) (looking-at comment-end-skip)))
      (re-search-backward-greedy comment-start-skip)
      (exchange-point-and-mark)
      (re-search-forward comment-end-skip)
      (exchange-point-and-mark))

     ;; marks whole previous comment
     (t
      (unless (looking-at comment-start-skip)
        (re-search-backward comment-start-skip))
      (push-mark (save-excursion (re-search-forward comment-end-skip)))))

    (when point-mark-exchanged
      (exchange-point-and-mark))
    (setq mark-active t)))


;;; custom / widget
;; ----------------------------------------------------------------------
(defun custom-prompt-value (var prompt-val &optional comment)
  "Similar to `custom-prompt-variable' but only for value of a given VAR"
  (let* ((minibuffer-help-form '(describe-variable var))
         (val
          (let ((prop (get var 'variable-interactive))
                (type (get var 'custom-type))
                (prompt (format prompt-val var)))
            (unless (listp type)
              (setq type (list type)))
            (cond (prop
                   ;; Use VAR's `variable-interactive' property
                   ;; as an interactive spec for prompting.
                   (call-interactively `(lambda (arg)
                                          (interactive ,prop)
                                          arg)))
                  (type
                   (widget-prompt-value type
                                        prompt
                                        (if (boundp var)
                                            (symbol-value var))
                                        (not (boundp var))))
                  (t
                   (eval-minibuffer prompt))))))
    (if comment
        (list var val
              (read-string "Comment: " (get var 'variable-comment)))
      (list var val))))

(defmacro custom-set-value-prompt (variable &optional comment)
  "Similar to `custom-prompt-value', but only prompts for the value."
  `(customize-set-variable
    ,variable
    (nth 1 (custom-prompt-value ,variable (concat "set value for " (symbol-name ,variable)  " (default: " (symbol-value ,variable) "): ")))
    ,comment))


;;; isearch
;; ----------------------------------------------------------------------
(defun isearch-yank-sexp ()
 "Copy the sexp after point to the end of the search string"
 (interactive)
  (isearch-yank-internal
   (lambda ()
     (forward-sexp 1)
     (point))))


;;; indent / tabify / whites
;; ----------------------------------------------------------------------
(defun column (&optional pos)
  "Returns the column pos is at. If pos is nil, point is taken."
  (save-excursion
    (goto-char (or pos (point)))
    (current-column)))

(defun indent-line-or-region ()
  (interactive)
  (call-interactively (if mark-active 'indent-region 'indent-for-tab-command)))

;; todo: send to maitainer of tabify.el
(defun tabify-indentation(&optional start end)
  "As `tabify', however only converts spaces at the beginning of the line.
Also additionaly, START and END are optional. If nil, 0
and (buffer-size) respectively is used."
  (interactive "r")
  (when (null start) (setq start 0))
  (when (null end) (setq end (buffer-size)))
  (when (< end start)
    (let ((tmp end))
      (setq end start)
      (setq start tmp)))
  (unless (eq end start) ; re-search-forward seems to hate it if end is where point is
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (let ((indent-tabs-mode t))
        (while (re-search-forward "^\\([ \t]+\\)" end t)
          (let ((end-col (current-column)))
            (delete-region (match-beginning 0) (point))
            (indent-to end-col)))))))

;; todo: sent to maintainer of tabify.el
(defun untabify-indentation (start end)
  (interactive "r")
  (when (null start) (setq start 0))
  (when (null end) (setq end (buffer-size)))
  (when (< end start)
    (let ((tmp end))
      (setq end start)
      (setq start tmp)))
  (save-excursion
    (save-restriction
      (let ((tab-replacement (make-string tab-width ?\ )))
        (goto-char start)
        (while (re-search-forward "^[\t ]+" end t)
          (let ((old-indent-str (match-string 0)))
            (delete-region (match-beginning 0) (match-end 0))
            (insert (replace-regexp-in-string "\t" tab-replacement old-indent-str))))))))

(defun replace-by-space ()
  (interactive)
  (let ((cnt (abs (- (point) (mark)))))
    (delete-region (point) (mark))
    (insert (make-string cnt ? ))))

(defvar indent-rigidly-backward t)

(defun indent-for-tab-command-ext (arg)
  (interactive "*P")
  (let ((same (member last-command '(indent-rigidly-ext indent-for-tab-command-ext))))
    (if (not same)
        (setq indent-rigidly-backward (equal arg '(4)))
      (if (equal arg '(4))
          (setq indent-rigidly-backward (not indent-rigidly-backward))))
    (if same
        (indent-rigidly-ext indent-rigidly-backward)
      (indent-for-tab-command))
    (setq deactivate-mark nil)))

(defun indent-rigidly-ext (backward)
  (interactive "P")
  (save-excursion
  (mark-whole-lines)
  (let* ((beg (if (< (point) (mark)) (point) (mark)))
         (end (if (< (point) (mark)) (mark) (point)))
         (end (1- end)))
    (indent-rigidly beg end (if backward (- standard-indent) standard-indent))
    (setq deactivate-mark nil))))

;; see also whitespace-cleanup
(defun fix-white ()
  (save-restriction
    (widen)
    ;; note that this is about all spaces, not just those at the start of a
    ;; line
    (if indent-tabs-mode
        (tabify (point-min) (point-max))
      (untabify (point-min) (point-max)))
    (delete-trailing-whitespace)))

(defun whitespace-ext-toggle ()
  (interactive)
  (whitespace-toggle-options 'tabs)
  (whitespace-toggle-options 'spaces)
  (whitespace-toggle-options 'newline)
  (whitespace-toggle-options 'tab-mark)
  (whitespace-toggle-options 'space-mark)
  (whitespace-toggle-options 'newline-mark))

(defun whitespace-forward-problem ()
  (interactive)
  (let* ((sub-regexps
          ;; copied from whitespace-report-region, i.e. rendunat
          (mapcar
           (lambda (option)
             (cond
              ((eq (car option) 'indentation)
               (whitespace-indentation-regexp))
              ((eq (car option) 'indentation::tab)
               (whitespace-indentation-regexp 'tab))
              ((eq (car option) 'indentation::space)
               (whitespace-indentation-regexp 'space))
              ((eq (car option) 'space-after-tab)
               (whitespace-space-after-tab-regexp))
              ((eq (car option) 'space-after-tab::tab)
               (whitespace-space-after-tab-regexp 'tab))
              ((eq (car option) 'space-after-tab::space)
               (whitespace-space-after-tab-regexp 'space))
              (t
               (cdr option))))
           whitespace-report-list))
         (regexp
          (concat "\\(?:\\(?:"
                  (mapconcat (lambda (x) x) sub-regexps "\\)\\|\\(?:")
                  "\\)\\)")))
    (re-search-forward regexp)))


;;; outline
;; ----------------------------------------------------------------------
(defun hide-body-no-font-lock()
  (interactive)
  (font-lock-mode 0)
  (hide-body))

(defun show-all-font-lock()
  (interactive)
  (show-all)
  (font-lock-mode 1))


;;; really misc
;; ----------------------------------------------------------------------
(defun concat-line ()
  "Appends next line to current line, leaving just one space in between."
  (interactive)
  (end-of-line)
  (kill-line)
  (just-one-space))

(defun add-list-element ()
  "Mirrors `concat-line'"
  (interactive)
  (let ((has-preceding-item (not (looking-back "\\s-*[[({]")))
        (has-trailing-item (not (looking-at "\\s-*[])}]"))))))

(defun chmod (mode &optional file)
  "Change the MODE of the visited file (or FILE, if called pragmatically).
MODE can be any format supported by the system `chmod` command, e.g. \"664\"
or \"g+w\".  When called interactively, or if the FILE is already being
visited, the user is first asked whether to save the buffer (if it's been
modified), and afterward it is reverted from disk (unless it's modified)."
  (interactive "sMode: ")
  (or file
      (setq file (buffer-file-name))
      (error "No specified or visited file"))
  (let ((buffer (find-buffer-visiting file)))
    (if (and buffer
             (buffer-modified-p buffer)
             (y-or-n-p (format "Buffer modified; save %s? " file)))
        (save-buffer))
    (shell-command (format "chmod %s %s"
                           (shell-quote-argument mode)
                           (shell-quote-argument file)))
    (if buffer
        (or (buffer-modified-p buffer)
            (save-excursion
              (set-buffer buffer)
              (revert-buffer t t t))))))

(defun insert-date ()
  "Insert today's date"
  (interactive)
  (insert (format-time-string "%d.%m.%Y")))

(defun insert-initials-date ()
  "Insert initials and today's date"
  (interactive)
  (insert (format-time-string "FLKA %d.%m.%Y: ")))

 (defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun toggle-background-color ()
  (interactive)
  (if (equal (face-attribute 'default :foreground) "white")
      (set-face-attribute 'default nil :background "white" :foreground "black")
    (set-face-attribute 'default nil :background "black" :foreground "white")))

(defun ansi-color-buffer ()
  (interactive)
  (ansi-color-apply-on-region 0 (buffer-size)))


(defun lisp-debugger-help()
  (interactive)
  (message "c continue, exit debugger
d continue, break on the next Lisp function call
b break when frame is exited
u removes b's mark
j similar to b and then c
e eval an expression
q terminate program bein debugged
l list breaking functions"))

(defun replace-cntrl-chars ()
  "Replace all control chars across the current buffer.

The goal is that file(1) and other tools treat the file as text,
not as data/binary. `replace-cntrl-chars' is intended only for
control characters appearing in strings representing a key
sequence.

The control characters are replaced with their C-... equivalent,
_blindly assuming_ they are in a string meant to represent a key
sequence."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char 0)
      ;; To understand the following, looking at an ascii control code chart may
      ;; help, e.g. http://en.wikipedia.org/wiki/Ascii#ASCII_control_code_chart
      ;;
      ;; Search for \001-\037 but exclusive \011-\015. \011-\015 = \t \n \v \f \r
      ;; = ^I - ^M are allowed to occur in a text (i.e. non-binary) file.
      (while (re-search-forward "[\001-\010]\\|[\016-\037]" nil t)
        (let* ((ctrl-char (string-to-char (match-string 0)))
               ;; map \001-\032 to a-z, and \033-037 to [ - _. This is like
               ;; mapping \001-\037 to A-_ however lowercase for the latin
               ;; characters
               (replacement (concat "\\C-" (char-to-string (downcase (+ ctrl-char (- ?A ?\1)))))))
          (replace-match replacement t t))))))

(defun mode-message-start (element)
  ;; (message (concat "<" element
  ;;                  " major-mode=" (symbol-name major-mode)
  ;;                  " buffer-name=" (if (buffer-name) (buffer-name) "nil")
  ;;                  " buffer-file-name=" (if buffer-file-name buffer-file-name "nil")
  ;;                  ">"))
  )

(defun mode-message-end (element)
  ;; (message (concat "</" element
  ;;                  " major-mode=" (symbol-name major-mode)
  ;;                  " buffer-name=" (if (buffer-name) (buffer-name) "nil")
  ;;                  " buffer-file-name=" (if buffer-file-name buffer-file-name "nil")
  ;;                  ">"))
  )

(defun is-edit-mode ()
  (and
   ;; if more than 5 keys in A-Z are not bound to self-insert-command then
   ;; it's not an edit mode
   (let ((ch ?A)
         (bogous-cnt 0))
     (while (<= ch ?Z)
       (when (not (equal 'self-insert-command (key-binding (make-string 1 ch) t)))
         (setq bogous-cnt (1+ bogous-cnt)))
       (setq ch (1+ ch)))
     (< bogous-cnt 5))

   (not (derived-mode-p 'comint-mode))))

(defun is-a-minibufer-mode ()
  (string-match "minibuffer" (symbol-name major-mode)))

;;; misc-ext.el ends here
