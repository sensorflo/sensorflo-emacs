;;; simple-ext.el --- extensions to emacs' simple.el, subr.el, c source code
;;
;; Copyright 2009-2012 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; Created: 2009
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
;; Unordered extensions for various things within simple.el, subr.el, c source
;; code. See also misc-ext.el
;;
;;; Code:

(defun duplicate-line (arg)
  "Duplicates the current line arg times.
Afterwards, point will be on the last inserted line, in the same
column as it was in the beginning."
  (interactive "*p")
  (let ((text (buffer-substring (line-beginning-position) (line-end-position)))
        (column (current-column)))
    (end-of-line)
    (while (> arg 0)
      (insert "\n" text)
      (setq arg (- arg 1)))
    (move-to-column column)))

(defun duplicate-region (arg)
  "Duplicates current region arg times.
The region is extended to the beginning/end of the line of
point/mark. The last inserted block has the region activated the
same way the initial block had."
  (interactive "*p")
  (let* (text
         insert-length
         (saved-point (point))
         (saved-mark (mark))
         (point-is-end (< (mark) (point)))
         (begin (if point-is-end (mark) (point)))
         (end (if point-is-end (point) (mark))))

    (goto-char begin)
    (beginning-of-line)
    (setq begin (point))
    (goto-char end)
    (unless (equal (current-column) 0)
      (forward-line)
      (unless (eobp)
        (beginning-of-line)))
    (setq end (point))

    (setq text (buffer-substring begin end))
    (when (and (eobp) (not (equal (current-column) 0)))
      (setq text (concat "\n" text)))

    (setq insert-length (* arg (length text)))

    (while (> arg 0)
      (insert text)
      (setq arg (- arg 1)))

    (push-mark (+ saved-mark insert-length))
    (goto-char (+ saved-point insert-length))
    (setq deactivate-mark nil)))

(defun duplicate-line-or-region (&optional arg)
  "If region active, call \\[duplicate-region], else \\[duplicate-line]"
  (interactive "*p")
  (if mark-active
      (duplicate-region arg)
    (duplicate-line arg)))

(defun mark-whole-sexp ()
  (interactive )
  (if (not mark-active)
      (progn
      (forward-sexp 1)
      (forward-sexp -1)
      (set-mark (point))))
  (mark-sexp 1))

(defun mark-whole-line ()
  (beginning-of-line)
  (push-mark (point))
  (end-of-line)
  (forward-char 1)
  (setq mark-active t)
  )

(defun mark-whole-line-region ()
  (if (> (point) (mark t)) (exchange-point-and-mark))
  (beginning-of-line)
  (exchange-point-and-mark)
  (end-of-line)
  (forward-char 1))

(defun mark-whole-lines (&optional point-end-of-last-line)
  "If region not active, mark whole line is point on.
If region is active, mark all lines between point and mark."
  (interactive)

  ;; mark region
  (if mark-active
      (let (beg)
        (if (> (point) (mark))
            (exchange-point-and-mark))
        (beginning-of-line)
        (setq beg (point))
        (exchange-point-and-mark)
        (unless (eq (point) (line-beginning-position))
          (forward-line))
        (set-mark beg))

    ;; mark line: mark to beginning, point to beginning of next line
    (beginning-of-line)
    (set-mark (point))
    (forward-line))

  (when point-end-of-last-line
    (forward-char -1)))

(defun mark-paragraph-ext(&optional arg)
  "With prefix argument, call \\[mark-paragraph-rect], else call \\[mark-paragraph]."
  (interactive "P")
  (if (equal arg '(4))
    (mark-paragraph-rect)
    (mark-paragraph arg)))

(defun kill-buffer-other-window ()
  (interactive)
  (other-window 1)
  (kill-buffer nil))

(defun delete-blank-lines-ext ()
  "Like \\[delete-blank-lines], just smarter for my needs.
If on a nonblank line, if following/preceding lines are blank,
delete those, leaving just one blank line. If there already just
one blank line, delete that one.

Behaviour of point is unspecified."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "\\s-*$")
      (call-interactively 'delete-blank-lines))
     ((progn (forward-line 2) (looking-at "\\s-*$"))
      (call-interactively 'delete-blank-lines))
     ((progn (forward-line -1) (looking-at "\\s-*$"))
      (call-interactively 'delete-blank-lines))
     ((progn (forward-line -2) (looking-at "\\s-*$"))
      (call-interactively 'delete-blank-lines)))))

(defun just-one-space-ext ()
  "As \\[just-one-space], but addionaly,
- if point is at the 'end' (only blanks follow) of the line, call \\[kill-line]
- if there is currently just one space around point, deletes it
- if there is currently no space, inserts one space"
  (interactive)
  (skip-syntax-backward " " (line-beginning-position))
  (cond
   ((looking-at "$")
    (kill-line)
    (just-one-space-ext))
   ((looking-at "\\s-+$")
    (delete-region (point) (line-end-position)))
   ((looking-at " \\S-")
    (delete-char 1))
   ((looking-at "\\S-")
    (insert " "))
   (t
    (call-interactively 'just-one-space))))

(defun scroll-up-block ()
  (interactive)
  (scroll-up 12))

(defun scroll-down-block ()
  (interactive)
  (scroll-down 12))

(defun open-line-above (&optional arg)
  (interactive "*p")
  (save-excursion
    (beginning-of-line)
    (open-line arg)))

(defun open-line-below (&optional arg)
  (interactive "*p")
  (end-of-line)
  (when (and arg (> arg 1)) (newline (- arg 1)))
  (newline-and-indent))

(defun move-line-up ()
  (interactive)
  (save-excursion
    (previous-line)
    (beginning-of-line)
    (if (looking-at "[ \t]*$") (kill-line))))

(defun kill-line-ext (&optional arg)
  "As \\[kill-line], but kills the whole line if it's a blank line."
  (interactive)
  (if (and (looking-at "\\s-*$") (looking-back "^\\s-*" (line-beginning-position)))
      (beginning-of-line))
  (call-interactively 'kill-line))

(defun copy-buffer-file-name-as-kill()
  (interactive)
  (kill-new (buffer-file-name)))

(defun beginning-of-line-dwim ()
  "Toggles between beginning of line and the first non white of the line."
  (interactive)
  (cond
   ((and (equal last-command 'beginning-of-line-dwim) (looking-at "^"))
      (back-to-indentation))
   (t
    (move-beginning-of-line nil))))

(defun end-of-line-dwim (&optional arg)
  "Cycles among different positions at the end of the line.
1) end of code
2) start of comment, i.e. the first delimiter
3) start of comment text
4) end of comment text
5) actual end of line
Arg specifies how many positions to forward. Arg can be negative.
On a line with no code, end of code is equal to start of comment.
If point is currently on none of those specific position, point
moves to real end of line."

  (interactive "p")
  (let ((saved-point (point))
        points
        last-non-white ; last non-white character on line
        end-code      ; end of code
        beg-com       ; beginning of comment
        beg-com-text  ; beginning of comment text
        end-com-text ); end of comment text

    ;; BUG: if comment-start-skip etc are not defined, end-of-line-dwim should
    ;; only distinguish between last non-white and real end of line If that is
    ;; done, end-of-line-dwim can be used in virtually all buffers (see
    ;; mybindings.el)

    ;; search last non white, which will be either end of code or end of comment
    ;; text
    (end-of-line)
    (skip-syntax-backward " ")
    (setq last-non-white (point))

    ;; search start of comment and start of comment text
    ;; BUG: the correct thing would be to use , but
    ;; c++-mode sets comment-start-skip incorrectly
    (when comment-start-skip
      (when (re-search-backward comment-start-skip (line-beginning-position) t)
        (setq beg-com (if (member major-mode (list 'c-mode 'c++-mode))
                          (match-beginning 0)
                        (or (match-end 1) (match-beginning 0)))
              beg-com-text (match-end 0)
              end-com-text last-non-white)
        (goto-char beg-com)))

    ;; search end of code.
    (if beg-com
        (progn
          (setq end-com-text last-non-white)
          (skip-syntax-backward " ")
          (setq end-code (point)))
      (setq end-code last-non-white))


    ;;
    (unless beg-com
      (setq beg-com end-code))
    (unless beg-com-text
      (setq beg-com-text end-code))
    (unless end-com-text
      (setq end-com-text end-code))
    (setq points (sort (delete-dups (list end-code beg-com beg-com-text end-com-text (line-end-position))) '<))
    (setq len (length points))
    (when (equal len 0) (error)) ; should never happen

    ;;
    (setq arg (% arg len)) ; 5 positions -> 5 periodic
    ;;     (when (< arg 0)        ; backward n positions = forward (5-n) positions
    ;;       (setq arg (+ len arg)))

    ;;
    (setq actual-index nil)
    (if (or (eq last-command 'end-of-line-dwim) (> arg 1))
        (progn
          (setq actual-index
                (if (setq tail (member saved-point points))
                    (- len (length tail))
                  (- len 2)))
          (setq next-index
                (if (< (+ actual-index arg) len)
                    (+ actual-index arg)
                  (- (+ actual-index arg) len))))
      (setq next-index (- len 1)))

    (goto-char (nth next-index points))))

(defun word-beginning-position ()
  (save-excursion
    (unless (looking-at "\\_<")
      (backward-word 1))
    (point)))

(defun word-end-position ()
  (save-excursion
    (unless (looking-back "\\_>")
      (forward-word 1))
    (point)))

(defun deactivate-mark()
  "Deactivates the mark.
Usefull within macro, since the usual \\[keyboard-quit] stops
macro recording."
  (interactive)
  (setq mark-active nil))

(defun toggle-mark(&optional arg)
  "Similar to `set-mark-command', but toggles"
  (interactive "P")
  (if (and mark-active (not arg))
      (setq mark-active nil)
    (call-interactively 'set-mark-command)))

(defun capitalize-dwim ()
  "Capitalize current word/region.

When region is active, it's identical to `capitalize-region'.
Otherwise, point is moved to beginning of current word first,
from then on its identical to `capitalize-word'."
  (interactive)
  (if mark-active
      (call-interactively 'capitalize-region)
    (unless (looking-at "\\<")
      (backward-word))
    (call-interactively 'capitalize-word)))

(defun downcase-dwim ()
  "Downcase current word/region.
Analogous to `capitalize-dwim'."
  (interactive)
  (if mark-active
      (call-interactively 'downcase-region)
    (unless (looking-at "\\<")
      (backward-word))
    (call-interactively 'downcase-word)))

(defun upcase-dwim ()
  "Upcase current word/region.
Analogous to `capitalize-dwim'."
  (interactive)
  (if mark-active
      (call-interactively 'upcase-region)
    (unless (looking-at "\\<")
      (backward-word))
    (call-interactively 'upcase-word)))

(defun backward-up-list-ext ()
  "As `backward-up-list', but also works within comments/strings."
  (interactive)
  (let ((ps (syntax-ppss)))
    (if (or (nth 3 ps) (nth 4 ps))
        (goto-char (nth 8 ps))
      (backward-up-list))))

(defun down-list-ext ()
  "As `down-list', but also works within comments/strings."
  (interactive)
  (let ((ps (syntax-ppss)))
    (if (or (nth 3 ps) (nth 4 ps))
        (goto-char (nth 8 ps))
      (down-list))))

(defun mark-sexp-ext (&optional arg allow-extend)
  "As `mark-sexp', but treat comments as an expression "
  (interactive)
  (if (and comment-start-skip (looking-at comment-start-skip))
      (mark-comment)
    (call-interactively 'mark-sexp)))

(defun transpose-sexps-ext  (arg)
  "Similar to `transpose-sexps', but can transpose around sexp at point.
It does that when just C-u is used as prefix arg. For example
'{foo} else {bar}' becomes '{bar} else {foo}."
  (interactive "*P")
  (let (text start)
    (if (not (equal arg '(4)))
        (call-interactively 'transpose-sexps)
      (unless (looking-at "\\_<")
        (backward-sexp 1))
      (setq start (point))
      (forward-sexp 2)
      (backward-sexp 1)
      (setq text (buffer-substring start (point)))
      (delete-region start (point))
      (transpose-sexps 1)
      (backward-sexp 1)
      (insert text))))

(defun comment-dwim-ext ()
  "Extends `comment-dwim' with:
If `comment-dwim' does not change point, `c-toggle-comment-style'
is called."
  (interactive)
  (when (eq (point)
            (progn (call-interactively 'comment-dwim) (point)))
    (c-toggle-comment-style)))

(defvar next-prev-start-buffer nil)

;; next-buffer goes into the past
(defun next-buffer-ext ()
  "Switch to the next buffer in cyclic order."
  (interactive)
  (let ((buffer (current-buffer)))
    (when (not (or (eq last-command 'next-buffer-ext)
                 (eq last-command 'previous-buffer-ext)))
        (setq next-prev-start-buffer buffer))
    (switch-to-buffer (other-buffer buffer t))
    (when (equal next-prev-start-buffer (current-buffer))
      (message "newest buffer"))
    (bury-buffer buffer)))

(defun previous-buffer-ext ()
  "Switch to the previous buffer in cyclic order."
  (interactive)
  (when (not (or (eq last-command 'next-buffer-ext)
                 (eq last-command 'previous-buffer-ext)))
    (setq next-prev-start-buffer (current-buffer)))
  (switch-to-buffer (last-buffer (current-buffer) t))
  (when (equal next-prev-start-buffer (current-buffer))
    (message "newest buffer")))

(defun shell-command-ext ()
  "As `shell-command', but when prefix argument is given, leaves
point after and mark before inserted text and removes the last
newline of the inserted text if there is any."
  (interactive)
  (call-interactively 'shell-command)
  (when current-prefix-arg
    (exchange-point-and-mark)
    (when (equal (char-before) ?\n)
      (delete-char -1))))

(defun blank-line ()
  "Returns non-nil when point is on a blank line"
  (save-excursion (beginning-of-line) (looking-at "\\s-*$")))

(defun kill-buffer-ext (&optional arg)
  "As `kill-buffer', however only when arg is non-nil, user can/must choose a buffer."
  (interactive "P")
  (if arg
      (call-interactively 'ido-kill-buffer)
    (kill-buffer (current-buffer))))

;; see also substitute-key-definition, info manual chapter 22.13 Remapping
;; Commands for better solutions, and c-subword-mode-map for an example. When
;; could I use advice instead?
(defun shadow-keys (orig-defun new-defun map)
  "Move all keys belonging to orig-defun (according to where-is-internal) to new-defun"
  (let ((orig-key-list (where-is-internal orig-defun map))
        (orig-key))
    (while orig-key-list
      (setq orig-key (car orig-key-list))
      (define-key map orig-key new-defun)
      (setq orig-key-list (cdr orig-key-list)))))

(defun nil-or-empty-p (str)
  (or (not str) (equal str "")))

(defun yank-and-indent ()
  "Yank and then indent all lines of the just yanked text. If
  delete-selection-mode is non-nil, an active region is deleted
  first."

  (interactive)

  ;; commentify if point or mark are in a comment, and no more comments are
  ;; in between

  ;; delete current region
  (when (and mark-active delete-selection-mode)
    (delete-region (point) (mark)))

  ;; standard yank. That will put region around yanked text
  (call-interactively 'yank)

  (save-excursion
    ;; ensure all lines are completely in the region, so really all lines are
    ;; indented
    (when (> (point) (mark t))
      (exchange-point-and-mark))
    (beginning-of-line)
    (exchange-point-and-mark)
    (when (> (point) (line-beginning-position))
      (forward-line))

    ;; indent region
    (exchange-point-and-mark)
    (indent-region (point) (mark t))))

(defun buffer-substring-sexp-no-properties ()
  "Returns the sexp at point."
  (save-excursion
    (let (end)
      (unless (looking-back "\\_>\\=")
          (forward-sexp))
      (setq end (point))
      (backward-sexp)
      (buffer-substring-no-properties (point) end))))

(defun locate-all-libraries (library &optional nosuffix path interactive-call)
  "As `locate-library', but finds ALL matching libraries in the list `load-path'.
Also, it uses `print', not `message' to output the result when
INTERACTIVE-CALL is non-nil.

BUG: Does not find multiple 'same' libraries within the same
directory (i.e. the .el and the .elc, only one of the two)."
  (interactive (list (completing-read "Locate library: "
                                      (apply-partially
                                       'locate-file-completion-table
                                       load-path (get-load-suffixes)))
                     nil nil
                     t))
  (let ((load-path-orig load-path)
        result-list)
    (while load-path-orig
      (let* ((load-path (list (car load-path-orig)))
             (result-item (locate-library library nosuffix path)))
        (when result-item
          (setq result-list (cons result-item result-list)))
        (setq load-path-orig (cdr load-path-orig))))
    (if interactive-call
        (print result-list)
      result-list)))

(defun undo-get-state ()
  "Return a handler for the current state to which we might want to undo.
The returned handler can then be passed to `undo-revert-to-handle'."
  (unless (eq buffer-undo-list t)
    buffer-undo-list))

(defun undo-revert-to-state (handle)
  "Revert to the state HANDLE earlier grabbed with `undo-get-handle'.
This undoing is not itself undoable (aka redoable)."
  (unless (eq buffer-undo-list t)
    (let ((new-undo-list (cons (car handle) (cdr handle))))
      ;; Truncate the undo log at `handle'.
      (when handle
        (setcar handle nil) (setcdr handle nil))
      (unless (eq last-command 'undo) (undo-start))
      ;; Make sure there's no confusion.
      (when (and handle (not (eq handle (last pending-undo-list))))
        (error "Undoing to some unrelated state"))
      ;; Undo it all.
      (while (not (memq pending-undo-list '(nil t))) (undo-more 1))
      ;; Reset the modified cons cell to its original content.
      (when handle
        (setcar handle (car new-undo-list))
        (setcdr handle (cdr new-undo-list)))
      ;; Revert the undo info to what it was when we grabbed the state.
      (setq buffer-undo-list handle))))

(defun re-search-backward-greedy (regexp &optional bound noerror count)
  "As `re-search-backward-greedy' but makes a match as long as possible.
I.e. a match extends to the left as far as possible."
  (when (re-search-backward regexp bound noerror count)
    (while (and (or (null bound) (>= (point) bound))
                (save-excursion (backward-char) (looking-at regexp)))
      (backward-char))
    (point)))

(defun rename-buffer-ext ()
  "As `rename-buffer' but additionally does some magic in certain cases."
  (interactive)
  (cond
   ((string-match "\\*grep.*\\*" (buffer-name))
    (let* ((default (save-excursion
                      (goto-char (point-min))
                      (re-search-forward "^-i?e\\s-+'\\(.*?\\)'\\s-*$")
                      (match-string-no-properties 1)))
           (new-name (read-string
                      (concat "Rename buffer (to new name, only base part. Default: " default "): ")
                      nil nil default)))
      (rename-buffer (concat "*grep " new-name "*"))))
   (t
    (call-interactively 'rename-buffer))))

;;; simple-ext.el ends here
