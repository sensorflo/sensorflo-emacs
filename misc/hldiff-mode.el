;;; hldiff-mode.el --- a minor mode for highlighting diffs/changes
;;
;; Copyright 2009-2012 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; Created: 2009
;; Version: 0.4.0
;; Keywords: wp AsciiDoc
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
;; Highlights regions which have changed, whereas changed can mean
;; one of the following:
;; - my current changes: file to buffer
;; - my changes: (file or buffer) to working(=base) revision
;; - their changes: working(=base) revision to head revision
;;
;; - conflicting regions specially highlighted
;;
;;; Variables:

(defgroup hldiff nil
  ""
  :group 'mmmmmmmm)

(defface hldiff-my-change
  '((t (:inherit font-lock-warning-face)))
  ""
  :group 'hldiff)

(defface hldiff-their-change
  '((t (:inherit font-lock-warning-face)))
  ""
  :group 'hldiff)

(defface hldiff-conflict
  '((t (:inherit font-lock-warning-face)))
  ""
  :group 'hldiff)

(defvar hldiff- nil "")

(defvar hldiff-lstate-buf "hldiff-lstate" "")
flo
(defvar hldiff-lstate-str "" "")
x
x
x
(defvar hldiff-lstate-running nil "")

;;; Code:

(defun hldiff-trigger-get-lock-state()
  "Triggers retreiving the lock state of the file.
This is an asynchronous process; the result is retreived by
`hldiff-lstate-sentinel'."
  ;; only call process every x seconds
  (message "trigger")
  (when (not hldiff-lstate-running)
    (let* ((proc (start-process
                  "hldiff-lstate-proc" nil;todo create/empty buf
                  "si" "viewlocks" (buffer-file-name))))
      (message "started process")
      (message (number-to-string (process-exit-status proc)))
      (setq hldiff-lstate-str "")
      (set-process-filter 'hldiff-lstate-filter)
      (set-process-sentinel proc 'hldiff-lstate-sentinel)
      (setq hldiff-lstate-running t))))

(defun hldiff-lstate-filter(proc str)
  (message "filter")
  (setq (concat hldiff-lstate-str str)))

(defun hldiff-lstate-sentinel(proc event)
  (message (concat "sentinel" event))
  (when (memq (process-status proc) '(signal exit))
    (message "exited")
    (setq hldiff-lstate-running nil)
    (let* ((exit-status (process-exit-status proc)))
      (progn ;(with-current-buffer hldiff-lstate-buf
        (message "start parsing")
        (goto-char (point-min))
        ;; (while (re-search-forward "file *\\(.*?\\): *\n" nil t)
        ;;   (message (concat "found file " (match-string 1)))
        ;;   (when (get-buffer (match-string 1))
        ;;     (with-current-buffer (match-string 1)
        ;;       (if (looking-at "bla")
        ;;           (setq heade-line-format "locked by")
        ;;         (setq heade-line-format "no locks")))))
        (message "finished parsing")
        (delete-process proc)))))

(defun hldiff-parse-diff()
  (while (re-search-forward "^@@ [-+]\\([0-9]+\\),\\([0-9]+\\) +[-+][0-9]+,[0-9]+ +@@")
    ))

(define-minor-mode hldiff-mode
  "Minor mode for making identifiers likeThis readable.
When this mode is active, it tries to add virtual separators (like underscores)
at places they belong to."
  :group 'glasses :lighter " hldiff"
  (if hldiff-mode
      (progn
        (set (make-local-variable 'hldiff-lstate-timer)
           (run-with-idle-timer 1 t 'hldiff-trigger-get-lock-state))
        (setq hldiff-lstate-running nil))
    (cancel-timer hldiff-lstate-timer)))

(provide 'hldiff)

;;; hldiff-mode.el ends here
