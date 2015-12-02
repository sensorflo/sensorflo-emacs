;;; git-irb-mode.el --- a major-mode for editing git's interative rebase files
;;
;; Copyright 2012 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; Created: 2012
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
;;
;;; Commentary:
;;
;; When 'git rebase --interactive' is started, git fires up an editor which lets
;; you edit, one line per commit, what exactly git should do in the interactive
;; rebase. git-irb-mode is for such files.
;;
;;
;;; Code:
(require 'git-rebase)

(defun git-irb-move-commit-at-point-to-referenced-commit ()
  (interactive)
  (let* ((src-start (line-beginning-position))
         (src-end (1+ (line-end-position)))
         (src-column (column))
         (move-to-after-dst-commit t)
         (re-prebanter "^\\s-*\\w+\\s-+\\w+\\s-*")
         (re-dst (concat re-prebanter "")))
    (beginning-of-line)
    (if (not (looking-at (concat re-prebanter ".*commit fix \\(?:for \\(?:commit \\)?\\)?['\"]\\(.*\\)['\"]")))
        (error "point is not on a commit line having a reference to another commit on it"))
    (goto-char (point-min))
    (when (not (save-match-data (re-search-forward (concat re-prebanter ".*" (match-string 1)) src-start t)))
      (goto-char src-end)
      (setq move-to-after-dst-commit nil)
      (when (not (save-match-data (re-search-forward (concat re-prebanter ".*" (match-string 1)) nil t)))
        (error (concat "referenced commit '" (match-string 1) "' not found"))))
    (forward-line (if move-to-after-dst-commit 1 0))
    (insert (delete-and-extract-region src-start src-end))

    ;; remove markers that this line is to be moved, so it will no longer be highlighted
    (forward-line -1)
    (when (re-search-forward "\\(\\s-*!+\\s-*\\(commit\\s-*\\)?fix\\s-*!+\\s-*\\|\\s-*!+\\s-*\\)$" (line-end-position) t)
      (delete-region (match-beginning 0) (match-end 0)))

    (end-of-line)
    (insert " << auto moved this line")
    (move-to-column src-column)))

(defun git-irb-move-all ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^\\s-*\\w+\\s-+\\w+\\s-*.*commit\\s-+fix\\s-+\\(?:for\\s-+\\(?:commit\\s-+\\)?\\)?'\\(.*\\)'" nil t)
    (git-irb-move-commit-at-point-to-referenced-commit)))

;; todo!!!!
;;   when changing comment, and marking as s or r, then the evuentually poping
;;   up 'edit commit msg' buffer shall fetch that changed commit msg

;;;###autoload
(define-derived-mode git-irb-mode git-rebase-mode "git-irb"
  "A major-mode for editing git's interative rebase files.

See (finder-commentary \"git-irb-mode\")."
  )

(provide 'git-irb-mode)

;;; git-irb-mode.el ends here
