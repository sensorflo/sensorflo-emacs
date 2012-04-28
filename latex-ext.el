;;; latex-ext.el --- extensions for latex-mode
;;
;; Copyright 2011 Florian Kaufmann <sensorflo@gmail.com>
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
;;; Code:

(defun toggle-yas (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (leave-yas)
    (enter-yas)))

(defun enter-yas ()
  (interactive)
  (let (begin)
    (re-search-backward "^\\\\begin{.*?}")
    (forward-line 1)
    (setq begin (point))
    (re-search-forward' "^\\\\end{.*?}")
    (beginning-of-line)
    (narrow-to-region begin (point))
    (yas-mode)))

(defun leave-yas ()
  (interactive)
  (widen)
  (latex-mode))

(defun latex-ext-up ()
  ""
  (interactive)
  (re-search-backward "\\\\\\(begin\\|chapter\\|sub\\{,3\\}section\\){")
  (down-list))

(defun latex-ext-toggle ()
  ""
  (interactive))

(defun latex-ext-remove-surrounding ()
  "Removes the surrounding
  - command, e.g. \textbf{ ... }
  - group e.g. {\tiny ...} "
  (interactive)
  (let ()
    ;; todo: add \verb|...|
    (re-search-backward "\\\\\\w+{\\|{\\s-*\\\\\\w+")
    (delete-region (point) (match-end 0))
    (set-mark (point))
    (re-search-forward "}")
    (delete-backward-char 1)))

(defun latex-ext-dec-level ()
  ""
  (interactive)
  (save-excursion
    (let ((r "\\\\\\(chapter\\|\\(sub\\)\\{,3\\}section\\){")
          match
          new)
      (end-of-line)
      (when (not (re-search-backward r (line-beginning-position) t))
        (error "Line not decrementable"))
      (setq old (match-string 1))
      (setq new
            (cond
             ((equal old "chapter") "section")
             ((equal old "section") "subsection")
             ((equal old "subsection") "subsubsection")
             ((equal old "subsubsection") "subsubsubsection")
             (t old)))
      (goto-char (match-beginning 1))
      (delete-region (point) (match-end 1))
      (insert new))))

(defun latex-ext-inc-level ()
  ""
  (interactive)
  (save-excursion
    (let ((r "\\\\\\(chapter\\|\\(sub\\)\\{,3\\}section\\){")
          match
          new)
      (end-of-line)
      (when (not (re-search-backward r (line-beginning-position) t))
        (error "line not incrementable"))
      (setq old (match-string 1))
      (setq new
            (cond
             ((equal old "section") "chapter")
             ((equal old "subsection") "section")
             ((equal old "subsubsection") "subsection")
             ((equal old "subsubsubsection") "subsubsection")
             (t old)))
      (goto-char (match-beginning 1))
      (delete-region (point) (match-end 1))
      (insert new))))

(provide 'latex-ext)

;;; latex-ext.el ends here