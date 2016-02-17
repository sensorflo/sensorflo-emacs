;;; rl-mode.el --- a major-mode for editing readline init files
;;
;; Copyright 2009-2012 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Filename: rl-mode.el
;; Description: a major-mode for editing readline init files
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

;;; Variables

(defconst rl-mode-version "0.1")

;;; Code

(defconst rl-font-lock-keywords
  (eval-when-compile
    (list
     (list "^\\s-*\\(set\\)\\s-*\\(\\sw+\\)\\s-*\\(\\sw+\\)?" (list 1 'font-lock-keyword-face) (list 2 'font-lock-variable-name-face) (list 3 'font-lock-constant-face))
     (cons "^#.*\n" 'font-lock-comment-face)
     (list "^\\(.*\\):" (list 1 'font-lock-constant-face))
     (list "^\\(\\$\\sw+\\)" (list 1 'font-lock-preprocessor-face))
     )))

(defun rl-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "rl-mode, version %s" rl-mode-version))

;;;###autoload
(define-derived-mode rl-mode prog-mode "rl"
  "Major mode for editing readline init files.
Turning on rl mode runs the normal hook `rl-mode-hook'."
  (interactive)

  ;; syntax table
  (modify-syntax-entry ?- "w")

  ;; comments
  (set (make-local-variable 'comment-column) 0)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "^#\\s-*")
  (set (make-local-variable 'comment-end-skip) "\\s-*\n")

  ;; TODO: make paragraphs
  ;; paragraphs
  ;; (set (make-local-variable 'paragraph-separate) "")
  ;; (set (make-local-variable 'paragraph-start) "")
  ;; (set (make-local-variable 'paragraph-ignore-fill-prefix) t)

  ;; pages
  ;; (set (make-local-variable 'page-delimiter) "^<<<+$")

  ;; misc
  (set (make-local-variable 'require-final-newline) t)

  ;; font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(rl-font-lock-keywords nil nil ((?_ . "w")) ))

  ;; outline mode
  ;; BUG: if there are many spaces\tabs after =, level becomes wrong
  ;; (set (make-local-variable 'outline-regexp) "^=\\{1,5\\}[ \t]+[^ \t\n]")

  ;; TODO: font-lock-mark-block-function etc
  ;; TODO: indent-functions
  ;; TODO: outline
  ;; TODO: spell check: do/dont check comments. only check 'real' text, i.e.
  ;; text that is also in the output.
  )

(add-to-list 'auto-mode-alist '("\\.?inputrc\\'" . rl-mode))

(provide 'rl-mode)

;;; rl-mode.el ends here
