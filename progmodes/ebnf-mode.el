;;; ebnf-mode.el --- a major-mode for editing ebnf files in emacs
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
;; EBNF is a family of metasyntax notations used for expressing context-free
;; grammars (http://en.wikipedia.org/wiki/Ebnf).
;; 
;; 
;;; Code:
(require 'font-lock-ext) ; https://github.com/sensorflo/font-lock-ext/
(require 'meta-syntax-faces) ; https://github.com/sensorflo/sensorflo-emacs/, then progmodes/meta-syntax-faces.el

(defvar ebnf-mode-hook nil)

(defvar ebnf-mode-map nil "Keymap for ebnf-mode.")
(unless ebnf-mode-map
  (setq ebnf-mode-map (make-sparse-keymap)))

(defconst ebnf-mode-font-lock-keywords
  (list
   (list "\\((\\*\\)\\([^*]*\\(?:\\*[^)][^*]*\\)*\\)\\(\\*)\\)"
	 '(1 font-lock-comment-delimiter-face)
	 '(2 font-lock-comment-face)
	 '(3 font-lock-comment-delimiter-face))
   (cons "\"[^\"]*\"" 'meta-syntax-literal-face)
   (cons "'[^']*'" 'meta-syntax-literal-face)
   (cons "\\?[^?]*\\?" 'font-lock-comment-face )
   (list "^\\s-*\\(\\w+\\)\\s-*=" '(1 meta-syntax-rule-def))
   ))

(defun ebnf-beginning-of-defun ()
  (interactive)
  (re-search-backward "^\\s-*\\w+\\s-*="))

(defun ebnf-end-of-defun ()
  (interactive)
  (re-search-forward "^\\s-*\\w+\\s-*=")
  (forward-line -1))

;;;###autoload
(defun ebnf-mode()
  (interactive)
  (kill-all-local-variables)
  
  ;; TODO: default compile string
  
  (use-local-map ebnf-mode-map)  
  (set (make-local-variable 'comment-start) "(* ")
  (set (make-local-variable 'comment-end) " *)")
  (set (make-local-variable 'comment-start-skip) "\\((\\*\\)\\s-*")
  (set (make-local-variable 'comment-end-skip) "\\s-*\\(\\*)\\)")
  
  (set (make-local-variable 'beginning-of-defun-function) 'ebnf-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'ebnf-end-of-defun)
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(ebnf-mode-font-lock-keywords t nil nil nil))
  
  (setq major-mode 'ebnf-mode mode-name "ebnf")

  (run-hooks 'ebnf-mode-hook))

(provide 'ebnf-mode)

;;; ebnf-mode.el ends here
