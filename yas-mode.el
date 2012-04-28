;;; yas-mode.el --- a major-mode for editing yasmala files in emacs
;; 
;; Copyright 2009-2011 Florian Kaufmann <sensorflo@gmail.com>
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
;;; Code:
(require 'font-lock-ext)

(defvar yas-mode-hook nil)

(defvar yas-mode-map nil "Keymap for yas-mode.")
(unless yas-mode-map
  (setq yas-mode-map (make-sparse-keymap)))

;; (defvar yas-mode-syntax-table
;;   (let ((st (make-syntax-table)))

;;     ;; white space
;;     (modify-syntax-entry ?\s " " st)	
;;     (modify-syntax-entry ?\t " " st)
;;     (modify-syntax-entry ?\n " " st)
;;     (modify-syntax-entry ?\f " " st)

;;     ;; word
;;     (modify-syntax-entry ?\_ "w" st)

;;     ;; symbol : none
    
;;     ;; punctuation (operators)
;;     (modify-syntax-entry ?\! "." st)
;;     (modify-syntax-entry ?\@ "." st)
;;     (modify-syntax-entry ?\^ "." st)
;;     (modify-syntax-entry ?\& "." st)
;;     (modify-syntax-entry ?\* "." st)
;;     (modify-syntax-entry ?\- "." st)
;;     (modify-syntax-entry ?\+ "." st)
;;     (modify-syntax-entry ?\= "." st)
;;     (modify-syntax-entry ?\; "." st)
;;     (modify-syntax-entry ?\: "." st)
;;     (modify-syntax-entry ?\| "." st)
;;     (modify-syntax-entry ?\. "." st)
;;     (modify-syntax-entry ?\, "." st)
;;     (modify-syntax-entry ?\/ "." st)

;;     ;; parentheses
;;     (modify-syntax-entry ?\( "(" st)
;;     (modify-syntax-entry ?\{ "(" st)
;;     (modify-syntax-entry ?\[ "(" st)
;;     (modify-syntax-entry ?\< "(" st)
;;     (modify-syntax-entry ?\) ")" st)
;;     (modify-syntax-entry ?\} ")" st)
;;     (modify-syntax-entry ?\] ")" st)
;;     (modify-syntax-entry ?\> ")" st)

;;     ;; escapes
;;     (modify-syntax-entry ?\\ "\\" st)

;;     ;; terminal/string quote
;;     (modify-syntax-entry ?\" "\"" st)

;;     ;; comments
;;     (modify-syntax-entry ?\# "<12" st)
;;     (modify-syntax-entry ?\n ">" st)

;;     st)
;;   "Syntax table used while in `yas-mode'.")

(require 'meta-syntax-faces)

(defconst yas-mode-font-lock-keywords
  (list
   ;; comment / plain
   ;;(cons "\\(%%.*\\)$" 'font-lock-unimportant-face)
   
   ;; comment / semantic
   (list "\\(#[_^]?\\)\\(\\(\\w\\|_\\)+\\)" '(1 font-lock-comment-delimiter-face) '(2 font-lock-comment-face) )
   (list "\\(##\\s-*\\)\\(.*\\)$" '(1 font-lock-comment-delimiter-face) '(2 font-lock-comment-face) )
   (list "\\(#\\*\\s-*\\)\\(\\(.\\|\n\\)*?\\)\\(\\*#\\)" '(1 font-lock-comment-delimiter-face) '(2 font-lock-comment-face) '(4 font-lock-comment-delimiter-face))
   
   ;; string / terminal
   (list "\\('\\)\\(\\(\\w\\|_\\)+\\)" '(1 meta-syntax-hide-delimiter-face) '(2 meta-syntax-literal-face) )
   (list "\\(''\\)\\(.*$\\)" '(1 meta-syntax-hide-delimiter-face) '(2 meta-syntax-literal-face) )
   (list "\\('\\*\\)\\(.*?\\)\\(\\*#\\)" '(1 meta-syntax-hide-delimiter-face) '(2 meta-syntax-literal-face) '(3 meta-syntax-hide-delimiter-face))
   
   (list "\\(`\\)\\(\\S-*\\)" '(1 meta-syntax-hide-delimiter-face) '(2 meta-syntax-literal-face))
   (list "\\(\"\\)\\(.*?\\)\\(\"\\)" '(1 meta-syntax-hide-delimiter-face) '(2 meta-syntax-literal-face) '(3 meta-syntax-hide-delimiter-face))
   (list "\\('\\[\\)\\(.*?\\)\\(\\]\\)" '(1 meta-syntax-hide-delimiter-face) '(2 meta-syntax-char-class-face) '(3 meta-syntax-hide-delimiter-face))
   (list "\\(\\\\\\)\\([^a-zA-Z0-9]\\)" '(1 meta-syntax-hide-delimiter-face) '(2 meta-syntax-literal-face))
   (cons "\\\\[a-zA-Z]" 'meta-syntax-replacement-face)
   
   ;; anchor/backref
   (cons "\\$." 'font-lock-constant-face)

   ;; preprocessor
   (cons "%\\w+" 'font-lock-keyword-face)

   ;; grammar
   (cons "^\\s-*\\(\\w+\\)\\s-*=" (list 1 'font-lock-variable-name-face))
   (cons "^\\s-*\\(\\w+\\)\\s-*%=" (list 1 'font-lock-function-name-face))
   (cons "^\\s-*\\(\\w+\\)\\s-*<.*?>\\s-*=" (list 1 'font-lock-function-name-face))
   ))

(defun yas-beginning-of-defun ()
  (interactive)
  (re-search-backward "^\\s-*\\w+\\s-*="))

(defun yas-end-of-defun ()
  (interactive)
  (re-search-forward "^\\s-*\\w+\\s-*=")
  (forward-line -1))

(defun yas-mode()
  (interactive)
  (kill-all-local-variables)
  ;; (set-syntax-table yas-mode-syntax-table)
  
  ;; TODO: mark/forward/backward defun
  
  ;; TODO: default compile string
  
  (use-local-map yas-mode-map)  
        
  (set (make-local-variable 'comment-start) "## ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#[#*]+\\s-*")
  
  (set (make-local-variable 'beginning-of-defun-function) 'yas-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'yas-end-of-defun)
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(yas-mode-font-lock-keywords t nil nil nil))
  
  (setq major-mode 'yas-mode mode-name "yas")

  ;; TODO: font-lock-mark-block-function
  
  (run-hooks 'yas-mode-hook))

(provide 'yas-mode)

;;; yas-mode.el ends here