;;; vssconf-mode.el --- a major-mode for editing vss configuration files in emacs
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
;;; Commentray:
;; 
;; A major-mode for editing vss configuration files in emacs
;; 
;;; Code:

(defvar vssconf-mode-hook nil)

(defvar vssconf-mode-map nil "Keymap for vssconf-mode.")
(unless vssconf-mode-map
  (setq vssconf-mode-map (make-sparse-keymap)))

(defconst vssconf-mode-font-lock-keywords
  (list
   ;; comment / semantic
   (cons "REM.*$" 'font-lock-comment-face)
   
   ;; string / terminal
   (cons "\".*?\"" 'font-lock-string-face)
   
   ;; preprocessor
   (cons "#define" 'font-lock-preprocessor-face)
   ))

(defun vssconf-mode()
  (interactive)
  (kill-all-local-variables)
  
  (use-local-map vssconf-mode-map)  
  
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start "\\s-*\\w+\\s-*=.*$\\|\\s-*$"
        paragraph-separate "\\s-*$")
        
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (setq comment-start "REM "
	comment-end ""
	comment-start-skip "REM\\s-*")
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(vssconf-mode-font-lock-keywords t nil nil nil))
  
  (setq major-mode 'vssconf-mode
        mode-name "vssconf")

  (local-set-key [(control j)] 'backward-char)
  (local-set-key [(control l)] 'forward-char)
  (run-hooks 'vssconf-mode-hook))

;; KLUDGE: only add if not already in there
(add-to-list 'auto-mode-alist '( "\\.cfg\\'" . vssconf-mode))

(provide 'vssconf-mode)


;;; vssconf-mode.el ends here
