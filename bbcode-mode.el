;;; bbcode-mode.el --- a major-mode for editing bbcode
;;
;; Copyright 2011 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: http://sensorflo-emacs.googlecode.com/svn/trunk/bbcode-mode.el
;; Created: 2011
;; Version: 0.1
;; Keywords: wp bbcode
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
;; A first try in implementing a bbcode major mode, far from being stable,
;; or from really being usable for the public.
;;

;;; Variables: 

(require 'outline)
(require 'markup-faces)

(defvar bbcode-mode-hook nil
  "Normal hook run when entering bbcode mode.")


;;; Code: 

(defconst bbcode-font-lock-keywords
  (list
   ;; formatting
   (list "\\(\\[i\\]\\)\\(.*?\\(?:\n.*?\\)*?\\)\\(\\[/i\\]\\)"
	 '(1 markup-hide-delimiter-face)
	 '(2 markup-emphasis-face)
	 '(3 markup-hide-delimiter-face))
   (list "\\(\\[b\\]\\)\\(.*?\\(?:\n.*?\\)*?\\)\\(\\[/b\\]\\)"
	 '(1 markup-hide-delimiter-face)
	 '(2 markup-strong-face)
	 '(3 markup-hide-delimiter-face))
   (list "\\(\\[u\\]\\)\\(.*?\\(?:\n.*?\\)*?\\)\\(\\[/u\\]\\)"
	 '(1 markup-hide-delimiter-face)
	 '(2 markup-underline-face)
	 '(3 markup-hide-delimiter-face))

   ;; blocks
   (list "\\(\\[code\\]\\)\\(.*?\\(?:\n.*?\\)*?\\)\\(\\[/code\\]\\)"
	 '(1 markup-delimiter-face)
	 '(2 markup-code-face)
	 '(3 markup-delimiter-face))
   (list "\\(\\[quote\\]\\)\\(.*?\\(?:\n.*?\\)*?\\)\\(\\[/quote\\]\\)"
	 '(1 markup-delimiter-face)
	 '(2 markup-gen-face)
	 '(3 markup-delimiter-face))

   ;; anchors/links
   (list "\\(\\[url\\(?:=[^]]*?\\)?\\]\\)\\(.*?\\(?:\n.*?\\)*?\\)\\(\\[/quote\\]\\)"
	 '(1 markup-delimiter-face)
	 '(2 markup-reference-face)
	 '(3 markup-delimiter-face))

   ;; images
   (list "\\(\\[img\\]\\)\\(.*?\\(?:\n.*?\\)*?\\)\\(\\[/img\\]\\)"
	 '(1 markup-delimiter-face)
	 '(2 markup-replacement-face)
	 '(3 markup-delimiter-face))

   ;; list
   (list "\\[/?list=?\\]" '(0 markup-delimiter-face))
   (list "\\[\\*\\]" '(0 markup-list-face))
   
   ;; table

   ;; misc
   (list "\\[linie\\]\\[/linie\\]" '(0 markup-replacement-face))
   )
  "Keywords to highlight in bbcode-mode")

(defun bbcode-re-paragraph-separate()
  (concat
   "\\(?:"
   ;; empty line
   "[ \t]*$" "\\|"
   ;; tables / lists
   "[ \t]*\\[/?:list=?\\]" "\\|"
   ;; block stuff
   "[ \t]*\\[\\(?:code\\|quote\\)\\]" 
   "\\)" ))

(defun bbcode-re-paragraph-start()
  (concat
   "\\(?:"
   (bbcode-re-paragraph-separate) "\\|"
   "\\(?:"
   ;; list/table items
   "[ \t]*\\[\\*\\]" 
   "\\)"
   "\\)"))

;;;###autoload
(define-derived-mode bbcode-mode text-mode "bbcode"
  "Major mode for viewing log files.
Turning on bbcode mode runs the normal hook `bbcode-mode-hook'."
  
  ;; pages
  ;; (set (make-local-variable 'page-delimiter) bbcode-re-page) 
  ;; (set (make-local-variable 'font-lock-extra-managed-props) (list 'invisible))

  ;; font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(bbcode-font-lock-keywords nil nil ((?_ . "w")) ))
  
  ;; paragraphs
  (set (make-local-variable 'paragraph-separate) (bbcode-re-paragraph-separate)) 
  (set (make-local-variable 'paragraph-start) (bbcode-re-paragraph-start))
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  
  ;; filladapt
  (when (boundp 'filladapt-token-table)
    (let ((bullet-regexp "\\[\\*\\]"))
      (unless (assoc bullet-regexp filladapt-token-table)
	(setq filladapt-token-table
	      (append filladapt-token-table
		      (list (list bullet-regexp 'bullet)))))))

  ;; ;; outline mode
  ;; todo: startup / prod start / homen sind 'subchapters'
  ;; (set (make-local-variable 'outline-regexp)
  ;;      (concat "\\(?:"
  ;; 	       (mapconcat 'identity (list bbcode-re-page bbcode-re-chapter bbcode-re-chapter-end) "\\|")
  ;; 	       "\\)"))
  ;; (set (make-local-variable 'outline-level) 'bbcode-outline-level)
  ;; (run-hooks 'bbcode-mode-hook)
)

(provide 'bbcode-mode)

;;; bbcode-mode.el ends here
