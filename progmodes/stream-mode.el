;;; stream-mode.el --- a major-mode for editing indel stream files in emacs
;; 
;; Copyright 2010-2012 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://sourceforge.net/projects/sensorflo-emacs/
;; Created: 2010
;; Keywords: languages
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
;;; Variables

(defconst stream-mode-version "0.1")

(defvar stream-mode-hook nil
  "Normal hook run when entering stream mode.")

;;; Code

(defconst stream-font-lock-keywords
  (list
   (cons "\\b\\(if\\|else\\|do\\|while\\|for\\|switch\\)\\b" font-lock-keyword-face)
   (list "#{\\([^}\n]*\\)}" '(1 font-lock-variable-name-face))
   (cons ":\\(\\w\\)" font-lock-keyword-face)
   (cons "\\b\\([0-9]+\.[0-9]\\|\\.?[0-9]+\\|[0-9]+\\.?\\)\\b" font-lock-constant-face)
   (list "\\b\\(Set\\) +\\(\\sw+\\) *=" '(1 font-lock-keyword-face) '(2 font-lock-variable-name-face))
   ))

;;;###autoload
(define-derived-mode stream-mode text-mode "stream"
  "Major mode for editing stream files.
Turning on stream mode runs the normal hook `stream-mode-hook'."
  (interactive)
  
  ;; syntax table
  (modify-syntax-entry ?\" "\"")
  (modify-syntax-entry ?\_ "w")
  (modify-syntax-entry ?\n "> b")
  (modify-syntax-entry ?\r "> b")
  (modify-syntax-entry ?/  ". 124b")
  (modify-syntax-entry ?*  ". 23")
  
  ;; comments
  (set (make-local-variable 'comment-column) 0)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "//\\s-*")
  (set (make-local-variable 'comment-end-skip) "\\s-*\n")
  
  ;; TODO: make paragraphs using blank lines / OBJ_DEF 
  ;; paragraphs
  ;;   (set (make-local-variable 'paragraph-separate) xxx)
  ;;   (set (make-local-variable 'paragraph-start) xxx )
  ;;   (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  
  ;; misc
  (set (make-local-variable 'require-final-newline) t)
  
  ;; font lock
  (set (make-local-variable 'font-lock-defaults) '(stream-font-lock-keywords))
  
  ;; 
  (run-hooks 'stream-mode-hook))

(provide 'stream-mode)

;;; stream-mode.el ends here
