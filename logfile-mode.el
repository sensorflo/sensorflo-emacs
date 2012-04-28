;;; logfile-mode.el --- a major-mode for viewing logfiles
;;
;; Copyright 2011 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; Created: 2011
;; Version: 0.1
;; Keywords: log
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
;; Curently pretty much tailored for the nova project, i.e. not yet a general
;; logfile viewer mode.
;;
;;; Variables: 

;; (require 'hide-lines)
(require 'outline)
(require 'markup-faces)
(require 'font-lock-ext)

(defvar logfile-mode-hook nil
  "Normal hook run when entering logfile mode.")

(setq logfile-re-prefix
      "^\\(?:\\([-a-zA-Z0-9_]+\\) +\\([[0-9:,]*]:\\)[ \t]*\\)")
(setq logfile-re-error
      (concat logfile-re-prefix "?\\(.*?\\([eE][rR][rR][oO][rR]\\|exception\\|[aA][Ss][sS][Ee][rR][tT]\\).*$\\)"))
(setq logfile-re-warning
      (concat logfile-re-prefix "?\\(.*?\\(?:[wW][aA][rR][nN]\\|!!+\\).*$\\)"))
(setq logfile-re-page
      (concat logfile-re-prefix "\\(MLoaderModules::load(libappstartlib.so).*$\\|logfile-continue\\)"))
(setq logfile-re-chapter
      (concat logfile-re-prefix "\\(\\(?:.*?Start Production\\|\\*+add tasks HomeTaskCamScA\\).*$\\)"))
(setq logfile-re-chapter-end
      (concat logfile-re-prefix "\\(\\(?:Startup done!!!\\|@REGTEST: Machine is homed\\).*$\\)"))

;; (defvar logfile-hide-keywords
;;   (list
;;    (list (mapconcat 'identity
;; 		    (list logfile-re-error
;; 			  logfile-re-warning
;; 			  logfile-re-page) 
;; 		    "\\|")
;; 	 '(1 '(face font-lock-unimportant invisible t) t))))

(defvar default 'default)

;;; Code: 

;; (defun logfile-filter(&optional arg)
;;   (interactive "p")
;;   (if (> arg 1)
;;       (font-lock-remove-keywords nil logfile-hide-keywords)
;;     (font-lock-add-keywords nil logfile-hide-keywords)))

(defconst logfile-font-lock-keywords
  (list
     ;; sections
     (list logfile-re-page
	   '(1 font-lock-semi-unimportant)
	   '(2 default)
	   '(3 markup-title-2-face))
     (list logfile-re-chapter '(3 markup-title-3-face))
     (list logfile-re-chapter-end '(3 markup-title-4-face))

     ;; normal (inclusive warning/error level) log lines with optional task prefix
     (list logfile-re-prefix '(1 font-lock-semi-unimportant) '(2 font-lock-semi-unimportant))
     (list logfile-re-error '(3 font-lock-warning-face))
     (list logfile-re-warning '(3 font-lock-variable-name-face))

     ;; sgml
     (list "^[ \t]*</?[^>\n]*>[ \t]*$"
	   '(0 font-lock-keyword-face))
     (list "^[ \t]*\\(</?[^>\n]*>\\)\\(.*?\\)\\(</?[^>\n]*>\\)[ \t]*$"
     	   '(1 font-lock-keyword-face)
     	   '(2 font-lock-constant-face)
     	   '(3 font-lock-keyword-face))

     ;; task backtrace
     (list "^\\(\[0x[a-fA-F0-9]+\]\\) *\\(?:\\(.*?\\)\\((.*?)\\)? in \\(.*?\\) at \\(.*?\\):\\(.*\\)\\)?$"
     	   '(0 font-lock-unimportant) ; overall
     	   '(1 font-lock-unimportant t t) ; address
     	   '(2 font-lock-keyword-face t t) ; method name
     	   '(3 font-lock-semi-unimportant t t) ; method args
     	   '(4 font-lock-semi-unimportant t t)	; lib
     	   '(5 default t t) ; filename
     	   '(6 default t t)) ; lineno
   )
  "Keywords to highlight in logfile-mode")

(defun logfile-outline-level ()
  (cond
   ((looking-at logfile-re-page) 1)
   ((looking-at logfile-re-chapter) 2)
   ((looking-at logfile-re-chapter-end) 3)
   (t 4)))

(define-derived-mode logfile-mode text-mode "logfile"
  "Major mode for viewing log files.
Turning on logfile mode runs the normal hook `logfile-mode-hook'."
  
  (setq buffer-read-only t)
  ;; pages
  (set (make-local-variable 'page-delimiter) logfile-re-page) 
  (set (make-local-variable 'font-lock-extra-managed-props) (list 'invisible))

  ;; font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(logfile-font-lock-keywords nil nil ((?_ . "w")) ))
  
  ;; ;; outline mode
  ;; todo: startup / prod start / homen sind 'subchapters'
  (set (make-local-variable 'outline-regexp)
       (concat "\\(?:"
	       (mapconcat 'identity (list logfile-re-page logfile-re-chapter logfile-re-chapter-end) "\\|")
	       "\\)"))
  (set (make-local-variable 'outline-level) 'logfile-outline-level)
  (run-hooks 'logfile-mode-hook))

(provide 'logfile-mode)

;;; logfile-mode.el ends here
