;;; yas-mode.el --- a major-mode for editing yasmala files in emacs
;;
;; Copyright 2009-2014 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://github.com/sensorflo/sensorflo-emacs then
;;      progmodes/yas-mode.el
;; Created: 2009
;;
;; Yasmala is Yet Another Syntax MetA LAnguage. Documentation is available on
;; https://github.com/sensorflo/sensorflo-docs.
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
(require 'font-lock-ext) ; https://github.com/sensorflo/font-lock-ext/
(require 'meta-syntax-faces) ; https://github.com/sensorflo/sensorflo-emacs/,
                             ; then progmodes/meta-syntax-faces.elo

(defvar yas-mode-map nil "Keymap for yas-mode.")
(unless yas-mode-map
  (setq yas-mode-map (make-sparse-keymap)))

(defconst yas-mode-font-lock-keywords
  (list
   ;; comment / semantic
   (list "\\(?:^\\|[^`'\"]\\)\\(#[_^]?\\)\\(\\(\\w\\|_\\)+\\)" '(1 font-lock-comment-delimiter-face) '(2 font-lock-comment-face) )
   (list "\\(?:^\\|[^`'\"]\\)\\(##\\s-*\\)\\(.*\\)$" '(1 font-lock-comment-delimiter-face) '(2 font-lock-comment-face) )
   (list "\\(?:^\\|[^`'\"]\\)\\(#\\*\\s-*\\)\\(\\(.\\|\n\\)*?\\)\\(\\*#\\)" '(1 font-lock-comment-delimiter-face) '(2 font-lock-comment-face) '(4 font-lock-comment-delimiter-face))

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
   (cons "^\\s-*\\(\\(?:\\w\\|_\\)+\\)\\s-*=" (list 1 'font-lock-variable-name-face))
   (cons "^\\s-*\\(\\(?:\\w\\|_\\)+\\)\\s-*%=" (list 1 'font-lock-function-name-face))
   (cons "^\\s-*\\(\\(?:\\w\\|_\\)+\\)\\s-*<.*?>\\s-*=" (list 1 'font-lock-function-name-face))
   ))

(defun yas-beginning-of-defun ()
  (interactive)
  (re-search-backward "^\\s-*\\w+\\s-*="))

(defun yas-end-of-defun ()
  (interactive)
  (re-search-forward "^\\s-*\\w+\\s-*=")
  (forward-line -1))

;;;###autoload
(define-derived-mode yas-mode prog-mode "yas"
  "Major mode for editing Yasmala grammar files"
  (set (make-local-variable 'comment-start) "## ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#[#*]+\\s-*")

  (set (make-local-variable 'beginning-of-defun-function) 'yas-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'yas-end-of-defun)

  (set (make-local-variable 'font-lock-defaults)
        '(yas-mode-font-lock-keywords t nil nil nil)))

(provide 'yas-mode)

;;; yas-mode.el ends here
