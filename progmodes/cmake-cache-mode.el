;;; cmake-cache-mode.el --- a major-mode for editing cmake cache files
;;
;; Copyright 2017 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; Created: 2017
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


(require 'font-lock-ext) ; https://github.com/sensorflo/font-lock-ext/


;;; Variables:
(defconst cmake-cache-mode-font-lock-keywords
  (list
   (list
    "^[ \t]*\\(#\\)\\(.*\\)"
    '(1 font-lock-comment-delimiter-face)
    '(2 font-lock-comment-face))
   (list
    "^[ \t]*\\(//\\)\\(.*\\)"
    '(1 font-lock-doc-face)
    '(2 font-lock-doc-face))
   (list
    "^[ \t]*\\(.*?\\):\\(.*\\)=\\(.*\\)"
    '(1 font-lock-variable-name-face)
    '(2 font-lock-type-face)
    '(3 font-lock-constant-face))))


;;; Code:
;;;###autoload
(define-derived-mode cmake-cache-mode prog-mode "cmakecache"
  "Major mode for editing cmake cache files"
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  (set (make-local-variable 'font-lock-defaults)
       '(cmake-cache-mode-font-lock-keywords t nil nil nil)))


(provide 'cmake-cache-mode)

;;; cmake-cache-mode.el ends here
