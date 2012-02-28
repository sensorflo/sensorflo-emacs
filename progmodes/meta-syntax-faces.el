;;; meta-syntax-faces.el --- collection of faces for meta syntax language modes
;; 
;; Copyright 2011-2012 Florian Kaufmann <sensorflo@gmail.com>
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

(defgroup meta-syntax-faces nil
  "Faces for meta syntax languages"
  :group 'faces)

(defface meta-syntax-rule-def
  '((t nil))
  "Rule name in a rule definition"
  :group 'meta-syntax-faces)

(defface meta-syntax-literal-face
  '((t nil))
  "Literal text"
  :group 'meta-syntax-faces)

(defface meta-syntax-replacement-face
  '((t nil))
  "Like \\w, \\s, \\x20, [[:alpha:]] which stand for another class"
  :group 'meta-syntax-faces)

(defface meta-syntax-char-class-face
  '((t nil))
  "E.g. [a-fA-F]"
  :group 'meta-syntax-faces)

(defface meta-syntax-hide-delimiter-face
  '((t nil))
  ""
  :group 'meta-syntax-faces)

;; font-lock.el says (see definition of the variable font-lock-comment-face)
;; that there is actually no need to create variables that specify face names.
;; However it seems to be needed all the same.
(defvar meta-syntax-rule-def 'meta-syntax-rule-def)
(defvar meta-syntax-literal-face 'meta-syntax-literal-face)
(defvar meta-syntax-replacement-face 'meta-syntax-replacement-face)
(defvar meta-syntax-char-class-face 'meta-syntax-char-class-face)
(defvar meta-syntax-hide-delimiter-face 'meta-syntax-hide-delimiter-face)

(provide 'meta-syntax-faces)