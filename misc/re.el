;;; re.el --- Extensions around regular expressions
;; 
;; Copyright 2013 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; Created: 2013
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
;; Extensions around regular expressions.
;; 
;;; Code:

(defun re-balanced (delimiters &optional depth)
  "Return regexp matching balanced delimiters.

E.g. (re-balanced \"<>\") matches \"<hello<world>>\".

DELIMITERS is a string of exactly two characters, the first being
the opening delimiter, the 2nd being the closing delimiter.

DEPTH is the maximal depth of nested constructs which the regexp
matches. DEPTH 2 matches \"<hello>\" or \"<hello<world>>\" but
not \"<hello<cool<world>>>\". For DEPTH>=1, the first and the
last character of the match must be the opening and the closing
delimiter respectively. So \"<hello>\" is matched, but not
\"hello\". For DEPTH 0 any string is matched which does not
contain the characters in DELIMITERS. When DEPTH is nil, it
defaults to 3.

See also the book 'Mastering Regular Expressions' by Jeffrey E.
F. Friedl, 2nd Edition, chapter 5.2.4 Matching Balanced Sets of
Parenthesis and chapter 6 The Real Unrolling-the-Loop Pattern"
  (when (not (equal (length delimiters) 2))
    (error))
  (unless depth (setq depth 3))
  (let ((content (concat "[^" delimiters "]*")))
    (if (eq depth 0)
        content
      (let* ((begin-del (regexp-quote (substring delimiters 0 1)))
             (end-del (regexp-quote (substring delimiters 1 2)))
             (nested-constructs
              (when (> depth 1)
                (concat "\\(?:" (re-balanced delimiters (1- depth)) content "\\)*"))))
        (concat "\\(?:" begin-del content nested-constructs end-del "\\)")))))

(provide 're)

;;; re.el ends here
