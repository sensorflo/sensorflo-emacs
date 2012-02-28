;;; kmacro-ext.el --- extensions to emacs' kmacro
;;
;; Copyright 2009-2012 Florian Kaufmann <sensorflo@gmail.com>
;; 
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://sourceforge.net/projects/sensorflo-emacs/
;; Created: 2009
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
;;; Code: 

(defun kmacro-start-stop-macro-ext ()
  "If currently defining a macro, end it, else start defining a new.
For how exactly the macro is ended, see
`end-and-global-set-key-kbd-macro'."
  (interactive)
  (if (not defining-kbd-macro)
      (call-interactively 'kmacro-start-macro)
    (call-interactively 'end-and-global-set-key-kbd-macro)))

(defun end-and-global-set-key-kbd-macro(arg)
  "Ends recording of a kbd-macro and lets you assign the new macro a local key sequence."
  (interactive "cAssign to M-m M-(h|j|k|l): ")
  (end-kbd-macro current-prefix-arg)
  (cond
   ((eq arg '?h) (name-last-kbd-macro 'last-kbd-macro-h) (global-set-key [(meta m)(meta h)] 'last-kbd-macro-h))
   ((eq arg '?j) (name-last-kbd-macro 'last-kbd-macro-j) (global-set-key [(meta m)(meta j)] 'last-kbd-macro-j))
   ((eq arg '?k) (name-last-kbd-macro 'last-kbd-macro-k) (global-set-key [(meta m)(meta k)] 'last-kbd-macro-k))
   ((eq arg '?l) (name-last-kbd-macro 'last-kbd-macro-l) (global-set-key [(meta m)(meta l)] 'last-kbd-macro-l))
   ((eq arg '? ) nil) ; space, dont assign to any key
   (t (error "unkown key"))))

;;; kmacro-ext.el ends here
