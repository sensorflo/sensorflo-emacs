;;; gud-ext.el --- extensions to emacs' gud mode
;;
;; Copyright 2009-2012 Florian Kaufmann <sensorflo@gmail.com>
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
;;
;;; Comentary:
;;
;; todo
;; - break-toggle
;; - if gud not already startet, start it
;;
;;; Code:

(defvar gud-ext-mode-map
  (let ((map (make-sparse-keymap)))

    ;; program flow
    (define-key map "f"   (make-sparse-keymap))
    (define-key map "s"  'gud-step)     ;; executes one src line; if that includes method(s), stop in the method(s)
    (define-key map [(f11)]  'gud-step)
    (define-key map "n"  'gud-next)     ;; like step, but dont stop in methods
    (define-key map [(f10)]  'gud-next)
    (define-key map "c"  'gud-cont)     ;;
    (define-key map "fi" 'gud-stepi)    ;; execute one machine instruction
    (define-key map "fr" 'gud-run)      ;; (optoins to set tbreak at main) start the program under gdb
    (define-key map "ff" 'gud-finish)   ;; continue until just after the function returns
    (define-key map "fu" 'gud-until)    ;; Continue running until a source line past the *current* line

    ;; alternate program flow
    (define-key map "a"  (make-sparse-keymap))
    (define-key map "aj" 'gud-jump)     ;;
    (define-key map "aq" 'gud-quit)     ;;
    (define-key map "ar" 'gud-return)   ;; premature return of a function

    ;; break/watch points
    (define-key map "b"  (make-sparse-keymap))
    (define-key map "bb" 'gud-break)
    (define-key map [(f9)] 'gud-break)
    (define-key map "br" 'gud-remove)
    (define-key map "bt" 'gud-tbreak)   ; temporary breakpoint

    ;; switch windows

    ;; print & co
    (define-key map "p"  (make-sparse-keymap))
    (define-key map "pp" 'gud-print)
    (define-key map "px" 'gud-ext-cmd)
    map))

(defun gud-ext-goto-source()
  (interactive)
  )

(defun gud-ext-gdb()
  (interactive)
  (gud-def gud-return "return" "" "Returns from the function prematurely")
  )

(define-minor-mode
  gud-ext-mode
  "gud-ext: extensions to the gud mode"
  nil
  " gudi"
  gud-ext-mode-map
  :group 'tools
  (setq buffer-read-only t ))

(provide 'gud-ext)

;;; gud-ext.el ends here
