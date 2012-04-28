;;; tempo-ext.el --- extensions to tempo
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
;;; Commentary: 
;;
;;; Code: 
(require 'tempo)
(require 'tempo-snippets)

;;- first remove any apperance for preceding for
;; - after first try cycle through different versions. Restore beginning point by doing some kind
;;   of 'save-point'. Maybe undohandling supports going back to a specific point.
;;- (newline-unless-blank)
(defun tempo-entry(&optional rubout-regexp)
  (interactive)
  (when rubout-regexp
    (let ((saved-point (point))) 
      (when (re-search-backward (concat rubout-regexp "\\=") (line-beginning-position) t)
        (delete-region (point) saved-point)))))

(defun my-tempo-handler (element on-region)
  (cond

   ((eq element '>n) (indent-according-to-mode) "\n")

   ;; region or blank-line
   ((eq element 'r-or-blank-line>) (if on-region 'r> '>n))

   ;; line wise start
   ((eq element 'lws)
    (cond
     (on-region
      (goto-char tempo-region-start)
      (set-mark tempo-region-stop)
      (mark-whole-lines)
      'sr)
     ((save-excursion (beginning-of-line) (not (looking-at "\\s-*$")))
      (end-of-line)
      'n>)
     (t "")))

   ;; (ro ...)
   ;; (r>o ...)
   ;; region or the ... part. Typically region or a blankline 
   ;; ...........

   ;; save region
   ((eq element 'sr)
    (when on-region
      (set-marker tempo-region-start (min (mark) (point)))
      (set-marker tempo-region-stop (max (mark) (point)))
      (goto-char tempo-region-start))
    "")))

(add-to-list 'tempo-user-elements 'my-tempo-handler)


(require 'doremi)
(require 'doremi-mac)

(define-doremi tempo-test
  ;; Doc string
  "tempo test doc string"
  "tempo test"                    ; Command menu name
  ;; Setter function
  'tempo-test-func
  ;; Initial value
  (car tempo-test-list)
  nil                                       ; Ignored
  tempo-test-list                        ; Cycle enumeration
  t)

(setq tempo-test-list '(tempo-template-flori1 tempo-template-flori2 tempo-template-flori3))
(defun tempo-test-func (newval)
  (undo)
  (funcall newval)
  newval)

(defun tempo-test-meta ()
  (interactive)
  (undo-boundary)
  (tempo-template-flori1)
  (doremi-tempo-test)
  ;(undo)
  )


(tempo-define-template "flori1" '("eins-" r "-eins"))  
(tempo-define-template "flori2" '("zwei-" r "-zwei"))  
(tempo-define-template "flori3" '("drei-" r "-drei"))  

(provide 'tempo-ext)

;;; tempo-ext.el ends here  