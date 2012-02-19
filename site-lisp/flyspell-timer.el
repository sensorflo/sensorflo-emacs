;;; flyspell-timer.el --- Check Spelling in the idle cycle

;; This file is not part of Emacs

;; Author: Phillip Lord <p.lord@russet.org.uk>
;; Maintainer: Phillip Lord <p.lord@russet.org.uk>
;; Maintainer (XEmacs): Martin Kuehl (martin.kuehl@gmail.com)
;; Website: http://www.russet.org.uk

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA. 

;;; Commentary:
;; 
;; This file enables flyspell checking in the idle cycle. Add
;;
;; (require 'flyspell-timer)
;; (add-hook 'flyspell-mode-hook 'flyspell-timer-ensure-idle-timer)
;;
;; to your .emacs. `flyspell-timer-mode' is also checked, so set this
;; to nil buffer-local if you want to disable this in some
;; buffers. Other configuration are `flyspell-timer-chunk-size' and
;; `flyspell-timer-delay'.  The system checks the former number of
;; words in each direction and then delays for the later number of
;; seconds. These are set conservatively, as flyspell can be fairly
;; intensive. Only displayed words (not the whole buffer) are checked
;; for much the same reason. Finally
;; `flyspell-timer-idle-timer-verbose' switches of the error
;; messages. These default to on, again, because this mode is CPU
;; intensive. 


;;; History:
;; 

;;; Bugs:
;; 
;; I still don't understand idle timers. Other packages with idle
;; timers may well interfere. 

;;; Code:
(defvar flyspell-timer-mode t)

(defvar flyspell-timer-idle-timer-verbose t)

;; (setq flyspell-timer-debug t)
(defvar flyspell-timer-debug nil)

(defvar flyspell-timer-chunk-size 1
 "Size of chunks checked in idle cycle.")

(defvar flyspell-timer-delay 1
  "Length of time delay between chunks.")

(defun flyspell-timer-ensure-idle-timer()
  (interactive)
  (unless nil
    (if (not flyspell-timer-long-idle-timer)
	(flyspell-timer-start-idle-timer))))

;; (setq flyspell-timer-long-idle-timer nil)
(defun flyspell-timer-start-idle-timer()
  (setq flyspell-timer-long-idle-timer
        (run-with-idle-timer 6 t 'flyspell-timer-idle-timer-function)))

(defvar flyspell-timer-long-idle-timer nil)
;;(setq  flyspell-timer-disable-timers t)
(defvar flyspell-timer-disable-timers nil)


;; I don't understand why this is necessary but it seems to help the
;; slow idle timer work in the correct buffer. I suspect someother
;; timer is screwing up with the current buffer...
(defvar flyspell-timer-timer-buffer nil)

(defun flyspell-timer-idle-timer-function(&optional buffer)
  ;; so this only works on the current buffer. Might want to scavenge
  ;; over other buffers
  (save-excursion
    (set-buffer (or buffer flyspell-timer-timer-buffer
                    (window-buffer
                     (selected-window))))
    (if (and flyspell-mode flyspell-timer-mode (not flyspell-timer-disable-timers))
        (flyspell-timer-idle-timer-function-0))))

;; for some reason that I do not understand yet, this sometimes
;; appears to work in the wrong buffer. I really have not got any idea
;; why this is the case.
(defun flyspell-timer-idle-timer-function-0()
  "Add all words to the buffer.
`flyspell-timer-scavenge-buffer' does this more efficiently interactively.
If this takes up too much processor power, see `flyspell-timer-scavenge-some-chunk-size'."
  (interactive)
  (let ((forward-marker (point))
	(backward-marker (point))
	;; Remember the current window positions...the movement in
        ;; this function will change these, keeping the cursor always
        ;; in the middle.
        (forward-end (window-end))
        (backward-start (window-start))
        (forward-complete nil)
	(backward-complete nil)
	(repeat t))
    (when flyspell-timer-idle-timer-verbose
        (message "flyspell buffer..."))
    (while
	(and repeat
	     (not (and forward-complete backward-complete)))
      (save-excursion
	(unless backward-complete
          (goto-char backward-marker)
 	  (setq backward-marker
 		(flyspell-timer-check-words -1
                                            flyspell-timer-chunk-size))
 	  (setq backward-complete
                (> backward-start backward-marker)))
        (unless forward-complete
	  (goto-char forward-marker)
	  (setq forward-marker
		(flyspell-timer-check-words 1 flyspell-timer-chunk-size))
	  (setq forward-complete
		(< forward-end forward-marker))))
      ;; leave a gap so that we spam the cpu and there is plenty of
      ;; time for interruption
      (setq repeat (sit-for flyspell-timer-delay))
      (message "repeat %s"  repeat))
    (if flyspell-timer-idle-timer-verbose
        (progn
          (message "flyspell buffer...done")
          (sit-for 2)
          (message nil)))))


(defun flyspell-timer-check-words (&optional direction number)
  "Check NUMBER words in DIRECTION. "
  (if (not direction)
      (setq direction 1))
  (if (not number)
      (setq number 5))
  (save-excursion
    (dotimes (i number)
      (forward-word direction)
      (flyspell-timer-word-maybe))
    (point)))
    

(defun flyspell-timer-bounds-marked-p (start end)
  "Return t if everywhere between START and END is marked."
  (save-excursion
    (let ((retn t))
      (do ((i start (1+ i)))
	  ((> i (- end 1)))
	(unless (get-text-property i 'flyspell-added)
            (setq retn nil)
	    (setq i end)))
      retn)))


(defun flyspell-timer-word-maybe ()
  "Add word in BOUNDS as abbreviation, and mark the buffer."
  (let ((start (car (bounds-of-thing-at-point 'word)))
        (end (cdr (bounds-of-thing-at-point 'word))))
    ;; has this word been checked already?
    (unless
        (flyspell-timer-bounds-marked-p start end)
      ;; set a property so that we know what we have done.
      (flyspell-timer-save-buffer-modified-p
       (add-text-properties start end
                            '(flyspell-added t)))
      (flyspell-word)))
  (if flyspell-timer-debug
      (message "flyspell word %s" (word-at-point))))


          

(defmacro flyspell-timer-save-buffer-modified-p (&rest body)
  "Eval BODY without affected buffer modification status"
  `(let ((buffer-modified (buffer-modified-p)))
     ,@body
     (set-buffer-modified-p buffer-modified)))


(provide 'flyspell-timer)

;;; flyspell-timer.el ends here
