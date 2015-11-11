;;; describe-char-display.el ---
;;
;; Copyright 2010-2013 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; Created: 2015
;; Keywords: faces, i18n, Unicode, multilingual
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
;;

;;; Commentary:

;; Describe position POS (interactively, point) and the char after POS using
;; `describe-char'.

;;; Code:
(require 'descr-text)

(defconst dcd-buffer-name " *describe-char-display*")
(defvar dcd-overlay         nil)
(defvar dcd-reference-count 0)
(defvar dcd-display nil)
(make-variable-buffer-local 'dcd-display)


;;;###autoload
(defun dcd-display (&optional arg)
  "Toggle DCD code display.

If ARG is null, toggle DCD code display.
If ARG is a number and is greater than zero, turn on display; otherwise, turn
off display.
If ARG is anything else, turn on display."
  (interactive "P")
  (if (if arg
          (> (prefix-numeric-value arg) 0)
        (not dcd-display))
      (dcd-on)
    (dcd-off)))

;;;###autoload
(defun dcd-on ()
  "Turn on DCD code display."
  (interactive)
  (unless nil
    (setq dcd-display         t
          dcd-reference-count (1+ dcd-reference-count))
    ;; local hooks
    (add-hook 'post-command-hook 'dcd-post-command nil t)
    (add-hook 'kill-buffer-hook 'dcd-off nil t)
    ;; own hook
    (run-hooks 'dcd-hook)
    (dcd-post-command)))


;;;###autoload
(defun dcd-off ()
  "Turn off DCD code display."
  (interactive)
  (when t
    (setq dcd-display         nil
          dcd-reference-count (1- dcd-reference-count))
    (remove-hook 'post-command-hook 'dcd-post-command t)
    (remove-hook 'kill-buffer-hook 'dcd-off t)
    (if (> dcd-reference-count 0)
        ;; at least one buffer with dcd activated
        (dcd-hide-table)
      ;; *no* buffer with dcd activated
      (and dcd-overlay
           (delete-overlay dcd-overlay))
      (let ((buffer (get-buffer dcd-buffer-name)))
        (and buffer
             (save-excursion
               (delete-windows-on buffer)
               (kill-buffer buffer)))))))


(defun dcd-post-command ()
  (describe-char (point)))

(provide 'describe-char-display)

;;; describe-char-display.el ends here
