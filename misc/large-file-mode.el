;;; large-file-mode.el --- a major-mode for editing large files
;;
;; Copyright 2015 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://github.com/sensorflo/sensorflo-emacs
;; Created: 2015
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

;;;###autoload
(defun large-buffer-p()
  "True when current buffer is considered large."
  (> (buffer-size) large-file-warning-threshold))

;;;###autoload
(define-derived-mode large-file-mode fundamental-mode "largefile"
  "Major mode for viewing large files.

It turns off many things that might make Emacs slow down so much
that working with the file becomes very cumbersome."
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set (make-local-variable 'bidi-display-reordering) nil)

  ;; turn off to be sure they are turned off. I don't want to check for each
  ;; minor mode which has a 'globally-enable' variable which I would need to
  ;; override here and which minor mode doesn't and I actually wouldn't need
  ;; to turn off since it was never turned on.
  (hl-line-mode -1)
  (font-lock-mode -1)
  (linum-mode -1)
  (line-number-mode -1) ; see also line-number-display-limit-width, line-number-display-limit
  (column-number-mode -1)
  (outline-minor-mode -1)
  (fci-mode -1)
  (whitespace-mode -1)
  (show-paren-mode -1)
  (flycheck-mode -1))

;;;###autoload
(add-hook 'magic-mode-alist (cons 'large-buffer-p 'large-file-mode))

(provide 'large-file-mode)

;;; large-file-mode.el ends here
