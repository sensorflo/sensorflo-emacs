;;; faces-ext.el --- extensions to Emacs' faces.el
;;
;; Copyright 2011-2012 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://github.com/sensorflo/sensorflo-emacs
;; Created: 2015
;; Keywords: faces
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

(require 'faces)

(defface info
  '((t (:foreground "blue")))
  "Basic face to highlight infos, as opposed to errors or warnings"
  :group 'basic-faces)

(provide 'faces-ext)
;;; faces-ext.el ends here
