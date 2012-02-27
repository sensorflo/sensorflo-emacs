;;; hi-lock-ext.el --- extends functionality of hi-lock.el
;;
;; Copyright 2011 Florian Kaufmann <sensorflo@gmail.com>
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


;;; Code:
(require 'hi-lock)

;;; Customization

;; group hi-lock-faces is defined within hi-lock.el

(defface hi-spect-blue1
  '((((background dark)) (:background "steel blue" :foreground "black"))
    (t (:background "steel blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-blue2
  '((((background dark)) (:background "cornflower blue" :foreground "black"))
    (t (:background "cornflower blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-blue3
  '((((background dark)) (:background "deep sky blue" :foreground "black"))
    (t (:background "deep sky blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-blue-green
  '((((background dark)) (:background "turquoise" :foreground "black"))
    (t (:background "turquoise")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-green1
  '((((background dark)) (:background "pale green" :foreground "black"))
    (t (:background "pale green")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-green2
  '((((background dark)) (:background "green" :foreground "black"))
    (t (:background "green")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-green3
  '((((background dark)) (:background "lime green" :foreground "black"))
    (t (:background "lime green")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-green-yellow
  '((((background dark)) (:background "yellow green" :foreground "black"))
    (t (:background "yellow green")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-yellow1
  '((((background dark)) (:background "kahki" :foreground "black"))
    (t (:background "khaki")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-yellow2
  '((((background dark)) (:background "yellow" :foreground "black"))
    (t (:background "yellow")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-orange1
  '((((background dark)) (:background "gold" :foreground "black"))
    (t (:background "gold")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-orange2
  '((((background dark)) (:background "orange" :foreground "black"))
    (t (:background "orange")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-red1
  '((((background dark)) (:background "salmon" :foreground "black"))
    (t (:background "salmon")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-red2
  '((((background dark)) (:background "indian red" :foreground "black"))
    (t (:background "indian red")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-red3
  '((((background dark)) (:background "red" :foreground "black"))
    (t (:background "red")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-spect-red4
  '((((background dark)) (:background "chocolate" :foreground "black"))
    (t (:background "chocolate")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(define-obsolete-face-alias 'hi-unimportant 'font-lock-unimportant "23.1")
(defface hi-unimportant
  '((((background dark)) (:foreground "gray80"))
    (t (:foreground "gray80")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(define-obsolete-face-alias 'hi-semi-unimportant 'font-lock-semi-unimportant "23.1")
(defface hi-semi-unimportant
  '((((background dark)) (:foreground "gray80"))
    (t (:foreground "gray80")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

;; font-lock.el says (see definition of the variable font-lock-comment-face)
;; that there is actually no need to create variables that specify face names.
;; However it seems to be needed all the same.
(defvar hi-semi-unimportant 'hi-semi-unimportant)
(defvar hi-unimportant 'hi-unimportant)

(defcustom highlight-face-list
  (list ; 'hi-spect-blue1
        ; 'hi-spect-blue2
        'hi-spect-blue3
        'hi-spect-blue-green
        'hi-spect-green1
        ;'hi-spect-green2
        'hi-spect-green3
        ;'hi-spect-green-yellow
        'hi-spect-yellow1
        'hi-spect-yellow2
        'hi-spect-orange1
        'hi-spect-orange2
        'hi-spect-red1
        'hi-spect-red2
        'hi-spect-red3
        'hi-spect-red4)
  "List of faces in available in hi-lock extension."
  :type '(repeat face)
  :group 'hi-lock)

;;; Command Definitions

;;;###autoload
(defun highlight-toggle-sexp-or-region ()
  "Toggle highlighting of sexp-at-point or region.

It's the region if mark is active, it's the sexp at point
otherwise. If it is already highlighted, unhighlight it. The face
used for highlighting is the first unused face of
`highlight-face-list'"
  (interactive)
  (let (search-str
        (face-list (copy-sequence highlight-face-list)))
    
    ;; get string to be searched
    (setq search-str
          (if mark-active
              (regexp-quote (buffer-substring-no-properties (point) (mark)))
            (concat "\\_<" (regexp-quote (buffer-substring-sexp-no-properties)) "\\_>")))

    ;; if search-str already highlightened -> unhighlight it
    (if (assoc search-str hi-lock-interactive-patterns)
        (unhighlight-regexp search-str)

      ;; if search-str not yet highlightened -> highlightened it with a face not yet used
      (progn
        ;; remove all used faces from face-list
        (mapc (lambda (pattern)
                (setq face-list (delete (highlight-pattern-face pattern) face-list)))
              hi-lock-interactive-patterns)
        ;; use first face of the remaining face-list
        (highlight-regexp search-str (car face-list))))))

;;;###autoload
(defun highlight-arguments ()
  "Highlights all arguments of current C/C++ method, each in a
  different face. This is done by calling
  highlight-toggle-sexp-or-region for each argument of the
  method."
  (interactive) (save-excursion
    (let (list-end param-end)
      (c-beginning-of-defun)
      (forward-list)
      (setq list-end (point))
      (backward-list)
      (down-list)
      (while (or (re-search-forward "," list-end t)
                 (re-search-forward ")" list-end t))
        (setq param-end (point))
        (backward-char) ; skip ) or ,
        (backward-sexp)
        (highlight-toggle-sexp-or-region)
        (goto-char param-end)))))

;;;###autoload
(defun highlight-members ()
  "Highlights all member identifiers, i.e. all identifiers that
  follow the naming ESEC guidelines for class members."
  (interactive) (let ((regex "\\_<[scm]+_\\sw+"))
    (if (assoc regex hi-lock-interactive-patterns)
        (unhighlight-regexp regex)
      (highlight-regexp regex 'hi-yellow))))

;;;###autoload
(defun highlight-arguments-uni ()
  "Highlights all argument identifiers (i.e. identifiers that follow the naming ESEC guidelines"
  (interactive)
  (let ((regex "\\_<[ciox]+_\\sw+"))
    (if (assoc regex hi-lock-interactive-patterns)
        (unhighlight-regexp regex)
      (highlight-regexp regex 'hi-pink))))

;;;###autoload
(defun highlight-locals ()
  "Highlights all locals identifiers - however with a imprecise regex"
  (interactive)
  (let ((regex "\\(\\_<\\([abchpndfsu]+\\|vec\\|mtx\\)[A-Z]\\sw*\\|\\_<ehr\\_>\\)"))
    (if (assoc regex hi-lock-interactive-patterns)
        (unhighlight-regexp regex)
      (highlight-regexp regex 'hi-green))))

;;;###autoload
(defun unhighlight-all ()
  "Unhighlight all highlightened sexp in current buffer"
  (interactive)
  (while hi-lock-interactive-patterns
    (hi-lock-unface-buffer (caar hi-lock-interactive-patterns))))

;;; Function Definitions

(defun highlight-pattern-face (pattern)
  "Returns the face contained in the passed pattern"
  (eval (nth 1 (nth 1 pattern))))

(provide 'hi-lock-ext)

;;; hi-lock-ext.el ends here