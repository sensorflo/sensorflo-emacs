;;; bib.el --- customizations specific to the bib project
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; 
;;; Commentary:
;; 
;;; Code:
(require 'sregex)
(require 'find-file-ext)
(require 'tempo-snippets)

(defun bib-c-mode-common-hook ()
  (when (eq (project-root-type) 'project-bib)
    (setq c-basic-offset 4)
    (c-set-offset 'access-label '-)
    (c-set-offset 'inclass '+)))

(defun bib-common-mode-hook ()
  (when (eq (project-root-type) 'project-bib)
    (setq tab-width 4)
    (shadow-keys 'compile 'compile-ext (current-local-map))))

;;; bib.el ends here
