;;; bib.el --- customizations specific to the bib project
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; 
;;; Commentary:
;; 
;;; Code:
(require 'project)         ; https://github.com/sensorflo/sensorflo-emacs/
(require 'sregex)
(require 'find-file-ext)
(require 'tempo-snippets)

(defun bib-c-mode-common-hook ()
  (when (eq (project-root-type) 'project-bib)
    (setq c-basic-offset 4)
    (c-set-offset 'access-label '-)
    (c-set-offset 'inclass '+)))

(add-hook 'c-mode-common-hook 'bib-c-mode-common-hook t)

(defun bib-common-mode-hook ()
  (when (eq (project-root-type) 'project-bib)
    (setq tab-width 4)
    (shadow-keys 'compile 'compile-ext (current-local-map))))

(add-hook 'common-mode-hook 'bib-common-mode-hook t)

;;; bib.el ends here
