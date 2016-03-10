;;; indel.el --- customizations specific to the indel (inos/inco) project
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;;
;;; Commentary:
;;
;;; Code:
(require 'project)         ; https://github.com/sensorflo/sensorflo-emacs/

(defun indel-c-mode-common-hook ()
  (when (eq (project-root-type) 'project-indel)))

(add-hook 'c-mode-common-hook 'indel-c-mode-common-hook t)

(defun indel-common-mode-hook ()
  (when (eq (project-root-type) 'project-indel)
    (setq tab-width 4)))

(add-hook 'common-mode-hook 'indel-common-mode-hook t)

;;; indel.el ends here
