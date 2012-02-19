;;; indel.el --- customizations specific to the indel (inos/inco) project
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;;
;;; Commentary:
;;
;;; Code:
(defun indel-c-mode-common-hook ()
  (when (eq (project-root-type) 'project-indel)
    (setq c-basic-offset 4)))

(defun indel-common-mode-hook ()
  (when (eq (project-root-type) 'project-indel)
    (setq tab-width 4)))

;;; indel.el ends here