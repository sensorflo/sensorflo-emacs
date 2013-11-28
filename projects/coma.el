;;; coma.el --- customizations specific to the coma project
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; 
;;; Commentary:
;; 

;;; Variables:
(require 'project)         ; https://github.com/sensorflo/sensorflo-emacs/
(require 'rx)
(require 'find-file-ext)
(require 'tempo-snippets)

;;; Code:

;;; misc settings

(defun coma-c-mode-common-hook ()
  (when (eq (project-root-type) 'project-coma)
    (setq c-basic-offset 2)))

(add-hook 'c-mode-common-hook 'coma-c-mode-common-hook t)

(defun coma-common-mode-hook ()
  (when (eq (project-root-type) 'project-coma)
    (setq tab-width 2)
    (setq indent-tabs-mode t)
    (when (not (coma-coding-system-p))
      (message "%s: encoding system is %S which is not coma's encoding system (utf-8-unix)"
               (buffer-name) buffer-file-coding-system))
    (shadow-keys 'compile 'compile-ext (current-local-map))))

(add-hook 'common-mode-hook 'coma-common-mode-hook t)

(defun coma-before-save-hook ()
  (when (eq (project-root-type) 'project-coma)
    (when (member major-mode '(c++-mode dt2-mode stream-mode doxym-mode))
      (tabify-indentation)) ; don't do delete-trailing-whitespace, since that is not a guideline and thus results in bigger diffs
    (when (and (not (coma-coding-system-p))
               (not (coma-pl2-file-p)))
      (if (yes-or-no-p (format "%s: change coding system from %S to utf-8-unix? "
                               (buffer-name) buffer-file-coding-system))
          (setq buffer-file-coding-system 'utf-8-unix)
        (if (yes-or-no-p "abort saving? ") (error "abort"))))))

(defun coma-coding-system-p ()
  (member buffer-file-coding-system '(utf-8-unix undecided-unix undecided)))

;;; functions

;; none currently

;;; file aliases / cache


;;; coma.el ends here
