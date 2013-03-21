;;; misc-small-projects.el --- customizations specific to miscellaneous small projects
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://github.com/sensorflo/sensorflo-emacs/, then
;;      projects/misc-small-projects.el 
;;
;;; Code:

;;; BesiMinisystemGui
;; ----------------------------------------------
(defun besiminisystemgui-python-mode-hook ()
  (when (string= "BesiMinisystemGui.python" (file-name-nondirectory (buffer-file-name)))
    (setq tab-width 4)))
(add-hook 'python-mode-hook 'besiminisystemgui-python-mode-hook)

;;; misc-small-projects.el ends here
