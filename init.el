;; I want my init.el in the directory ".emacs.d./customization", but Emacs
;; expects an ".emacs.d/init.el", so ".emacs.d/init.el" 'forwards' to
;; ".emacs.d./customization/init.el".
(load-file (concat user-emacs-directory "customization/init.el"))