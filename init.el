;; I want my init.el in the directory ".emacs.d./customization", but Emacs
;; expects an ".emacs.d/init.el", so ".emacs.d/init.el" 'forwards' to
;; ".emacs.d./customization/init.el".

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load-file (concat user-emacs-directory "customization/init.el"))