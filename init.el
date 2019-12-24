;; I want my init.el in the directory ".emacs.d./customization", but Emacs
;; expects an ".emacs.d/init.el", so ".emacs.d/init.el" 'forwards' to
;; ".emacs.d./customization/init.el".

;; package-initialize is called within customization/init.el. Package.el sais
;; that the following line should not be removed if not needed but commented
;; out.
; (package-initialize)

(load-file (concat user-emacs-directory "customization/init.el"))