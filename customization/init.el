;;; init.el - Sensorflo's Emacs init file
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://github.com/sensorflo/sensorflo-emacs then
;;      customization/init.el
;; 
;;; Commentary:
;;
;; TODO
;; - Try to avoid require. Try to insert all into mode hooks.
;; - have a look at Drew Adam's init: site-lisp/emacs-init.el
;;
;; Summary of how to customize Emacs:
;; - Emacs' customization
;;   - Init file (~/.emacs.d/init.el). Run only once at Emacs' startup.
;;   - Customization variables (in file denoted by `custom-file'). Loaded within
;;     my init file.
;;
;; - find-file-hook. I used to use that earlier, but no longer. One of the
;;   reasons is that `normal-mode' naturally doesn't run it.
;;
;; - mode hooks, see .emacs.d/customiation/mode-hooks.el. Is hierarchical:
;;   - change-major-mode-after-body-hook
;;   - parent modes, such as text-mode-hook or prog-mode-hook
;;   - leaf modes, e.g. python-mode-hook
;;   - after-change-major-mode-hook
;;     - edit-mode-hook
;;     - project hooks

;;; Code:


;;; stettings part 1 - before loading libraries
;; ==================================================
(message "init file: settings part 1")

;; set the config variable
(let ((file (concat user-emacs-directory "site-config.el")))
  (if (file-readable-p file)
      (load-file file)
    (setq config nil)))

;; load-path
(dolist (x '("misc" "customization" "tempos" "projects" "textmodes" "progmodes" "modified-site-lisp"))
  (let ((default-directory (concat user-emacs-directory x)))
		(add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path))) 
(dolist
    (x (append
	'("site-lisp/"
	  "site-lisp/ecb-2.40/")
	(if (not (and (>= emacs-major-version 23) (>= emacs-minor-version 2)))
	    (mapcar
	     (lambda (x) (concat "site-lisp/cedet-1.0pre7/" x))
	     '("cogre" "common" "contrib" "ede" "eieio" "semantic" "speedbar" "srecode" "tests")))))
  (add-to-list 'load-path (concat user-emacs-directory x)))
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/dvc/")

;; find-function-C-source-directory
(setq find-function-C-source-directory
      (concat (getenv "HOME") "/src/emacs-"
	      (progn
		(string-match "[0-9]+\\.[0-9]+" emacs-version)
		(match-string 0 emacs-version))
	      "/src/"))

;; environment
(when (eq config 'office-xp)
  (add-to-list 'Info-default-directory-list "D:/cygwin/usr/share/info")
  (add-to-list 'exec-path "D:/cygwin/bin")
  (setq explicit-shell-file-name "D:/cygwin/bin/bash.exe")
  (setq shell-file-name "D:/cygwin/bin/bash.exe")
  (setenv "PATH" (concat "D:\\cygwin\\bin" path-separator "D:\\cygwin\\usr\\local\\bin" path-separator (getenv "PATH"))) 
  (setenv "HOMESHARE" nil)
  (setenv "HOME1" "D:\\")
  (setenv "HOMEDRIVE" "D:\\")
  (setenv "SHELL" "D:/cygwin/bin/bash.exe")
  (setenv "ESHELL" "D:/cygwin/bin/bash.exe"))

;; autoload
(setq generated-autoload-file (concat user-emacs-directory "customization/loaddefs-custom.el"))
(load-file generated-autoload-file)

;; todo!!!! when not yet exists, generate & byte compile !!!!!!!
(load-library "loaddefs-site-lisp")

(load-file (concat user-emacs-directory "site-lisp/loaddefs-local-site-lisp.el"))

;; custom-file
;; some modes initialize stuff using their custom variables while loading, thus
;; load custom file before loading modes
(setq custom-file (concat user-emacs-directory "customization/custom-file.el"))
(load custom-file) 

;; aliases
(load-library "aliases") 



;;; required libraries
;; ==================================================
(message "init file: required libraries")

(require 'powerkey)

(when (equal system-type 'windows-nt)
  (require 'cygwin-mount)
  (require 'w32-symlinks)  
  (require 'w32-browser))
(require 'ido)  
(load-library "find-file")
(require 'window-numbering)

;; nxhtml is not part of sensorflo-emacs git repo
(when (file-readable-p "nxhtml/autostart.el") 
  (load-library "nxhtml/autostart.el"))

(require 'server)
(require 'cppkoans-mode)
(require 'filladapt)

(load-library "misc-ext")
(load-library "simple-ext") 
(load-library "cc-mode-ext") 
(load-library "hi-lock-ext") 
(load-library "project") 
(load-library "kmacro-ext") 
(load-library "find-file-ext") 



;;; settings part 2
;; =======================================================
(message "init file: settings part 2")

;; notable variables defined with custom, i.e. they don't need to be set
;; elsewhere:
;; - global-auto-revert-mode
;; - global-hi-lock-mode
;; - global-font-lock-mode
;; - indent-tabs-mode, standard-indent
;; - require-final-newline, mode-require-final-newline, c-require-final-newline

(put 'upcase-region 'disabled nil)   
(put 'set-goal-column 'disabled nil) 
(put 'narrow-to-region 'disabled nil) 
(put 'dired-find-alternate-file 'disabled nil) 
(put 'downcase-region 'disabled nil) 
(put 'scroll-left 'disabled nil)
(setq minibuffer-max-depth nil)
(setq find-dired-default-fn (lambda() "" "-iname ''"))
(setq resize-mini-windows t) ; needs truncate-lines == nil in minibuffer
(when (require 'speedbar nil t)
  ;; not customizeable, but maybe put it in a hook?
  (speedbar-enable-update))
(setq-default filladapt-mode t)  

;; interpreter-mode-alist
(dolist (x '(("perl" . cperl-mode)))
  (add-to-list 'interpreter-mode-alist x))

;; magic-mode-alist 
(dolist (x '(("\\s-*/\\*[*!][ \t]*$" . doxym-mode)
             ("\\(\\s-*\\(//.*\\)?\n\\)*\\([.*]\s-*\n\\)?\\(=+ \\w\\)" . adoc-mode)))
  (add-to-list 'magic-mode-alist x))

;; auto-mode-alist
;; Things are setup such that the first (top down) match is taken. Also the
;; following terms take precedence over the terms already in auto-mode-alist
;; before this modification.
(let ((my-auto-mode-alist
       '(("\\(/\\|\\`\\)\\(\\.?profile\\|\\.bash_\\(profile\\|login\\|logout\\|aliases\\)\\|\\.bash\\(rc\\|\\.bashrc\\)\\)\\'" . shell-script-mode)
         ("git-?flow\\(?:-\\w+\\)?\\'" . shell-script-mode)
         ("\\.env\\'" . shell-script-mode)
         ("\\.bas\\'" . visual-basic-mode)
         ("inputrc" . rl-mode)
         ("\\.bat\\'" . batch-mode)
         ("\\.ps1\\'" . powershell-mode)
         ("\\.h\\'" . c++-mode) ; .h is c-mode by default
         ("\\.tl[hi]\\'". c++-mode)
         ;; .text is text-mode by default
         ("\\.\\(text\\|mdwn\\|mdown\\|md\\|mdt\\)\\'" . markdown-mode)
         ("\\.lo[g0-9]\\'" . logfile-mode)
         ("personality.*\\.txt\\'" . pm-mode)
         ("\\.wiki\\'" . mediawiki-mode)
         ("\\.env\\'" . conf-mode)
         ("\\(\\`\\|/\\)\\.dmrc\\'" . conf-mode)
         (".ssh/config\\'"  . ssh-config-mode)
         ("sshd?_config\\'" . ssh-config-mode)
         ("\\.typ\\'" . gtypist-mode)
         ;; is perl-mode by default
         ("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . cperl-mode)
         ("\\.dt2\\'" . dt2-mode)
         ("\\.yas\\'" . yas-mode)
         ("\\.stream\\'" . stream-mode)
         ("\\.m\\'" . matlab-mode)
         ("\\.html?\\'" . (lambda () (if (require 'nxhtml-mode nil t) (nxhtml-mode) (sgml-mode))))
         ("\\.svg\\'" . (lambda () (if (require 'nxhtml-mode nil t) (nxhtml-mode) (sgml-mode))))
         ("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)
         ("\\.\\(frm\\|bas\\|cls\\|vb\\)$" . vbnet-mode)    
         ("\\.sln\\'" . sln-mode)    
         ("\\(\\`\\|/\\)git-rebase-todo" . git-irb-mode)    
         ("\\b[Aa]utoexp\\.dat\\'" . autoexp-mode)    
         ("\\.\\(dot\\|gv\\)\\'" . graphviz-dot-mode)    
         ("\\.g4?\\'" . antlr-mode)  ; default only has .g
         ("\\.zu\\'" . zimbu-mode)    
         ("\\.ef\\'" . ef-mode)    
         ("\\.l\\'" . flex-mode)    
         ("\\.yy?\\'" . bison-mode)
         ("\\.ef\\'" . ef-mode)
         ("\\.ll\\'" . llvm-mode)
         ("\\.rs\\'" . rust-mode)
         ("\\.\\(txt\\|asciidoc\\)\\'" . adoc-mode)
         ("\\(\\`\\|/\\)[-0-9A-Z_]+\\'" . adoc-mode))))
  (setq auto-mode-alist (nconc my-auto-mode-alist auto-mode-alist)))

;; 1) note the "", that is if an empty extension was given, i.e. the abbrev "ao."
(setq ffe-ext-map '(
  ("h" ("h" "")) ; 1)
  ("cpp" ("c"))
  ("idl" ("i"))
  ("dt2" ("d"))
  ("stream" ("s"))))

(dolist (x '(".html" ".htm" ".x" ".s" ".str" ".map" ".pj" ".mpd" ".603" ".bmp" ".jpg" ".jpeg" "_"))
  (add-to-list 'completion-ignored-extensions x))

(load-library "mybindings")
(load-library "mode-hooks")


;;; autostart
;; ----------
(message "init file: autostart")

;; put here at the end of the startup instead within custom-file so starting up
;; emacs with --debug-init has an effect. Else, modifying debug-on-error within
;; custom-file, which is loaded rather early within startup, would override
;; --debug-init.
(setq debug-on-error nil) 


;;; init.el ends here
