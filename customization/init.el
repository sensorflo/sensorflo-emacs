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
;; - Work more with autoload
;; - have a look at Drew Adam's init: site-lisp/emacs-init.el
;;
;;; Code:


;;; stettings part 1 - before loading libraries
;; ==================================================
(message "init file: settings part 1")

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

;; (setq generated-autoload-file (concat "~/.emacs.d/my-loaddefs.el"))
;; (load-file generated-autoload-file)

;; 
(setq custom-file (concat user-emacs-directory "customization/custom-file.el"))
(setq find-function-C-source-directory
      (concat (getenv "HOME") "/src/emacs-"
	      (progn
		(string-match "[0-9]+\\.[0-9]+" emacs-version)
		(match-string 0 emacs-version))
	      "/src/"))

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

;; some modes initialize stuff using their custom variables while loading, thus
;; load custom file before loading modes
(load custom-file) 

(load-library "aliases") 



;;; required libraries
;; ==================================================
(message "init file: required libraries")

;;; things that say they have to be first
;; --------------------------------------------------
(message "init file: required libraries: things that say they have to be first")
(require 'powerkey)

;;; buffers & files
;; --------------------------------------------------
(message "init file: required libraries: buffers & files")
(when (equal system-type 'windows-nt)
  (require 'cygwin-mount)
  (require 'w32-symlinks)  
  (require 'w32-browser))
(require 'ido)  
(load-library "find-file")
(require 'window-numbering)
;(require 'bs)
;(require 'dired-sort-menu)  

;;; text/markup modes
;; --------------------------------------------------
(message "init file: required libraries: text/markup modes")
(require 'markup-faces)
(require 'html-helper-mode)
(require 'html-font)
;; nxhtml is not part of sensorflo-emacs git repo
(when (file-readable-p "nxhtml/autostart.el") 
  (load-library "nxhtml/autostart.el"))
(require 'yas-mode)
(require 'adoc-mode)
;(require 'mediawiki)
(require 'doxym-mode)
(require 'bbcode)
(require 'latex-ext)
;(require 'yatex)
;(require 'tex)
; on hold: auctex mode

;;; programming / config-file  modes
;; --------------------------------------------------
(message "init file: required libraries: programming / conf-file modes")
(require 'cc-mode)
(require 'cperl-mode) ; TODO: check out ped or sepia mode
(require 'ruby-mode)
(require 'python)
(require 'lisp-mode)
(require 'batch-mode)
(require 'sh-script)
(require 'conf-mode)
(require 'vssconf-mode)
(require 'stream-mode)
(require 'dt2-mode)
(require 'rl-mode)
(require 'vbnet-mode)
;(require 'apt-sources)
;jdee for java

;;; external tools
;; --------------------------------------------------
(message "init file: required libraries: tools")
(require 'edebug)
(require 'gud)
(require 'mode-compile)
(require 'compile)
(require 'doxymacs) 
(require 'psvn) 
(require 'mks) 
(require 'speedbar)

;(load-file (concat emacs-dir "/site-lisp/cedet-1.0pre7/common/cedet.el"))
;(require 'xcscope)
;(require 'gdb-highlight)
;(require 'vc-ediff) vc+ already has an own vc-ediff command defined
; cedet, oo-browser, ede, semantic


;;; misc major modes
;; --------------------------------------------------
(message "init file: required libraries: misc major modes")
(require 'grep)
(require 'shell)
(require 'info)
;;(eval-after-load "info" '(require 'info+)) ; doesn't work with emacs 23
(require 'apropos)
(require 'browse-kill-ring)
(require 'gtypist-mode)
(require 'linkd)
;(require 'palette) ; info won't work in emacs 23 anymore
(require 'cus-edit)
(require 'logfile-mode)
;(require 'help+20)  
;(require 'igrep)
;(require 'color-grep)
;(require 'help-mode+)  
;(require 'ehelp)
;(require 'no-word)

;;; markup helper functionality 
;; --------------------------------------------------

;;; programming helper functionality 
;; --------------------------------------------------
;; (load-library "cedet-1.0pre7/common/cedet.el")
;; (require 'ecb)

;;; misc helper functionality 
;; --------------------------------------------------
(message "init file: required libraries: misc helper functionality")

;; (require 'globalff)
(require 'server)
(require 'x-dict)  
(require 'column-marker)
(require 'hideshow)  
(require 'filladapt) 
(require 'recentf)
(require 'glasses)
(require 'eol-conversion)
(require 'highlight-parentheses) 
(require 'whitespace)
(require 'align)
(require 'make-mode)
(require 'kill-ring-search)
(require 'markerpen)
(require 'out-xtra)
(require 'hide-lines)
(require 'ebuff-menu)
(require 'bm)
(require 'auto-complete)
(require 'autopair)
(require 'ace-jump-mode)
(eval-after-load "outline" '(require 'foldout))
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t) ;; there's also a shell-mode-hook

;; I need load-library-ext; using autoload it should work, but currently it
;; doesn't, so for now I load load-library-ext viawith load-library.
;; todo: make it work
(message "init file: required libraries: misc helper functionality: mine")
(load-library "misc-ext")
(load-library "simple-ext") 
(load-library "cc-mode-ext") 
(load-library "hi-lock-ext") 
(load-library "project") 
(load-library "kmacro-ext") 
(load-library "find-file-ext") 
;(load-library "subr-patch") 

;; on hold 
; predictive mode
;(require 'highlight) ; emacs's 23 info won't work
;(require 'menu-bar+) ; emacs's 23 info won't work
;(require 'icicles) ; emacs's 23 info won't work
;(require 'tmm)
;(require 'highlight-symbol)
;(require 'facemenu+)
;(require 'flyspell-timer) ;; keybindings !!
;(require 'yasnippet)
;(require 'dropdown-list)
;(require 'pi-tempo-abbrev)
;(require 'sidebrain)
;(require 'planner)



;;; settings part 2
;; =======================================================
;; note that there are more in custom.el
(message "init file: settings part 2")

;; notable variables defined with custom:
;; - global-auto-revert-mode
;; - global-hi-lock-mode
;; - global-font-lock-mode
;; - indent-tabs-mode, standard-indent
;;
;; 
;; - require-final-newline, mode-require-final-newline, c-require-final-newline

(put 'upcase-region 'disabled nil)   
(put 'set-goal-column 'disabled nil) 
(put 'narrow-to-region 'disabled nil) 
(put 'dired-find-alternate-file 'disabled nil) 
(put 'downcase-region 'disabled nil) 
(put 'scroll-left 'disabled nil)
(setq minibuffer-max-depth nil)
(setq find-dired-default-fn (lambda() "" "-iname ''"))
(setq resize-mini-windows t)		; needs truncate-lines == nil in minibuffer
(when (require 'speedbar nil t)
  ;; not customizeable, but maybe put it in a hook?
  (speedbar-enable-update))
(setq-default filladapt-mode t)  

;; interpreter-mode-alist
(dolist (x '(("perl" . cperl-mode)))
  (add-to-list 'interpreter-mode-alist x))

;; magic-mode-alist 
(dolist (x '(("\\s-*/\\*[*!][ \t]*$" . doxym-mode)))
  (add-to-list 'magic-mode-alist x))

;; auto-mode-alist
;;
;; see also python.el how it does add itself using autoload
;; 
(let ((my-auto-mode-alist '(
    ("bash\\|profile" . shell-script-mode)
    ("git-?flow\\(?:-\\w+\\)?\\'" . shell-script-mode)
    ("\\.env\\'" . shell-script-mode)
    ("\\.bas\\'" . visual-basic-mode)
    ("inputrc" . rl-mode)
    ("\\.bat\\'" . batch-mode)
    ("\\.h\\'" . c++-mode)
    ("\\.tl[hi]\\'". c++-mode)
    ("\\.\\(txt\\|asciidoc\\)\\'" . adoc-mode)
    ("\\`[-0-9A-Z_]+\\'" . adoc-mode) ;; all upercase file name
    ("\\.\\(text\\|mdwn\\|mdown\\|md\\|mdt\\)\\'" . markdown-mode)
    ("\\.\\(py\\|python\\)\\'" . python-mode)
    ("\\.lo[g0-9]\\'" . logfile-mode)
    ("personality.*\\.txt\\'" . pm-mode)
    ("swat\\.eu\\.besi\\..*\\.txt" . mediawiki-mode)
    ("\\.env\\'" . conf-mode)
    ("\\.git\\(config\\|modules\\)\\'" . conf-mode)
    ("\\.typ\\'" . gtypist-mode)
    ("\\.pl\\'" . cperl-mode)
    ("\\.dt2\\'" . dt2-mode)
    ("\\.tex\\'" . latex-mode)
    ("\\.yas\\'" . yas-mode)
    ("\\.stream\\'" . stream-mode)
    ("\\.m\\'" . matlab-mode)
    ("\\.xml\\'" . xml-mode)
    ("\\.html?\\'" . (lambda () (if (require 'nxhtml-mode nil t) (nxhtml-mode) (sgml-mode))))
    ("\\.svg\\'" . (lambda () (if (require 'nxhtml-mode nil t) (nxhtml-mode) (sgml-mode))))
    ("makefile" . makefile-gmake-mode)
    ("\\.make\\'" . makefile-gmake-mode)    
    (".*doxy\\(?:file\\|path\\).*\\'" . makefile-gmake-mode)    
    ("\\.\\(frm\\|bas\\|cls\\|vb\\)$" . vbnet-mode)    
    )))
  (dolist (x my-auto-mode-alist) 
    (add-to-list 'auto-mode-alist x)))

(mapcar 'file-cache-add-file
        '("~/.emacs.d"
          "~/.emacs.d/_emacs.el"
          "~/.emacs.d/site-lisp"
          "~/.emacs.d/modified-lisp"
          "/usr/share/doc/asciidoc/doc/asciidoc.txt.gz"))

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

;; project stuff
;; I don't know yet how to avoid having that in the emacs init file
(load-library "dragon") 
(load-library "nova") 
(load-library "indel") 
(load-library "bib") 
(load-library "coma") 
(load-library "misc-small-projects") 


;;; autostart
;; ----------
(message "init file: autostart")
;; manual says to call it for emacs' own menus (file, edit, ...), but it results
;; in an error
;;(update-power-keys t) 
;; (when (require 'ede nil t)
;;   (global-ede-mode 1))
;; (when (require 'semantic nil t)
;;   (semantic-load-enable-minimum-features)
;;   (semantic-load-enable-code-helpers)
;;   (semantic-load-enable-gaudy-code-helpers))
;; (when (require 'srecode-mode nil t)
;;   (global-srecode-minor-mode 1))
(require 'icicles)
;(icy-mode) ; icy-mode wants to be the last thing called 

;; put here at the end of the startup instead within custom-file so starting up
;; emacs with --debug-init has an effect. Else, modifying debug-on-error within
;; custom-file, which is loaded rather early within startup, would override
;; --debug-init.
(setq debug-on-error nil) 


;;; init.el ends here
