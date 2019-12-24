;;; init.el - Sensorflo's Emacs init file
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://github.com/sensorflo/sensorflo-emacs then
;;      customization/init.el
;;
;;; Commentary:
;;
;; Summary of how to customize Emacs:
;; - Emacs' customization
;;   - personal site local configuration: ~/.emacs.d/my-site-local.el
;;   - Init file (~/.emacs.d/init.el). Run only once at Emacs' startup.
;;   - Customization variables (in file denoted by `custom-file'). Loaded within
;;     my init file.
;;     - custom themes
;;   - file local variables
;;   - directory variables
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
;;
;;
;; installed ubuntu packages, see also autoloads below: emacs-goodies,
;; irony-mode, debian-el, emacs25-common-non-dfsg (for info pages)

;; help to install irony on ubuntu:
;; https://gist.github.com/soonhokong/7c2bf6e8b72dbc71c93b. What i did after M-x
;; irony-install-server:
;; cmake -DLIBCLANG_INCLUDE_DIR=/usr/lib/llvm-3.8/include -DLIBCLANG_LIBRARY=/usr/lib/llvm-3.8/lib/libclang.so.1 -DCMAKE_INSTALL_PREFIX\=/home/sensorflo/.emacs.d/irony/ /home/sensorflo/.emacs.d/elpa/irony-20170920.1417/server && cmake --build . --use-stderr --config Release --target install
;; make flycheck use
;;   cppcheck
;;     I am not sure whether it works, so far the errors always say the checker is irony
;;     flycheck-verify-setup
;;     flycheck-checker is nil as it ought to be
;;     flycheck-checkers contains c/c++-cppcheck
;;     flycheck-cppcheck-checks is "all"
;;     explicitely selecting c/c++-cppcheck via flycheck-select-checker works
;;   clang-tidy
;;   clang static analyzer
;;
;; Emacs as IDE:
;;   http://tuhdo.github.io/c-ide.html#sec-2
;;   projectile
;;   helm
;;   locate file
;;   flyspell only in comments, and ignoring identifiers
;;   cycle around file groups (header/src, class,test)
;;   goto definition
;;   show declaration & comment
;;   auto complete
;;     identifiers
;;     include paths
;;     argumens for function, template, macro, ...
;;   snippets / templates
;;   refactor
;;     http://tuhdo.github.io/c-ide.html#sec-2 chapter code refactoring
;;     https://github.com/tuhdo/semantic-refactor

;;; Code:

(defconst message2-enabled nil)
(defvar message2-last-time nil)
(defun message2 (msg)
  "As `message', but additionally prints time since last call.
Meant to profile startup time."
  (when message2-enabled
    (let ((duration (if (null message2-last-time)
                        0
                      (- (float-time) message2-last-time))))
      (setq message2-last-time (float-time))
      (message (concat (number-to-string duration) "\n\n" msg)))))


;;; personal site local configuration
;; ==================================================
(message2 "personal site local configuration")
(let ((my-site-local-fn (concat user-emacs-directory "my-site-local.el")))
  (when (file-readable-p my-site-local-fn)
    (load-file my-site-local-fn)))


;;; stettings part 1 - before loading libraries
;; ==================================================
(message2 "init file: settings part 1")

;; load-path
;; Since add-to-list adds to the front, the order of add-to-list is in
;; increasing priority, i.e. the path with the lowest priority is add-to-list
;; first.
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/dvc/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path (or (and (boundp 'emacs-goodies-el-path) emacs-goodies-el-path) "/usr/share/emacs/site-lisp/emacs-goodies-el"))
(add-to-list 'load-path (or (and (boundp 'debian-el-path) debian-el-path) "/usr/share/emacs/site-lisp/debian-el"))
(dolist (x '("misc" "customization" "tempos" "projects" "textmodes" "progmodes" "modified-site-lisp"))
  (let ((default-directory (concat user-emacs-directory x)))
                (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
(dolist
    (x (append
        '("site-lisp/"
          "site-lisp/ecb-2.40/"
          "site-lisp/vlfi/")
        (if (not (and (>= emacs-major-version 23) (>= emacs-minor-version 2)))
            (mapcar
             (lambda (x) (concat "site-lisp/cedet-1.0pre7/" x))
             '("cogre" "common" "contrib" "ede" "eieio" "semantic" "speedbar" "srecode" "tests")))))
  (add-to-list 'load-path (concat user-emacs-directory x)))

;; find-function-C-source-directory
(setq find-function-C-source-directory
      (concat (getenv "HOME") "/src/emacs-"
              (progn
                (string-match "[0-9]+\\.[0-9]+" emacs-version)
                (match-string 0 emacs-version))
              "/src/"))

;; Setup quick and correct (according to my personal preferences) loading of
;; libraries. Obviously this should be done before loading any library. It
;; woudn't be an error if a library was loaded before.
(setq load-prefer-newer t)
(require 'auto-compile) ; not that .elc are only ever updated but never created
(setq auto-compile-display-buffer nil)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

;; custom-file
;; some modes initialize stuff using their custom variables while loading, thus
;; load custom file before loading modes
(setq custom-file (concat user-emacs-directory "customization/custom-file.el"))
(load custom-file)


;;; required libraries / packages
;; ==================================================
(message "init file: required libraries / packages")

(require 'powerkey)

;; packages before other libraries, such that the libraries can require
;; libraries from the packages.
(setq package-enable-at-startup nil)
(package-initialize)

(when (equal system-type 'windows-nt)
  (require 'cygwin-mount)
  (require 'w32-symlinks)
  (require 'w32-browser))

;; nxhtml is not part of sensorflo-emacs git repo
(when (file-readable-p "nxhtml/autostart.el")
  (load-library "nxhtml/autostart.el"))

;; My customization of different modes references faces markup-faces. It's
;; error prone to try that each respective mode-hook contains a (require
;; 'markup-faces). So it's done once here.
(require 'markup-faces)

(load-library "misc-ext")
(load-library "faces-ext")
(load-library "simple-ext")
(load-library "project")
(load-library "find-file")


;;; settings part 2
;; =======================================================
(message2 "init file: settings part 2")

(load-library "aliases")
(load-library "mybindings")
(load-library "mode-hooks")

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
(when (require 'filladapt nil t)
  (setq-default filladapt-mode t))

;; auto-mode-alist
;; Things are setup such that the first (top down) match is taken. Also the
;; following terms take precedence over the terms already in auto-mode-alist
;; before this modification.
(let ((my-auto-mode-alist
       '(;; The default for auto-mode-alist forgets to include the following
         ;; default bash related files (partly defined by bash, partly by
         ;; Ubuntu)
         ("\\(/\\|\\`\\)profile\\'" . sh-mode)
         ("\\(/\\|\\`\\)bash\\.bashrc\\'" . sh-mode)
         ("\\(/\\|\\`\\)\\.bash_aliases\\'" . sh-mode)
         ;; my own additinal bash related files
         ("\\(/\\|\\`\\)\\.profile_[^/]*\\'" . sh-mode)
         ("\\(/\\|\\`\\)localprofile\\'" . sh-mode)
         ("\\(/\\|\\`\\)\\.bash\\(rc\\)?_[^/]*\\'" . sh-mode)

         ("\\.env\\'" . shell-script-mode)
         ("\\.bas\\'" . visual-basic-mode)
         ("inputrc" . rl-mode)
         ("\\.bat\\'" . batch-mode)
         ("\\.ps1\\'" . powershell-mode)

         ;; gitconfig-mode and gitattributes-mode already autonomously added
         ;; the standard git config / attribute files to auto-mode-aliast via
         ;; autoload.
         ("\\(/\\|\\`\\)\\.gitconfig_[^/]*\\'" . gitconfig-mode)
         ("git-?flow\\(?:-\\w+\\)?\\'" . sh-mode)
         ("\\(/\\|\\`\\)\\.gitshrc\\'" . sh-mode)
         ("\\(/\\|\\`\\)git-rebase-todo\\'" . git-irb-mode)

         ("\\.h\\'" . c++-mode) ; .h is c-mode by default
         ("\\.tl[hi]\\'". c++-mode)
         ("\\.\\(boc\\|boh\\)\\'". c++-mode)
         ;; .text is text-mode by default
         ("\\.\\(text\\|mdwn\\|mdown\\|md\\|mdt\\)\\'" . markdown-mode)
         ("\\.lo[g0-9]\\'" . logfile-mode)
         ("personality.*\\.txt\\'" . pm-mode)
         ("\\.env\\'" . conf-mode)
         ("\\(\\`\\|/\\)\\.dmrc\\'" . conf-mode)
         (".ssh/config\\'"  . ssh-config-mode)
         ("sshd?_config\\'" . ssh-config-mode)
         ("\\(/\\|\\`\\)CMakeCache.txt\\'" . cmake-cache-mode)
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
         ("\\b[Aa]utoexp\\.dat\\'" . autoexp-mode)
         ("\\.\\(dot\\|gv\\)\\'" . graphviz-dot-mode)
         ("\\.g4?\\'" . antlr-mode)  ; default only has .g
         ("\\.zu\\'" . zimbu-mode)
         ("\\.ef\\'" . ef-mode)
         ("\\.l\\'" . flex-mode)
         ("\\.yy?\\'" . bison-mode)
         ("\\.ef\\'" . ef-mode)
         ("\\.ll\\'" . llvm-mode)
         ("\\.rs\\'" . rust-mode))))
  (setq auto-mode-alist (nconc my-auto-mode-alist auto-mode-alist)))

;; 1) note the "", that is if an empty extension was given, i.e. the abbrev "ao."
(setq ffe-ext-map '(
  ("h" ("h" "")) ; 1)
  ("cpp" ("c"))
  ("idl" ("i"))
  ("dt2" ("d"))
  ("stream" ("s"))))

(dolist (x '(".bmp" ".jpg" ".jpeg"))
  (add-to-list 'completion-ignored-extensions x))


;;; autoload
;; =======================================================
;; Must come after loading basic libraries such as simple-ext, since
;; the autoloads itself or the code within the hooks they install (and
;; which are run during further processing of the init.el) depend on
;; them.

;; autoload
;;(load-library "debian-el-loaddefs")
;(load-library "emacs-goodies-loaddefs")
;; intendet for own libraries
(message2 "autoload loaddefs-custom")
(load-library "loaddefs-custom")
;; intendet for system wide libraries
(message2 "autoload loaddefs-site-lisp")
(load-library (concat user-emacs-directory "site-lisp/loaddefs-site-lisp"))
;; intendet for libraries in .emacs.d/site-lisp
(message2 "autoload loaddefs-local-site-lisp")
(load-library "loaddefs-local-site-lisp")


;;; autostart
;; =======================================================
(message2 "init file: autostart")
(require 'server)
(let ((running-p (server-running-p)))
  (cond
   ((not running-p)
    (server-start))
   ((eq running-p t)
    (message "Emacs server is alrady running. Not starting it."))
   (t
    (message "Cannot determine wheter Emacs server is running. Not starting it."))))

;; put here at the end of the startup instead within custom-file so starting up
;; emacs with --debug-init has an effect. Else, modifying debug-on-error within
;; custom-file, which is loaded rather early within startup, would override
;; --debug-init.
(setq debug-on-error nil)

(require 'powerline)
(powerline-default-theme)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(message2 "init file done")
;;; init.el ends here
