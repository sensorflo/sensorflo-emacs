;;; nova.el --- customizations specific to the nova project
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; 
;;; Commentary:
;; 

;;; Variables:
(require 'project)        ; https://github.com/sensorflo/sensorflo-emacs/
(require 'rx)
(require 'find-file-ext)
(require 'tempo-snippets) 
(require 'markup-faces)   ; https://github.com/sensorflo/markup-faces
(require 'font-lock-ext)  ; https://github.com/sensorflo/font-lock-ext/

(defgroup nova-project nil ""
  :group 'project)

(defcustom nova-build-server "atracmp07"
  "build server for `compile-ext'"
  :type '(choice (const :tag "radfeld" "atracmp07")
                 (const :tag "cham" "compile-ch.eu.besi.corp"))
  :group 'nova-project)

;; (mapcar
;;  (lambda (x)
;;    (mapcar
;;     (lambda (y)
;;       (concat x y))
;;     '("1" "2" "3")))
;;  '("flo" "edi" "remo"))

;; (defmacro mapcar2d (f seq1 seq2)
;;   `(mapcar (lambda (x) (mapcar (lambda (y) (funcall ,f x y)) ,seq1)) ,seq2))

;; (mapcar2d 'concat '("1" "2" "3")  '("flo" "edi" "remo"))

;; todo: make it more general. build / run is actually a set of the following,
;; each entry having an active|inactive flag:
;; 
;; make target...
;; msync | msyncgeil | ( cp ./install/vwork;  dcinstall )
;; dckill && dcstart
;;
;; the additional (to the make) stuff can be seen as a target to yet another,
;; outher, makefile
(defcustom nova-compile-target "default"
  ""
  :type '(radio
          string
          (const "default")
          (const "mclean")
          (const "mclean default")
          (const "mclear")
          (const "mcp")
          (const "dokuall")
          (const "samall")
          (const "base.base")
          (const "base.doku")
          (const "base.sam")
          (const "base_dev.base")
          (const "base_dev.doku")
          (const "base_dev.sam")
          (const "nfw.base")
          (const "nfw.doku")
          (const "nfw_dev.base")
          (const "nfw_dev.doku")
          (const "drivers.mut")
          (const "drivers.doku")
          (const "drivers.sam")
          (const "drivers_dev.mut")
          (const "drivers_dev.doku")
          (const "drivers_dev.sam")
          (const "guibricks_dev.base")
          (const "scheduler_dev.mut")
          (const "workerbh_dev.mut")
          (const "workersh_dev.mut")
          (const "workerdi_dev.mut")
          (const "waferhandling_dev.mut")
          (const "clninst head make_enums build_base build_intf buildpl2 build_parm"))
  :group 'nova-project)

(defcustom nova-pc-p t
  "Wether pc or sam is meant in ambigous situations."
  :type 'boolean
  :group 'nova-project)

;;; Code:

;;; misc settings

(defun nova-c-mode-common-hook ()
  (when (eq (project-root-type) 'project-nova)
    (setq c-basic-offset 4)
    (c-set-offset 'access-label '-)
    (c-set-offset 'inclass '+)
    (c-set-offset 'arglist-intro 'c-lineup-arglist-intro-after-paren)
    (c-set-offset 'arglist-cont '(add c-lineup-arglist-close-under-paren 1))
    (c-set-offset 'arglist-cont-nonempty '(add c-lineup-arglist-close-under-paren 1))
    (nova-font-lock-add-c++-keywords)
    (setq c-class-name-alist
	  '(("dispenserhead" . "DispensHead")
	    ("bondhead" . "BondHeadLeDa")))
    (nova-c-mode-common-bindings)))

(add-hook 'c-mode-common-hook 'nova-c-mode-common-hook t)

(defun nova-c-mode-common-bindings()
  ;; definitions / declarations
  (local-set-key [(control ?,)(d)(d)] 'tempo-template-c-nova-method)
  (local-set-key [(control ?,)(d)(c)] 'tempo-template-c-nova-class)

  ;; comment stuf
  (local-set-key [(control ?,)(k)(i)] 'tempo-template-c-nova-internal)
  (local-set-key [(control ?,)(k)(c)] 'tempo-template-doxy-code)

  ;; statements
  (local-set-key [(control ?,)(s)(?\;)] 'tempo-template-c-nova-statement-common)
  (local-set-key [(control ?,)(s)(r)] 'tempo-template-c-nova-early-return)
  
  ;; text
  (local-set-key [(control ?,)(t)(o)] 'tempo-template-c-nova-oi18n)
  (local-set-key [(control ?,)(t)(t)] 'tempo-template-c-nova-ti18n)

  ;; nova project
  (local-set-key [(control f)(p)] (make-sparse-keymap))
  (local-set-key [(control f)(p)(l)] 'nova-rm-log)
;  (local-set-key [remap dired-jump] 'nova-dired-jump) 
  ) 

(defun nova-common-mode-hook ()
  (when (eq (project-root-type) 'project-nova)
    (make-local-variable 'grep-find-command)
    (grep-apply-setting 'grep-find-command (concat
      "cd ~/prog/nova && find $(lsentities) \\\n"
      "-not \\( -iname bin_capella64 -prune \\) -not -iregex '.*/\\.#.*' \\\n" 
      "-iregex '.*\\.\\(h\\|cpp\\)' \\\n"
      "-print0 | xargs -0 grep --color=always \\\n"
      "-nIPe ''"))
    (setq tab-width 4)
    (unless (eq major-mode 'makefile-gmake-mode)
      (setq indent-tabs-mode nil))
    (when (not (nova-coding-system-p))
      (message "%s: encoding system is %S which is not nova's encoding system (utf-8-unix)"
               (buffer-name) buffer-file-coding-system))
    (my-mks-bindings)
    ;; (set (make-local-variable 'globalff-databases) "/home/emp8118035/prog/nova/.locatedb")
    (set (make-local-variable 'find-dired-dir) "prog/nova")
    (local-set-key [remap compile] 'compile-ext)))

(add-hook 'common-mode-hook 'nova-common-mode-hook t)

(defun nova-before-save-hook ()
  (when (eq (project-root-type) 'project-nova)
    (when (member major-mode '(c++-mode dt2-mode stream-mode doxym-mode))
      (save-restriction
	(widen)
	(untabify (point-min) (point-max)))) 
    (when (and (not (nova-coding-system-p))
               (not (nova-pl2-file-p)))
      (if (yes-or-no-p (format "%s: change coding system from %S to utf-8-unix? "
                               (buffer-name) buffer-file-coding-system))
          (setq buffer-file-coding-system 'utf-8-unix)
        (if (yes-or-no-p "abort saving? ") (error "abort"))))))

(add-hook 'before-save-hook 'nova-before-save-hook t)

(defun nova-conf-after-save-hook () 
  (when (and (memq major-mode '(conf-space-mode conf-unix-mode))
	     (buffer-file-name)
	     (string-match "\.env$" (buffer-file-name))
	     (y-or-n-p "run qtconfig? "))
    (when (get-buffer "*qtconfig output*")
      (kill-buffer "*qtconfig output*"))
    (let ((proc (start-process "qtconfig" (get-buffer-create "*qtconfig output*") "qtconfig")))
      (set-process-sentinel proc (lambda (p reason)
				   (when (memq (process-status p) '(signal exit))
				     (if (and (equal (process-status p) 'exit) (eq 0 (process-exit-status p))) 
					 (call-process "notify-send" nil nil nil "-t" "1000" "qtconfig finished")
				       (call-process "notify-send" nil nil nil "-t" "1000" "qtconfig finished with error"))
				     (delete-process p)))))))

(add-hook 'after-save-hook 'nova-conf-after-save-hook t)

(defun nova-font-lock-add-c++-keywords ()
  (font-lock-add-keywords nil (list
     ;; --- unimportant comments parts ---

     ;; --- unimportant ---
     (list "}\\s-*\\(//\\s-*end\\b.*\n\\)" '(1 font-lock-unimportant t))
     (list "^\\s-*\\(//\\s-*end\\s-*\\w+::\\w+\\s-*\n\\)" '(1 font-lock-unimportant t))
     (list "^\\s-*\\(/\\*+\\s-*virtual\\s-*\\*+/\\)" '(1 font-lock-unimportant t))
     (cons "\\$Log\\s-*" 'font-lock-unimportant)

     ;; --- semi-unimportant ---

     ;; pre-,postfixes
     (list (concat
            ; Context prefix. only _ or v_ is fontified.
            "\\_<\\(?:[ioxscmg]\\(_\\)\\|\\(v_\\)\\)?" 
            ; type prefix. Fontified.
            "\\(\\(?:[pcra]\\|sp\\)*\\(?:[nfeobs]\\|vec\\|mat\\)?\\)" 
            ;; identifiers start upercase. Not fontified 
            "[A-Z0-9_]\\(?:\\w\\|\\s_\\)*\\_>"                  
            ;; exclude identifier binding methods
            "[ \t]*[^( \t]")                          
           '(1 font-lock-semi-unimportant nil t) 
           '(2 font-lock-semi-unimportant nil t)
           '(3 font-lock-semi-unimportant)
           )
     (cons "\\_<eCmd\\B" 'font-lock-semi-unimportant)

     ;; casts.
     ;; Mostly, but not always!, they don't really do anything, its
     ;; 'just' to satisfy compiler)
     (list "\\_<\\(\\(?:static\\|const\\|dynamic\\|reinterpret\\)_cast\\s-*<[^>]*>\\s-*\\)" '(1 font-lock-semi-unimportant))
     (list (concat "\\("
		     "\\(?:->\\|\\.\\)[ \t]*"
		     (rx (or "getm" "getm_s" "getm_s2" "getm_s3" "gets" "getkg" "getN" "getPa" "getK" "getrad" "getdegree" "getPas" "getCentipoise" "getm2" "getm3" "getrad_s" "getrad_s2" "getrad_s3" "getdegree_s" "getdegree_s2" "getdegree_s3"))
		     "[ \t]*"
		     "([ \t]*)"
		   "\\)")
	   '(1 font-lock-semi-unimportant t))
     (list (concat "\\_<\\("
		   (rx (or "meter" "sec" "kilogramm" "kelvin" "radian" "meter_sec" "meter_sec2" "newton" "pascal" "meter_sec3" "Pas" "meter2" "meter3" "radian_sec" "radian_sec2" "radian_sec3" "millimeter" "mikrometer" "millisec" "gramm" "centipoise" "degree" "degree_sec" "degree_sec2" "degree_sec3" "celsius" "sec"))
		   "\\)[ \t]*\\(?:\\((\\)[ \t]*[^()\n]*[ \t]*\\()\\)\\|(\\)")
	   '(1 font-lock-semi-unimportant t)
	   '(2 font-lock-semi-unimportant t t)
	   '(3 font-lock-semi-unimportant t t))

     ;; namespaces
     (list "\\_<\\(\\(?:\\(?:\\w\\|\\s_\\)+\\s-*::\\s-*\\)+\\)\\(?:\\w\\|\\s_\\)+\\_>" '(1 font-lock-semi-unimportant))

     ;; trace/log/assert/result
     (list "\\_<Result[ \t]+res[ \t]*\\(?:=[ \t]*Result::Ok[ \t]*\\)?;" '(0 font-lock-semi-unimportant t))
     (cons "\\_<res[ \t]*[|&]?=" 'font-lock-semi-unimportant)
     (list (rx (group              
                (regex "\\_<")
                (or "ASSERT" "ASSERT_ALWAYS" "TESTOMA_MESSAGE" (regex "\\(?:LOG\\|RETURN_\\)\\(?:\\w\\|_\\)*"))
                (regex "\\s-*([^;]*)\\s-*;"))) 
           '(0 font-lock-semi-unimportant t))
     (list "\\bResult::failed[ \t]*\\((.*?\\(?:\n.*?\\)*?)\\)[ \t]*;" 
           '(1 font-lock-semi-unimportant t))
     
     ;; string stuff
     (list "\\_<\\(oi18n\\)\\_>[ \t]*(" '(1 font-lock-semi-unimportant t))
     (list "\\_<\\(ti18n\\)[ \t]*([ \t]*\\(\"sw\"[\t]*,\\)" '(1 font-lock-semi-unimportant t) '(2 font-lock-semi-unimportant t)))))

(add-hook 'find-file-hook 'nova-find-file-hook t)

(defun nova-find-file-hook()
  (let ((buffer-name (buffer-name)))
    (when (and (eq (project-root-type) 'project-nova)
	       (string-match "\\(.*\\)<[0-9]+>$" buffer-name)
	       (not (string-match "makefile" buffer-name)))
      (let* ((base-name (match-string 1 buffer-name))
	     (other-buf (get-buffer base-name))
	     (act-buf (current-buffer)))
	(when other-buf
	  (rename-buffer (concat base-name "<" (nova-build-name) ">" ) t)
	  (set-buffer other-buf)
	  (rename-buffer (concat base-name "<" (nova-build-name) ">" ) t)
	  (set-buffer act-buf))))))

(defun nova-conf-mode-hook()
  (when (and (buffer-file-name) (string-match "\.env$" (buffer-file-name)))
    (let* ((known-entities '("base"
			     "gui_bricks"
			     "nfw"
			     "scheduler"
			     "drivers"
			     "workerbh"
			     "workersc"
			     "workerdi"
			     "waferhandling"
			     "ctsdualgripper"
			     "ischeduler_worker"
			     "ischeduler_driver"
			     "idriver_workerbh"
			     "idriver_workersc"
			     "idriver_workerdi"
			     "idieprovider_worker"
			     "icts_driver"))
	   (re (concat "\\_<\\(?:" (mapconcat 'identity known-entities "\\|") "\\)\\(?:[-a-zA-Z0-9_]\\)+")))
    (font-lock-add-keywords nil (list
      (cons re 'font-lock-function-name-face)
      (list (concat "^LOCAL_ROOT\\s-*=\\s-*\\(?:" (getenv "HOME") "[ \t]*$\\|\\(.+?\\)[ \t]*$\\)") '(1 font-lock-warning-face t t))
      (list (concat "^BIB_DIR\\s-*=\\s-*\\(?:" (getenv "HOME") "/bib/release/zorro/[ \t]*$\\|\\(.+?\\)[ \t]*$\\)") '(1 font-lock-warning-face t t))
      (list "^SW_VERSION\\s-*=\\s-*\\(?:flka[ \t]*$\\|\\(\\w+\\)\\)" '(1 font-lock-warning-face t t))
      (list "^MAKE_SAM\\s-*=\\s-*\\(?:YES[ \t]*$\\|\\(\\w+\\)\\)" '(1 font-lock-warning-face t t))
      (list "=.*?\\(\\s-+\\)\\(?:$\\|#\\)" '(1 font-lock-warning-face t t)))))))

(add-hook 'conf-mode-hook 'nova-conf-mode-hook t)

;; (when (require 'ede nil t)
;;   (ede-cpp-root-project "nova" :file "/home/emp8118035/prog/nova/ledb.env")
;;   (ede-cpp-root-project "nova" :file "/home/emp8118035/prog/nova/ledb.env"
;; 			:include-path '("/home/emp8118035/prog/nova/include"
;; 					"include"
;; 					"../include"
;; 					"../../include"
;; 					"../../../include" )
;; 			:system-include-path '( "/home/emp8118035/bib/release/zorro/include"
;; 						"/home/emp8118035/bib/release/zorro/include/oma6rtos/oma"
;; 						"/home/emp8118035/bib/release/zorro/include/oma6rtos/os/inco/inc"
;; 						"/home/emp8118035/bib/release/zorro/include/oma6rtos/os/inos/inc"
;; 						"/home/emp8118035/bib/release/zorro/base"
;; 						)))

;;; compile stuff 

(defconst nova-comp-title-0
  "^\\(---+ +\\)\\(Masterbuild for M_.*?\\)\\( +-+ *\\)$")

(defconst nova-comp-title-1
  "^\\(---+ +\\)\\(\\(?:\\(?:building\\|linking\\).*?M_HSFC\\|packaging M_HSFC\\|buildstep making enums \\).*?\\)\\( +-+ *\\)$")

(defconst nova-comp-title-2
  "^\\(---+ +\\)\\(Masterbuild for .*?\\)\\( +-+ *\\)$")

(defconst nova-comp-title-3
  "^\\(---+ +\\)\\(.*?\\)\\(-\\{4,\\}\\)\\(.*?\\)\\( +-+ *\\)$")

(defconst nova-comp-title-4
  "^\\(---+ +\\)\\(.*?\\)\\( +-+ *\\)$")

(defconst nova-comp-font-lock-keywords
  (list
   ;; sections
   (list nova-comp-title-0
	 '(1 font-lock-unimportant)
	 '(2 markup-title-0-face)
	 '(3 font-lock-unimportant))
   (list nova-comp-title-1
	 '(1 font-lock-unimportant)
	 '(2 markup-title-1-face)
	 '(3 font-lock-unimportant))
   (list nova-comp-title-2
	 '(1 font-lock-unimportant)
	 '(2 markup-title-2-face)
	 '(3 font-lock-unimportant))
   (list nova-comp-title-3
	 '(1 font-lock-unimportant)
	 '(2 markup-title-3-face)
	 '(3 font-lock-unimportant)
	 '(4 markup-title-3-face)
	 '(5 font-lock-unimportant))
   (list nova-comp-title-4
	 '(1 font-lock-unimportant)
	 '(2 markup-title-4-face)
	 '(3 font-lock-unimportant))

   ;; trash
   (list "^echo\\b.*$" '(0 font-lock-unimportant)))
  "Keywords for compilation-mode of a nova build")

(defun nova-comp-outline-level ()
  (cond
   ((looking-at nova-comp-title-0) 1)
   ((looking-at nova-comp-title-1) 2)
   ((looking-at nova-comp-title-2) 3)
   ((looking-at nova-comp-title-3) 4)
   (t 5)))

(defun nova-comp-add ()
  (interactive)
  (font-lock-mode 0)
  (font-lock-add-keywords nil nova-comp-font-lock-keywords)
  (set (make-local-variable 'outline-regexp)
       (concat "\\(?:"
	       (mapconcat
		'identity
		(list nova-comp-title-0 nova-comp-title-1 nova-comp-title-2 nova-comp-title-3 nova-comp-title-4)
		"\\|")
	       "\\)"))
  (set (make-local-variable 'outline-level) 'logfile-outline-level)
  (outline-minor-mode t)
  (font-lock-mode 1))

(defun nova-compilation-start-hook(proc)
  (nova-comp-add))

(defun nova-compilation-finish-function(buf result)
  (shell-command (concat "notify-send -t 1000 'compilation " result "'")))

(add-to-list 'compilation-start-hook 'nova-compilation-start-hook)
(add-to-list 'compilation-finish-functions 'nova-compilation-finish-function)

;;; functions

(defun nova-coding-system-p ()
  (member buffer-file-coding-system '(utf-8-unix undecided-unix undecided)))

(defun nova-pl2-file-p ()
  (member major-mode '(sgml-mode)))

(defun nova-find-matrix-file (vel acc)
  (interactive "nVelocity Index : \nnAcceleration Index : ")
  (find-file (concat "~/office/Nova/lookuptable/scratch3/Mat_v" (number-to-string vel) "_a" (number-to-string acc) ".csv"))
  (mark-whole-buffer)
  (clipboard-kill-ring-save (point) (mark)))

(defun nova-make-class(name &optional proj-dir)
  (interactive "sclass name: \nDproject dir: ")

  (when (or (null proj-dir) (equal proj-dir ""))
    (setq proj-dir default-directory))
  (setq proj-dir (expand-file-name proj-dir))
  (when (string-match "\\(.*?\\)/\\(?:source\\|include\\)/?$" proj-dir)
    (setq proj-dir (match-string 1 proj-dir)))
  (when (string-match "^.*?[^/]$" proj-dir)
    (setq proj-dir (concat proj-dir "/")))

  (let* ((source-name (concat (downcase name) ".cpp"))
  	 (header-name (concat (downcase name) ".h"))
  	 (source-rel-path (concat "source/" source-name))
  	 (source-path (concat proj-dir source-rel-path))
  	 (header-path (concat proj-dir "include/" header-name))
	 (make-path (concat proj-dir "makefile")))

    (when (file-exists-p header-path)
      (error "%s already exists" header-path))
    (when (file-exists-p source-path)
      (error "%s already exists" source-path))
    (when (not (file-exists-p make-path))
      (error "%s does not exist" make-path))
    (when (not (file-writable-p make-path))
      (if (featurep 'mks)
	  (progn 
	    (message "%s is not writable, checking it out" make-path)
	    (mks-co make-path ":head" t)
	    (when (not (file-writable-p make-path))
	      (error "%s still not writable, mks checkout probably failed" make-path)))
	(error "%s is not writable and emacs mks frontend not available" make-path)))

    (setq nova-tempo-class-name name)

    (set-buffer (generate-new-buffer header-name))
    (write-file header-path)
    (tempo-template-c-nova-h-file-1-ni)
    (goto-char (point-max))
    (tempo-template-c-nova-class-ni)
    (goto-char (point-max))
    (tempo-template-c-nova-h-file-2-ni)
    (write-file header-path)
    (goto-char (point-min))

    (set-buffer (generate-new-buffer source-name))
    (write-file source-path)
    (tempo-template-c-nova-src-file-1-ni)
    (goto-char (point-max))
    (tempo-template-c-nova-ctor-def-ni)
    (goto-char (point-max))
    (insert "\n")
    (tempo-template-c-nova-dtor-def-ni)
    (goto-char (point-max))
    (tempo-template-c-nova-src-file-2-ni)
    (write-file source-path)
    (goto-char (point-min))

    ;; edit makefile
    (nova-adapt-makefile make-path source-rel-path)

    ;; end showing the header file
    (find-file header-path)))

;; todo: if source-name is nil, deduce it from current buffers name
;; todo: auto-find makefile, or at least its default when prompting
(defun nova-adapt-makefile(make-path source-rel-path)
  (interactive "fmakefile: \nssource-name: ")

  (find-file make-path)

  ;; todo: if write-protected check it out

  (save-excursion
    ;; first check if it's already listed
    (goto-char (point-min))
    (unless (re-search-forward (concat "^[ \t]*SRC_LIST[ \t]*[:+]?=[ \t]*" source-rel-path "\\_>") nil t)
      (let ((indent " "))
	;; move point to where to insert the new entry
	(if (re-search-forward "^[ \t]*SRC_LIST\\([ \t]*\\)[:+]?=" nil t)
	    ;; append to an existing SRC_LIST
	    (progn 
	      (setq indent (match-string 1))
	      (beginning-of-line)
	      (while (re-search-forward "^[ \t]*SRC_LIST[ \t]*[:+]?=" nil t))
	      (end-of-line)
	      (insert "\n"))
	  ;; create a new SRC_LIST - list
	  (goto-char (point-max))
	  (while (re-search-backward "^[ \t]*include\\b" nil t)) ;do nothing in while's body
	  (open-line 2)
	  (insert "SRC_LIST :=\n"))

	;; actually insert new line
	(insert (concat "SRC_LIST" indent "+= " source-rel-path))))))

(defun nova-qtconfig () 
  (interactive)
  (shell-command "qtconfig /home/emp8118035/prog/nova"))

(defun nova-rm-log ()
  "Removes the mks log commen"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (not (re-search-forward "^/+\\s-*\n//\\s-*\\$Log:.*\\$"))
	(message "No log found")
      (let ((start (match-beginning 0)))
	(re-search-forward "^[^/]\\|\\'")
	(delete-region start (point))))))

(defun nova-dired-jump (&optional other-window)
  (interactive "P")
  (let* ((file buffer-file-name)
         (dir (if file (file-name-directory file) default-directory))
	 (root-dir (project-root-dir file)))
    (if other-window
	(dired-other-window root-dir)
      (dired root-dir))
    (dired-insert-subdir "source" nil t)
    (dired-insert-subdir "include" nil t)
    (if file
	(or (dired-goto-file file)
	    ;; refresh and try again
	    (progn
	      (dired-insert-subdir (file-name-directory file))
	      (dired-goto-file file))
	    ;; Toggle omitting, if it is on, and try again.
	    (if dired-omit-mode
		(progn
		  (dired-omit-mode)
		  (dired-goto-file file)))))))

;;; file aliases / cache

(setq filealias-default-root-dir "~/prog/nova")
(setq filealias-alist
  '(
    ("d". "~~drivers_dev")
    ("b". "~~base_dev")
    ("s". "~~scheduler_dev")
    ("n". "~~nfw_dev")
    ("wbh". "~~workerbh_dev")
    ("wsc". "~~workersc_dev")
    ("wh". "~~waferhandling_dev")
    
    ;; nfw 
    ("nc". "~~nfw_dev/calibration/source")
    ("nig". "~~nfw_dev/interpreter/ipsgui/source")

    ;; drivers
    ("dhh". "~~drivers_dev/highspeedhandling/source")
    ("ddi". "~~drivers_dev/dispensehandling/source")
    ("dsh". "~~drivers_dev/sliderhandling/source")
    ("dsc". "~~drivers_dev/substratecamera/source")
    ("dsh". "~~drivers_dev/substratehandling/source")
    
    ;; schmarrn
    ("didi". "~~idriver_workerdi_dev/interface/include/idriver_workerdi.h")))

(mapcar 'file-cache-add-file 
        '("~/prog/nova"
          "~/prog/nova/nova.env"
          "~/prog/nova/drivers_dev"
          "~/prog/nova/drivers_dev/substratecamera"
          "~/prog/nova/drivers_dev/dispensehandling"
          "~/prog/nova/drivers_dev/highspeedhandling"
          "~/prog/nova/base_dev/rtosbase"
          "~/prog/nova/rtos/sam_config"
          "~/prog/nova/nfw_dev"
          "~/prog/nova/nfw_dev/calibration"
          "~/prog/nova/nfw_dev/interpreter"
          "~/prog/nova/nfw_dev/mcmodel"
          "~/prog/nova/nfw_dev/interpreter/ipsgui"
          "~/prog/nova/base_dev/docoverall"
          "~/prog/nova/base_dev/errorhandling"
          
          "~/datacon/sd0/datacon/log"

          "~/bib/release/zorro"
          "~/bib/release/zorro/base/muesli"
          "~/bib/release/zorro/base/ipu/vipu"

          "~/office/nova"
          "~/office/nova/todo_qanda.txt"
          "~/office/nova/misc.txt"
          "~/office/nova/calib"

          "~/src/oma6rtos/os/inos"))

(let ((mylist '(
  ("/dispensehandling/source" nova-ffe-dispensehandling)
  ("/dispensehandling/include" nova-ffe-dispensehandling)
  ("/highspeedhandling/source" nova-ffe-highspeedhandling)
  ("/highspeedhandling/include" nova-ffe-highspeedhandling)
  ("/substratecamera/source" nova-ffe-substratecamera)
  ("/substratecamera/include" nova-ffe-substratecamera)
  ("/ipsgui/source" nova-ffe-ipsgui)
  ("/ipsgui/include" nova-ffe-ipsgui)
  ("/calibration/source" nova-ffe-calibration)
  ("/calibration/include" nova-ffe-calibration)
  )))
  (dolist (elt mylist) 
    (add-to-list 'ffe-map-map elt)))

(setq nova-ffe-highspeedhandling '(
  ;; both
  ;; pc
  ("highspeedhandling" ("hsh") )
  ("bondhead" ("bh") )

  ;; rtos
  ("highspeedhandlingrtos" ("hshr") )
  ("bhcrashcalc" ("c") )
  ("bhcrashguard" ("g") )
  ("bhcrashlog" ("l") )
  ("bhcrashramp" ("r") )
  ("bhcrashrampfactory" ("rf") )
  ("bhcrashrampinos" ("ri") )
  ("bhcrashramplookup" ("rl") )
  ("bhcrashramplookupcalc" ("rlc") )
  ("bhcrashramppredefined" ("rp") )
  ("bhcrashrampset" ("rse") )
  ("bhcrashrampsimple" ("rsi") )
  ("bhcrashtypes" ("t") )
  ("bhpassby" ("pb"))))

(setq nova-ffe-dispensehandling '(
  ;; both
  ("diaction" ("a" "dia") "h")
  ("diposition" ("p" "dip") "h")

  ;; pc
  ("dispenserhead" ("dh") )
  ("diactiontable" ("at") )
  ("dipositionlist" ("pl" "dipl") )
  ("dispenserheadrtosif" ("rif" "if") )
  ("testcmds" ("tc") )

  ;; rtos
  ("dispenserheadrtos" ("dhr") )
  ("diccycle" ("dic" "dc") )
  ("calibconv" ("cc" "c") )
  ("patternwriter" ("pw") )
  ("pulseseq" ("ps") )
  ("prestank" ("pt") )
  ("vactank" ("vt") )
  ("writesys" ("ws"))
  ("syringe" ("s"))
  ("prestankhw" ("pthw" "pth") "h")
  ("vactankhw" ("vthw" "vth") "h")
  ("hosehw" ("hhw" "hh") "h")
  ("writesyshw" ("wshw" "wsh") "h")
  ("actiontablertos" ("atr") )
  ("ditypes" ("dit" "dt" "t") )
  ("pdssim" ("psim" "sim") )))

(setq nova-ffe-substratecamera '(
  ("scyaxis" ("y" "scya") )
  ("substratecamera" ("sc" "c") )
  ("substratecamera_os" ("sco" "os") )
  ("substratecamera_view" ("scv" "view"))))

(setq nova-ffe-ipsgui '(
  ("ipsgui" ("ips") )
  ("calibassiguisca" ("asc") )
  ("calibassiguiwca" ("awca" "awc") )
  ("calibassiguihshsvd" ("ahshsvd" "ahsh") )
  ("calibstepguicam" ("sc" "scam"))))

(setq nova-ffe-calibration '(
  ("basiscalibration" ("bc" "asca" "as") )
  ("calibassiwca" ("asca" "as") )
  ("calibassihsh" ("ahsh" "ah"))

  ("calibstepcam" ("scam" "sc"))

  ("calibmeths" ("m"))
  ("calibnames" ("n"))
  ("calculations" ("c"))
  ))

;;; tempos

;; I have yet to find out how I can do both in one single tempo template def.
;; Now I always have to make two definitions of the 'same' temlate, once x and
;; once x-ni.
;; - (p "Name: " var) and (s name)
;; - nova-tempo-class-name
(defvar nova-tempo-class-name nil "used within some tempo templates" )

(tempo-define-template "c-nova-oi18n"
 '( "oi18n(\"" r "\")")
 "oi18n")

(tempo-define-template "c-nova-ti18n"
 '( "ti18n(\"sw\",\"" r "\")")
 "ti18n")

(tempo-define-snippet "c-nova-class"
 '( &
    "/** " p "*/" > n>
    "class " (p "classname: " class) > n> 
    "{" > n>
    "public:" > n>
    (s class) "();" > n>
    "~" (s class) "();" > n>
    p n>
    "private:" > n>
    p n>
    "};" > n>
    ))

;; non interactive - see nova-tempo-class-name
(tempo-define-snippet "c-nova-class-ni"
 '( "/** " p "*/" > n>
    "class " nova-tempo-class-name > n> 
    "{" > n>
    "public:" > n>
    nova-tempo-class-name "();" > n>
    "virtual ~" nova-tempo-class-name "();" > n>
    p n>
    "private:" > n>
    p n>
    "};" > n>
    ))

(tempo-define-template "c-nova-method-def"
 '( &
    "/** " p "*/" > n>
    "Result " '(insert-class-name) "::" p "()" > n>
    "{" > n>
    "Result res;" > n>
    p n>
    "return res;" n>
    "}" > n> ))

(tempo-define-template "c-nova-method-decl"
 '( &
    "Result " p "(" p ");" > ))

(defun tempo-template-c-nova-method()
  (interactive)
  (open-line 1)
  (if (c-src-buffer-p)
      (tempo-template-c-nova-method-def)
    (tempo-template-c-nova-method-decl)))

;; non interactive - see nova-tempo-class-name
(tempo-define-template "c-nova-ctor-def-ni"
 '( nova-tempo-class-name "::" nova-tempo-class-name "()" > n>
    "{" > n>
    "}" > n> ))

(tempo-define-template "c-nova-dtor-def-ni"
 '( nova-tempo-class-name "::~" nova-tempo-class-name "()" > n>
    "{" > n>
    "}" > n> ))

;; non interactive - see nova-tempo-class-name
(tempo-define-template "c-nova-h-file-1-ni"
 '( "#ifndef _" (upcase nova-tempo-class-name) "_H_\n"
    "#define _" (upcase nova-tempo-class-name) "_H_\n"
    "\n"
    "// local includes\n"
    "// system includes\n" 
    "#include \"muesli.h\"\n" p
    "// forwards\n"
    "\n"))

;; non interactive - see nova-tempo-class-name
(tempo-define-template "c-nova-h-file-2-ni"
 '( "\n"
    "///////////////////////////////////////////////////////////////////////////////\n"
    "// $Log: include/" (downcase nova-tempo-class-name) ".h  $\n"
    "\n"
    "#endif // _" (upcase nova-tempo-class-name) "_H_\n"))

;; non interactive - see nova-tempo-class-name
(tempo-define-template "c-nova-src-file-1-ni"
 '( "// local includes\n"
    "#include \"../include/" (downcase nova-tempo-class-name) ".h\"\n"
    "// system includes\n"
    "\n"))

;; non interactive - see nova-tempo-class-name
(tempo-define-template "c-nova-src-file-2-ni"
 '("\n"
   "///////////////////////////////////////////////////////////////////////////////\n"
   "// $Log: source/" (downcase nova-tempo-class-name) ".cpp  $\n"))

(tempo-define-template "c-nova-internal"
 '( "\\internal FLKA " '(format-time-string "%d.%m.%Y") ": "))


;;; nova.el ends here
