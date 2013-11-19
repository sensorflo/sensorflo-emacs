;;; dragon.el --- customizations specific to the dragon project
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://github.com/sensorflo/sensorflo-emacs/, then
;;      projects/dragon.el 
;;
;;; Commentary:
;;
;;; Code:
(require 'project)         ; https://github.com/sensorflo/sensorflo-emacs/
(require 'tempo-ext) 	   ; https://gitorious.org/tempo-ext
(require 'tempo-snippets)  ; http://nschum.de/src/emacs/tempo-snippets/
(require 'font-lock-ext)   ; https://github.com/sensorflo/font-lock-ext/

;;; misc settings
(defun dragon-c-mode-common-hook()
  (message "dragon-c-mode-common-hook")
  (when (or (eq (project-root-type) 'project-diebonder-pc)
            (eq (project-root-type) 'project-diebonder-rtos))
    (make-local-variable 'grep-find-command)
    (grep-apply-setting 'grep-find-command (dragon-grep-find-command))
    (set (make-local-variable 'grep-find-ext-command-function) 'dragon-grep-find-command)
    (set (make-local-variable 'grep-find-ext-regexp-function) 'dragon-grep-find-regexp)
    (set (make-local-variable 'ediff-default-filtering-regexp) "\\.\\(cpp\\|h\\|idl\\)")
    (set (make-local-variable 'require-final-newline) nil)
    (set (make-local-variable 'tempos-c++-open-brace-style) 'behind-conditional)
    (setq tab-width 2)
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 2)
    (c-set-offset 'access-label '-)
    (c-set-offset 'inclass '++)
    (dragon-font-lock-add-keywords)
    (set (make-local-variable 'vc-handled-backends) nil)

    ;; MPS specific
    (let ((actual-fn (or (buffer-file-name) default-directory)))
      (when (string-match "/DieCarrier/" actual-fn)
	(set (make-local-variable 'tempos-c++-open-brace-style) 'new-line)
	(set (make-local-variable 'dragon-method-decl-empty-comment) t)))

    ;; todo: maybe its cleaner to make dragon-abbrev-table the
    ;; local-abbrev-table, and the ex local-abbrev-table a parent of it
    (abbrev-mode 1)
    ;; (when (or (not (listp local-abbrev-table))
    ;; 	      (not (member dragon-abbrev-table local-abbrev-table)))
    ;;   (if (not (listp local-abbrev-table))
    ;; 	  (setq local-abbrev-table (list local-abbrev-table dragon-abbrev-table))
    ;; 	(setq local-abbrev-table (list local-abbrev-table dragon-abbrev-table))))
    (dolist (x dragon-abbrev-table) 	
      (when (nth 2 x)
	(put (nth 2 x) 'no-self-insert t))
      (define-abbrev local-abbrev-table (nth 0 x) (nth 1 x) (nth 2 x)
	:enable-function (dragon-create-abbrev-enable-function (nth 3 x))
	:case-fixed t))
    (abbrev-table-put local-abbrev-table
                      :regexp "\\(\\(?:^\\|#\\|\\_<\\)\\(?:\\s_\\|\\sw\\)+\\)")

    (when (and (dragon-file-p) (not (dragon-coding-system-p)))
      (message (concat "%s: encoding system is %S which is not one of Dragons's "
                       "encoding systems, see dragon-coding-system-p")
               (buffer-name) buffer-file-coding-system)
      (shell-command (concat "notify-send -t 1000 "
                             "'" (buffer-name) " has invalid encoding system!'")))
    (dragon-c-mode-common-bindings)))

(add-hook 'c-mode-common-hook 'dragon-c-mode-common-hook t)

(defun dragon-c-mode-common-bindings()
  ;;
  (local-set-key [f6] 'dragon-checkall)

  ;; definitions
  (local-set-key [(control ?\,)(d)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(d)(d)] 'tempo-template-dragon-method-std) ;d because its fast

  ;; control flow
  (local-set-key [(control ?\,)(c)(t)] 'tempo-template-dragon-try-catch-std) 

  ;; statements
  (local-set-key [(control ?\,)(s)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(s)(r)] 'tempo-template-dragon-early-return-std)
  (local-set-key [(control ?\,)(s)(a)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(s)(a)(a)] 'tempo-template-dragon-eassert)
  (local-set-key [(control ?\,)(s)(a)(n)] 'tempo-template-dragon-eassert-new)
  (local-set-key [(control ?\,)(s)(a)(p)] 'tempo-template-dragon-eassert-pointer)
  (local-set-key [(control ?\,)(s)(s)] 'tempo-template-dragon-statement-common)

  ;; comments
  (local-set-key [(control ?\,)(k)(u)] 'tempo-template-dragon-utf-dwim)

  ;; definitions / declarations
  (local-set-key [(control ?\,)(d)(c)] 'tempo-template-dragon-class-dwim)

  ;; misc
  (local-set-key [(control ?\,)(m)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(m)(t)] 'tempo-template-dragon-todo) ; todo
  (local-set-key [(control f)(control o)] 'dragon-find-other-file)

  ;; traces & text/string literals
  (local-set-key [(control ?\,)(t)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(t)(t)] 'tempo-template-dragon-wstring-literal)
  (local-set-key [(control ?\,)(t)(e)] 'tempo-template-dragon-trace-error)
  (local-set-key [(control ?\,)(t)(w)] 'tempo-template-dragon-trace-warning)
  (local-set-key (kbd "C-, t 0") 'tempo-template-dragon-trace-l0)
  (local-set-key (kbd "C-, t 1") 'tempo-template-dragon-trace-l1)
  (local-set-key (kbd "C-, t 2") 'tempo-template-dragon-trace-l2)
  (local-set-key [(control ?\,)(t)(i)] 'tempo-template-dragon-trace-ignore)
  (local-set-key [(control ?\,)(t)(m)] 'tempo-template-dragon-trace-method-enter))

(defun dragon-before-save-hook ()
  (when (and (or (eq (project-root-type) 'project-diebonder-pc)
                 (eq (project-root-type) 'project-diebonder-rtos))
             (dragon-file-p))

    ;; ensure character encoding is windows-1252, and end-of-line char is DOS
    ;; style
    (when (not (dragon-coding-system-p))
      (if (y-or-n-p (format "%s: change coding system from %S to windows-1252-dos? "
			    (buffer-name) buffer-file-coding-system))
          (setq buffer-file-coding-system 'windows-1252-dos)
        (if (y-or-n-p "abort saving? ") (error "user aborted abort aving"))))

    ;; autocorrect whitespace erros
    (when (member major-mode '(c++-mode idl-mode dt2-mode stream-mode doxym-mode))
      (save-restriction
	(widen)
	(untabify (point-min) (point-max))
	(delete-trailing-whitespace)))))

(add-hook 'before-save-hook 'dragon-before-save-hook t)

(defun dragon-find-file-hook()
  (when (and buffer-file-name (string-match "/drives/builds/" buffer-file-name))
    (toggle-read-only 1)
    (rename-buffer (concat (file-name-nondirectory buffer-file-name) "<" (dragon-build-name) ">" ) t)))

(add-hook 'find-file-hook 'dragon-find-file-hook t)

;;   ;; files on buildpages should be read only
;;   (when (string-match "^//eseczgbuilds\\.esec\\.com/" (buffer-file-name))
;;     (setq buffer-read-only t))

;;   ;; 
;;   (when (and (equal major-mode 'c++-mode)
;;              (string-match "^//eseczgbuilds\\.esec\\.com/\\|^W:" (buffer-file-name)))

(defun flf-helper(func end)
  (let (found)
    (while (and (< (point) end)
		(not (setq found (funcall func end))))
      (forward-char 1))
    found))

(defun dragon-looking-at-method-issues (end)
  "Returns t if between point and END is an issue concerning a method.
Additionaly match data is set to mark the culprit by match group 1."
  (let ((method-start-pos t)
	(method-end-pos t)
	an-issue-found) 
    ;; iterate over methods and search for issues in each method
    (while (and method-start-pos
		method-end-pos
		(not an-issue-found)
    		(< (point) (1- end)))
      (setq method-start-pos
	    (text-property-any (point) end 'face 'font-lock-function-name-face))
      (when method-start-pos
	(setq method-end-pos
	      (text-property-not-all method-start-pos end 'face
				     'font-lock-function-name-face)))
      (when (and method-start-pos method-end-pos)

	;; -- getters should be const
	(when (not an-issue-found)
	  (save-excursion
	    (let ((is-getter
		   (progn
		     (goto-char method-start-pos)
		     (looking-at (concat "i?\\(?:Is\\|Has\\|Get\\|Was\\|Had\\|"
					 "Show\\|Display\\|Print\\|Trace\\|"
					 "Log\\)\\(?:[A-Z_1-9]\\|\\b\\)"))))
		  (is-static
		   (progn
		     (beginning-of-line)
		     (looking-at "\\s-*static\\b")))
		  (is-const
		   (progn
		     (goto-char method-end-pos)
		     (forward-list 1)
		     (looking-at "\\s-*\\(?:=\\s-*0\\s-+\\)?const\\b")))
		  (is-surpressed
		   (progn
		     (goto-char method-end-pos)
		     (forward-list 1)
		     (looking-at (concat "\\s-*\\(=\\s-*const\\s-*\\)?;"
					 "?\\s-*\\(?://\\|/\\*+\\)\\s-*"
					 "cppcheck-surpress")))))
	      (when (and is-getter (not is-const)
			 (not is-static) (not is-surpressed))
		;; set match data for group 1 beginning at closing paranthesis
		;; of argument list
		(goto-char method-end-pos)
		(forward-list 1)
		(re-search-backward ")")
		(looking-at "\\()\\(?:[ \t]*;\\)?[ \t]*\\)") 
		(setq an-issue-found t)))))

	;; -- declarations & specifications must have space between name and
	;;    opening paranthesis. methods declared via macros are excluded
	(when (and (not an-issue-found)
                   (not (string-match "/Dispenser/" buffer-file-name)))
	  (save-excursion
	    (goto-char method-end-pos)
	    (setq an-issue-found
		  (and (not (looking-back "^[A-Z0-9_]+"))
		       (re-search-forward "\\=\\((\\|\\s-\\s-+(\\)" end t)))))

	;; next iter
	(goto-char method-end-pos)))
    an-issue-found))

(defun dragon-font-lock-add-keywords ()
  (font-lock-add-keywords
   nil
   (list
    ;; 
    (list 'dragon-looking-at-method-issues '(1 font-lock-warning-face append t)) 

    ;; white space errors
    (list "^[ ]*\t[ \t]*" '(0 font-lock-warning-face append t))
    (list "[ \t]+$" '(0 font-lock-warning-face append t))

    ;; empty comments 
    (list "/\\*+\\s-*\\*+/\\|//+\\s-*$" '(0 font-lock-unimportant t)) 
    
    ;; acess to well known objects/classes
    (list "\\b\\(\\(CPPSeqMeth\\|CPPSeqRTOSBase\\|CPPSeqIfc\\|CPPSeqMatMgmtDieLocDef\\)::\\(Inst()\\.\\)?\\)" '(1 font-lock-semi-unimportant t))
    (list "\\bCPPSeqIfc::\\w+()\\." '(0 font-lock-semi-unimportant t))
    (list "\\bCPPSeqIfc::Inst().Get\\w+Wrp()\\." '(0 font-lock-semi-unimportant t))
    (list "m_pMenuAccess->Get\\w*()\\." '(0 font-lock-semi-unimportant t))
    
    (list "^\\s-*EHRESULT\\s-+ehr\\s-*[;=]" '(0 font-lock-semi-unimportant t))
    (list "^\\s-*ehr\\s-*\\+=" '(0 font-lock-semi-unimportant t))
    ;; dont gray out CPPUNIT_ASSERT, thus EASSERT instead ASSERT
    (list "^\\s-*[a-zA-Z0-9_]*\\(STOP\\|RETURN\\|EASSERT\\|THROW\\)[a-zA-Z0-9_]*\\s-*(.*" '(0 font-lock-semi-unimportant t))
    (list "\\(?:^\\|}\\)\\s-*\\([a-zA-Z0-9_]*CATCH[a-zA-Z0-9_]*\\s-*(.*\\)" '(1 font-lock-semi-unimportant t))
    (list "^\\s-*\\(return\\s-*ehr\\s-*;\\)\\s-*\n}" '(1 font-lock-semi-unimportant t))
    (list "^\\s-*E\\(END\\|BEGIN\\)_COM_METHOD.*" '(0 font-lock-semi-unimportant t))

    (list "\\be[PBDE]VIGraphicalObject" '(0 font-lock-semi-unimportant t))
    (list "\\bscIID_\\(sSI\\|sMS\\)[a-zA-Z0-9]*_\\([a-zA-Z0-9]+_\\)?" '(0 font-lock-semi-unimportant t))
    
    (list "^\\s-*ETRACE[a-zA-Z0-9_]+.*" '(0 font-lock-semi-unimportant t))
    (list "^\\s-*UNREFERENCED_PARAMETER.*" '(0 font-lock-semi-unimportant t))
    
    (list "\\w+_cast\\s-*<[^>\n]*>" '(0 font-lock-semi-unimportant t))
    
    ;; googletest / googlemock
    (list "^\\s-*TEST\\(?:_F\\)?\\s-*(\\s-*\\(DISABLED_\\)?[^,]*,\\s-*\\(DISABLED_\\)?"
          '(1 font-lock-warning-face append t)
          '(2 font-lock-warning-face append t))
    (list "MAKE_\\(DISABLED_\\)?TEST_NAME" '(1 font-lock-warning-face append t))

    ;; --- real garbage ---
    (list "^\\s-*//\\.+\\s-*\\(begin\\|end\\)\\b.*\n" '(0 font-lock-unimportant t))
    (list "^\\s-*\\(#define\\s-*\\)?_\\(START\\|STOP\\)_SKIP.*" '(0 font-lock-unimportant t))
    ;; eol DCID info
    (list "//\\(\\s-*DC\\w+\\s-*=\\)?\\s-*0[xX][0-9a-fA-F]\\{8\\};?\\s-*\n" '(0 font-lock-unimportant t)) 
    ;; "} // end if"  bullshit
    (list "/[/*]+\\s-*\\(end\\s-*\\)?\\(if\\|else\\|for\\|while\\|do\\|try\\)\\s-*\\(\n\\|\\*/\\)" '(0 font-lock-unimportant t)) 
    ;; 
    ;; (list 'dragon-font-lock-method '(2 font-lock-warning-face t))
    )
   t))

(add-to-list 'auto-mode-alist '("PPSeqDoxygen\\.h" . doxym-mode))

(add-to-list 'auto-mode-alist '("cppcheck[^.]*" . compilation-mode))

;;; functions

(defun dragon-build-name (&optional file-name)
  (setq file-name (or file-name (buffer-file-name)))
  (string-match "/drives/builds/[^/]*?/[^/]*?/\\([^/]*?\\)/" file-name)
  (match-string 1 file-name))

(defun trans ()
  "Trans the current rtos project to localsam."
  (interactive)
  (let (saved-shell-file-name
        (proj-dir (project-root-dir (buffer-file-name)))
        (proj-file (project-file (buffer-file-name))))
    (save-some-buffers t)
    (compile (concat "trans32 localsam " proj-dir proj-file))
    ;;(setq saved-shell-file-name shell-file-name)
    ;;(setq shell-file-name "C:/winnt/system32/cmd.exe")
    ;;(compile (concat "/C cd .. && trans32 localsam " proj-file))
    ;;(setq shell-file-name saved-shell-file-name)))
    ))

(defun doxygen-run()
  "Generates doxygen documentation for the current's buffer project, pc or rtos."
  (interactive)
  
  ;; save all buffers without query
  (save-some-buffers t)

  ;; call doxygen with the correct config file
  (cond ((eq (project-root-type) 'project-diebonder-pc)
         (compile "C:/Progra~1/doxygen/bin/doxygen.exe C:\\Progra~1\\doxygen\\DoxygenConfig\\DoxyDragonPCProject_Flo.ini"))
        ((eq (project-root-type) 'project-diebonder-rtos)
         (compile "C:/Progra~1/doxygen/bin/doxygen.exe C:\\Progra~1\\doxygen\\DoxygenConfig\\DoxyDragonRTOSProject.ini"))
        (t (error "Not a pc or rtos project"))))

(defun dragon-checkall(&optional path)
  "Run the dragoncheckall tool"
  (interactive (list (let ((default (file-name-directory (directory-file-name (project-root-dir)))))
                       (read-string
                        (concat "Working directory (Default: " default "): ")
                        nil nil default))))
  (compile (combine-and-quote-strings (list "dragoncheckall" path))))

(defun st2grep(arg)
  "Runs the content of the current buffer through st2grep and
  then put the buffer into grep mode. With arg, first a new
  buffer is created and the content of the clipboard is
  inserted."
  
  (interactive "P")
  (when arg
    (set-buffer (get-buffer-create "*dragon dc*"))
    (setq buffer-read-only nil)
    (clipboard-yank)
    (pop-to-buffer "*dragon dc*"))
  (shell-command-on-region (point-min) (point-max) "st2grep" t t)
  (cd "W:/DieBonder/PC/PickPlace/ppseqbaselib")
  (grep-mode))

(defun qn(arg)
  "Runs the content of the current buffer through qn and then put
  the buffer into grep mode. With arg, first a new buffer is
  created and the content of the clipboard is inserted."
  
  (interactive "P")
  (when arg
    (set-buffer (get-buffer-create "*quality notices*"))
    (setq buffer-read-only nil)
    (clipboard-yank)
    (pop-to-buffer "*quality notices*"))
  (shell-command-on-region (point-min) (point-max) "qn" t t)
  (cd "W:/diebonder/")
  (grep-mode))

(defun dragon-common-mode-hook ()
  (message "dragon-common-mode-hook")
  (when (or (eq (project-root-type) 'project-diebonder-pc)
            (eq (project-root-type) 'project-diebonder-rtos))
    (local-set-key [remap compile] 'compile-ext)))

(add-hook 'common-mode-hook 'dragon-common-mode-hook t)

(defun dragon-coding-system-p ()
  "t if the buffer's coding system is allowed in Dragon.

Unfortunatly the dragon projecty does not really define its valid
coding systems. So I have to assume things, and I am not quite
sure what the good decisions are."
  ;; Note (sumary from list-coding-systems):
  ;; - iso-8859-1 and latin-1 are aliases to iso-latin-1
  ;; - Difference windows-1252 to iso-8859-1 (from wikipedia on Windows-1252):
  ;;   ... This encoding is a superset of ISO 8859-1, but differs from the IANA's
  ;;   ISO-8859-1 by using displayable characters rather than control characters
  ;;   in the 80 to 9F (hex) range...
  (member buffer-file-coding-system
          '(iso-latin-1-dos windows-1252-dos undecided-dos utf-8-dos)))

;; note that this should somehow belong to the
(defun dragon-file-p ()
  "Returns non-nil if the file visited by the current buffer belongs to the dragon project."
  (not (string-match "\\(^\\|/\\)\\.\\(git\\|svn\\)" (or buffer-file-name (buffer-name)))))

(defun dragon-create-tags-table()
  (interactive)
  (shell-command
   (concat
    "find "
    (if (eq (project-root-type) 'project-diebonder-pc)
        (concat 
         default-directory "../Sources "
         default-directory "../UnitTest "
         default-directory "../UnitTest2 "
         "~/src/DieBonder/MatpackLib/Include "
         "~/src/DieBonder/PC/INC "
         "~/src/Common/INC "
         "~/drives/xpc/Program\\ Files/ACE_Wrappers/Inc "
         "~/src/DieBonder/RTOSExport "
         "~/drives/xpc/Program\\ Files/Microsoft\\ Visual\\ Studio\\ 10.0/VC/include/ ")
      (concat
       default-directory "../Sources "
       default-directory "../UnitTest "
       default-directory "../UnitTest2 "
       "~/src/DieBonder/rtos/Inc "
       "~/src/DieBonder/RTOSExport "
       "~/src/DieBonder/RTOS/_IndelBase/os/inos/inc "
       "~/src/DieBonder/RTOS/_IndelBase/os/inco/inc " ))
    "-iregex '.*\\.\\(cpp\\|cc\\|c\\|h\\|tlh\\)$' -print0 | xargs -0 etags -f ../TAGS")))

(defun dragon-grep-find-command(&optional regexp)
  (concat
   "find \\\n"
   "$( find .. -maxdepth 1 -iregex '.*/\\(Sources\\|UnitTest2?\\)$' ) -maxdepth 1 \\\n"
   "-regextype posix-egrep \\\n"
   "-type f \\\n"
   "-iregex '.*\\.(idl|h|cpp)'  \\\n"
   "-print0 | xargs -0 grep --color=always -nIP \\\n"
   "-ie '" regexp "'"))

(defvar dragon-well-known-types
  (concat
   "\\(?:"
   "\\(?:const\\s-*\\)?"
   "\\(?:"
   (regexp-opt
    '("int" "double" "char" "uint32" "real64" 
      "HRESULT" "EHRESULT"
      "tPST_ExtInteger" "tPST_ExtReal" "tPST_ExtStruct" "tPST_INDELInteger" "tPST_INDELReal" "tPST_INDELStruct"
      "tPstContainerInteger" "tPstContainerIterator" "tPstContainerReal" "tPstContainerStruct" "tPstContainerText" "tPstDynEnumTransient" "tPstInteger" "tPstReal" "tPstStruct" "tPstText"))
   "\\)"
   "\\(?:\\s-*const\\)?\\(?:\\s-*[&*]\\(?:\\s-*const\\)?\\)*"
   "\\)"))

;; static const tDiagCondId DC_PPSeq_PickFailed                                             = 0x0F0A0021; //PC / RTOS: Pick failed
(defun dragon-grep-find-regexp()
  (or
   (when
       (save-excursion
	 (beginning-of-line)
	 (or (looking-at "\\s-*\\(?:class\\|struct\\)\\s-+\\(.*?\\)\\_>")
	     (looking-at "\\s-*static\\s-+const\\s-+\\(?:tDiagCondId\\|tItemId\\)\\s-+\\(.*?\\)\\_>")
	     (looking-at "\\s-*SetBaseItemID\\s-*([^,\n]*,\\s-*\\(.*?\\)\\_>\\s-*)")
	     (looking-at ".*PRS_INIT\\s-*([^,\n]*,\\s-*\\(.*?\\)\\_>")
	     (looking-at ".*ACTUALS_INIT\\s-*(\\s-*\\(.*?\\)\\_>")
	     (looking-at "\\s-*REGISTER_KEY_COMMAND\\s-*([^,\n]*,[^,\n]*,\\s-*&\\w+::\\(.*?\\)\\_>")
	     (looking-at "\\s-*DECLARE_\\(?:READ\\|WRITE\\)_METHOD\\s-*([^,\n]*,\\s-*\\(.*?\\)\\_>")
	     (and (looking-at "\\s-*\\(?:virtual\\s-+\\)?\\(?:EHRESULT\\|int32\\|void\\|double\\)\\(?:\\s-+\\|\\s-*&\\s-*\\)\\(\\(?:\\w\\|_\\)+\\)\\(.\\)")
		  (not (string= ":" (match-string 2)))
		  (not (string= "ehr" (match-string 1))))
	     (looking-at (concat dragon-well-known-types "\\s-*\\(?:\\w\\|_\\)+::\\(\\(?:\\w\\|_\\)+\\)"))
	     (and (looking-at (concat "\\s-*" dragon-well-known-types "\\s-*\\(\\(?:\\w\\|_\\)+\\)"))
		  (not (string= "ehr" (match-string 1))))))
     (concat "\\b" (match-string-no-properties 1) "\\b"))
   (when (and (save-excursion (beginning-of-line) (looking-at "\\s-*ehr\\s-*\\+?=\\s-\\(\\(?:\\w\\|_\\)+\\)"))
	      (<= (point) (match-end 1)))
     (concat "\\b" (match-string-no-properties 1) "\\b"))))

(defun dragon-dwim ()
  (interactive)
  (cond
   ((save-excursion (beginning-of-line) (looking-at "\\s-*return\\s-+\\(S_OK\\|ehr\\)\\s-*;"))
    (save-excursion
      (let ((replacement (if (string= (match-string 1) "S_OK") "ehr" "S_OK"))) 
	(goto-char (match-beginning 1))
	(delete-region (point) (match-end 1))
	(insert replacement))))
   ((save-excursion (beginning-of-line) (looking-at "\\s-*\\(ERETURN_IF_FAILED\\|ETHROW_IF_FAILED\\)\\b"))
    (save-excursion
      (let ((replacement (if (string= (match-string 1) "ERETURN_IF_FAILED") "ETHROW_IF_FAILED" "ERETURN_IF_FAILED"))) 
	(goto-char (match-beginning 1))
	(delete-region (point) (match-end 1))
	(insert replacement))))))

;; types
;; - interface
;; - implementation
;; - test case
;; - test helpers
;; - associated data/bussiness-logic class
;; 
;; ortoghonal to type
;; - header file
;; - source file
(defun dragon-find-other-file()
  (interactive)
  (let ((case-fold-search nil))
    (cond
     ((string-match "\\(.*?\\)/Sources/\\(.*?\\)\\.\\(cpp\\|h\\)$" buffer-file-name)
      (let* ((fn (replace-match "\\1/UnitTest2/\\2Test.\\3" t nil buffer-file-name)))
        (if (file-exists-p fn)
          (find-file-existing fn)
          (message "File %S does not exist" fn))))
      ((string-match "\\(.*?\\)/UnitTest2/\\(.*?\\)Test\\.\\(cpp\\|h\\)$" buffer-file-name)
       (let* ((fn (replace-match "\\1/Sources/\\2.\\3" t nil buffer-file-name)))
         (if (file-exists-p fn)
             (find-file-existing fn)
           (message "File %S does not exist" fn))))
      (t (error "No 'other' file for the current file")))))

(defun dragon-replace-unreferenced-parameter-macro()
  "Replaces UNREFERENCED_PARAMETER by commenting arg name.

Replaces the UNREFERENCED_PARAMETER macro on the current line by
deleting this line and commenting out the associated argumen name
in the methods signature. For example:

void foo(int i) {
  UNREFERENCED_PARAMETER(i);
  ...
}

becomes:

void foo(int /*i*/) {
  ...
}"
  (interactive)
  (let ((argument "") argument-pos)

    (beginning-of-line)
    (re-search-forward "UNREFERENCED_PARAMETER\\s-*(\\s-*")
    (setq argument-pos (point))
    (forward-sexp)
    (setq argument (buffer-substring-no-properties argument-pos (point)))

    (c-beginning-of-defun-body)
    (backward-up-list)
    (let ((case-fold-search)
          (method-end (save-excursion (forward-sexp) (point))))
      (while (re-search-forward (concat "\\_<" (regexp-quote argument) "\\_>") method-end t)
        (when (not (equal (match-beginning 0) argument-pos))
          (error "%s _is_ referenced in this method" argument))))

    (c-forward-defun-name)
    (forward-sexp)
    (let ((case-fold-search))
      (when (not (re-search-forward (concat "\\_<" (regexp-quote argument) "\\_>")
                                    (save-excursion (forward-sexp) (point))
                                    t))
        (goto-char argument-pos)
        (error "%s is not an argument to this method" argument)))
    (unless (save-match-data (looking-at "\\s-*\\*+/"))
        (insert "*/"))
    (goto-char (match-beginning 0))
    (unless (looking-back "/\\*+\\s-*")
      (insert "/*"))
        
    (goto-char argument-pos)
    (delete-region (line-beginning-position)
                   (progn (forward-line) (point)))))


;;; abbrevs
;; (setq dragon-abbrev-table (make-abbrev-table)) ;props are added in mode hook
(setq dragon-abbrev-table '(
  ;; dragon types / often used 'general' types
  ("vec" "EVecXy")
  ("r" "real64")
  ("ui" "uint32")
  ("i" "int32")
  ("E" "EHRESULT")		
  ("up" "std::unique_ptr<>")		

  ;; sequencer types
  ("cdbm" "CPPSeqDBMenu")
  ("dbm" "DBMenu")
  ("cmh" "CPPSeqMenuHandler")
  ("mh" "MenuHandler")
  ("cdh" "CPPSeqDataHandler")
  ("dh" "DataHandler")
  ("cdhb" "CPPSeqDataHandlerBase")
  ("dhb" "DataHandlerBase")
  ("cf" "CPPSeqFeature")
  ("f" "Feature")
  ("cfc" "CPPSeqFeatureCoordinator")
  ("fc" "FeatureCoordinator")
  ("cmc" "CPPSeqMenuCoordinator")
  ("mc" "MenuCoordinator")
  ("ce" "CPPSeqEnabler")
  ("e" "Enabler")
  ("ceo" "CPPSeqEnablerObserver")
  ("eie" "EPPSeqIsEnabled")
  ("ie" "IsEnabled")
  ("cls" "CPPSeqLogicState")
  ("ls" "LogicState")
  ("csl" "CPPSeqStateHdlList")
  ("sl" "StateHdlList")
  ("cog" "CPPSeqOptGroup")
  ("og" "PPSeqOptGroup")
  ("cfs" "CPPSeqFuncSel")
  ("fs" "FuncSel")
  ("cs" "CPPSeqSelector")
  ("s" "Selector")
  ("cso" "CPPSeqSelectorObserver")
  ("so" "SelectorObserver")
  ("cm" "CPPSeqMeth")
  ("cgd" "CPPSeqGlobalData")
  ("ifc" "CPPSeqIfc")
  ("dr" "GetDataReference()")
  ("gdr" "GetDataReference()")

  ;; features
  ("cfpu" "CPPSeqFeaturePickUpToolOffset" )
  ("fpu" "FeaturePickUpToolOffset" )
  ("cfp" "CPPSeqFeaturePickerToolOffset")
  ("fp" "FeaturePickerToolOffset")
  ("cft" "CPPSeqFeatureTableToolOffset")
  ("ft" "FeatureTableToolOffset")

  ;; ifc access
  ("wwps" "CPPSeqIfc::GetDCWaferProcSeqCOMWrp()")
  ("ww" "CPPSeqIfc::GetDCWaferProcSeqCOMWrp()")
  ("wdc" "CPPSeqIfc::GetDCWaferProcSeqCOMWrp()")
  ("wfm" "CPPSeqIfc::GetPPForceModCOMWrp()")
  ("wcm" "CPPSeqIfc::GetPPCalibModCOMWrp()")
  ("wpvi" "CPPSeqIfc::GetPVISeqCOMWrp()")
  ("wbvi" "CPPSeqIfc::GetBVISeqCOMWrp()")
  ("igd" "CPPSeqIfc::GlobalData()")

  ("de" "EHRESULT ehr")		;define ehr
  ("rok" "return S_OK")
  ("retok" "return S_OK")
  ("re" "return ehr")
  ("rete" "return ehr")
  ("er" "ERETURN_IF_FAILED(ehr)")
  ("eret" "ERETURN_IF_FAILED(ehr)")
  ("et" "ETHROW_IF_FAILED(ehr)")
  ("sok" "S_OK")
  ("isok" "S_OK == ehr")

  ;; C++ constructs
  ;; currently disabled until I found tempo bug
  ;; ("if" "" tempo-template-c-if 'only-in-code)
  ;; ("elif" "" tempo-template-c-else-if 'only-in-code)
  ;; ("elseif" "" tempo-template-c-else-if 'only-in-code)
  ;; ("ei" "" tempo-template-c-else-if 'only-in-code)
  ;; ("else" "" tempo-template-c-else 'only-in-code)
  ;; ("for" "" tempo-template-doremi-c-for 'only-in-code)
  ;; ("case" " " tempo-template-c-case 'only-in-code)
  ;; ("default" " " tempo-template-c-default 'only-in-code)
  ;; ("do" " " tempo-template-c-do 'only-in-code)
  ;; ("inc" " " tempo-template-c-include 'only-in-code)
  ;; ("include" " " tempo-template-c-include 'only-in-code)
  ;; ("switch" " " tempo-template-c-switch 'only-in-code)
  ;; ("while" " " tempo-template-c-while 'only-in-code)
  ))

(defun dragon-create-abbrev-enable-function (arg)
  "Returns a function which suitable for abbrev's :enable-function.
Arg is the 3rd items of a dragon-abbrev-table item"
  `(lambda () (dragon-abbrev-enable-function ,arg)))

(defun dragon-abbrev-enable-function (arg) 
  (and
   ;; Inserting characters with symbol syntax shall not trigger checking for
   ;; possible abbrev-expansion.
   ;;
   ;; Actually emacs-23.1/src/cmd.c internal_self_insert should be patched so
   ;; abbrev is only tried after a non-word, non-symbol character has been
   ;; inserted. Currently its only non-word
   (or (not (eq this-command 'self-insert-command))
       (not (memq (char-syntax last-command-event) '(?w ?_))))

   ;; in-code = not in (comment or string)
   (let ((in-code (not (nth 8 (syntax-ppss)))))
     (cond
      ;; -- only expand in code syntax
      ((eq arg 'only-in-code)
       ;; also only if on empty line
       (and in-code (looking-at "[ \t]*$")))


      ;; -- expand (potentially) always
      (t
       ;; within comment/strings, prevent expansion if 'abbrev' is preceded by '
       ;; or . or \
       (if in-code t
	 (save-excursion
	   (backward-word)
	   (looking-back "[^.'\\]"))))))))

;;; file aliases / cache
(setq filealias-default-root-dir "W:")

(setq filealias-alist '(
  ("p"  . "~~DieBonder/PC/")
  ("r"   . "~~DieBonder/RTOS/")
  ("re"  . "~~DieBonder/rtosexprt/")
  
  ("ppp"  . "~~DieBonder/PC/PickPlace/")
  ("pfc"  . "~~DieBonder/PC/flipchip/")
  ("psh"  . "~~DieBonder/PC/substrathandler/")
  ("pdc"  . "~~DieBonder/PC/diecarrier/")
  ("pvi"  . "~~DieBonder/PC/vision/")
  ("psrv" . "~~DieBonder/PC/services/")
  ("rpp"  . "~~DieBonder/RTOS/PickPlace/")
  ("rsh"  . "~~DieBonder/RTOS/substrathandler/")
  ("rdc"  . "~~DieBonder/RTOS/diecarrier/")
  ("rvi"  . "~~DieBonder/RTOS/vision/")
  ("rge"  . "~~DieBonder/RTOS/generics/")
  
  ("pmcac" . "~~DieBonder/PC/Controllers/MCAssistController")
  ("pac"   . "~~DieBonder/PC/Controllers/MCAssistController")

  ("pts"  . "~~DieBonder/PC/Services/TeachService")
  ("ptst"  . "~~DieBonder/PC/Services/TeachServiceType")
  ("pcs"  . "~~DieBonder/PC/Services/CalibrationServices/CalibrationService")
  ("pcst"  . "~~DieBonder/PC/Services/CalibrationServices/CalibrationServiceType")
  
  ("pde" . "~~DieBonder/PC/PickPlace/dcdemod/")
  ("pfo"  . "~~DieBonder/PC/PickPlace/ppforcemod/")
  ("pba" . "~~DieBonder/PC/PickPlace/ppbamod/")
  ("pbh" . "~~DieBonder/PC/PickPlace/ppbhmod/")
  ("pfa" . "~~DieBonder/PC/flipchip/ppfamod/")
  ("pfx" . "~~DieBonder/PC/flipchip/ppfxmod/")
  ("pca" . "~~DieBonder/PC/PickPlace/PPCalibMod/")
  ("pse" . "~~DieBonder/PC/PickPlace/PPSeqBaseLib/")
  
  ("rca" . "~~DieBonder/RTOS/PickPlace/ppcalibmod/sources/")
  ("rde" . "~~DieBonder/RTOS/PickPlace/dcdemod/sources/")
  ("rba" . "~~DieBonder/RTOS/PickPlace/ppbamod/sources/")
  ("rde" . "~~DieBonder/RTOS/PickPlace/dcdemod/sources/")
  ("rbh" . "~~DieBonder/RTOS/PickPlace/ppbhmod/sources/")
  ("rfo" . "~~DieBonder/RTOS/PickPlace/ppforcemod/sources/")
  ("rse" . "~~DieBonder/RTOS/PickPlace/ppsequencer/sources/")
  ("rco". "~~DieBonder/RTOS/PickPlace/ppcommon/sources/")
  ("rpr". "~~DieBonder/RTOS/PickPlace/PPModProxy/sources/")
  ("rtd". "~~DieBonder/RTOS/PickPlace/PPTeachDataMgr/sources/")))

(mapc 'file-cache-add-file '(
  "~/office/dragon"
  "~/src/DieBonder"
  "~/src/DieBonder/RTOS"
  "~/src/DieBonder/PC"
  "~/src/DieBonder/RTOS/PickPlace"
  "~/src/DieBonder/PC/PickPlace"
  "~/src/DieBonder/PC/PickPlace/PPSeqBaseLib"
  "~/src/DieBonder/PC/PickPlace/PPCalibMod"
  "~/src/DieBonder/RTOS/PickPlace/PPCalibMod"
  "~/src/DieBonder/PC/PickPlace/PPPPickerMod"
  "~/src/DieBonder/RTOS/PickPlace/PPPPickerMod"
  "~/src/DieBonder/PC/PickPlace/PPPPickerShuttleMod"
  "~/src/DieBonder/RTOS/PickPlace/PPPPickerShuttleMod"
  "~/src/DieBonder/RTOS/DieCarrier/DCWaferHdlMod"
  "~/src/DieBonder/RTOS/Dispenser"
  "~/src/DieBonder/PC/Dispenser"
  "~/src/DieBonder/PC/Dispenser/DIDispenserSeq"
  "~/src/DieBonder/PC/Dispenser/DIDispenserMod"
  "~/src/DieBonder/pc/SubstrateHandler/SHSTHModule"
  "~/drives/xpc/Program Files/Esec/DieBonder/Data/BuildVersion.txt"
  "~/office/dragon/todo.txt" ))

(setq ffe-dir-map-map '(
  ;; ROOT
  ("W:/DieBonder"
   (("PC" ("p"))
    ("RTOS" ("r"))
    ("RTOSExport" ("re"))))
			
  ;; Level MPS
  ("W:/DieBonder/PC"
   (("PickPlace" ("pp" "p"))
    ("DieCarrier" ("dc" "d"))
    ("DataMgmt" ("dm"))
    ("SubstrateHandler" ("sh" "s"))))
  
  ("W:/DieBonder/RTOS"
   (("PickPlace" ("pp" "p"))
    ("DieCarrier" ("dc" "d"))
    ("DataMgmt" ("dm"))
    ("SubstrateHandler" ("sh" "s"))))
  
  ;; Level Components
  ("W:/DieBonder/PC/PickPlace"
   (("PPBAMod" ("ba" "bam"))
    ("PPBHMod" ("bh" "bhm"))
    ("DCDEMod" ("de" "dem"))
    ("PPCalibMod" ("c" "cm"))
    ("PPSeqBaseLib" ("s" "sbl"))
    ("PPSequencer" ("seq"))))

  ("W:/DieBonder/RTOS/PickPlace"
   (("PPBAMod/Sources" ("ba" "bam"))
    ("PPBHMod/Sources" ("bh" "bhm"))
    ("DCDEMod/Sources" ("de" "dem"))
    ("PPCalibMod/Sources" ("c" "cm"))
    ("PPSequencer/Sources" ("s"))))))

(let ((mylist '(
		("/UST/PersistentContainer/PstCnt" ust-ffe-pstcnt)
		("/DieBonder/PC/Services/CalibrationServices/CalibrationService" dragon-ffe-pc-calibsrv)
		("/DieBonder/PC/Services/GlobalServices" dragon-ffe-pc-globalservices)
		("/DieBonder/PC/SubstrateHandler/SHSTHModule" dragon-ffe-pc-sh-sth-mod)
		("/DieBonder/PC/SubstrateHandler/SHSTHSequencer" dragon-ffe-pc-sh-sth-seq)
		("/DieBonder/PC/Dispenser/DIDispenserMod" dragon-ffe-pc-di-mod)
		("/DieBonder/PC/Dispenser/DIDispenserSeq" dragon-ffe-pc-di-seq)
		("/DieBonder/PC/PickPlace/PpcalibMod" dragon-ffe-pc-calib)
		("/DieBonder/PC/PickPlace/DCDEMod" dragon-ffe-pc-de)
		("/DieBonder/PC/PickPlace/PPBAMod" dragon-ffe-pc-ba)
		("/DieBonder/PC/PickPlace/PPBHMod" dragon-ffe-pc-bh)
		("/DieBonder/PC/PickPlace/PPForceMod" dragon-ffe-pc-force)
		("/DieBonder/PC/PickPlace/PPSeqBaseLib" dragon-ffe-pc-seq)
		("/DieBonder/RTOS/DieCarrier/DCWaferHdlMod" dragon-ffe-rtos-dcwaferhdlmod)
		("/DieBonder/RTOS/PickPlace/PPCalibMod" dragon-ffe-rtos-calib)
		("/DieBonder/RTOS/PickPlace/PPSequencer/Sources/SeqV2" dragon-ffe-rtos-seqv2)
		("/DieBonder/RTOS/PickPlace/PPSequencer" dragon-ffe-rtos-seq)
		("/DieBonder/RTOS/PickPlace/DCDEMod" dragon-ffe-rtos-de)
		("/DieBonder/RTOS/PickPlace/PPBAMod" dragon-ffe-rtos-ba)
		("/DieBonder/RTOS/PickPlace/PPForceMod" dragon-ffe-rtos-force)
		("/DieBonder/RTOS/PickPlace/PPModProxy" dragon-ffe-rtos-proxy))))
  (dolist (elt mylist) 
    (add-to-list 'ffe-map-map elt)))

(setq ust-ffe-pstcnt `(
  ("PstContainerElementBase" ("pceb" "eb"))))

(setq dragon-ffe-pc-globalservices `(
  ("PstContainerElementBase2" ("ceb"))
  ("IExtendedIndelItemType" ("ieiit"))
  ("ExtendedIndelItemType2" ("eiit"))
  ("IExtendedItemType" ("ieit"))
  ("ExtendedItemType2" ("eit"))
  ("IItemFactory" ("iif") "h")
  ("ExtendedItemTypeFactory" ("eitf"))))

(setq dragon-ffe-pc-sh-sth-mod `(
  ("SHSTHP1ModDebugActiveZ" ("p1mdaz" "1az"))
  ("SHSTHP2ModDebugActiveZ" ("p2mdaz" "2az"))
  ("SHSTHP1ModRunIn" ("p1mri" "1mri" "1mr"))
  ("SHSTHP2ModRunIn" ("p2mri" "2mri" "2mr"))
  ("SHSTHP1ModCommon" ("p1mc" "1mc"))
  ("SHSTHP2ModCommon" ("p2mc" "2mc"))
  ("SHSTHIModRunIn" ("mri" "ri"))
  ("SHSTHMod" ("mod" "m"))
  ("SHSTHModAO" ("ao"))
  ("SHSTHItemIDs" ("ii") "h")
  ("SHSTHRTOSStrings" ("rs") "h")
  ("stdafx" ("afx") "h")))

(setq dragon-ffe-pc-sh-sth-seq `(
  ("SHSTHSequencer" ("idl") "idl")
  ("SHSTHSeq" ("seq"))
  ("SHSTHAO" ("ao"))
  ("CSHSTHSeqRTOSCommon" ("rc" "rb"));rb=rtosbase
  ("SHSTHInterfaceContainer" ("ifc"))
  ("SHForeignItemIDs" ("fii") "h")
  ("SHSTHItemIDs" ("ii") "h")
  ("SHGeneralRTOSStrings" ("grs" "rs") "h")
  ("stdafx" ("afx") "h")

  ("SHSTHCapabilityBase" ("cb"))
  ("SHSTHCapabilityP1ActiveZ" ("cp1az" "caz1"))
  ("SHSTHCapabilityActiveZ" ("caz" "cp2az" "caz2"))

  ("SHSTHTeachClientF_3_10_ProductionSettings" ("f310" "ps"))
  ("SHSTHTeachServiceF_3_23_MountDispenseProcessInsert" ("f323" "mdpi"))
  ("SHSTHTeachClientF_3_1_FunctionSelection" ("f31" "fs"));fs=function selection. It is the most important func sel in sh
  ("SHSTHTeachClientF_3_9_VerifyStripTeaching" ("f39" "vst"))))

(setq dragon-ffe-pc-di-seq `(
  ("stdafx" ("afx") "h")
  ("ItemIDsSeq" ("ii") "h")
  ("DIDispenserSeqRTOSString" ("rs") "h")
  ("DIDispenserSeq" ("seq"))
  ("DIDispenserSeqIFContainer" ("ifc"))
  ("DIDispenserSequencer" ("idl") "idl")
  ("DIDispenserSeqAO" ("ao"))

  ("DIDispenserSeqCommon" ("c"))
  ("DICommandHandler" ("ch"))
  ("DITimeHelper" ("th"))
  ("DIOptGroup" ("og"))
  ("DIMountableTool" ("mt"))

  ("DIPatternDecisionParams" ("pdp"))
  ("DIDispenseProlog" ("dpl"))
  ("DIDispensePrologSingleDot" ("dplsd"))
  ("DIDispenseEpilog" ("del"))
  ("DIDispensePulseWrite" ("dpw"))
  ("DIDispensePulseSingleDot" ("dpsd"))
  ("DIDispensePulse" ("dp"))
  ("DIPatternWriteWindow" ("pww"))
  ("DIPatternWrite" ("pw"))
  ("DIPatternSingleDot" ("psd"))
  ("DIPattern" ("p"))
  ("IDIPattern" ("ip") "h")
  ("DIPatternControl" ("pc"))
  ("DIPatternScaleCorrection" ("psc"))
  ("DINode" ("n"))
  ("DINodeList" ("nl"))))

(setq dragon-ffe-pc-di-mod `(
  ("stdafx" ("afx") "h")))

(setq dragon-ffe-pc-calib `(
  ("PPCalibMod" ("mod"))
  ("PPCalibModAO" ("ao"))
  ("PPCalibModInterfaceContainer" ("ifc" "ic"))
  ("PPCalibMethods" ("m"))
  ("PPCalibToolSetupData" ("tsd" "ts"))
  ("PPCalibModRtosBase" ("rb" "rtos"))
  ("PPCalibModRTOSString" ("rs") "h")
  ("PPCalibModItems" ("i" "ii") "h")
  ("PPCalibModDiag" ("di") "h")
  ("PPCalibModule" ("idl") "idl")
  ("PPCalibDoxygen" ("doxy") "h")
  ("stdafx" ("afx" "std") "h")
  
  ("PPCalibJob" ("j"))
  ("PPCalibJobList" ("jl"))
  ("PPCalibJobListManager" ("jlm"))
  ("PPCalibData" ("d"))
  
  ("PPCalibJobListProcModDataBase" ("lpmb") "h")
  ("PPCalibJobListProcModData" ("lpm") "h")
  ("PPCalibJobListProcModDataImpl" ("lpmi") "h")
  ("PPCalibJobListBAAxesAtBond" ("lbabo"))
  ("PPCalibJobListBAAxesAtPick" ("lbap"))
  ("PPCalibJobListBondheadSensors" ("lbhs"))
  ("PPCalibJobListCalibDie" ("lcd"))
  ("PPCalibJobListDieEjector" ("lde"))
  ("PPCalibJobListPPAxesEncoder" ("le"))
  ("PPCalibJobListRotAxisPos" ("lrap" "lra" "lr"))
  ("PPCalibJobListWaferTable" ("lwt" "lw"))
  
  ("PPCalibPCEDieEjector" ("pcede" "pde"))
  ("PPCalibPCECalibDie" ("pcecd" "pcd"))
  
  ("PPCalibDynCalib" ("dc"))
  ("PPCalibForceCalibUnit" ("fcu"))
  ("PPCalibBondForceUtils" ("fu" "frc"))
  
  ("PPCalibNotifyCBMgr" ("ncbm" "n" "nm" "cbm"))
  
  ("PPCalibJobCalibDiePicktoolTilt" ("cdpt" "cdptt" "ptt" "pt"))
  ("PPCalibJobCalibDiePicktoolZ" ("cdptz" "cdpz" "ptz" "pz"))
  ("PPCalibJobDEHeightBase" ("dehb" "dhb" "hb"))
  ("PPCalibJobDECalibHeight" ("dech" "dch"))
  ("PPCalibJobDieHandlingHeight" ("dhh" "dh" "cdhh"))
  ("PPCalibJobExpRingHeight" ("erh"))
  ("PPCalibJobBAAxesBase" ("bb" "bab" "baab"))
  ("PPCalibJobBAAxesBond" ("bbo" "babo" "baabo"))
  ("PPCalibJobBAAxesPick" ("bp" "bap" "baap"))
  ("PPCalibJobRotAxisBond" ("rab" "rabo"))
  ("PPCalibJobRotAxisPick2" ("rap" "rp"))
  ("PPCalibJobRotAxisExch" ("rae" "re"))
  ("PPCalibJobBHRotAxisExchAndBond" ("raeab" "reb"))
  ("PPCalibJobBondForce" ("bf" "f"))
  ("PPCalibJobBAYMoveRange" ("baymr" "bymr" "ymr"))
  ("PPCalibJobBAXMoveRange" ("baxmr" "bxmr" "xmr"))
  ("PPCalibJobThetaOrientation" ("to"))
  ("PPCalibJobSliderXPos" ("sxp" "sp"))
  ("PPCalibJobDEXYPos" ("dexyp" "dp" "xyp" "dexy" "dxy"))))

(setq dragon-ffe-pc-de `(
  ("DCDEModule" ("idl") "idl")
  ("DCDEMod" ("mod"))
  ("StdAfx" ("afx") "h")
  ("DCDEModAO" ("ao"))
  ("DCDEModRTOSBase" ("rb"))
  ("DCDEDiagCondition" ("dc") "h")
  ("DCDEModRTOSString" ("rs") "h")
  ("DCDEModItemIDs" ("ii") "h")
  ("DCDEModItemIDForeignComp" ("iifc" "iif") "h")
  ("DCDEModInterfaceContainer" ("ifc"))
  ("DCDERunInBase" ("rib"))))

(setq dragon-ffe-pc-ba `(
  ("PPBAModule" ("idl") "idl")
  ("PPBAMod" ("mod"))
  ("StdAfx" ("afx") "h")
  ("PPBAModAO" ("ao"))
  ("PPBAModRTOSBase" ("rb"))
  ("PPBADiagCondition" ("dc") "h")
  ("PPBAModRTOSString" ("rs") "h")
  ("PPBAModItemIDs" ("ii") "h")
  ("PPBAModItemIDForeignComp" ("iifc" "iif") "h")
  ("PPBADistanceSensor" ("ds"))
  ("PPBAModInterfaceContainer" ("ifc"))
  ("PPBARunInBase" ("rib"))
  ("PPBASenorsDebug" ("sd"))
  ("PPBAXNeDebug" ("xnd" "xd" "nd"))
  ("PPBAXNeRunIn" ("xnr" "xr" "nr"))))

(setq dragon-ffe-pc-bh `(
  ("PPBHModule" ("idl") "idl")
  ("PPBHMod" ("mod"))
  ("StdAfx" ("afx") "h")
  ("PPBHModAO" ("ao"))
  ("PPBHModRTOSBase" ("rb"))
  ("PPBHDiagCondition" ("dc") "h")
  ("PPBHModRTOSString" ("rs") "h")
  ("PPBHModItemIDs" ("ii") "h")
  ("PPBHModItemIDForeignComp" ("iifc" "iif") "h")
  ("PPBHModInterfaceContainer" ("ifc"))
  ("PPBHRunIn" ("ri"))
  ("PPBHValvesDebug" ("vd"))
  ("PPBHAxesDebug" ("ad"))))

(setq dragon-ffe-pc-force `(
  ("PPForceModule" ("idl") "idl")
  ("PPForceMod" ("mod"))
  ("StdAfx" ("afx") "h")
  ("PPForceModAO" ("ao"))
  ("PPForceModRTOSBase" ("rb"))
  ("PPForceModDiagConditions" ("dc") "h")
  ("PPForceModRTOSString" ("rs") "h")
  ("PPForceModItemIDs" ("ii") "h")
  ("PPForceModInterfaceContainer" ("ifc"))
  ("PPForceAxesDebug" ("ad" "d"))))

(setq dragon-ffe-pc-seq `(
  ("PPSeqAO" ("ao"))
  ("PPSeqATLBase" ("seq" "atlb" "atl"))
  ("StdAfx" ("afx") "h")
  ("PPSeqDoxygen" ("doxy" "dox") "h")
  ("PPSeqMeth" ("m"))
  ("PPSeqMethods" ("mo"))
  ("IPPSeqMethods" ("imo") "h")
  ("PPSeqHMIMethods" ("hmimo" "hmim"))
  ("IPPSeqHMIMethods" ("ihmimo" "ihmim") "h")
  ("PPSeqVIMethods" ("vimo" "vim"))
  ("IPPSeqVIMethods" ("ivimo" "ivim") "h")
  ("PPSeqStateHandlingMethods" ("shmo" "shm"))
  ("IPPSeqStateHandlingMethods" ("ishmo" "ishm") "h")
  ("PPSeqRTOSModData" ("rmd"))
  ("PPSeqRTOSBase" ("rb"))
  ("PPSeqGlobalData" ("gd"))
  ("PPSeqGlobalDataCalc" ("gdc"))
  ("PPSeqGlobalMenuDataAccess" ("gmda"))
  ("PPSeqItemIDs" ("ii" "i") "h")
  ("PPSeqForeignItemIDs" ("fii" "fi") "h")
  ("PPSeqDiagCondition" ("dc" "di") "h")
  ("PPSeqTypeDefinitions" ("td" "type") "h")
  ("PPSeqIfc" ("ic" "ifc") "h")
  ("IPPSeq" ("idl") "idl")
  ("PPSeqDataMgrPC" ("dmp"))
  ("PPSeqRTOSDataTransferCtrl" ("rdtc" "dtc"))
  ("PPSeqRTOSString" ("rs") "h")
  ("PPSeqMessages" ("msg") "h")
  ("PPSeqFuncSel" ("fs") )
  ("PPSeqFuncSelTeachSetup" ("fsts") )
  ("PPSeqOptGroup" ("og") )
  ("PPSeqStateHdlList" ("shl" "sl") )
  ("IPPSeqLogicState" ("ils") "h" )
  ("PPSeqLogicState" ("ls") )
  ("MCDynComboboxHdlList" ("dchl" "dcl") )
  ("PPSeqDisplayDepData" ("ddd") )
  ("PPSeqSingleton" ("s") "h")
  ("PPSeqAutoPtr" ("ap") "h")
  ("PPSeqFeature" ("f"))
  ("PPSeqFeatureCoordinator" ("fc"))
  ("PPSeqEnabler" ("er") "h")
  ("IPPSeqEnablerObserver" ("eo") "h")
  ("PPSeqEnablers" ("ers"))
  ("PPSeqSelector" ("sr") "h")
  ("IPPSeqSelectorObserver" ("so") "h")
  ("PPSeqSelectors" ("srs"))
  ("PPSeqCommandHandler" ("ch") "h")
  ("PPProcessSelection" ("ps"))
  ("PPSeqServiceProvider" ("sp"))
  ("PPSeqKeyHandler" ("kh"))
  ("PPSeqParamHandler" ("ph"))

  ;; wrappers
  ("PPSeqWPPBAMod" ("wba"))
  ("PPSeqWPPBHMod" ("wbh"))
  ("PPSeqWPPForceMod" ("wfm"))
  ("PPSeqWPPCalibMod" ("wcm" "wc"))
  ("PPSeqWDCDEMod" ("wdem" "wde"))
  ("PPSeqWPPPTrftMod" ("wtrft" "wtable") )
  ("PPSeqWPPPPickerMod" ("wpckh" "wpicker"))
  ("PPSeqWTeachSrv" ("wts"))
  ("PPSeqWPPPPickerMod" ("wpm" "wp"))
  ("PPSeqWVIBondVisionSeq" ("wbvis" "wbvi"))
  ("PPSeqWVIPickVisionSeq" ("wpvis" "wpvi"))
  ("PPSeqWDCWaferProcessingSeq" ("wwps" "wws" "ww" "wdc"))
  
  ;; teach base
  ("PPSeqPCProcessCalculations" ("pcpc"))
  ("PPSeqTeachDataProcessBase" ("tdpb" "dpb"))
  ("PPSeqTeachMenuProcessBase" ("tmpb" "mpb"))
  ("PPSeqMenuCoordinator" ("mc"))
  ("PPSeqMenuHandlerBase" ("mhb"))
  ("PPSeqMenuHandler" ("mh"))
  ("PPSeqDBMenu" ("dbm"))
  ("PPSeqLink" ("l"))
  ("PPSeqLinkCoordinator" ("lc"))
  ("PPSeqDataHandlerBase" ("dhb"))
  ("PPSeqDataHandler" ("dh"))
  ("PPSeqProcessData" ("pd") "h")
  ("PPBaseProcess" ("bp") "h")
  ("PPPickupProcess" ("pp"))
  ("PPPeelProcess" ("pep"))
  ("PPBondProcess" ("bop"))
  ("PPSeqTeachCoordinator" ("tc"))
  ("PPSeqTeachDataDSBase" ("tdsb" "ddsb" "dsb"))
  ("PPSeqForceData" ("fd"))
  
  ;; setup base
  ("IPPSeqToolSetupMenuHandler" ("itsmh") "h")
  ("PPSeqToolSetupMenuHandler" ("tsmh"))
  ("PPSeqToolSetupCoordinator" ("tsc"))
  ("PPSeqTool" ("t"))

  ;; config base
  ("PPSeqConfigDataBase" ("cdb"))
  ("PPSeqConfigParam" ("cp"))


  ;; setup specialized classes
  ;; --------------------------

  ;; setup / insert tools
  ("PPSeqToolSetupDataPI7_InsertPPTools" ("tsdi" "sdi")) 
  ("PPSeqToolSetupMenuPI7_InsertPPTools" ("tsmi" "smi")) 
  
  ;; setup / optimize and change tools
  ("PPSeqToolSetupDataPI4_NeedleAndPepperPot" ("tsdnapp" "sdnapp" "sdn")) 
  ("PPSeqToolSetupMenuPI4_NeedleAndPepperPot" ("tsmnapp" "smnapp" "smn")) 
  ("PPSeqToolSetupDataPI4_PickUpTool" ("tsdp" "sdp")) 
  ("PPSeqToolSetupMenuPI4_PickUpTool" ("tsmp" "smp")) 
  
  ;; setup / optimize process
  ("PPSeqBondProcessSetupDataPI_5_4" ("bopsd")) 
  ("PPSeqBondProcessSetupMenuPI_5_4" ("bopmd")) 
  ("PPSeqPickProcessSetupDataPI_5_3" ("ppsd")) 
  ("PPSeqPickProcessSetupMenuPI_5_3" ("ppsm")) 
  
  ;; teach specialized classes
  ;; ---------------------------
  
  ;; base classes
  ("PPSeqTeachMenuPickProcessBase" ("tmppb" "mppb"))
  ("PPSeqTeachDataPickProcessBase" ("tdppb" "mppb"))
  ("PPSeqTeachMenuProcessVerifyBase" ("tmpvb"))
  ("PPSeqTeachDataProcessVerifyBase" ("tdpvb"))
  ("PPSeqTeachMenuBAProcessBase" ("tmbapb" "mbapb" "bapb"))
  ("PPSeqTeachDataBAProcessBase" ("tdbapb" "dbapb"))
  ("PPSeqTeachMenuMFDieSenBase" ("tmmfdsb" "mmfdsb" "mmfb"))
  ("PPSeqTeachDataMFDieSenBase" ("tdmfdsb" "dmfdsb" "dmfb" "mfb"))
  
  ;; teach / new recipe & install 
  ("PPSeqTeachMenuA51InsertAndDefinePPTools" ("tmiadppt" "tmippt" "tmipt" "tmit" "tmi" "ma51"))
  ("PPSeqTeachDataA51InsertAndDefinePPTools" ("tdiadppt" "tdippt" "tdipt" "tdit" "tdi" "da51" "a51"))

  ;; function selection
  ("PPSeqTeachMenuE11FunctionSelection" ("tmpfs" "mfsp" "me11")) 
  ("PPSeqTeachDataE11FunctionSelection" ("tdpfs" "dfsp" "de11" "e11")) 
  ("PPSeqTeachMenuK21FunctionSelection" ("tmbfs" "tmbofs" "mfsbo" "mk21")) 
  ("PPSeqTeachDataK21FunctionSelection" ("tdbfs" "tdbofs" "dfsbo" "dk21" "k21")) 
  ("PPSeqTeachMenuE31DSFuncSel" ("tmdsfs" "mdsfs" "mfsds" "me31"))
  ("PPSeqTeachDataE31DSFuncSel" ("tddsfs" "ddsfs" "dfsds" "de31" "e31"))
  ("PPSeqTeachMenuS21FunctionSelection" ("tms21" "ms21"))
  ("PPSeqTeachDataS21FunctionSelection" ("tds21" "ds21" "s21"))
  
  ;; process
  ("PPSeqTeachMenuE17PeelProcess" ("tmpep" "mpep" "me17"))
  ("PPSeqTeachDataE17PeelProcess" ("tdpep" "mpep" "de17" "e17"))
  ("PPSeqTeachMenuE18DieEjectorCooling" ("tmdec" "mdec" "mdc" "me18"))
  ("PPSeqTeachDataE18DieEjectorCooling" ("tddec" "ddec" "ddc" "de18" "e18"))
  ("PPSeqTeachMenuE13PickProcess" ("tmpp" "mpp" "me13"))
  ("PPSeqTeachDataE13PickProcess" ("tdpp" "dpp" "de13" "e13"))
  ("PPSeqTeachMenuE43PickProcess" (             "me43"))
  ("PPSeqTeachDataE43PickProcess" (             "de43" "e43"))
  ("PPSeqTeachMenuQ1PlaceProcess" (             "mq1"))
  ("PPSeqTeachDataQ1PlaceProcess" (             "dq1" "q1"))
  ("PPSeqTeachMenuE32DSProcess" ("tmdsp" "mdsp" "me32lb" "me3lb"))
  ("PPSeqTeachDataE32DSProcess" ("tddsp" "ddsp" "de32lb" "de3lb" "e32lb" "e3lb"))
  ("PPSeqTeachMenuE3MFDieSen" ("tme3mf" "me3mf"))
  ("PPSeqTeachDataE3MFDieSen" ("tde3mf" "de3mf" "e3mf"))
  ("PPSeqTeachMenuE8MFDieSen" ("tme8mf" "me8mf"))
  ("PPSeqTeachDataE8MFDieSen" ("tde8mf" "de8mf" "e8mf"))
  ("PPSeqTeachMenuR2MFDieSen" ("tmr2mf" "mr2mf"))
  ("PPSeqTeachDataR2MFDieSen" ("tdr2mf" "dr2mf" "r2mf"))
  ("PPSeqTeachMenuE52ExchProcess" ("tmep" "mep" "me52"))
  ("PPSeqTeachDataE52ExchProcess" ("tdep" "dep" "de52" "e52"))
  ("PPSeqTeachMenuE62FluxProcess" ("tmfp" "mfp" "me62"))
  ("PPSeqTeachDataE62FluxProcess" ("tdfp" "dfp" "de62" "e62"))
  ("PPSeqTeachMenuK24BondProcess" ("tmbp" "mbp" "tmbop" "mbop" "mk24"))
  ("PPSeqTeachDataK24BondProcess" ("tdbp" "dbp" "tdbop" "dbop" "dk24" "k24"))
  ("PPSeqTeachMenuS22TakeProcess" ("tmtp" "tms22" "mtp" "ms22"))
  ("PPSeqTeachDataS22TakeProcess" ("tdtp" "tds22" "dtp" "ds22" "s22"))
  
  ;; verify
  ("PPSeqTeachMenuE14PickProcessVerify" ("tmppv"))
  ("PPSeqTeachDataE14PickProcessVerify" ("tdppv"))
  ("PPSeqTeachMenuK25BondProcessVerify" ("tmbpv"))
  ("PPSeqTeachDataK25BondProcessVerify" ("tdbpv"))
  ("PPSeqTeachMenuE33DSVerify" ("tmdsv" "mdsv"))
  ("PPSeqTeachDataE33DSVerify" ("tddsv" "ddsv"))
  
  ;; optimize
  ("PPSeqTeachMenuPickOptimizeBase" ("tmpob" "mpob" "pob"))
  ("PPSeqTeachDataPickOptimizeBase" ("tdpob" "dpob"))
  ("PPSeqTeachMenuE16OptimizePickProcess" ("tmoppt" "mopp" "tmo" "mo" "me16"))
  ("PPSeqTeachDataE16OptimizePickProcess" ("tdoppt" "dopp" "tdo" "do" "de16" "e16"))
  ("PPSeqTeachMenuE46OptimizePickProcess" (                           "me46"))
  ("PPSeqTeachDataE46OptimizePickProcess" (                           "de46" "e46"))
  ("PPSeqTeachMenuS23OptimizePickProcess" (                           "ms23"))
  ("PPSeqTeachDataS23OptimizePickProcess" (                           "ds23" "s23"))
  ("PPSeqTeachMenuR15OptimizeTransferTablePosition" (                 "mr15"))
  ("PPSeqTeachDataR15OptimizeTransferTablePosition" (                 "dr15" "r15"))


  ;; config specialized classes
  ;; ----------------------------
  ("PPSeqConfigDataA51_QA_10_1_PickUpTool" ("cpup" "cp"))
  ("PPSeqConfigDataA51_QA_10_2_NeedleAndPPT" ("cnap" "cnp"))
  ("PPSeqConfigDataA51_QA_10_6_FluxTool" ("cft"))
  ("PPSeqConfigDataE13_QA_2_2_PickProcess" ("cpp"))
  ("PPSeqConfigDataK24_QA_2_6_BondProcess" ("cb" "cbop"))
  ("PPSeqConfigDataProcMod" ("cdpm" "cpm"))
  ("PPSeqConfigDataProc" ("cdp" "cp"))

  ;; config specialized classes
  ;; ----------------------------
  ("PPSeqFeaturePickUpToolOffset" ("fputo" "fput" "fpu"))
  ("PPSeqFeaturePickerToolOffset" ("fpto" "fpt" "fp"))
  ("PPSeqFeatureTableToolOffset" ("ftto" "ft" "ft"))
  ("PPSeqFeatureTransferTableCooling" ("fttc"))
  ("PPSeqFeaturePickUpToolCleaning" ("fputc"))))

(setq dragon-ffe-pc-calibsrv `(
  ("CalibrationService" ("idl") "idl" )
  ("CalibrationSrv" ("cs" "srv"))
  ("CalibSrvAO" ("ao"))
  ("StdAfx" ("afx"))
  ("CalibrationServiceItemIDs" ("csii" "ii") "h")

  ;; misc
  ("CalibSrvInterfaceContainer" ("csifc" "ifc"))
  ("CalibStateHolder" ("csh" "sh"))
  ("EnumItemIdMapper" ("eiim"))      
  
  ;; services: implemenation of interfaces
  ("PPFCCalibSrv" ("ppfccs" "pps"))
  ("SubstrateHdlCalibSrv" ("shcs" "shs"))
  ("VisionCalibSrv" ("vcs" "vs"))
  ("DieCarrierCalibSrv" ("dccs" "dcs"))
  ("DispenserCalibSrv" ("dics" "dis"))

  ;; proxies: 
  ("PPFCModulesProxyBase" ("ppfcmpb" "pb" "bp"))
  ("PPModulesProxy" ("ppmp" "ppp"))
  ("PPFCModulesProxy" ("ppfcmp" "fcp"))
  ("CasHdlModuleProxy" ("chmp"))
  ("DCWaferSeqProxy" ("dcwsp"))
  ("DIModuleProxy" ("dimp"))
  ("SHModulesProxy" ("shmp"))
  ("VisionModulesProxy" ("vmp"))
  ("WTModuleProxy" ("wtmp"))

  ;; debug menus
  ("DebugPPFCBaseCalibSrv" ("dppfcbcs" "db"))
  ("DebugPPCalibSrv" ("dppcs" "dpp"))
  ("DebugPPFCCalibSrv" ("dppfccs" "dfc"))))

(setq dragon-ffe-rtos-dcwaferhdlmod `(
  ("DCWaferHdlMod" ("m"))
  ("DCWaferHdlModIDs" ("mid" "id"))
  ("DCWHAxisPool" ("ap"))
  ("DCWHCalibData" ("cd"))
  ("DCWHCmds" ("c"))
  ("DCWHCollisionChecker" ("cc"))
  ("DCWHExpansion" ("e"))
  ("DCWHExpansionOneMot" ("eom"))
  ("DCWHExpansionSim" ("es"))
  ("DCWHExpansionSimOneMot" ("esom"))
  ("DCWHGripper" ("g"))
  ("DCWHGripperSim" ("gs"))
  ("DCWHModErrHdl" ("meh" "eh"))
  ("DCWHMoveCtrl" ("mc"))
  ("DCWHSimuBase" ("sb"))
  ("DCWHSubModBase" ("smb"))
  ("DCWHWaferTable" ("wt"))))

(setq dragon-ffe-rtos-calib `(
  ("PPCalibModSeqIf" ("sif" "si" "s"))
  ("PPModFacade" ("f"))
  ("PPCalibVIFacade" ("vif"))
  ("PPCalibMed" ("med"))
  ("PPCalibDynCalib" ("dc"))
  ("PPCalibPos" ("p"))
  ("PPCalibSysPos" ("s" "sp"))
  ("PPCalibCoordSystems" ("cs") "h")
  ("PPCalibMod" ("m" "mod"))
  ("PPCalibModAxis" ("a") "h")
  ("PPCalibModDescription" ("md" "d"))
  ("PPCalibModGetPos" ("gp"))
  ("PPCalibModItemIDs" ("ii" "i"))))

(setq dragon-ffe-rtos-seq `(
  ("PPCoordinatorIF" ("ci") "h")
  ("PPSeqBAZSpeedSet" ("zss" "bzss") "h")
  ("PPSeqBaseDelegate" ("bd"))
  ("PPSeqCalcTimingDelegate" ("ctd" "td"))
  ("PPSeqData" ("d"))
  ("PPSeqHooksHandling" ("hh" "h"))
  ("PPSeqItemIDs" ("ii" "i") "h")
  ("PPSeqPCRTOSProcessCalculations" ("pc"))
  ("PPSeqProductionDelegate" ("pd"))
  ("PPSeqTeachDelegate" ("ted"))
  ("PPSequencer" ("seq" "s"))))

(setq dragon-ffe-rtos-de `(
  ("DCDEMod" ("m"))
  ("DCDEModDescription" ("md" "d"))
  ("DCDEModItemIDs" ("ii" "i") "h")
  ("DCDECalibTeachData.h" ("ctd") "h")
  ("DCDEFirstVersionHid.h" ("fvhid" "fvh") "h")
  ("DCDENullSeriesHid.h" ("nshid" "nsh") "h")
  ("DCDEPPTRampNERange.h" ("rnr" "rner") "h")
  ("DCDEPPTShape.h" ("ppts") "s")
  ("DCDETestingDelegate.h" ("td") "h")
  ("DCDETouchDetection.h" ("td") "h")
  ("DCDETouchSensorSim.h" ("tss") "h")))

(setq dragon-ffe-rtos-ba `(
  ("PPBAMod" ("m"))
  ("PPBADieSensorConnection" ("dsc"))
  ("PPBADieSensorDelegate" ("dsd"))
  ("PPBADieSensorVerifyCtrl" ("dsvc"))
  ("PPBADieSensorVerifyPos" ("dsvp"))
  ("PPBADistSensorDelegate" ("disd"))
  ("PPBADistSensorHelper" ("dish"))
  ("PPBAModDescription" ("md" "d"))
  ("PPBAModItemIDs" ("ii" "i") "h")
  ("PPBAPosCalculator" ("pc"))
  ("PPBASimulation" ("s"))
  ("PPBAYTempObserver" ("to" "t"))))

(setq dragon-ffe-rtos-force `(
  ("PPForceMod" ("mod" "m"))
  ("PPForceModItemIDs" ("ii"))
  ("PPForceModRTOSPCDefinition" ("d"))
  ("PPForceSMCDescription" ("smcd"))
  ("PPForce" ("f"))
  ("PPForceAdjustSim" ("as"))
  ("PPForceAxis" ("a"))
  ("PPForceMid0" ("mid0" "m0"))
  ("PPForceStabilitySupervision" ("ss"))))

(setq dragon-ffe-rtos-proxy `(
  ("PPModProxy" ("mod" "m"))
  ("PPModProxyBase" ("pb" "b"))
  ("PPBAModProxy" ("ba" "a"))
  ("PPBHModProxy" ("bh" "h"))
  ("PPCalibModProxy" ("c"))
  ("PPFAModProxy" ("fa"))
  ("PPForceModProxy" ("f"))
  ("DCDEModProxy" ("de" "d"))))

(setq dragon-ffe-rtos-seqv2 `(
  ("PPSequenceHandler" ("sh") )
  ("PPSubSequence" ("ss") )
  ("PPSeqNotification" ("n") )
  ("PPSeqNotificationHandler" ("nh") )
  ("FastDelegate" ("fd") )
  
  ("PPSeqElementBase" ("eb") )
  ("PPSeqElementMove" ("em") )
  
  ("PPSeqElementBHBlow" ("bhb") )
  ("PPSeqElementBHVac" ("bhv") )
  ("PPSeqElementBHVacOffPrestart" ("bhvoffp") )
  ("PPSeqElementBHVacOnPrestart" ("bhvonp") )
  ("PPSeqElementBondPhi" ("bp") )
  ("PPSeqElementBondUpZ" ("buz") )
  ("PPSeqElementBondDownZ" ("bdz") )
  ("PPSeqElementBondY" ("by") )
  ("PPSeqElementDEVacAndBlow" ("devab") )
  ("PPSeqElementNeFoilUp" ("nfu") )
  ("PPSeqElementNeStartUp" ("nsu") )
  ("PPSeqElementNeTopDown" ("ntd") )
  ("PPSeqElementPickDownZ" ("pdz") )
  ("PPSeqElementPickPhi" ("pp") )
  ("PPSeqElementPickUpZ" ("puz") )
  ("PPSeqElementPickY" ("py") )
  ("PPSeqElementToolTouchDie" ("tdd") )
  ("PPSeqElementZFOV2Travel" ("zfov2t") )
  ("PPSeqNotificationIdList" ("nil") )
  ("PPSeqSubSequence" ("ss") )
  ("PPSeqZPick" ("zp") )
  ("PPSequenceHandlerGlobal" ("shg") )
  ("PPSubSeqBHBlowAndVac" ("bhbav") )
  ("PPSubSeqNeedle" ("sn"))))

;;; tempos
;; todo: choose better prefix than 'c-'
(defvar dragon-method-decl-empty-comment nil)

(defun dragon-new-class (class-base-name)
  (interactive "sClass base name: ")
  (let ((class-name (concat "C" class-base-name)))
    (find-file (concat class-base-name ".h"))
    (insert
     "#pragma once\n"
     "\n"
     "/** */\n"
     "class " class-name "\n"
     "{\n"
     "  public:\n"
     "    " class-name " ();\n"
     "    virtual ~" class-name " ();\n"
     "\n"
     "  private:\n"
     "};\n")
    (find-file (concat class-base-name ".cpp"))
    (insert
     "//..begin \"UTF:Includes\"\n"
     "#include \"stdafx.h\"\n"
     "#include \"" class-base-name ".h\"\n"
     "//..end \"UTF:Includes\"\n"
     "\n"
     class-name "::" class-name " ()\n"
     "{\n"
     "}\n"
     "\n"
     class-name "::~" class-name " ()\n"
     "{\n"
     "}\n"
     "\n"
     )))

(tempo-define-snippet "dragon-class-decl"
  '( lws
     "/** */" >n
     "class " (p "class-name" name) >n
     "{" >n
     "public:" >n
     (s name) " ();" >n
     "virtual ~" (s name) " ();" >n
     "" >n
     "private:" >n
     "}" >n))

(tempo-define-snippet "dragon-class-def"
  '( lws
     "/** */" >n
     (s name) ":" (s name) " ()">n
     "{" >n
     "}" >n
     "" >n
     "/** */" >n
     (s name) ":~" (s name) " ()">n
     "{" >n
     "}" >n))

(defun tempo-template-dragon-class-dwim(&optional arg)
  (interactive "*P")
  (if (c-src-buffer-p)
      (tempo-template-dragon-class-def arg)
    (tempo-template-dragon-class-decl arg)))

(tempo-define-template
 "dragon-method-decl-std"
 '( lws
    '(progn
       (when dragon-method-decl-empty-comment
	 (insert "/** */")
	 (indent-according-to-mode)
	 (insert "\n")))
    "EHRESULT " p " (" p ");" > % ))

;; implement such that it can be called when point is at the beginning of an
;; existing method and the insertation takes place nicely.
(tempo-define-template
 "dragon-method-def-std"
 '( "/** " p " */" >n
    "EHRESULT " '(insert-class-name) "::" p " ()" >n
    "{" >n
    "EHRESULT ehr;" >n
    p n>
    "ERETURN_IF_FAILED(ehr);" n>
    "return ehr;" n>
    "}" > % %))

(defun tempo-template-dragon-method-std ()
  (interactive)
  (if (c-src-buffer-p)
      (tempo-template-dragon-method-def-std)
    (tempo-template-dragon-method-decl-std)))

(tempo-define-template
 "dragon-early-return-std"
 '( lws "ERETURN_IF_FAILED(ehr);" > %)
 "ERET")

(tempo-define-template
 "dragon-ethrow"
 '( lws "ETHROW_IF_FAILED(ehr);" > ))

(tempo-define-template
 "dragon-return-std"
 '( lws "return ehr;" > ))

(tempo-define-template
 "dragon-wstring-literal"
 '( "_T(\"" r "\")")
 "_T")

(tempo-define-template
 "dragon-statement-common"
 '( '(beginning-of-line) "ehr += " r '(when (not (looking-at "[ \t]*;")) (insert ";"))  > )
 "ehr")

(tempo-define-template
 "dragon-trace-method-enter"
 '( lws "ETRACE_LEVEL2(_T(\"" '(insert-class-and-defun-name)  ": " p "\"));" > %))

(tempo-define-template
 "dragon-trace-error"
 '( lws "ETRACE_ERROR(_T(\"" '(insert-class-and-defun-name) ": " p "\"));" > %))

(tempo-define-template
 "dragon-trace-warning"
 '( lws "ETRACE_WARNING(_T(\"" '(insert-class-and-defun-name) ": " p "\"));" > %))

(tempo-define-template
 "dragon-trace-l0"
 '( lws "ETRACE_LEVEL0(_T(\"" '(insert-class-and-defun-name) ": " p "\"));" > %))

(tempo-define-template
 "dragon-trace-l1"
 '( lws "ETRACE_LEVEL1(_T(\"" '(insert-class-and-defun-name) ": " p "\"));" > %))

(tempo-define-template
 "dragon-trace-l2"
 '( lws "ETRACE_LEVEL2(_T(\"" '(insert-class-and-defun-name) ": " p "\"));" > %))

(tempo-define-template
 "dragon-trace-ignore"
 '( lws "ETRACE_AND_IGNORE_IF_FAILED(_T(\"" '(insert-class-and-defun-name) ": " p "\"), ehr);" > %))

(tempo-define-template
 "dragon-try-catch-std"
 '( lws
    "try"
    (tempos-c++-open-brace)
    r-or-blank-line>
    "}" >n
    "ECATCH_COMPLETE_EHR(ehr);" > %))

(tempo-define-template
 "dragon-eassert"
 '( lws "EASSERT( " p " , _T(\"" '(insert-class-and-defun-name) ": " p "\"));" > %)
 "ea")

(tempo-define-template
 "dragon-eassert-new"
 '( lws "EASSERT_NEW( " r " );" > %)
 "ean")

(tempo-define-template
 "dragon-eassert-pointer"
 '( lws "EASSERT_POINTER_NOT_NULL( " r " );" > %)
 "eap")

(tempo-define-template
 "dragon-todo"
 '( "#pragma message( __TODO__ \"FLKA " '(format-time-string "%d.%m.%Y") " : " r "\" )" > %))

(tempo-define-template
 "dragon-utf-block"
 '( lws
    "//..begin \"UTF:" (P "Type : " type) "\"" n>
    r-or-blank-line>
    "//..end \"UTF:" (s type) "\"" >))

(tempo-define-template
 "dragon-utf-forwards"
 '( lws
    "//..begin \"UTF:Forwards\"" >n
    r-or-blank-line>
    "//..end \"UTF:Forwards\"" >n))

(tempo-define-template
 "dragon-utf-includes"
 '( lws
    "//..begin \"UTF:Includes\"" >n
    r-or-blank-line>
    "//..end \"UTF:Includes\"" >n))

(defun tempo-template-dragon-utf-dwim(&optional arg)
  (interactive "*P")
  (cond
   ((not mark-active)
    (tempo-template-dragon-utf-block arg))
   ((looking-at "#\\(include\\|import\\)\\b")
    (tempo-template-dragon-utf-includes arg))
   ((looking-at "\\(class\\|struct\\)\\b")
    (tempo-template-dragon-utf-forwards arg))
   (t
    (tempo-template-dragon-utf-block arg))))

;;; dragon.el ends here
