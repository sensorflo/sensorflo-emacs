;;; dragon.el --- customizations specific to the dragon project
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;;
;;; Commentary:
;;
;;; Code:
(require 'tempo-ext)
(require 'font-lock-ext)

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
    (setq tab-width 2)
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 2)
    (c-set-offset 'access-label '-)
    (c-set-offset 'inclass '++)
    (dragon-font-lock-add-keywords)
    (when (and (dragon-file-p) (not (dragon-coding-system-p)))
      (message "%s: encoding system is %S which is not dragons's encoding system (windows-1252-dos undecided-dos)"
               (buffer-name) buffer-file-coding-system)
      (shell-command (concat "notify-send -t 1000 '" (buffer-name) " ist scheisse!!'")))
    (dragon-c-mode-common-bindings)))

(add-hook 'c-mode-common-hook 'dragon-c-mode-common-hook t)

(defun dragon-c-mode-common-bindings()
  ;; definitions
  (local-set-key [(control ?\,)(d)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(d)(d)] '(lambda () (interactive) (open-line 1) (tempo-template-dragon-method-def-std)))
  (local-set-key [(control ?\,)(d)(D)] '(lambda () (interactive) (end-of-line) (tempo-template-dragon-method-def)))
  (local-set-key [(control ?\,)(d)(m)] '(lambda () (interactive) (end-of-line) (tempo-template-dragon-method-decl-std)))
  (local-set-key [(control ?\,)(d)(M)] '(lambda () (interactive) (end-of-line) (tempo-template-dragon-method-decl)))

  ;; control flow
  (local-set-key [(control ?\,)(c)(t)] 'tempo-template-dragon-try-catch-std) 
  (local-set-key [(control ?\,)(c)(f)(e)] 'tempo-template-dragon-for-each) 

  ;; statements
  (local-set-key [(control ?\,)(s)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(s)(r)] 'tempo-template-dragon-early-return-std)
  (local-set-key [(control ?\,)(s)(a)] 'tempo-template-dragon-eassert)
  (local-set-key [(control ?\,)(s)(s)] 'tempo-template-dragon-statement-common-ehr)

  ;; misc
  (local-set-key [(control ?\,)(m)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(m)(t)] 'tempo-template-dragon-todo) ; todo

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
  (when (or (eq (project-root-type) 'project-diebonder-pc)
            (eq (project-root-type) 'project-diebonder-rtos))
    (when (and (dragon-file-p) (not (dragon-coding-system-p)))
      (if (y-or-n-p (format "%s: change coding system from %S to windows-1252-dos? "
                               (buffer-name) buffer-file-coding-system))
          (setq buffer-file-coding-system 'windows-1252-dos)
        (if (y-or-n-p "abort saving? ") (error "user aborted abort aving"))))
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

(defun dragon-method-spaces (end)
  (let ((method-start t)
	(method-end t)
	warning-found)
    (while (and method-start
		method-end
		(not warning-found)
    		(< (point) (1- end)))
      (setq method-start (text-property-any (point) end 'face 'font-lock-function-name-face))
      (when method-start
	(setq method-end (text-property-not-all method-start end 'face 'font-lock-function-name-face))
	(when method-end
	  (goto-char method-end)
	  ;; methods declared via macros are excluded
	  (setq warning-found (and (not (looking-back "^[A-Z0-9_]+"))
				   (re-search-forward "\\=\\((\\|\\s-\\s-+(\\)" end t))))))
    warning-found))

(defun dragon-font-lock-add-keywords ()
  (font-lock-add-keywords nil
    (list
      ;; 
     (list 'dragon-method-spaces '(1 font-lock-warning-face))

      ;; empty comments 
      (list "/\\*+\\s-*\\*+/\\|//+\\s-*$" '(0 font-lock-unimportant t)) 
       
      ;; acess to well known objects/classes
      (list "\\b\\(\\(CPPSeqMeth\\|CPPSeqRTOSBase\\|CPPSeqIfc\\|CPPSeqMatMgmtDieLocDef\\)::\\(Inst()\\.\\)?\\)" '(1 font-lock-semi-unimportant t))
      (list "\\bCPPSeqIfc::\\w+()\\." '(0 font-lock-semi-unimportant t))
      (list "\\bCPPSeqIfc::Inst().Get\\w+Wrp()\\." '(0 font-lock-semi-unimportant t))
      (list "m_pMenuAccess->Get\\w*()\\." '(0 font-lock-semi-unimportant t))
      
      (list "^\\s-*EHRESULT\\s-+ehr\\s-*[;=]" '(0 font-lock-semi-unimportant t))
      (list "^\\s-*ehr\\s-*\\+=" '(0 font-lock-semi-unimportant t))
      (list "^\\s-*[a-zA-Z0-9_]*\\(STOP\\|RETURN\\|ASSERT\\|THROW\\)[a-zA-Z0-9_]*\\s-*(.*" '(0 font-lock-semi-unimportant t))
      (list "\\(?:^\\|}\\)\\s-*\\([a-zA-Z0-9_]*CATCH[a-zA-Z0-9_]*\\s-*(.*\\)" '(1 font-lock-semi-unimportant t))
      (list "^\\s-*\\(return\\s-*ehr\\s-*;\\)\\s-*\n}" '(1 font-lock-semi-unimportant t))
      (list "^\\s-*E\\(END\\|BEGIN\\)_COM_METHOD.*" '(0 font-lock-semi-unimportant t))

      (list "\\be[PBDE]VIGraphicalObject" '(0 font-lock-semi-unimportant t))
      (list "\\bscIID_\\(sSI\\|sMS\\)[a-zA-Z0-9]*_\\([a-zA-Z0-9]+_\\)?" '(0 font-lock-semi-unimportant t))
      
      (list "^\\s-*ETRACE[a-zA-Z0-9_]+.*" '(0 font-lock-semi-unimportant t))
      (list "^\\s-*UNREFERENCED_PARAMETER.*" '(0 font-lock-semi-unimportant t))
      
      (list "\\w+_cast\\s-*<[^>\n]*>" '(0 font-lock-semi-unimportant t))
      
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
  (member buffer-file-coding-system '(windows-1252-dos undecided-dos utf-8-dos))) ; undecided

;; note that this should somehow belong to the
(defun dragon-file-p ()
  "Returns non-nil if the file visited by the current buffer belongs to the dragon project."
  (not (string-match "\\(^\\|/\\)\\.\\(git\\|svn\\)" (or buffer-file-name (buffer-name)))))

(defun dragon-create-tags-table()
  (interactive)
  ;(shell-command "find . UnitTest ~/src/Common/INC ~/src/DieBonder/PC/INC ~/src/DieBonder/RTOSExport '/home/emp8118035/drives/xpc/Program Files/Microsoft Visual Studio 10.0/VC/include/' -iname '*.cpp' -o -iname '*.h' -o -iname '*.tlh' -print0 | xargs -0 etags")
  ;(shell-command "find . UnitTest ~/src/Common/INC ~/src/DieBonder/PC/INC ~/src/DieBonder/RTOSExport  \\( -iname '*.cpp' -o -iname '*.h' \\) -print0 | xargs -0 etags")
  (shell-command
   (concat "find "
	   default-directory " "
	   default-directory "UnitTest "
	   "~/src/Common/INC "
	   "~/src/DieBonder/PC/INC "
	   "~/src/DieBonder/RTOSExport "
	;   "'/home/emp8118035/drives/xpc/Program Files/Microsoft Visual Studio 10.0/VC/include/' "
	   ;; '.*\\.\\(c\\|cpp\\|h\\|tlh\\)'
	   "\\( -iname '*.h' -o -iname '*.cpp' -o -iname '*.tlh' \\) -print0 | xargs -0 etags")))

(defun dragon-grep-find-command(&optional regexp)
   (concat
   "find . UnitTest -maxdepth 1 -not \\( -type d -iname '*~*' -prune \\) \\\n"
   "-regextype posix-egrep \\\n"
   "-type f \\\n"
   "-iregex '.*\\.(idl|h|cpp)'  \\\n"
   "-print0 | xargs -0 grep --color=always \\\n"
   "-nIPie '" regexp "'"))

(defvar dragon-well-known-types
  (concat
   "\\(?:"
     "\\(?:const\\s-*\\)?"
     "\\(?:" (regexp-opt '(
       "int" "double" "char" "uint32" "real64" 
       "HRESULT" "EHRESULT"
       "tPST_ExtInteger" "tPST_ExtReal" "tPST_ExtStruct" "tPST_INDELInteger" "tPST_INDELReal" "tPST_INDELStruct"
       "tPstContainerInteger" "tPstContainerIterator" "tPstContainerReal" "tPstContainerStruct" "tPstContainerText" "tPstDynEnumTransient" "tPstInteger" "tPstReal" "tPstStruct" "tPstText"))
     "\\)"
     "\\(?:\\s-*const\\)?\\(?:\\s-*[&*]\\(?:\\s-*const\\)?\\)*"
   "\\)"))

;; static const tDiagCondId DC_PPSeq_PickFailed                                             = 0x0F0A0021; //PC / RTOS: Pick failed
(defun dragon-grep-find-regexp()
  (or
   (when (save-excursion
	   (beginning-of-line)
	   (or (looking-at "\\s-*\\(?:class\\|struct\\)\\s-+\\(.*?\\)\\_>")
	       (looking-at "\\s-*static\\s-+const\\s-+\\(?:tDiagCondId\\|tItemId\\)\\s-+\\(.*?\\)\\_>")
	       (looking-at "\\s-*SetBaseItemID\\s-*([^,\n]*,\\s-*\\(.*?\\)\\_>\\s-*)")
	       (looking-at ".*PRS_INIT\\s-*([^,\n]*,\\s-*\\(.*?\\)\\_>")
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

;;; file aliases / cache
(setq filealias-default-root-dir "W:")

(setq filealias-alist
      '(
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

(mapc 'file-cache-add-file 
        '("~/office/dragon"
	  "~/src/DieBonder"
	  "~/src/DieBonder/RTOS"
	  "~/src/DieBonder/PC"
	  "~/src/DieBonder/RTOS/PickPlace"
	  "~/src/DieBonder/PC/PickPlace"
	  "~/src/DieBonder/PC/PickPlace"
	  "~/src/DieBonder/PC/PickPlace/PPSeqBaseLib"
	  "~/src/DieBonder/PC/PickPlace/PPCalibMod"
	  "~/src/DieBonder/RTOS/PickPlace/PPCalibMod"
	  "~/drives/xpc/Program Files/Esec/DieBonder/Data/BuildVersion.txt"
	  ))

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
  ("/DieBonder/PC/Services/CalibrationServices/CalibrationService" dragon-ffe-pc-calibsrv)
  ("/DieBonder/PC/PickPlace/PpcalibMod" dragon-ffe-pc-calib)
  ("/DieBonder/PC/PickPlace/DCDEMod" dragon-ffe-pc-de)
  ("/DieBonder/PC/PickPlace/PPBAMod" dragon-ffe-pc-ba)
  ("/DieBonder/PC/PickPlace/PPBBMod" dragon-ffe-pc-bh)
  ("/DieBonder/PC/PickPlace/PPForceMod" dragon-ffe-pc-force)
  ("/DieBonder/PC/PickPlace/PPSeqBaseLib" dragon-ffe-pc-seq)
  ("/DieBonder/RTOS/PickPlace/PPCalibMod" dragon-ffe-rtos-calib)
  ("/DieBonder/RTOS/PickPlace/PPSequencer/Sources/SeqV2" dragon-ffe-rtos-seqv2)
  ("/DieBonder/RTOS/PickPlace/PPSequencer" dragon-ffe-rtos-seq)
  ("/DieBonder/RTOS/PickPlace/DCDEMod" dragon-ffe-rtos-de)
  ("/DieBonder/RTOS/PickPlace/PPBAMod" dragon-ffe-rtos-ba)
  ("/DieBonder/RTOS/PickPlace/PPForceMod" dragon-ffe-rtos-force)
  ("/DieBonder/RTOS/PickPlace/PPModProxy" dragon-ffe-rtos-proxy))))
  (dolist (elt mylist) 
    (add-to-list 'ffe-map-map elt)))

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
      ("DCDERunInBase" ("rib"))
      ))

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
      ("PPBAXNeRunIn" ("xnr" "xr" "nr"))
      ))

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
      ("PPBHAxesDebug" ("ad"))
      ))

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
      ("PPForceAxesDebug" ("ad" "d"))
      ))

(setq dragon-ffe-pc-seq `(
      ("PPSeqAO" ("ao"))
      ("PPSeqATLBase" ("seq" "atlb" "atl"))
      ("StdAfx" ("afx") "h")
      ("PPSeqDoxygen" ("doxy" "dox") "h")
      ("PPSeqMeth" ("m"))
      ("PPSeqRTOSModData" ("rmd"))
      ("PPSeqRTOSBase" ("rb"))
      ("PPSeqGlobalData" ("gd"))
      ("PPSeqItemIDs" ("ii" "i") "h")
      ("PPSeqForeignItemIDs" ("fii" "fi") "h")
      ("PPSeqDiagCondition" ("dc" "di") "h")
      ("PPSeqTypeDefinitions" ("td") "h")
      ("PPSeqIfc" ("ic" "ifc") "h")
      ("PPSequencer" ("idl") "idl")
      ("PPSeqDataMgrPC" ("dmp"))
      ("PPSeqRTOSDataTransferCtrl" ("rdtc" "dtc"))
      ("PPSeqRTOSString" ("rs") "h")
      ("PPSeqMessages" ("msg") "h")
      ("PPSeqOptGroup" ("og") )
      ("PPSeqStateHdlList" ("shl" "sl") )
      ("MCDynComboboxHdlList" ("dchl" "dcl") )
      ("PPSeqDisplayDepData" ("ddd") )
      ("PPSeqSingleton" ("s") "h")
      ("PPSeqAutoPtr" ("ap") "h")
      ("PPSeqFeature" ("f"))
      ("PPSeqFeatureCoordinator" ("fc"))
      ("IPPSeqEnabler" ("er") "h")
      ("IPPSeqEnablee" ("ee") "h")
      ("PPSeqCommandHandler" ("ch") "h")

      ;; wrappers
      ("PPSeqWPPBAMod" ("wba"))
      ("PPSeqWPPBHMod" ("wbh"))
      ("PPSeqWPPForceMod" ("wfm"))
      ("PPSeqWPPCalibMod" ("wcm" "wc"))
      ("PPSeqWDCDEMod" ("wdem" "wde"))
      ("PPSeqWTeachSrv" ("wts"))
      ("PPSeqWPPPPickerMod" ("wpm" "wp"))
      
      ;; teach base
      ("PPSeqPCProcessCalculations" ("pcpc"))
      ("PPSeqTeachDataProcessBase" ("tdpb" "dpb"))
      ("PPSeqTeachMenuProcessBase" ("tmpb" "mpb"))
      ("PPSeqMenuCoordinator" ("mc"))
      ("PPSeqMenuHandlerBase" ("mhb"))
      ("PPSeqMenuHandler" ("mh"))
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
      
      ;; teach / new recipe & install 
      ("PPSeqTeachMenuA51InsertAndDefinePPTools" ("tmiadppt" "tmippt" "tmipt" "tmit" "tmi" "ma51"))
      ("PPSeqTeachDataA51InsertAndDefinePPTools" ("tdiadppt" "tdippt" "tdipt" "tdit" "tdi" "da51"))

      ;; function selection
      ("PPSeqTeachDataE11FunctionSelection" ("tdpfs" "dfsp" "de11")) 
      ("PPSeqTeachMenuE11FunctionSelection" ("tmpfs" "mfsp" "me11")) 
      ("PPSeqTeachDataK21FunctionSelection" ("tdbfs" "tdbofs" "dfsbo" "dk21")) 
      ("PPSeqTeachMenuK21FunctionSelection" ("tmbfs" "tmbofs" "mfsbo" "mk21")) 
      ("PPSeqTeachDataE31DSFuncSel" ("tddsfs" "ddsfs" "dfsds" "de31"))
      ("PPSeqTeachMenuE31DSFuncSel" ("tmdsfs" "mdsfs" "mfsds" "me31"))
      ("PPSeqTeachDataS21FunctionSelection" ("tds21" "ds21"))
      ("PPSeqTeachMenuS21FunctionSelection" ("tms21" "ms21" "s21"))
      
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
      ("PPSeqTeachMenuE32DSProcess" ("tmdsp" "mdsp"))
      ("PPSeqTeachDataE32DSProcess" ("tddsp" "ddsp"))
      ("PPSeqTeachMenuE52ExchProcess" ("tmep" "mep"))
      ("PPSeqTeachDataE52ExchProcess" ("tdep" "dep"))
      ("PPSeqTeachMenuE62FluxProcess" ("tmfp" "mfp"))
      ("PPSeqTeachDataE62FluxProcess" ("tdfp" "dfp"))
      ("PPSeqTeachMenuK24BondProcess" ("tmbp" "mbp" "tmbop" "mbop" "mk24"))
      ("PPSeqTeachDataK24BondProcess" ("tdbp" "dbp" "tdbop" "dbop" "dk24" "k24"))
      ("PPSeqTeachDataS22TakeProcess" ("tdtp" "tds22" "dtp" "ds22"))
      ("PPSeqTeachMenuS22TakeProcess" ("tmtp" "tms22" "mtp" "ms22" "s22"))
      
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
      ("PPSeqTeachMenuR15OptimizeTransferTablePosition" (                 "r15"))
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
      ("PPSeqFeatureTableToolOffset" ("ftto" "ft" "ft"))))

(setq dragon-ffe-pc-calibsrv `(
      ;; common
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
      ("DebugPPFCCalibSrv" ("dppfccs" "dfc"))
      ))

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
      ("PPSeqCalcTimingDelegate" ("ctd" "td") )
      ("PPSeqData" ("d") )
      ("PPSeqHooksHandling" ("hh" "h") )
      ("PPSeqItemIDs" ("ii" "i") "h")
      ("PPSeqPCRTOSProcessCalculations" ("pc") )
      ("PPSeqProductionDelegate" ("pd") )
      ("PPSeqTeachDelegate" ("ted") )
      ("PPSequencer" ("seq" "s") )))

(setq dragon-ffe-rtos-de `(
      ("DCDEMod" ("m"))
      ("DCDEModDescription" ("md" "d") )
      ("DCDEModItemIDs" ("ii" "i") "h")
      ("DCDECalibTeachData.h" ("ctd") "h")
      ("DCDEFirstVersionHid.h" ("fvhid" "fvh") "h")
      ("DCDENullSeriesHid.h" ("nshid" "nsh") "h")
      ("DCDEPPTRampNERange.h" ("rnr" "rner") "h")
      ("DCDEPPTShape.h" ("ppts") "s")
      ("DCDETestingDelegate.h" ("td") "h")
      ("DCDETouchDetection.h" ("td") "h")
      ("DCDETouchSensorSim.h" ("tss") "h")
      ))

(setq dragon-ffe-rtos-ba `(
      ("PPBAMod" ("m"))
      ("PPBADieSensorConnection" ("dsc") )
      ("PPBADieSensorDelegate" ("dsd"))
      ("PPBADieSensorVerifyCtrl" ("dsvc") )
      ("PPBADieSensorVerifyPos" ("dsvp") )
      ("PPBADistSensorDelegate" ("disd") )
      ("PPBADistSensorHelper" ("dish") )
      ("PPBAModDescription" ("md" "d") )
      ("PPBAModItemIDs" ("ii" "i") "h")
      ("PPBAPosCalculator" ("pc") )
      ("PPBASimulation" ("s") )
      ("PPBAYTempObserver" ("to" "t") )
      ))

(setq dragon-ffe-rtos-force `(
      ("PPForceMod" ("mod" "m"))
      ("PPForceModItemIDs" ("ii"))
      ("PPForceModRTOSPCDefinition" ("d"))
      ("PPForceSMCDescription" ("smcd"))
      ("PPForce" ("f"))
      ("PPForceAdjustSim" ("as"))
      ("PPForceAxis" ("a"))
      ("PPForceMid0" ("mid0" "m0"))
      ("PPForceStabilitySupervision" ("ss"))
      ))

(setq dragon-ffe-rtos-proxy `(
      ("PPModProxy" ("mod" "m"))
      ("PPModProxyBase" ("pb" "b"))
      ("PPBAModProxy" ("ba" "a"))
      ("PPBHModProxy" ("bh" "h"))
      ("PPCalibModProxy" ("c"))
      ("PPFAModProxy" ("fa"))
      ("PPForceModProxy" ("f"))
      ("DCDEModProxy" ("de" "d"))
      ))
      
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
       ("PPSubSeqNeedle" ("sn") )
       ))

;;; tempos
;; todo: choose better prefix than 'c-'
(tempo-define-template "dragon-method-decl-std"
 '( &
    "EHRESULT " p "(" p ");" > ))

(tempo-define-template "dragon-method-def-std"
 '( &
    "/** */" > n>
    "EHRESULT " '(insert-class-name) "::" p "()" > n>
    "{" > n>
    "EHRESULT ehr;" > n>
    p n>
    "ERETURN_IF_FAILED(ehr);" n>
    "return ehr;" n>
    "}" > n> ))

(tempo-define-template "dragon-early-return-std"
 '( lws "ERETURN_IF_FAILED(ehr);" > %)
 "ERET")

(tempo-define-template "dragon-ethrow"
 '( &
    "ETHROW_IF_FAILED(ehr);" > ))

(tempo-define-template "dragon-return-std"
 '( &
    "return ehr;" > ))

(tempo-define-template "dragon-wstring-literal"
 '( "_T(\"" r "\")")
 "_T")

(tempo-define-template "dragon-statement-common-ehr"
 '( lws
    "ehr += " r ";" >n
    "ERETURN_IF_FAILED(ehr);" > %)
 "ehr")

(tempo-define-template "dragon-statement-common"
 '( &
    "ehr += " p ";" > ))

(tempo-define-template "dragon-trace-method-enter"
 '( lws
    "ETRACE_LEVEL2(_T(\"" '(insert-class-and-defun-name)  ": " p "\"));" > %))

(tempo-define-template "dragon-trace-error"
 '( lws "ETRACE_ERROR(_T(\"" '(insert-class-and-defun-name) ": " p "\"));" > %))

(tempo-define-template "dragon-trace-warning"
 '( lws "ETRACE_WARNING(_T(\"" '(insert-class-and-defun-name) ": " p "\"));" > %))

(tempo-define-template "dragon-trace-l0"
 '( lws "ETRACE_LEVEL0(_T(\"" '(insert-class-and-defun-name) ": " p "\"));" > %))

(tempo-define-template "dragon-trace-l1"
 '( lws "ETRACE_LEVEL1(_T(\"" '(insert-class-and-defun-name) ": " p "\"));" > %))

(tempo-define-template "dragon-trace-l2"
 '( lws "ETRACE_LEVEL2(_T(\"" '(insert-class-and-defun-name) ": " p "\"));" > %))

(tempo-define-template "dragon-trace-ignore"
 '( lws "ETRACE_AND_IGNORE_IF_FAILED(_T(\"" '(insert-class-and-defun-name) ": " p "\"), ehr);" > %))

(tempo-define-template "dragon-try-catch-std"
 '( lws
    "try {" >n
    r-or-blank-line>
   "}" >n
    "ECATCH_COMPLETE_EHR(ehr);" > %))

(tempo-define-template "dragon-for-each"
 '( lws
    "for each (" p " in " p ") {" >n
    r-or-blank-line>
    "}" > %))

(tempo-define-template "dragon-eassert"
 '( lws "EASSERT( " p " , _T(\"" '(insert-class-and-defun-name) ": " p "\"));" > %)
 "ea")

(tempo-define-template "dragon-todo"
 '( "#pragma message( __TODO__ \"FLKA " '(format-time-string "%d.%m.%Y") " : " r "\" )" > %))

(tempo-define-template "dragon-utf-block"
 '( &
    "//..begin \"UTF:" (P "Type : " type) "\"" n>
    r> n>
    "//..end \"UTF:" (s type) "\"" >))

(tempo-define-template "dragon-utf-forwards"
 '( &
    "//..begin \"UTF:Forwards\"" n>
    r> n>
    "//..end \"UTF:Forwards\"" >))

(tempo-define-template "dragon-utf-includes"
 '( &
    "//..begin \"UTF:Includes\"" n>
    r> n>
    "//..end \"UTF:Includes\"" >))

;;; dragon.el ends here