;;; xentis.el --- customizations specific to the xentis project
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://github.com/sensorflo/sensorflo-emacs/, then
;;      projects/xentis.el
;;
;;; Commentary:
;;
;;; Code:
(require 'project)         ; https://github.com/sensorflo/sensorflo-emacs/
(require 're)              ; https://github.com/sensorflo/sensorflo-emacs/
(require 'tempo-ext)       ; https://gitorious.org/tempo-ext
(require 'tempo-snippets)  ; http://nschum.de/src/emacs/tempo-snippets/
(require 'font-lock-ext)   ; https://github.com/sensorflo/font-lock-ext/
(require 'find-file-ext)   ; https://github.com/sensorflo/find-file-ext/

;;; misc settings

;;;###autoload
(defvar xentis-file-name-regex "/src/[^/]*xentis[^/]*/"
  "Files matching this regexp belong to xentis project")

;; TODO: Actually I don't want to _define_ the coding system to be used, I
;; want to prefer it over other possible alternatives.
;;;###autoload
(add-to-list 'file-coding-system-alist (list xentis-file-name-regex 'iso-latin-1-unix))

;;;###autoload
(add-hook 'change-major-mode-after-body-hook 'xentis-hook)

;;;###autoload
(add-hook 'c-mode-common-hook 'xentis-c-mode-common-hook)

;;;###autoload
(defun xentis-hook()
  (when (and
         (not (is-a-minibufer-mode))
         (not (large-buffer-p))
         (eq (project-root-type) 'project-xentis))
    (mode-message-start "xentis-hook")

    ;; some of these variables might only make sense in within an is-edit-mode
    ;; buffer, but it's more simple to do all in one place and it doesn't
    ;; hurt.
    (set (make-local-variable 'ediff-default-filtering-regexp) "\\.\\(cpp\\|h\\|boc\\|boh\\|msg\\)")
    (set (make-local-variable 'require-final-newline) nil)
    (set (make-local-variable 'fill-column) 100)
    (set (make-local-variable 'tab-width) 4)
    (set (make-local-variable 'indent-tabs-mode) t)
    (set (make-local-variable 'c-doc-comment-char) ?\!)
    (make-local-variable 'grep-find-command)
    (grep-apply-setting 'grep-find-command (xentis-grep-find-command))
    (set (make-local-variable 'compile-command) "sshx pdxenlin52 buildxentis")
    (set (make-local-variable 'grep-find-ext-command-function) 'xentis-grep-find-command)
    (set (make-local-variable 'grep-find-ext-regexp-function) 'xentis-grep-find-regexp)
    (set (make-local-variable 'compilation-skip-threshold) 2)
    ;; is also used by non-c[++] (minor) modes, e.g. find other file
    (set (make-local-variable 'cc-search-directories) nil)
    (dolist (x '("include" "include/investment_compliance"
                 "include/investment_compliance/db_abstraction" "source/bo/investment_compliance"
                 "test/unit/investment_compliance" "test/unit/investment_compliance/db_abstraction"
                 "source/bo/investment_performance" "include/investment_performance"
                 "test/unit/investment_performance"
                 "source/bo/converter" "source/bo/converter/interpreter"
                 "include/converter" "include/converter/interpreter" "source/bo/composite"
                 "include/composite" "source/bo/business_rules" "include/business_rules"
                 "include/xtl" "source/bo/xtl/src" "include/risk" "source/bo/risk"
                 "include/xml" "source/libs/xml"))
      (add-to-list 'cc-search-directories (concat (eamis-root-dir) "/" x)))

    (when (and (not (is-a-minibufer-mode)) (is-edit-mode))
      (when (not (xentis-coding-system-p))
        (message (concat "%s: encoding system is %S which is not one of Xentiss's "
                         "encoding systems, see xentis-coding-system-p")
                 (buffer-name) buffer-file-coding-system)
        (shell-command (concat "notify-send -t 1000 "
                               "'" (buffer-name) " has invalid encoding system!'"))))

    (mode-message-end "xentis-hook")))

;;;###autoload
(defun xentis-c-mode-common-hook()
  (when (eq (project-root-type) 'project-xentis)
    (mode-message-start "xentis-c-mode-common-hook")
    (set (make-local-variable 'tempos-c++-open-brace-style) 'behind-conditional)

    (set (make-local-variable 'c-basic-offset) 4)
    (c-set-offset 'access-label '-)
    (c-set-offset 'innamespace 0)
    (c-set-offset 'inclass '+)
    (c-set-offset 'member-init-intro '++)
    (c-set-offset 'topmost-intro-cont '++) ; note that elements in ctor initializer list after an element initialized with {...} have that

    (make-local-variable 'cc-other-file-alist)
    (add-to-list 'cc-other-file-alist '("\\.boh$" (".boc")))
    (add-to-list 'cc-other-file-alist '("\\.boc$" (".boh")))

    (xentis-font-lock-add-keywords)
    (mode-message-end "xentis-c-mode-common-hook")))

;; buffer-file-name might be nil
;; - in minibuffer-inactive?
;; - dired

(defun fucker()
  (or
   (when (stringp buffer-file-name) buffer-file-name)
   (when (stringp default-directory) default-directory)
   (error "buffer is not associated with anything on file system")))

(defun eamis-root-dir ()
  (locate-dominating-file (fucker) "eamis") "eamis")

(defun eamis-current-project-name ()
  (let ((bfnd (or default-directory (error))))
    (when (or
         (string-match "/eamis/source/bo/\\([^/]+\\)/" bfnd)
         (string-match "/eamis/include/\\([^/]+\\)/" bfnd)
         (string-match "/eamis/test/unit/\\([^/]+\\)/" bfnd))
        (concat (match-string 1 bfnd)))))

;;;###autoload
(defun xentis-before-save-hook ()
  (when (and
         (eq (project-root-type) 'project-xentis)
         (not (xentis-coding-system-p)))
    (if (y-or-n-p (format "%s is encoded in %S. Change coding to Xentis' requirement being iso-latin-1-unix?"
                          (buffer-name) buffer-file-coding-system))
        (setq buffer-file-coding-system 'iso-latin-1-unix)
      (if (y-or-n-p "abort saving? ")
          (error "user aborted abort aving")))))

;;;###autoload
(add-hook 'before-save-hook 'xentis-before-save-hook t)

(defun xentis-looking-at-method-issues (end)
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
            (let (
                  (is-getter
                   (progn
                     (goto-char method-start-pos)
                     (or (looking-at (concat "i?\\(?:Get\\|Show\\|Display\\|Print\\|Trace\\|Log\\)"
                                             "\\(?:[A-Z_0-9]\\|\\b\\)"))
                         (looking-at (concat "[a-zA-Z_0-9]*\\(?:Is\\|Has\\|Was\\|Had\\|Contains\\)"
                                             "\\(?:[A-Z_0-9]\\|\\b\\)")))))
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
                                         "cppcheck-suppress")))))
              (when (and is-getter (not is-const)
                         (not is-static) (not is-surpressed))
                ;; set match data for group 1 beginning at closing paranthesis
                ;; of argument list
                (goto-char method-end-pos)
                (forward-list 1)
                (re-search-backward ")")
                (looking-at "\\()\\(?:[ \t]*;\\)?[ \t]*\\)")
                (setq an-issue-found t)))))

        ;; next iter
        (goto-char method-end-pos)))
    an-issue-found))

(defun xentis-font-lock-add-keywords ()
  (font-lock-add-keywords
   nil
   (list
    ;;
    (list 'xentis-looking-at-method-issues '(1 font-lock-warning-face append t))

    ;; white space errors
    (list "[ \t]+$" '(0 font-lock-warning-face append t))

    ;; empty comments
    (list "/\\*+\\s-*\\*+/\\|//+\\s-*$" '(0 font-lock-unimportant t))

    ;; acess to well known objects/classes

    ;; object names
    ;; collections object names are nouns in plural, i.e. ending in s
    (list (lambda (end)
            (when (re-search-forward
                   (concat "\\b\\(?:list\\|map\\|vector\\|map\\|array\\|stack"
                           "\\|deque\\|queue\\|set\\|unordered_set\\|unordered_map\\)"
                           "\\s-*" (re-balanced "<>")
                           "\\s-*[A-Za-z0-9_]+?\\([A-RT-Za-rt-z0-9_]\\)\\b")
                   end t)
              (save-excursion
                (save-match-data
                  (beginning-of-line)
                  (not (looking-at "\\s-*typedef\\b"))))))
          '(1 font-lock-warning-face t))
        (list "\\bm_impl->" '(0 font-lock-semi-unimportant))


    ;; testing
    (list "\\b\\(TEST\\(?:_F\\)?\\)[ \t\n]*([ \t\n]*\\([A-Za-z0-9_]+\\)[ \t\n]*,\\(?:[ \t\n]*\\([A-Za-z0-9_]+\\)[ \t\n]*)\\)?"
          '(1 font-lock-keyword-face t)
          '(2 font-lock-variable-name-face t)
          '(3 font-lock-function-name-face t t))
    (list (concat "\\(\\bMAKE_\\(?:DISABLED_\\)?\\(?:CHARACTERIZATION_\\)?TEST_NAME1\\)[ \t\n]*("
                  "[ \t\n]*\\([A-Za-z0-9_]+\\)[ \t\n]*)")
          '(1 font-lock-unimportant t)
          '(2 font-lock-function-name-face t))
    (list (concat "\\b\\(MAKE_\\(?:DISABLED_\\)?\\(?:CHARACTERIZATION_\\)?TEST_NAME2\\)[ \t\n]*("
                  "[ \t\n]*\\([A-Za-z0-9_]+\\)[ \t\n]*,"
                  "[ \t\n]*\\([A-Za-z0-9_]+\\)[ \t\n]*)")
          '(1 font-lock-unimportant t)
          '(2 font-lock-function-name-face t)
          '(3 font-lock-function-name-face t))
    (list (concat "\\b\\(MAKE_\\(?:DISABLED_\\)?\\(?:CHARACTERIZATION_\\)?TEST_NAME3?\\)[ \t\n]*("
                  "[ \t\n]*\\([A-Za-z0-9_]+\\)[ \t\n]*,"
                  "[ \t\n]*\\([A-Za-z0-9_]+\\)[ \t\n]*,"
                  "[ \t\n]*\\([A-Za-z0-9_]+\\)[ \t\n]*)")
          '(1 font-lock-unimportant t)
          '(2 font-lock-function-name-face t)
          '(3 font-lock-function-name-face t)
          '(4 font-lock-function-name-face t))
    (list "\\bMAKE_\\(DISABLED_\\)?\\(?:CHARACTERIZATION_\\)?TEST_NAME[123]?\\b"
          '(1 font-lock-warning-face append t t))
    (list "\\bTEST\\(?:_F\\)?[ \t\n]*([ \t\n]*[A-Za-z0-9_]+[ \t\n]*,[ \t\n]*\\(Disabled\\)"
          '(1 font-lock-warning-face append t))
    (list "\\bBIL\\b" '(0 font-lock-semi-unimportant t))


    ;; --- real garbage ---
    (list "^\\s-*//\\.+\\s-*\\(begin\\|end\\)\\b.*\n" '(0 font-lock-unimportant t))
    (list "^\\s-*\\(#define\\s-*\\)?_\\(START\\|STOP\\)_SKIP.*" '(0 font-lock-unimportant t))
    ;; "} // end if"  bullshit
    (list "/[/*]+\\s-*\\(end\\s-*\\)?\\(if\\|else\\|for\\|while\\|do\\|try\\|switch\\|case\\|default\\|catch\\)\\s-*\\(\n\\|\\*/\\)" '(0 font-lock-unimportant t))
    )
   t))

(add-to-list 'auto-mode-alist '("PPSeqDoxygen\\.h" . doxym-mode))

(add-to-list 'auto-mode-alist '("cppcheck[^.]*" . compilation-mode))

;;; functions

(defun xentis-build-name (&optional file-name)
  (setq file-name (or file-name (fucker)))
  (string-match "/drives/builds/[^/]*?/[^/]*?/\\([^/]*?\\)/" file-name)
  (match-string 1 file-name))

(defun trans ()
  "Trans the current rtos project to localsam."
  (interactive)
  (let (saved-shell-file-name
        (proj-dir (project-root-dir (fucker)))
        (proj-file (project-file (fucker))))
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
         (compile "C:/Progra~1/doxygen/bin/doxygen.exe C:\\Progra~1\\doxygen\\DoxygenConfig\\DoxyXentisPCProject_Flo.ini"))
        ((eq (project-root-type) 'project-diebonder-rtos)
         (compile "C:/Progra~1/doxygen/bin/doxygen.exe C:\\Progra~1\\doxygen\\DoxygenConfig\\DoxyXentisRTOSProject.ini"))
        (t (error "Not a pc or rtos project"))))

(defun xentis-checkall(&optional path)
  "Run the xentischeckall tool"
  (interactive (list (let ((default (file-name-directory (directory-file-name (project-root-dir)))))
                       (read-string
                        (concat "Working directory (Default: " default "): ")
                        nil nil default))))
  (compile (combine-and-quote-strings (list "xentischeckall" path))))

(defun st2grep(arg)
  "Runs the content of the current buffer through st2grep and
  then put the buffer into grep mode. With arg, first a new
  buffer is created and the content of the clipboard is
  inserted."

  (interactive "P")
  (when arg
    (set-buffer (get-buffer-create "*xentis dc*"))
    (setq buffer-read-only nil)
    (clipboard-yank)
    (pop-to-buffer "*xentis dc*"))
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

(defun xentis-common-mode-hook ()
  (message "xentis-common-mode-hook")
  (when (or (eq (project-root-type) 'project-diebonder-pc)
            (eq (project-root-type) 'project-diebonder-rtos))
    (local-set-key [remap compile] 'compile-ext)))

(add-hook 'common-mode-hook 'xentis-common-mode-hook t)

(defun xentis-coding-system-p ()
  "t if the buffer's coding system is allowed in Xentis."
  ;; Note (sumary from list-coding-systems):
  ;; - iso-8859-1 and latin-1 are aliases to iso-latin-1
  ;; - Difference windows-1252 to iso-8859-1 (from wikipedia on Windows-1252):
  ;;   ... This encoding is a superset of ISO 8859-1, but differs from the IANA's
  ;;   ISO-8859-1 by using displayable characters rather than control characters
  ;;   in the 80 to 9F (hex) range...
  (member buffer-file-coding-system
          '(iso-latin-1-unix us-ascii-unix)))

(defun xentis-create-tags-table()
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

(defun xentis-grep-find-command(&optional regexp)
  (concat
   "find "
   (let ((mp (eamis-current-project-name)))                 ;
         (if mp
                 (mapconcat
                  'identity
                  (remove-if-not
                   #'file-exists-p
                   (mapcar
                        '(lambda(dir) (file-relative-name (concat (eamis-root-dir) dir mp)))
                        '("/source/bo/" "/include/" "/test/unit/" "/test/module/")))
                  " ")
           ". ")) " \\\n"
   "-regextype posix-egrep \\\n"
   "-type f \\\n"
   "-iregex '.*\\.(h|cpp|boc|boh)'  \\\n"
   "-print0 | xargs -0 grep --color=always -nIP \\\n"
   "-ie '" regexp "'"))

(defvar xentis-well-known-types
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
(defun xentis-grep-find-regexp()
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
             (looking-at (concat xentis-well-known-types "\\s-*\\(?:\\w\\|_\\)+::\\(\\(?:\\w\\|_\\)+\\)"))
             (and (looking-at (concat "\\s-*" xentis-well-known-types "\\s-*\\(\\(?:\\w\\|_\\)+\\)"))
                  (not (string= "ehr" (match-string 1))))))
     (concat "\\b" (match-string-no-properties 1) "\\b"))
   (when (and (save-excursion (beginning-of-line) (looking-at "\\s-*ehr\\s-*\\+?=\\s-\\(\\(?:\\w\\|_\\)+\\)"))
              (<= (point) (match-end 1)))
     (concat "\\b" (match-string-no-properties 1) "\\b"))))

(defun xentis-dwim ()
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
(defun xentis-find-other-file()
  (interactive)
  (let ((case-fold-search nil))
    (cond
     ((string-match "\\(.*?\\)/Sources/\\(.*?\\)\\.\\(cpp\\|h\\)$" (fucker))
      (let* ((fn (replace-match "\\1/UnitTest2/\\2Test.\\3" t nil (fucker))))
        (if (file-exists-p fn)
            (find-file-existing fn)
          (message "File %S does not exist" fn))))
     ((string-match "\\(.*?\\)/UnitTest2/\\(.*?\\)Test\\.\\(cpp\\|h\\)$" (fucker))
      (let* ((fn (replace-match "\\1/Sources/\\2.\\3" t nil (fucker))))
        (if (file-exists-p fn)
            (find-file-existing fn)
          (message "File %S does not exist" fn))))
     (t (error "No 'other' file for the current file")))))

(defun xentis-replace-unreferenced-parameter-macro()
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

(defun xentis-fix-all ()
  (interactive)
  ;; todo: fix character encoding to windows-1252
  ;; todo: fix end of line character to DOS
  (xentis-fix-file-general-comments)
  (xentis-fix-whites))

(defun xentis-dired-do-query-replace-regexp (arg1 arg2)
  (goto-char (point-min))
  (condition-case nil
      (dired-do-query-replace-regexp arg1 arg2)))

(defun xentis-fix-doxygen ()
  (interactive)

  ;; -- syntax
  ;; canonicalize Doxygen comment start delimiter to QT style (/*!)
  (xentis-dired-do-query-replace-regexp "/\\*\\*" "/*!")
  ;; canonicalize Doxygen keywords to \mykeyword
  (xentis-dired-do-query-replace-regexp "@\\(param\\|return\\|pre\\|post\\|warning\\|bug\\|note\\|caution\\|name\\)\\b" "\\\\\\1")

  ;; -- param paragraph
  ;; canonicalize to '\param id decription' (also note exactly one space before & after id)
  ;; i.e. no '\param [in] id description' or '\param id: description'
  (xentis-dired-do-query-replace-regexp "\\([\\@]param\\s-*\\)\\[.*?\\]" "\\1")
  (xentis-dired-do-query-replace-regexp "\\([\\@]param\\s-+[a-zA-Z0-9_]+\\)\\s-*:\\s-*" "\\1 ")
  (xentis-dired-do-query-replace-regexp "\\([\\@]param\\s-+[a-zA-Z0-9_]+\\)\\s-*[oi]f\\s-+type\\s-+\\S-+\\s-*\\(?::\\s-*\\)" "\\1 ")
  (xentis-dired-do-query-replace-regexp "\\([\\@]param\\)\\s-+\\([a-zA-Z0-9_]+\\)\\s-+" "\\1 \\2 ")
  (xentis-dired-do-query-replace-regexp "\\([\\@]param\\)\\s-+\\(?:bool\\|EHRESULT\\|u?int\\(?:64\\|32\\|16\\|8\\)\\|real\\(?:64\\|32\\)\\)\\s-+" "\\1 ")
  ;; todo: remove '\param i_bProcessPointP2 of type bool :'

  ;; -- return paragraph
  ;; remove non-information return paragraphs
  (xentis-dired-do-query-replace-regexp "^\\s-*[\\@]return\\s-*\\(?:[:.]\\s-*\\)?\\(?:bool\\|EHRESULT\\|u?int\\(?:64\\|32\\|16\\|8\\)\\|real\\(?:64\\|32\\)\\)\\s-*\\([:.]\\s-*\\)?*\n" "")
  ;; remove type information from return paragraphs
  (xentis-dired-do-query-replace-regexp "^\\(\\s-*[\\@]return\\)\\s-*\\(?:[:.]\\s-*\\)?\\(?:bool\\|EHRESULT\\|u?int\\(?:64\\|32\\|16\\|8\\)\\|real\\(?:64\\|32\\)\\)\\s-*\\(?:[:.]\\s-*\\)?\\(\\S-\\)" "\\1 \\2")
  ;; remove return paragraphs with bullshit information content (clean up resulting white errors later)
  (xentis-dired-do-query-replace-regexp "^\\(?:\\s-*[\\@]return\\)\\s-[Nn][Oo][Nn][Ee]\\s-*\\(\n\\|\\*+/\\)" "\\1")
  ;; remove empty \return paragraphs (clean up resulting white errors later)
  (xentis-dired-do-query-replace-regexp "^\\(?:\\s-*[\\@]return\\)\\s-*\\(?:[:.]\\s-*\\)?\\(\n\\|\\*+/\\)" "\\1")

  ;; -- whites / empty comments
  ;; replace 2+ whites before and/or after id in '\param id ....' by exactly one space
  (xentis-dired-do-query-replace-regexp
   (concat
    "\\([\\@]param\\)\\(?:"
    "\\s-\\{2,\\}\\(\\S-+\\)\\s-+\\|"
    "\\s-\\(\\S-+\\)\\s-\\{2,\\}\\)")
   "\\1 \\2 ")

  ;; todo:
  ;; move/merge method comments in headers into source
  )

(defun xentis-fix-whites ()
  ;; whites in the context of comments are _not_ handled here

  (interactive)
  ;; remove trailing blanks
  (dired-do-query-replace-regexp "[ \t]+$" "")
  ;; removing blank lines at beginning/end of file
  (dired-do-query-replace-regexp "\\`\\(?:[ \t]*\n\\)+" "")
  (dired-do-query-replace-regexp "^\\(?:[ \t]*\n\\)+\\'" "")
  ;; canonicalize blanks between methods to 1 blank line
  (dired-do-query-replace-regexp "^}\\s-*\n\\([ \t]*\n\\)\\{2,\\}" "}\n\n") ;only in .cpp files
  ;; canonicalize empty method body
  (dired-do-query-replace-regexp "^{[ \t]*\n\\([ \t]*\n\\)+}" "{\n}"); only in .cpp files

  ;; no blank lines between #includes/#imports/forward declarations
  ;; todo
  )

(defun my-dired-do-query-replace-regexp (regexp replacement)
  (let ((local-my-dired-buffer
                 (if (boundp 'my-dired-buffer)
                         my-dired-buffer
                   (current-buffer))))
        (switch-to-buffer local-my-dired-buffer)
        (ignore-errors (dired-do-query-replace-regexp regexp replacement))
        (switch-to-buffer local-my-dired-buffer)))

(defun xentis-fix-file-general-comments ()
  (interactive)
  (let ((my-dired-buffer (current-buffer)))
        ;; method banners
        (my-dired-do-query-replace-regexp
         (concat
          "^[ \t]*//[ \t]*-+[ \t]*\n"
          "\\([ \t]*//[ \t]*\\(\\([a-zA-Z_0-9]+::\\)?~?[a-zA-Z_0-9]+\\([ \t]*()\\)?[ \t]*\\)?\n\\)?"
          "[ \t]*//[ \t]*-+[ \\t]*\n")
         "")

        ;; general banners surrounded by ///// lines
        (my-dired-do-query-replace-regexp
         (concat
          ;; leading blank lines
          "^\\([ \t]*\n\\)*"
          ;; (leading and trailing) or (only trailing)
          "\\("
      "\\([ \t]*/\\{5,\\}[ \t]*\n\\)?"
      "\\([ \t]*//.*\n\\)+"
      "\\([ \t]*/\\{5,\\}[ \t]*\n\\)"
          ;; only leading
          "\\|"
      "\\([ \t]*/\\{5,\\}[ \t]*\n\\)"
      "\\([ \t]*//.*\n\\)+"
          "\\)"
          ;; trailing blank lines
          "\\([ \t\n]*\n\\)*")
         "\n")

        ;; "} end ..." bullshit
        (my-dired-do-query-replace-regexp "}[ \t]*//.*$" "}")
        (my-dired-do-query-replace-regexp "\\(#endif\\)[ \t]*//.*" "\\1")

        ;; remove blank lines before closing brace
        (my-dired-do-query-replace-regexp "^\\(?:[ \t]*\n\\)+\\([ \t]*}\\)" "\\1")

        ;; replace tabs by spaces after comment-start delimiters
        (my-dired-do-query-replace-regexp "//\t" "// ")
        (my-dired-do-query-replace-regexp "\\(/\\*\\(?:\\*+\\|!+\\)?\\)\t" "\\1 ")

        ;; -- whites in comments
        ;; remove empty comment
        (my-dired-do-query-replace-regexp "^\\s-*/\\*[*!]*\\(\\s-*\n\\)*\\s-*\\*/\\s-*\n" "")

        ;; trailing ; after class inline method definition
        (my-dired-do-query-replace-regexp "^[ \t]*;[ \t]*\n" "")

        ;; The trailing whites after /** and leading whites before */ is exactly one
        ;; space
        ;; (xentis-dired-do-query-replace-regexp "\\(/\\*[*@]+\\)\\(?:\\(?:[ \t]*\n\\)+[ \t]*\\|[ \t]\\{2,\\}\\)" "\\1 ")
        ;; (xentis-dired-do-query-replace-regexp "\\(?:\\(?:[ \t]*\n\\)+[ \t]*\\|[ \t]\\{2,\\}\\)\\(\\*+/\\)" " \\1")
        ;; [ensure an empty comment has only exactly one space]
        ;; obsolete since empty comments are removed altogether
        ;; (xentis-dired-do-query-replace-regexp "/\\*+[ \t\n]\\{2,\\}\\*+/" "/** */")

        ;; todo:
        ;; search for //-- and //== banners ("^[ 	]*//[ \t]*[-=/]\{3,\}") and modify manually
        ;; search for overly long lines: "^.\{110,\}"

        ;; new commit
        ;; replace #ifndef by #pragma once

        ;; new commit
        ;; coding system / end-of-line delimiter
        ))

(defun xentis-create-abbrev-enable-function (arg)
  "Returns a function which suitable for abbrev's :enable-function.
Arg is the 3rd items of a xentis-abbrev-table item"
  `(lambda () (xentis-abbrev-enable-function ,arg)))

(defun xentis-abbrev-enable-function (arg)
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
                ("eamis/source/bo/investment_compliance" xentis-investment-compliance)
                ("eamis/include/investment_compliance" xentis-investment-compliance)
                )))
  (dolist (elt mylist)
    (add-to-list 'ffe-map-map elt)))

(setq xentis-investment-compliance `(
  ("AssginmentInfo" ("ai"))
  ("Jobs" ("j"))
  ("Message" ("m"))
  ("MessageHelper" ("mh"))
  ("Scheduler" ("s"))
  ("TestEngine" ("te"))
  ("TestProcess" ("tp"))
  ("TestProcessExecutor" ("tpe"))
  ("TestProcessTrx" ("tpt" "trx"))
  ("TestRun" ("tr"))
  ("TestRunResult" ("trr"))
  ("Valuator" ("trr"))
  ("Worker" ("w"))))

;;; tempos
;; todo: choose better prefix than 'c-'
(defvar xentis-method-decl-empty-comment nil)

(defun xentis-new-class (class-base-name)
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

(tempo-define-snippet "xentis-class-decl"
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

(tempo-define-snippet "xentis-class-def"
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

(defun tempo-template-xentis-class-dwim(&optional arg)
  (interactive "*P")
  (if (c-src-buffer-p)
      (tempo-template-xentis-class-def arg)
    (tempo-template-xentis-class-decl arg)))

(tempo-define-template
 "xentis-method-decl-std"
 '( lws
    '(progn
       (when xentis-method-decl-empty-comment
         (insert "/** */")
         (indent-according-to-mode)
         (insert "\n")))
    "EHRESULT " p " (" p ");" > % ))

;; implement such that it can be called when point is at the beginning of an
;; existing method and the insertation takes place nicely.
(tempo-define-template
 "xentis-method-def-std"
 '( "/** " p " */" >n
    "EHRESULT " '(insert-class-name) "::" p " ()" >n
    "{" >n
    "EHRESULT ehr;" >n
    p n>
    "ERETURN_IF_FAILED(ehr);" n>
    "return ehr;" n>
    "}" > % %))

(defun tempo-template-xentis-method-std ()
  (interactive)
  (if (c-src-buffer-p)
      (tempo-template-xentis-method-def-std)
    (tempo-template-xentis-method-decl-std)))

;;; xentis.el ends here
