;;; project.el ---
;; 
;; Copyright 2010 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;;
;;; Commentary
;;
;; Mindstorming
;; ------------
;; Dimensions 
;; 1) Project/File tree (company,project,sub*project)
;; 2) File type aka major-mode
;; 3) Defined by whom aka, stored in which distributable file 
;;
;; - A given option is either defined by a.1/a.2 or by b. Thus 
;; - Wheter for a given option a) takes precedence over b) or vice verca is
;;   customizable.
;;   * x for all files projects below node a, but .cpp's do y, and .el's do z.
;;   * x for all .cpp's, but projecttree a does y (for cpps)
;;


;; Store in distributable files: yes/no
;;
;;; Variables
(defgroup project nil
  "")

;; (defstruct project-def is-p compile)

;; (defcustom project-list nil
;;   "List of known projects.
;; `project-def' structs."
;;   :group 'project)

;;; Code
(defgroup project nil "")

(defun project-root-type (&optional file-name)
  "Returns the root type of the project of the passed file/directory name.
Returns nil if it is unknown."
  (let ((actual-fn (or file-name (buffer-file-name) default-directory))
        (case-fold-search t))
    (cond
     ((null actual-fn) nil)
     ((string-match "/diebonder/pc\\(/\\|$\\)" actual-fn) 'project-diebonder-pc)
     ((string-match "/diebonder/rtos\\(/\\|$\\)" actual-fn) 'project-diebonder-rtos)
     ((string-match "/\\(inos\\|inco\\)\\(/\\|$\\)" actual-fn) 'project-indel)
     ((string-match "/prog\\w*/\\w+\\(/\\|$\\)" actual-fn) 'project-nova)
     ((string-match "/bib\\(/\\|$\\)" actual-fn) 'project-nova)
     ((directory-files (file-name-directory actual-fn) nil ".*\\.el$") 'project-el)
     (t nil))))

;; todo: specify which of the possible multiple 'project files'. Probably the
;; one needed for building the project.
(defun project-file (file-name)
  "Returns path of the associated the project file.
Nil if there is none."
  (let (type (project-root-type file-name))
    (cond
     ((eq type 'project-diebonder-pc)
      (car (directory-files (project-root-dir file-name) nil ".*\\.dsp$")))
     ((eq type 'project-diebonder-rtos)
      (car (directory-files (project-root-dir file-name) nil ".*\\.mpd$")))
     ((eq type 'project-nova)
      (concat "B:/prog/m/makefile"))
     (t nil))))

(defun project-all-dirs (file-name &optional relative-p)
  "Returns a list of directories associated with the project."
  
  ;; whole-list can contain nil's or ""; furtermore it can contain
  ;; duplictes. These addle eggs have to be stripped off.
  
  (let ((dir-whole-list (append
                         (project-src-dirs file-name relative-p)
                         (list (project-unit-test-dir file-name relative-p))
                         (list (project-root-dir file-name relative-p))))
        dir-stripped-list)
    (mapc
     (lambda (x)
       (when (and x (not (equal x "")) (not (member x dir-stripped-list)))
         (setq dir-stripped-list (append dir-stripped-list (list x)))))
     dir-whole-list)
    dir-stripped-list))
    
(defun project-root-dir (file-name &optional relative-p)
  "Returns the path to the root directory of the project the given file is a member of."

  (let (abspath
        (type (project-root-type file-name)))
    (setq abs-path
          (cond
           ((eq type 'project-diebonder-pc)
            (replace-regexp-in-string "\\(/UnitTest\\)?/[^/]*$" "/" file-name))
           ((eq type 'project-diebonder-rtos)
            (replace-regexp-in-string "\\(/sources\\|/unittest\\)?/[^/]*$" "/" file-name))
           ((eq type 'project-nova)
            (replace-regexp-in-string "\\(/source\\|/include\\|/testing\\)?/[^/]*$" "/" file-name))
           (t
            (replace-regexp-in-string "/[^/]*$" "/" file-name))))
    (if relative-p
        (file-relative-name abs-path (file-name-directory file-name))
      abs-path)))

(defun project-src-dirs (file-name &optional relative-p)
  "Returns the path to the source (in contrast to unittest)
  directory of the project the given file is a member of."
  (let (add-rel-paths
        abs-paths
        (root-path (project-root-dir file-name))
        (type (project-root-type file-name)))
    
    (cond
     ((eq type 'project-diebonder-pc)
      (setq add-rel-paths '("")))
     ((eq type 'project-diebonder-rtos)
      (setq add-rel-paths '("Sources"))
      (let ((case-fold-search t))
        (if (string-match "/rtos/pickplace/ppsequencer" root-path)
            (setq add-rel-paths (append "Sources/SeqV2" add-rel-paths)))))
     ((eq type 'project-nova)
      (setq add-rel-paths '("source" "include"))))

    (setq abs-paths (mapcar (lambda (x) (concat root-path x)) add-rel-paths))
              
    ;; if requested make paths relative to directory of file-name
    (if relative-p
        (mapcar
         (lambda (x)
           (file-relative-name x (file-name-directory file-name)))
         abs-paths)
      abs-paths)))

(defun project-unit-test-dir (file-name &optional relative-p)
  "Returns the path to the unittest directory of the project the
  given file is a member of. Return nil when there is none."

  (let (add-rel-path
        abs-path
        (type (project-root-type file-name)))
    
    (cond
     ((eq type 'project-diebonder-pc)
      (setq add-rel-path "unittest/"))
     ((eq type 'project-diebonder-rtos)
      (setq add-rel-path "unittest/"))
     ((eq type 'project-nova)
      (setq add-rel-path "testing/")))

    (setq abs-path (concat (project-root-dir file-name) add-rel-path))

    ;; if requested make paths relative to directory of file-name
    (if relative-p
        (file-relative-name abs-path (file-name-directory file-name))
      abs-path)))

(defun project-files-regexp (file-name)
  "Returns a regexp which matches all files associated with the project the given
  file name is a member of."
  (let ((type (project-root-type file-name)))
    (cond
     ((member type '(project-diebonder-pc project-diebonder-rtos project-nova))
      ".*\\.\\(cpp\\|h\\|idl\\)$")
     ((equal type 'project-el)
      ".*\\.el$")
     (t
      ".*"))))

;;; dired extension for project
; ----------------------------------------------------------------------
(defun project-dired-mark-files ()
  "Add interesting sub directories, and mark all 'interesting'
  files in the resulting directories, kill all other files."
  
  (interactive)
    
  ;; to start from a clean table
  (dired-unmark-all-marks)
  (revert-buffer)
    
  ;; add all additional directories
  (dolist (x (project-all-dirs default-directory))
    (dired-maybe-insert-subdir x))
    
  ;; Kill files not intesting, mark the remaining interesting files
  (dired-mark-files-regexp (project-files-regexp default-directory))
  (dired-toggle-marks)
  (dired-do-kill-lines)
  (dired-toggle-marks))

(defun project-dired-find-main-file ()
  "Finds the 'main' file of the project in current the dired
  buffer, i.e. the file you most probably wan't to open."
  
  (interactive)
  (let (found
        (saved-point (point))
        (type (project-root-type (dired-current-directory))))
    (goto-char 0)
    (forward-line 2)
    (cond
     ((eq type 'project-diebonder-pc)
      (setq found (re-search-forward "\\.idl$" nil t)))
     ((eq type 'project-diebonder-rtos)
      (setq found (or
                   (re-search-forward "\\bSources\\b" nil t)
                   (re-search-forward "\\(Mod\\(ProxyBase\\)??\\|Seq\\(uencer\\)?\\)\\.h\\b" nil t))))
     ((eq type 'project-nova)
      (setq found (or
                   (re-search-forward "\\binclude\\b" nil t)
		   (re-search-forward "\\(handling\\|head\\)\\(rtos\\)?\\b" nil t)))))
    (if found
        (call-interactively 'dired-find-alternate-file)
      (goto-char saved-point)
      (message "No 'main' file found"))))
  
;;; afs 
;; -----------------------------------------------------------------------------
  
(defun compile-ext ()
  "Save all buffers without query and build the current project."
  (interactive)
  (save-some-buffers t)

  (cond
   
   ;; PC projects: use incredibuild
   ;; ----------------------------------------------------------------------
   ((eq (project-root-type) 'project-diebonder-pc)
    (let (src-dsp-path unit-test-dsp-path)
      ;; define src-dsp-path 
      (let* ((src-dir (first (project-src-dirs (buffer-file-name) t)))    
             (src-dir-dsps (directory-files src-dir t ".*\\.dsp$")))
        (cond
         ((equal (length src-dir-dsps) 0)
          (error (concat "No .dsp file found in" src-dir)))          
         ((> (length src-dir-dsps) 1)
          (error (concat "More than one .dsp file in" src-dir))))
        (setq src-dsp-path (car src-dir-dsps)))
      
      ;; define unit-test-dsp-path
      (let* ((unit-test-dir (project-unit-test-dir (buffer-file-name) t))    
             unit-test-dir-dsps)
        (when unit-test-dir
          (setq unit-test-dir-dsps (directory-files unit-test-dir t ".*\\.dsp$"))
          (when (> (length unit-test-dir-dsps) 1)
            (error (concat "More than one .dsp file in" unit-test-dir))) 
          (setq unit-test-dsp-path (car unit-test-dir-dsps))))
      
      ;; assemble dsp's to be build
      (setq compile-command (concat "BuildConsole " src-dsp-path))
      (when unit-test-dsp-path
        (setq compile-command (concat compile-command " && BuildConsole " unit-test-dsp-path)))
      
      ;; actually compile
      (compile compile-command)))
    
    ;; RTOS projects: use make and prepend imd's gnu32 dir to PATH env variable
    ;; ----------------------------------------------------------------------
    ((eq (project-root-type) 'project-diebonder-rtos)
     (let ((saved-path-env (getenv "PATH"))
           (saved-shell-file-name shell-file-name)
           (working-dir (replace-regexp-in-string "/\\(Sources\\(/SeqV2\\)?\\|UnitTest\\)/[^/]*$" "" (buffer-file-name))))
       (setenv "PATH" (concat (getenv "IMD_PATH") "\\bin\\gnu32" path-separator (getenv "PATH")))
       (setq shell-file-name "C:/winnt/system32/cmd.exe")
       
       (compile (concat "/C cd .. && "
                        "C:/imd/bin/make.exe " target " -C " working-dir " TARGET=LocalSAM && "
                        "postbuild.bat"))
       (setenv "PATH" saved-path-env)
       (setq shell-file-name saved-shell-file-name)))

    ;; Nova projects: 
    ;; ----------------------------------------------------------------------
    ;; -- rtos only
    ((and (eq (project-root-type) 'project-nova) (not nova-pc-p))
     (let* ((target "depend all")
            (bib_dir "/home/flka/bib/release/zorro")
            (m_dir "/home/flka/prog/m")
            (envlist
             (concat
              "M_DIR="m_dir " "
              "M_BASE_DIR="m_dir"/base "
              "M_DRIVERS_DIR="m_dir"/drivers_dev "
              "M_BIB_DIR="bib_dir " "
              "OMA6RTOS_CONFIG=" (concat bib_dir "/bin_indel_ppc750/config") " "
              "OMA6RTOS_INC=" (concat bib_dir "/include/oma6rtos") " "
              "OMA6RTOS_LIB=" (concat bib_dir "/bin_indel_ppc750") " "
              "TARGET=RTOS "
              "HOST=LINUX " )))
       (compile (concat
                 "ssh flka@" nova-build-server " '(\n"
                 "cd " (replace-regexp-in-string "/emp8118035/" "/flka/" (project-root-dir (buffer-file-name))) "\n"
                 "make " target " -f makefilertos " envlist " 2>&1 | \\\n"
                 "sed s@/flka/@/emp8118035/@"
                 ")'"))))

    ;; -- pc & rtos
    ((and (eq (project-root-type) 'project-nova) nova-pc-p)
     (when current-prefix-arg
       (custom-set-value-prompt 'nova-compile-target))
     (compile
;      (concat "ssh " nova-build-server " /bin/bash -c \"'cd ~/prog/nova && mmake " nova-compile-target  "'\"")
      (concat "cd ~/prog/nova && set -o pipefail && mmake " nova-compile-target " 2>&1 | sed 's@/home/emp8118035/prog/nova/@@' ")
      )) 

    ;; unkown project type
    ;; ----------------------------------------------------------------------
    (t (error "Unknown project"))))

  
; grep - exension for project
; --------------------------------------------------------------

(defun project-find-ii-filen-name ()
  "Returns the item id file-name of the project the current buffer belongs to."
  (let* ((dir-name (buffer-file-name)))))

;; Based on the passed string, returns a list where the first element is a
;; string containing paths to directorys/files to be searched, and the 2nd
;; element is a regexp understood by the -iregex option of the gnu find command
;; line tool.
(defun project-find-parse-modifier (modifier initial-search-regex)

  (let (find-in        ; in which dirs/files find searches, strings separated by blanks
        find-regexp    ; argument to find's -iregexp option, e.g. "foo.*bar"
        (type (project-root-type))
        (search-regexp initial-search-regex)) ; the regex to be searched for within the files found by find-in/find-regexp
    
    ;; 1st round
    (cond
     ;; no modifier given : choose a default
     ((nil-or-empty-p modifier)
      (setq modifier
            (cond
             ((member type '(project-diebonder-pc project-diebonder-rtos project-nova))
              "hci")
             ((equal type 'project-el)
              "e")
             (t "e"))))
     
     ;; indirect search : a special file contains the regexp to be searched for. 
     ((or (equal modifier "ii") (equal modifier "dc"))
      (save-excursion
        (let* ((case-fold-search t)
               (pattern (if (equal modifier "ii") ".*\\(mod\\|seq\\)itemids\\sw*\\.h$" ".*diagcond\\sw*\\.h$"))
               (file-names (directory-files (nth 0 (project-src-dirs (buffer-file-name))) t pattern))
               (file-name (nth 0 file-names))
               identifier)
          
          (when (> (length file-names) 1)
            (error (concat "Multiple files matches the pattern '" pattern "'")))
          (when (nil-or-empty-p file-name)
            (error (concat "No file matches the pattern '" pattern "'")))
          
          (set-buffer (generate-new-buffer "temp"))
          (insert-file-contents-literally file-name)
          (re-search-forward (concat "\\(\\(\\w\\|_\\)+\\)\\s-*=\\s-*" initial-search-regex "\\b") nil t)
          (setq identifier (match-string-no-properties 1))
          (kill-buffer (current-buffer))

          (if identifier
              (setq search-regexp
                    (concat "\\b" identifier "\\b|\\b" initial-search-regex "\\b"))
            (message (concat "Can't find " initial-search-regex " in " file-name ". Searching only for " initial-search-regex))
            (setq search-regexp (concat "\\b" initial-search-regex "\\b")))
          (setq modifier "hci")))))

    ;; 2nd round
    (cond
     ;; search only within the current file
     ((equal modifier "f")
      (setq find-in (file-relative-name (buffer-file-name))))
    
     ;; search within this, i.e. the current file and its other file - in other words
     ;; the current class
     ((equal modifier "t")
      (setq find-in (file-relative-name (buffer-file-name)))
      (save-excursion
        (ff-find-other-file)
        (setq find-in
              (concat find-in " " (file-relative-name (buffer-file-name))))
        (ff-find-other-file)))
     
     ;; search files with given file extension within the directories of the current project
     ;; c -> .cpp , h -> .h , i -> .idl
     (t

      ;; all directories of the project are to be searched
      (setq find-in
            (mapconcat
             (lambda (x) (shell-quote-argument x))
             (project-all-dirs (buffer-file-name) t)
             " "))
      
      ;; determine actual file extensions to be searched for
      (setq file-ext-regexp
            (mapconcat
             (lambda (c) (cdr (assoc c '((?c . "cpp") (?h . "h") (?i . "idl") (?e . "el")))))
             modifier
             "\\|"))
      (if (equal (length file-ext-regexp) 0)
          (error "No file extensions to search for"))
      (setq find-regexp (shell-quote-argument (concat ".*\\(" file-ext-regexp "\\)")))))

    ;; return
    (list find-in find-regexp search-regexp)))

(defun project-grep-find ()
  "As grep-find, however sets grep-find-command, i.e. the default
  argument to grep-find, adaptively to current project."
  
  (interactive)
  (let* ((tmp (project-find-parse-modifier "" ""))
         (find-in (nth 0 tmp))   
         (find-regexp (nth 1 tmp)) ;; argument to find's -iregexp option, e.g. "foo.*bar"
         (find-regexp-option       ;; whole option, e.g. "-iregexp 'foo.*bar'
          (if (nil-or-empty-p find-regexp)
              ""
            (concat "-iregex " find-regexp))))
    
    (setq grep-find-command
          (concat
           "find " find-in " -maxdepth 1 " find-regexp-option " -print0 "
           "| xargs -0 "
           "grep --color=always -nPe ''"))
    (call-interactively 'grep-find)))

(defun project-grep-find-ext (regexp regexp-modifier find-modifier)
  "Runs perl-grep via find. Collect output in a buffer.

Searches lines matching the perl REGEXP (with modifier REGEXP-MODIFIER) in
files specified by FIND-MODIFIER.

Values for FIND-MODIFIER. Values having an x in the 2nd colum can not be mixed
with other values, i.e. they only cand stand by themselves.
f:x: Current file
t:x: Current class (this)
h: : all header files
c: : all source files
i: : all idl files
e: : all el files"
  
  (interactive "sRegexp : \nsRegexp-modifier : \nsFind-modifier : " )
  
  (let* ((tmp (project-find-parse-modifier find-modifier regexp))
         (regexp (nth 2 tmp))   
         (find-in (nth 0 tmp))   
         (find-regexp (nth 1 tmp)) ;; argument to find's -iregexp option, e.g. "foo.*bar"
         (find-regexp-option       ;; whole option, e.g. "-iregexp 'foo.*bar'"
          (if (nil-or-empty-p find-regexp)
              ""
            (concat "-iregex " find-regexp))))
    
    (grep-find
     (concat
      
      ;; compose list of files whose contents is to be searched
      find-program " " find-in " -maxdepth 1 " find-regexp-option " -print0 "
           
      ;; pass that list as command line argument to ...
      "| xargs -0 "

      ;; ... grep which searches these files for the given regexp
      "grep --color=always -nP" regexp-modifier " -e " (shell-quote-argument regexp) " /dev/null"))))

(defun project-grep-find-sexp-at-point ()
  "Finds sexp under point in current project."
  (interactive)
  (let (name end)
    (save-excursion
      (cond
       ((save-excursion
          (beginning-of-line)
          (looking-at "static\\s-+const\\s-+\\(tItemId\\|tDiagCondId\\)\\s-+\\w+"))
        (beginning-of-line)
        (forward-sexp 4))
       ((looking-at "\\_>")) ;; point is already at end of word, do nothing
       (t (forward-sexp)))  ;; goto end of word
      (setq end (point))
      (backward-sexp)
      (setq name (buffer-substring-no-properties (point) end)))
    (project-grep-find-ext (concat "\\b" name "\\b") "" "")))

;;; project.el ends here
