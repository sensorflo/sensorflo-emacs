;;; mode-hooks.el --- sensorflo's mode hooks
;;
;;; Commentary
;;
;; See init.el for a summary of how to customize Emacs.
;;
;;
;; autoload support
;; ----------------
;;
;; - Since add-hook creates hook if the passed hook is void, the passed hook
;;   symbol must not yet exist, i.e. be loaded.
;; - Maybe chop mode-hooks.el into multiple files, one per hook. Then autload
;;   those small files at the time they are really needed.
;;
;;; Code

(require 'rx)
(require 'font-lock-ext) ; https://github.com/sensorflo/font-lock-ext/

;;; programming modes 
;;; ===================================================================

;;; shell
;; ----------------------------------------------
(add-hook 'sh-mode-hook 'my-sh-mode-hook)
(defun my-sh-mode-hook ()
  (ansi-color-for-comint-mode-on)
  (my-sh-mode-bindings))
  
;; todo: think about moving all bindings in this buffer to mybindings.el
(defun my-sh-mode-bindings()
  (require 'tempos-bash)

  ;; control flow
  (local-set-key [(control ?\,)(c)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(control ?/)] 'tempo-complete-tag)

  (local-set-key [(control ?\,)(c)(i)] 'tempo-template-bash-if)
  (local-set-key [(control ?\,)(c)(e)] 'tempo-template-bash-elif)
  (local-set-key [(control ?\,)(c)(f)] 'tempo-template-bash-for)
  (local-set-key [(control ?\,)(c)(w)] 'tempo-template-bash-while)
  (local-set-key [(control ?\,)(c)(s)] 'tempo-template-bash-case) ; s for switch
  (local-set-key [(control ?\,)(c)(c)] 'tempo-template-bash-case-clause) 
  
  ;; definitions
  (local-set-key [(control ?\,)(d)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(d)(s)] 'tempo-template-bash-def-sub) ;s for subroutin
  (local-set-key [(control ?\,)(d)(f)] 'tempo-template-bash-def-sub) ;f for function
  (local-set-key [(control ?\,)(d)(m)] 'tempo-template-bash-def-sub) ;m for method
  )

;;; php
;; ----------------------------------------------
(add-hook 'php-mode-hook 'my-php-mode-hook)

(defun my-php-mode-hook ()
  )

;;; perl
;; ----------------------------------------------
(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)
(defun my-cperl-mode-hook ()
  (my-cperl-mode-bindings))
  
(defun my-cperl-mode-bindings()
  (require 'tempos-perl)

  (local-set-key [(f5)] 'perldb)
  (local-set-key [(f7)] 'cperl-check-syntax)
  (local-set-key [(f8)] 'cperl-check-syntax)
  
  (local-set-key [(control ?\,)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(control ?/)] 'tempo-complete-tag)

  ;; control flow
  ;; todo: else
  (local-set-key [(control ?\,)(c)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(c)(i)] 'tempo-template-perl-if)
  (local-set-key [(control ?\,)(c)(e)] 'tempo-template-perl-elsif)
  (local-set-key [(control ?\,)(c)(f)] 'tempo-template-perl-for)
  (local-set-key [(control ?\,)(c)(w)] 'tempo-template-perl-while)
  (local-set-key [(control ?\,)(c)(c)] 'tempo-template-perl-continue)
  
  ;; definitions
  (local-set-key [(control ?\,)(d)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(d)(s)] 'tempo-template-perl-def-sub) ;s for subroutin
  (local-set-key [(control ?\,)(d)(f)] 'tempo-template-perl-def-sub) ;f for function
  (local-set-key [(control ?\,)(d)(m)] 'tempo-template-perl-def-sub) ;m for method
  )

;;; python
;; ----------------------------------------------
(add-hook 'python-mode-hook 'my-python-mode-hook)
(defun my-python-mode-hook ()
  (hs-minor-mode t)
  (outline-minor-mode t)
  (if (featurep 'subword)
      (subword-mode t))
  (my-python-mode-bindings))

(defun my-python-mode-bindings ()
  (require 'tempos-python)

  ;; (local-set-key [(f5)] 'python-debug)
  (local-set-key [(f7)] 'python-check)
  (local-set-key [(f8)] 'python-check)
  
  (local-set-key [(control ?\,)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(control ?/)] 'tempo-complete-tag)

  ;; control flow
  ;; todo: else
  (local-set-key [(control ?\,)(c)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(c)(i)] 'tempo-template-py-if)
  (local-set-key [(control ?\,)(c)(e)] 'tempo-template-py-elsif)
  (local-set-key [(control ?\,)(c)(f)] 'tempo-template-py-for)
  (local-set-key [(control ?\,)(c)(w)] 'tempo-template-py-while)
  (local-set-key [(control ?\,)(c)(c)] 'tempo-template-py-continue)
  
  ;; definitions
  (local-set-key [(control ?\,)(d)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(d)(c)] 'tempo-template-py-def-class)
  (local-set-key [(control ?\,)(d)(s)] 'tempo-template-py-def-func) ;s for subroutin
  (local-set-key [(control ?\,)(d)(f)] 'tempo-template-py-def-func) ;f for function
  (local-set-key [(control ?\,)(d)(m)] 'tempo-template-py-def-func) ;m for method
  )


;;; c common mode
;; ----------------------------------------------

;; the first items in the list overwrite the settings made in the later items of
;; the list

(add-hook 'c-mode-common-hook 'default-c-mode-common-hook)

;; defaults for all projects
(defun default-c-mode-common-hook ()
  (message "default-c-mode-common-hook")
  (auto-fill-mode t)
  (c-setup-filladapt)
  (hs-minor-mode t)
  (setq filladapt-token-table (append filladapt-token-table (list (list " *[@\\]\\w+\\b" 'bullet)))) 
  (setq comment-start-skip "\\(//[/!]*\\|/\\*[*!]*\\)\\s-*") ;should be part of doxymacs?
  (when (null comment-end-skip)
    (setq comment-end-skip "\\s-*\\*+/"))
  (abbrev-mode 0)
  (doxymacs-mode 1)
  (doxymacs-font-lock)

  (font-lock-add-keywords nil
    (list
     ;; C++11
     "\\_<static_assert\\_>" "\\_<alignas\\_>" "\\_<alignof\\_>" "\\_<decltype\\_>" "\\_<constexpr\\_>"
     "\\_<noexcept\\_>" "\\_<thread_local\\_>"
     (list "\\_<\\(?:char\\(?:16\\|32\\)_t\\|nullptr_t\\)\\_>" '(0 font-lock-type-face))
     (list "\\_<nullptr\\_>" '(0 font-lock-constant-face))
     
     ;; MS visual studio
     "\\_<__abstract\\_>" "\\_<__alignof\\_>" "\\_<__asm\\_>" "\\_<__assume\\_>"
     "\\_<__based\\_>" "\\_<__box\\_>" "\\_<__cdecl\\_>" "\\_<__declspec\\_>"
     "\\_<__delegate\\_>" "\\_<__event\\_>" "\\_<__except\\_>"
     "\\_<__fastcall\\_>" "\\_<__finally\\_>" "\\_<__forceinline\\_>"
     "\\_<__gc\\_>" "\\_<__hook\\_>" "\\_<__identifier\\_>" "\\_<__if_exists\\_>"
     "\\_<__if_not_exists\\_>" "\\_<__inline\\_>" "\\_<__int8\\_>"
     "\\_<__int16\\_>" "\\_<__int32\\_>" "\\_<__int64\\_>" "\\_<__interface\\_>"
     "\\_<__leave\\_>" "\\_<__m64\\_>" "\\_<__m128\\_>" "\\_<__m128d\\_>"
     "\\_<__m128i\\_>" "\\_<__multiple_inheritance\\_>" "\\_<__nogc\\_>"
     "\\_<__noop\\_>" "\\_<__pin\\_>" "\\_<__property\\_>" "\\_<__raise\\_>"
     "\\_<__sealed\\_>" "\\_<__single_inheritance\\_>" "\\_<__stdcall\\_>"
     "\\_<__super\\_>" "\\_<__try\\_>" "\\_<__except\\_>" "\\_<__try\\_>"
     "\\_<__finally\\_>" "\\_<__try_cast\\_>" "\\_<__unaligned\\_>"
     "\\_<__unhook\\_>" "\\_<__uuidof\\_>" "\\_<__value\\_>"
     "\\_<__virtual_inheritance\\_>" "\\_<__w64\\_>" "\\_<__wchar_t\\_>"))

  (subword-mode t)

  (setq c-basic-offset 2)
  ;; (set (make-local-variable 'compilation-error-regexp-alist)
  ;; '(gnu-sensorflo gcc-include))
  (setq compilation-error-regexp-alist '(gnu-sensorflo gcc-include doxygen))
  (outline-c-mode-common)
  (my-c-mode-common-bindings))

(defun outline-c-mode-common()
  (if (c-src-buffer-p)
      (progn
        (setq outline-regexp
              (concat
               "\\(" c++-cpp-outline-level1-regex "\\)\\|"
               "\\(" c++-cpp-outline-level2-regex "\\)\\|"
               "\\(" c++-cpp-outline-level3-regex "\\)\\|"
               "\\(" c++-cpp-outline-level4-regex "\\)"))
        (set (make-local-variable 'outline-level) 'c++-cpp-outline-level))

    (setq outline-regexp
          (concat
           "\\(" c++-h-outline-level1-regex "\\)\\|"
           "\\(" c++-h-outline-level2-regex "\\)\\|"
           "\\(" c++-h-outline-level3-regex "\\)\\|"
           "\\(" c++-h-outline-level4-regex "\\)"))
    (set (make-local-variable 'outline-level) 'c++-h-outline-level)))

(setq c++-cpp-outline-level1-regex "^\\S-.*\\w+\\s-*::\\s-*\\w+.*(" ) 
(setq c++-cpp-outline-level2-regex "xxxblabliblabluxxx2" ) 
(setq c++-cpp-outline-level3-regex "xxxblabliblabluxxx3" ) 
(setq c++-cpp-outline-level4-regex "xxxblabliblabluxxx4" ) 

;; TODO: separate into C++ / doxy / dragon regexes
(setq c++-h-outline-level1-regex "\\(struct\\|class\\)\\s-*\\sw+\\s-*[^;]*$" ) 
(setq c++-h-outline-level2-regex "\\s-*\\(private\\|public\\|protected\\)\\s-*:" )
(setq c++-h-outline-level31-regex "\\s-*\\(//+\\|/\\**!?\\)\\s-*\\([\\\\@]name\\|misc\\)" ) ; groups
(setq c++-h-outline-level32-regex "\\s-*//[^/]\\|\\s-*/\\*[^*]" ) ; pseudo-sub groups
(setq c++-h-outline-level3-regex (concat "\\(" c++-h-outline-level31-regex "\\)\\|\\(" c++-h-outline-level32-regex "\\)"))
(setq c++-h-outline-level41-regex "\\s-*\\(\\s-\\|\\sw\\|[~*&_:<>]\\)+\\(;\\|(\\(\\s-*)\\s-*\\(const\\s-*\\)?;\\)?\\|{\\)\\s-*\\(//.*\\|/\\*.*\\)?$")
(setq c++-h-outline-level42-regex "\\s-*\\sw+\\s-*(.*)\\s-*;\\s-*$" ) ; like FOO( arg1, arg2 );
(setq c++-h-outline-level43-regex "\\s-*\\sw+\\s-*(\\s-*\\sw+\\s-*)\\s-*" ) ; like FOO( name ) ( ... 
(setq c++-h-outline-level4-regex (concat "\\(" c++-h-outline-level41-regex "\\)\\|\\(" c++-h-outline-level42-regex "\\)\\|\\(" c++-h-outline-level43-regex "\\)"))

(defun c++-cpp-outline-level ()
  (cond
   ((looking-at c++-cpp-outline-level1-regex) 1)
   ((looking-at c++-cpp-outline-level2-regex) 2)
   ((looking-at c++-cpp-outline-level3-regex) 3)
   ((looking-at c++-cpp-outline-level4-regex) 4)
   (t 5)))

(defun c++-h-outline-level ()
  (cond
   ((looking-at c++-h-outline-level1-regex) 1)
   ((looking-at c++-h-outline-level2-regex) 2)
   ((looking-at c++-h-outline-level3-regex) 3)
   ((looking-at c++-h-outline-level4-regex) 4)
   (t 5)))

(defun my-c-mode-common-bindings ()
  ;; overide existing bindings with similar functionality
  (local-set-key [remap newline] 'c-context-line-break)
  (local-set-key [remap narrow-to-defun] 'c-narrow-to-function-incl-comment)
  (local-set-key [remap yank] 'yank-and-indent)
  (local-set-key [remap c-mark-function] 'c-mark-function-incl-comment)
  (local-set-key [remap mark-pagee] 'c-mark-block)
  (local-set-key [remap c-beginning-of-defun] 'c-beginning-of-defun-ext) 
  (local-set-key [remap c-end-of-defun] 'c-end-of-defun-ext)

  ;; jump
  (local-set-key [(control ?\.)(o)] 'c-goto-other-defun)
  (local-set-key [(control ?\.)(d)] 'c-goto-declaration)
  (local-set-key [(control ?\.)(D)] 'c-goto-specific-defun-name) 
  (local-set-key [(control ?\.)(n)] 'c-forward-defun-name) 
  (local-set-key [(control ?\.)(p)] 'c-beginning-of-defun-param-list)
  (local-set-key [(control ?\.)(b)] 'c-beginning-of-defun-body) 
  (local-set-key [(control ?\.)(r)] 'c-recenter-defun-or-region) 
  (local-set-key [(control ?\.)(c)] 'c-goto-class-declaration) 
  
  ;; misc
  (local-set-key [(control ?\,)(control ?/)] 'tempo-complete-tag)
  (local-set-key [(control f)(p)] 'remove-parantheses)
  (local-set-key [(f10)] 'c-copy-signature)
  (local-set-key [(control x)(n)(b)] 'c-narrow-to-block)

  (my-c-mode-common-bindings-tempo))

;; remember that dragon.el, nova.el etc will add further bindings
(defun my-c-mode-common-bindings-tempo ()
  (require 'tempos-c++)

  ;; flow control
  (let ((map (make-sparse-keymap)))
    (local-set-key [(control ?\,)(c)] map)
    (local-set-key [(control ?\,)(f)] map))
  (local-set-key [(control ?\,)(c)(i)] 'tempo-template-c-if)
  (local-set-key [(control ?\,)(c)(e)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(c)(e)(e)] 'tempo-template-c-else)
  (local-set-key [(control ?\,)(c)(e)(i)] 'tempo-template-c-else-if)
  (local-set-key [(control ?\,)(c)(f)] 'tempo-template-doremi-c-for) 
  (local-set-key [(control ?\,)(c)(w)] 'tempo-template-c-while)
  (local-set-key [(control ?\,)(c)(d)] 'tempo-template-c-do)
  (local-set-key [(control ?\,)(c)(s)] 'tempo-template-c-switch)
  (local-set-key [(control ?\,)(c)(c)] 'tempo-template-c-case)
  (local-set-key [(control ?\,)(c)(d)] 'tempo-template-c-default) 
  (local-set-key [(control ?\,)(c)(t)] 'tempo-template-c-try) 
  (local-set-key [(control ?\,)(c)(k)] 'tempo-template-c-catch) 
  (local-set-key [(control ?\,)(c)(b)] 'tempo-template-c-block)

  ;; comments
  (local-set-key [(control ?\,)(k)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(k)(k)] 'tempo-template-c-comment-block) 
  (local-set-key [(control ?\,)(k)(b)] 'tempo-template-c-comment-block) 
  (local-set-key [(control ?\,)(k)(g)] 'tempo-template-c-member-group-named)
  (local-set-key [(control ?\,)(k)(G)] 'tempo-template-c-member-group)
  (local-set-key [(control ?\,)(k)(u)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(k)(u)(b)] 'tempo-template-c-utf-block)
  (local-set-key [(control ?\,)(k)(u)(f)] 'tempo-template-c-utf-forwards)
  (local-set-key [(control ?\,)(k)(u)(i)] 'tempo-template-c-utf-includes)

  ;; preprocessor
  (local-set-key [(control ?\,)(p)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(p)(i)] 'tempo-template-c-include) 
  (local-set-key [(control ?\,)(p)(I)] 'tempo-template-c-include-system) 
  
  ;; names / ids
  (local-set-key [(control ?\,)(n)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(n)(n)] 'insert-class-name-dwim)
  (local-set-key [(control ?\,)(n)(b)] 'insert-base-class-name-with-scope)

  ;; text
  (local-set-key [(control ?\,)(t)] (make-sparse-keymap))
  
  ;; log / messages
  (local-set-key [(control ?\,)(l)] (make-sparse-keymap))
  
  ;; error
  (local-set-key [(control ?\,)(e)] (make-sparse-keymap))
  
  ;; definitions / declarations
  (local-set-key [(control ?\,)(d)] (make-sparse-keymap))

  ;; statements
  (local-set-key [(control ?\,)(s)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(s)(?\;)] 'tempo-template-c-statement-common-ehr)

  ;; expressions
  (local-set-key [(control ?\,)(e)] (make-sparse-keymap))

  ;; misc / whole-fragments
  (local-set-key [(control ?\,)(m)] (make-sparse-keymap))


  ;; (local-set-key [(control ?\,)(N)] 'tempo-template-c-delete)
  )

;; idl mode
;; ------------------------------------------------------------------
(setq idl-outline-level1-regex "[i]nterface\\|[l]ibrary" ) 
(setq idl-outline-level2-regex "\\s-*\\(/\\*+!?\\|//+\\)\\s-*\\(@name\\b\\|misc\\)" )  
(setq idl-outline-level3-regex "\\(\\s-+\\sw+\\)\\{,4\\}\\s-*[({]\\s-*\\()\\s-*;\\s-*\\)?$" )

(defun my-idl-mode-hook ()
  (interactive)
  (outline-minor-mode t)
  (set (make-local-variable 'outline-regexp)
       (concat
        "\\(" idl-outline-level1-regex "\\)\\|"
        "\\(" idl-outline-level2-regex "\\)\\|"
        "\\(" idl-outline-level3-regex "\\)" ))
  (set (make-local-variable 'outline-level) 'idl-outline-level))

(defun idl-outline-level ()
  (cond
   ((looking-at idl-outline-level1-regex) 1)
   ((looking-at idl-outline-level2-regex) 2)
   ((looking-at idl-outline-level3-regex) 3)
   (t 4)))

(add-hook 'idl-mode-hook 'my-idl-mode-hook)

;;; cppkoans-mode
; ------------------------------------------------------------------
(defun my-cppkoans-mode-hook ()
  (set (make-local-variable 'compile-command)
       "cd ~/src/cppkoans/build/make && make")
  (local-set-key [(f3)] 'cppkoans-forward-edit-field)
  (local-set-key [(shift f3)] 'cppkoans-backward-edit-field))

(add-hook 'cppkoans-mode-hook 'my-cppkoans-mode-hook)

(add-hook 'find-file-hook
          (lambda() (if (cppkoans-koans-buffer-p) (cppkoans-mode))))


;;; emacs lisp / lisp / lisp interacton major mode
; ------------------------------------------------------------------
(let ((hook-list      
       (list
        'lisp-mode-hook
        'lisp-interaction-mode
        'emacs-lisp-mode-hook))
      hook)
  (while hook-list
    (setq hook (car hook-list))
    (add-hook hook 'my-gen-lisp-hook)
    (setq hook-list (cdr hook-list))))

(defun my-gen-lisp-hook ()
  (hs-minor-mode t)
  (outline-minor-mode t)
  (setq tab-width 8)
  
  (local-set-key [(meta m)(meta m)] 'kmacro-start-stop-macro-ext)
  
  (local-set-key [(control ?\,)] (make-sparse-keymap))

  (setq paragraph-start "\f\\|[ \t]*$\\|^[ \t]*\"")
  
  (font-lock-add-keywords nil (list
     (list (concat "^\\s-*(\\s-*def\\w*\\s-*[^ \t(]+\\s-*([^)]*)\\s-*"
		   "\\(?:\n\\s-*\\)?\"\\(\\)")
	   '(1 font-lock-unimportant t)) 
     ))

  ;; (require 'autopair)
  ;; (push '(?` . ?') (getf autopair-extra-pairs :code))

  (when (eq major-mode 'emacs-lisp-mode)
    (require 'tempos-elisp))

  ;; control flow
  (local-set-key [(control ?\,)(c)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(c)(i)] 'tempo-template-elisp-if)
  (local-set-key [(control ?\,)(c)(w)] 'tempo-template-elisp-while)

  ;; definitions
  (local-set-key [(control ?\,)(d)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(d)(f)] 'tempo-template-elisp-defun) ;f for function
  (local-set-key [(control ?\,)(d)(d)] 'tempo-template-elisp-defun) ;f for function
  
  ;; misc
  (local-set-key [(control ?\,)(m)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(m)(l)] 'tempo-template-elisp-let))

;;; stream / dt2
;; ----------------------------------------------
(add-hook 'stream-mode-hook 'my-stream-mode-hook)
(defun my-stream-mode-hook ()
  (require 'tempos-stream)
  (doxymacs-mode 1)
  (doxymacs-font-lock))

(add-hook 'dt2-mode-hook 'my-dt2-mode-hook)
(defun my-dt2-mode-hook ()
  (outline-minor-mode t))


;;; conf mode 
;; ----------------------------------------------
(defun my-conf-mode-hook()
  )

(add-hook 'conf-mode-hook 'my-conf-mode-hook)

;;; visual basic .NET
;; ----------------------------------------------
(add-hook 'vbnet-mode-hook 'my-vbnet-mode-hook)
(defun my-vbnet-mode-hook ()
  (setq indent-tabs-mode nil)

  (require 'tempos-vbnet)
  (require 'tempos-xml-doc)

  (font-lock-add-keywords nil (list
     (list "<[^>]+?>" '(0 font-lock-semi-unimportant prepend)) 
     ))

  (local-set-key [(return)] 'indent-new-comment-line)

  ;; control flow
  (local-set-key [(control ?\,)(c)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(c)(i)] 'tempo-template-vbnet-if)
  (local-set-key [(control ?\,)(c)(e)] 'tempo-template-vbnet-elsif)
  (local-set-key [(control ?\,)(c)(f)] 'tempo-template-vbnet-for)
  (local-set-key [(control ?\,)(c)(a)] 'tempo-template-vbnet-for-each)
  (local-set-key [(control ?\,)(c)(w)] 'tempo-template-vbnet-while)

  ;; comment
  (local-set-key [(control ?\,)(k)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(k)(s)] 'tempo-template-xml-doc-summary) 
  (local-set-key [(control ?\,)(k)(p)] 'tempo-template-xml-doc-param))
  
;;; autoexp
;; ----------------------------------------------
(add-hook 'autoexp-mode-hook 'my-autoexp-mode-hook)
(defun my-autoexp-mode-hook ()
  (outline-minor-mode t)
  (setq indent-tabs-mode t))

(add-hook 'before-save-hook 'my-autoexp-before-save-hook t)
(defun my-autoexp-before-save-hook ()
  (when (eq major-mode 'autoexp-mode)
    (save-restriction
      (widen)
      (if indent-tabs-mode
          (tabify (point-min) (point-max))
        (untabify (point-min) (point-max)))
      (delete-trailing-whitespace))))

;;; graphviz-dot
;; ----------------------------------------------
(add-hook 'graphviz-dot-mode-hook 'my-graphviz-dot-mode-hook)
(defun my-graphviz-dot-mode-hook ()
  (set (make-local-variable 'compile-command)
       (let* ((srcfilename (if (buffer-file-name)
                                 (file-name-nondirectory (buffer-file-name))
                               (buffer-name)))
              (outfilename
               (concat (file-name-sans-extension (file-name-nondirectory srcfilename))
                       ".png")))
         (concat "dot -Tpng " srcfilename " >" outfilename " && display " outfilename))))

;;; text modes 
;;; ===================================================================

;;; html helper minor mode
;; -------------------------------------------------------------------
(add-hook 'html-helper-mode-hook '(lambda () (font-lock-mode 1)))

;;; (la)tex
;; --------------------------------------------------------------------------------
(add-hook 'tex-mode-hook 'my-latex-hook)

(defun my-latex-hook ()
  (outline-minor-mode t)
  (my-latex-bindings))

(defun my-latex-bindings ()
  (require 'tempos-latex)

  (local-set-key [(control j)] 'backward-char)
  
  (local-set-key [(control c)(control o)(d)] 'latex-ext-dec-level)
  (local-set-key [(control c)(control o)(i)] 'latex-ext-inc-level)
  (local-set-key [(control c)(u)] 'latex-ext-up)
  
  (local-set-key [(meta ?\,)] (make-sparse-keymap))
  (local-set-key [(meta ?\,)(p)] 'latex-ext-remove-surrounding)
  (local-set-key [(meta ?\,)(s)] 'latex-ext-remove-surrounding)

  (local-set-key [(control ?\,)] (make-sparse-keymap))
  
  ;; (greek) symbols
  ;; xi is on o, and omicron itself can be written using regular o and O
  (local-set-key [(control ?\,)(s)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(s)(a)] '(lambda () (interactive) (insert "\\alpha")))
  (local-set-key [(control ?\,)(s)(b)] '(lambda () (interactive) (insert "\\beta")))
  (local-set-key [(control ?\,)(s)(c)] '(lambda () (interactive) (insert "\\gamma"))) ;!
  (local-set-key [(control ?\,)(s)(d)] '(lambda () (interactive) (insert "\\delta")))
  (local-set-key [(control ?\,)(s)(e)] '(lambda () (interactive) (insert "\\epsilon")))
  (local-set-key [(control ?\,)(s)(f)] '(lambda () (interactive) (insert "\\zeta"))) ;!
  (local-set-key [(control ?\,)(s)(g)] '(lambda () (interactive) (insert "\\eta")))  ;!
  (local-set-key [(control ?\,)(s)(h)] '(lambda () (interactive) (insert "\\theta"))) ;!
  (local-set-key [(control ?\,)(s)(i)] '(lambda () (interactive) (insert "\\iota")))
  (local-set-key [(control ?\,)(s)(k)] '(lambda () (interactive) (insert "\\kappa")))
  (local-set-key [(control ?\,)(s)(l)] '(lambda () (interactive) (insert "\\lambda")))
  (local-set-key [(control ?\,)(s)(m)] '(lambda () (interactive) (insert "\\mu")))
  (local-set-key [(control ?\,)(s)(n)] '(lambda () (interactive) (insert "\\nu")))
  (local-set-key [(control ?\,)(s)(o)] '(lambda () (interactive) (insert "\\xi"))) ;!
  (local-set-key [(control ?\,)(s)(p)] '(lambda () (interactive) (insert "\\pi")))
  (local-set-key [(control ?\,)(s)(r)] '(lambda () (interactive) (insert "\\rho"))) ; ! (upercase)
  (local-set-key [(control ?\,)(s)(s)] '(lambda () (interactive) (insert "\\sigma")))
  (local-set-key [(control ?\,)(s)(t)] '(lambda () (interactive) (insert "\\tau")))
  (local-set-key [(control ?\,)(s)(u)] '(lambda () (interactive) (insert "\\upsilon")))
  (local-set-key [(control ?\,)(s)(v)] '(lambda () (interactive) (insert "\\phi"))) ;!
  (local-set-key [(control ?\,)(s)(x)] '(lambda () (interactive) (insert "\\chi"))) ;!
  (local-set-key [(control ?\,)(s)(y)] '(lambda () (interactive) (insert "\\psi"))) ;!
  (local-set-key [(control ?\,)(s)(z)] '(lambda () (interactive) (insert "\\omega"))) ;!
  
  (local-set-key [(control ?\,)(s)(A)] '(lambda () (interactive) (insert "A")))
  (local-set-key [(control ?\,)(s)(B)] '(lambda () (interactive) (insert "B")))
  (local-set-key [(control ?\,)(s)(C)] '(lambda () (interactive) (insert "\\Gamma")))
  (local-set-key [(control ?\,)(s)(D)] '(lambda () (interactive) (insert "\\Delta")))
  (local-set-key [(control ?\,)(s)(E)] '(lambda () (interactive) (insert "E")))
  (local-set-key [(control ?\,)(s)(F)] '(lambda () (interactive) (insert "Z"))) ;! 
  (local-set-key [(control ?\,)(s)(G)] '(lambda () (interactive) (insert "H")))  ;! 
  (local-set-key [(control ?\,)(s)(H)] '(lambda () (interactive) (insert "\\Theta"))) ;!
  (local-set-key [(control ?\,)(s)(I)] '(lambda () (interactive) (insert "I")))
  (local-set-key [(control ?\,)(s)(K)] '(lambda () (interactive) (insert "K")))
  (local-set-key [(control ?\,)(s)(L)] '(lambda () (interactive) (insert "\\Lambda")))
  (local-set-key [(control ?\,)(s)(M)] '(lambda () (interactive) (insert "M")))
  (local-set-key [(control ?\,)(s)(N)] '(lambda () (interactive) (insert "N")))
  (local-set-key [(control ?\,)(s)(O)] '(lambda () (interactive) (insert "\\Xi"))) ;!
  (local-set-key [(control ?\,)(s)(P)] '(lambda () (interactive) (insert "\\Pi")))
  (local-set-key [(control ?\,)(s)(R)] '(lambda () (interactive) (insert "P"))) ;!
  (local-set-key [(control ?\,)(s)(S)] '(lambda () (interactive) (insert "\\Sigma")))
  (local-set-key [(control ?\,)(s)(T)] '(lambda () (interactive) (insert "T")))
  (local-set-key [(control ?\,)(s)(U)] '(lambda () (interactive) (insert "\\Upsilon")))
  (local-set-key [(control ?\,)(s)(V)] '(lambda () (interactive) (insert "\\Phi"))) ;!
  (local-set-key [(control ?\,)(s)(X)] '(lambda () (interactive) (insert "X"))) ;!
  (local-set-key [(control ?\,)(s)(Y)] '(lambda () (interactive) (insert "\\Psi"))) ;!
  (local-set-key [(control ?\,)(s)(Z)] '(lambda () (interactive) (insert "\\Omega"))) ;!
  
  (local-set-key [(control ?\,)(control s)(e)] '(lambda () (interactive) (insert "\\varepsilon")))
  (local-set-key [(control ?\,)(control s)(h)] '(lambda () (interactive) (insert "\\vartheta")))
  (local-set-key [(control ?\,)(control s)(p)] '(lambda () (interactive) (insert "\\varpi")))
  (local-set-key [(control ?\,)(control s)(r)] '(lambda () (interactive) (insert "\\varrho")))
  (local-set-key [(control ?\,)(control s)(s)] '(lambda () (interactive) (insert "\\varsigma")))
  (local-set-key [(control ?\,)(control s)(v)] '(lambda () (interactive) (insert "\\varphi")))
  
  (local-set-key [(control ?\,)(s)(control c)] '(lambda () (interactive) (insert "\\chi")))
  (local-set-key [(control ?\,)(s)(control e)] '(lambda () (interactive) (insert "\\eta")))
  (local-set-key [(control ?\,)(s)(control g)] '(lambda () (interactive) (insert "\\gamma")))
  (local-set-key [(control ?\,)(s)(control t)] '(lambda () (interactive) (insert "\\theta")))
  (local-set-key [(control ?\,)(s)(control o)] '(lambda () (interactive) (insert "\\omega")))
  (local-set-key [(control ?\,)(s)(control r)] '(lambda () (interactive) (insert "\\rho")))
  (local-set-key [(control ?\,)(s)(control x)] '(lambda () (interactive) (insert "\\xi")))
  (local-set-key [(control ?\,)(s)(control z)] '(lambda () (interactive) (insert "\\zeta")))
                                 
  (local-set-key [(control ?\,)(s)(control C)] '(lambda () (interactive) (insert "X"))) ; chi
  (local-set-key [(control ?\,)(s)(control E)] '(lambda () (interactive) (insert "H")))  ;  eta 
  (local-set-key [(control ?\,)(s)(control G)] '(lambda () (interactive) (insert "\\Gamma"))) 
  (local-set-key [(control ?\,)(s)(control T)] '(lambda () (interactive) (insert "\\Theta"))) 
  (local-set-key [(control ?\,)(s)(control O)] '(lambda () (interactive) (insert "\\Omega"))) 
  (local-set-key [(control ?\,)(s)(control R)] '(lambda () (interactive) (insert "P"))) ; rho
  (local-set-key [(control ?\,)(s)(control X)] '(lambda () (interactive) (insert "\\Xi"))) ;
  (local-set-key [(control ?\,)(s)(control Z)] '(lambda () (interactive) (insert "Z"))) ;zeta
  
  (local-set-key [(control ?\,)(f)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(f)(v)] 'tempo-template-latex-verbatim)
  (local-set-key [(control ?\,)(f)(b)] 'tempo-template-latex-bold)
  (local-set-key [(control ?\,)(f)(e)] 'tempo-template-latex-emph)
  (local-set-key [(control ?\,)(f)(m)] 'tempo-template-latex-math)
  (local-set-key [(control ?\,)(f)(t)] 'tempo-template-latex-text-in-math)
  
  (local-set-key [(control ?\,)(b)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(b)(v)] 'tempo-template-latex-verbatim-block)
  (local-set-key [(control ?\,)(b)(m)] 'tempo-template-latex-math-block)
  (local-set-key [(control ?\,)(b)(e)] 'tempo-template-latex-equation)
  
  (local-set-key [(control ?\,)(i)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(i)(i)] 'tempo-template-latex-item)
  (local-set-key [(control ?\,)(i)(b)] 'tempo-template-latex-item)
  (local-set-key [(control ?\,)(i)(d)] 'tempo-template-latex-def-item)
  (local-set-key [(control ?\,)(i)(e)] 'tempo-template-latex-item)
  
  (local-set-key [(control ?\,)(l)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(l)(b)] 'tempo-template-latex-itemize) ; bulleted list
  (local-set-key [(control ?\,)(l)(i)] 'tempo-template-latex-itemize) ; bulleted list
  (local-set-key [(control ?\,)(l)(d)] 'tempo-template-latex-description)
  (local-set-key [(control ?\,)(l)(e)] 'tempo-template-latex-enumeration)
  
  (local-set-key [(control ?\,)(h)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(h)(?0)] 'tempo-template-latex-h0)
  (local-set-key [(control ?\,)(h)(?t)] 'tempo-template-latex-h0)
  (local-set-key [(control ?\,)(h)(?1)] 'tempo-template-latex-h1)
  (local-set-key [(control ?\,)(h)(?2)] 'tempo-template-latex-h2)
  (local-set-key [(control ?\,)(h)(?3)] 'tempo-template-latex-h3)
  (local-set-key [(control ?\,)(h)(?4)] 'tempo-template-latex-h4)
  (local-set-key [(control ?\,)(h)(?5)] 'tempo-template-latex-h5)
  
  ;; misc
  (local-set-key [(control ?\,)(m)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(m)(e)] 'tempo-template-latex-env)
  (local-set-key [(control ?\,)(m)(c)] 'tempo-template-latex-command)
  )

;;; adoc
;; --------------------------------------------------------------------------------
(add-hook 'adoc-mode-hook 'my-adoc-mode-hook)

(defun my-adoc-mode-hook ()
  ;; note that adoc-mode derives from text-mode, thus more is handled there
  (outline-minor-mode t)
  (set (make-local-variable 'compile-command)
       (concat "asciidoc "
               (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (buffer-name))))

  (my-adoc-bindings))

(defun my-adoc-bindings ()
  (require 'tempos-adoc) 

  (local-set-key [f5] 'flyspell-goto-next-error)
  (local-set-key [f6] 'ispell-word)
  (local-set-key [f8] 'adoc-browse-url-output)
  (local-set-key "\C-c\C-p" 'adoc-promote-title)
  (local-set-key "\C-c\C-t" 'adoc-toggle-title-type)

  (local-set-key [(control ?\,)(f)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(f)(c)] 'tempo-template-adoc-code)
  (local-set-key [(control ?\,)(f)(b)] 'tempo-template-adoc-strong)
  (local-set-key [(control ?\,)(f)(e)] 'tempo-template-adoc-emph)

  (local-set-key [(control ?\,)(h)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(h)(?0)] 'tempo-template-adoc-title-0)
  (local-set-key [(control ?\,)(h)(?t)] 'tempo-template-adoc-title-0)
  (local-set-key [(control ?\,)(h)(?1)] 'tempo-template-adoc-title-1)
  (local-set-key [(control ?\,)(h)(?2)] 'tempo-template-adoc-title-2)
  (local-set-key [(control ?\,)(h)(?3)] 'tempo-template-adoc-title-3)
  (local-set-key [(control ?\,)(h)(?4)] 'tempo-template-adoc-title-4))

(add-to-list 'auto-coding-regexp-alist '("\\`\\s-*:encoding:\\s-*UTF-8\\b" . utf-8))


;;; doxym
;; --------------------------------------------------------------------------------
(add-hook 'doxym-mode-hook 'my-doxym-mode-hook)

(defun my-doxym-mode-hook ()
  (outline-minor-mode t)
  (my-doxym-bindings)
  (buffer-face-mode t)
  (set (make-local-variable 'comment-multi-line) t))

(defun my-doxym-bindings()
  (require 'tempos-html)
  (require 'tempos-doxy)

  (local-set-key [(control ?\,)] (make-sparse-keymap))

  ;; structure
  ;; title, section, subsection, ... paragraph
  (local-set-key [(control ?\,)(s)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(s)(a)] 'tempo-template-doxy-title)
  (local-set-key [(control ?\,)(s)(0)] 'tempo-template-doxy-title)
  (local-set-key [(control ?\,)(s)(b)] 'tempo-template-doxy-section)
  (local-set-key [(control ?\,)(s)(1)] 'tempo-template-doxy-section)
  (local-set-key [(control ?\,)(s)(c)] 'tempo-template-doxy-subsection)
  (local-set-key [(control ?\,)(s)(2)] 'tempo-template-doxy-subsection)
  (local-set-key [(control ?\,)(s)(d)] 'tempo-template-doxy-subsubsection)
  (local-set-key [(control ?\,)(s)(3)] 'tempo-template-doxy-subsubsection)
  (local-set-key [(control ?\,)(s)(p)] 'tempo-template-html-paragraph)

  ;; referencing: references, anchors, index, biblio, ...
  (local-set-key [(control ?\,)(r)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(r)(a)] 'tempo-template-html-anchor) ; generic anchor
  (local-set-key [(control ?\,)(r)(r)] 'tempo-template-html-ref)    ; generic reference

  ;; table
  (local-set-key [(control ?\,)(t)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(t)(t)] 'tempo-template-html-table+)
  (local-set-key [(control ?\,)(t)(r)] 'tempo-template-html-tr+)
  (local-set-key [(control ?\,)(t)(d)] 'tempo-template-html-td)
  (local-set-key [(control ?\,)(t)(h)] 'tempo-template-html-th)
  
  ;; list
  (local-set-key [(control ?\,)(l)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(l)(u)] 'tempo-template-html-ul+)
  (local-set-key [(control ?\,)(l)(o)] 'tempo-template-html-ol+)
  (local-set-key [(control ?\,)(l)(d)] 'tempo-template-html-dl+)
  (local-set-key [(control ?\,)(l)(i)] 'tempo-template-html-li)
  (local-set-key [(control ?\,)(l)(t)] 'tempo-template-html-dt+)

  ;; (inline) formatting
  (local-set-key [(control ?\,)(f)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(f)(e)] 'tempo-template-html-em)
  (local-set-key [(control ?\,)(f)(i)] 'tempo-template-html-i)
  (local-set-key [(control ?\,)(f)(s)] 'tempo-template-html-strong)
  (local-set-key [(control ?\,)(f)(b)] 'tempo-template-html-b)
  (local-set-key [(control ?\,)(f)(c)] 'tempo-template-html-code)
  (local-set-key [(control ?\,)(f)(v)] 'tempo-template-html-var)
  (local-set-key [(control ?\,)(f)(p)] 'tempo-template-html-pre)

  ;; block formatting
  (local-set-key [(control ?\,)(b)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(b)(v)] 'tempo-template-doxy-verbatim)
  (local-set-key [(control ?\,)(b)(c)] 'tempo-template-doxy-code)

  ;; generic
  (local-set-key [(control ?\,)(g)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(g)(e)] 'tempo-template-html-element)
  (local-set-key [(control ?\,)(g)(t)] 'tempo-template-html-tag))

;;; pod
;; -----------------------------------------------------------------------------
(add-hook 'pod-mode-hook 'my-pod-hook)

(defun my-pod-hook ()
  (outline-minor-mode t))

;;; markdown
;; -----------------------------------------------------------------------------
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

(defun my-markdown-mode-hook ()
  (outline-minor-mode t)

  (set (make-local-variable 'word-wrap) t)
  (toggle-truncate-lines -1)
  (require 'screen-lines) ; or is screenline.el better?
  (screen-lines-mode 1))

;;; bbcode
;; -----------------------------------------------------------------------------
(add-hook 'bbcode-hook 'my-bbcode-hook)

(defun my-bbcode-hook ()
  (my-bbcode-bindings))

(defun my-bbcode-bindings ()
  (require 'tempos-bbcode)

  ;; referencing: references, anchors, index, biblio, ...
  (local-set-key [(control ?\,)(r)] (make-sparse-keymap))

  ;; ;; table
  ;; (local-set-key [(control ?\,)(t)] (make-sparse-keymap))
  ;; (local-set-key [(control ?\,)(t)(t)] 'tempo-template-html-table+)
  ;; (local-set-key [(control ?\,)(t)(r)] 'tempo-template-html-tr+)
  ;; (local-set-key [(control ?\,)(t)(d)] 'tempo-template-html-td)
  ;; (local-set-key [(control ?\,)(t)(h)] 'tempo-template-html-th)
  
  ;; list
  (local-set-key [(control ?\,)(l)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(l)(u)] 'tempo-template-bbcode-ulist)
  (local-set-key [(control ?\,)(l)(o)] 'tempo-template-bbcode-olist)
  (local-set-key [(control ?\,)(l)(i)] 'tempo-template-bbcode-list-item)

  ;; (inline) formatting
  (local-set-key [(control ?\,)(f)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(f)(b)] 'tempo-template-bbcode-bold)
  (local-set-key [(control ?\,)(f)(i)] 'tempo-template-bbcode-italic)
  (local-set-key [(control ?\,)(f)(u)] 'tempo-template-bbcode-underline)

  ;; block formatting
  (local-set-key [(control ?\,)(b)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(b)(c)] 'tempo-template-bbcode-code)
  (local-set-key [(control ?\,)(b)(q)] 'tempo-template-bbcode-quote)
  )

;;; mediawiki
;; -----------------------------------------------------------------------------
(add-hook 'mediawiki-mode-hook 'my-mediawiki-hook)

(defun my-mediawiki-hook ()
  (visual-line-mode 1)
  (outline-minor-mode t)
  (set (make-local-variable 'word-wrap) t)
  (toggle-truncate-lines -1)
  (my-mediawiki-bindings))

(defun my-mediawiki-bindings ()
;  (require 'tempos-mediawiki)

  ;; referencing: references, anchors, index, biblio, ...
  (local-set-key [(control ?\,)(r)] (make-sparse-keymap))

  ;; structure
  ;; title, section, subsection, ... paragraph
  (local-set-key [(control ?\,)(s)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(s)(a)] 'tempo-template-mediawiki-title-0)
  (local-set-key [(control ?\,)(s)(0)] 'tempo-template-mediawiki-title-0)
  (local-set-key [(control ?\,)(s)(b)] 'tempo-template-mediawiki-title-1)
  (local-set-key [(control ?\,)(s)(1)] 'tempo-template-mediawiki-title-1)
  (local-set-key [(control ?\,)(s)(c)] 'tempo-template-mediawiki-title-2)
  (local-set-key [(control ?\,)(s)(2)] 'tempo-template-mediawiki-title-2)
  (local-set-key [(control ?\,)(s)(d)] 'tempo-template-mediawiki-title-3)
  (local-set-key [(control ?\,)(s)(3)] 'tempo-template-mediawiki-title-3)
  (local-set-key [(control ?\,)(s)(e)] 'tempo-template-mediawiki-title-4)
  (local-set-key [(control ?\,)(s)(4)] 'tempo-template-mediawiki-title-4)

  ;; table
  (local-set-key [(control ?\,)(t)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(t)(t)] 'tempo-template-html-table+)
  (local-set-key [(control ?\,)(t)(r)] 'tempo-template-html-tr+)
  (local-set-key [(control ?\,)(t)(d)] 'tempo-template-html-td)
  (local-set-key [(control ?\,)(t)(h)] 'tempo-template-html-th)
  
  ;; ;; list
  ;; (local-set-key [(control ?\,)(l)] (make-sparse-keymap))
  ;; (local-set-key [(control ?\,)(l)(u)] 'tempo-template-mediawiki-ulist)
  ;; (local-set-key [(control ?\,)(l)(o)] 'tempo-template-mediawiki-olist)
  ;; (local-set-key [(control ?\,)(l)(i)] 'tempo-template-mediawiki-list-item)

  ;; (inline) formatting
  (local-set-key [(control ?\,)(f)] (make-sparse-keymap))
  (local-set-key [(control ?\,)(f)(b)] 'tempo-template-mediawiki-bold)
  (local-set-key [(control ?\,)(f)(i)] 'tempo-template-mediawiki-italic)
  (local-set-key [(control ?\,)(f)(e)] 'tempo-template-html-em)
  (local-set-key [(control ?\,)(f)(s)] 'tempo-template-html-strong)
  (local-set-key [(control ?\,)(f)(c)] 'tempo-template-html-code)
  (local-set-key [(control ?\,)(f)(v)] 'tempo-template-html-var)
  (local-set-key [(control ?\,)(f)(p)] 'tempo-template-html-pre)

  ;; ;; block formatting
  ;; (local-set-key [(control ?\,)(b)] (make-sparse-keymap))
  ;; (local-set-key [(control ?\,)(b)(c)] 'tempo-template-mediawiki-code)
  ;; (local-set-key [(control ?\,)(b)(q)] 'tempo-template-mediawiki-quote)
  )

;;; enriched
;; -----------------------------------------------------------------------------
(defun my-enriched-mode-hook()
  )

(add-hook 'enriched-mode-hook 'my-enriched-mode-hook)


;;; misc modes
;;; ============================================================================

;;; gdb
;; -----------------------------------------------------------------------------
(defun my-perldb-mode-hook ()
  (gud-def gud-print   "x %e" "\C-p" "Evaluate perl expression at point.")
  (gud-def gud-refresh "." nil "refresh")
  (gud-def gud-quit "q" nil "quit"))
(add-hook 'perldb-mode-hook 'my-perldb-mode-hook)



;;; vc
;; -----------------------------------------------------------------------------
(substitute-key-definition 'vc-diff 'vc-ediff vc-prefix-map)

;;; view mode
;; -------------------------------------------------------------------
(defun my-view-mode-hook()
  (define-key view-mode-map [(s)] 'isearch-forward-regexp)
  (define-key view-mode-map [(r)] 'isearch-backward-regexp))
(add-hook 'view-mode-hook 'my-view-mode-hook)

;;; help mode
;; -------------------------------------------------------------------
(defun my-help-mode-hook()
  (local-set-key [(S)] 'help-mode-goto-src)
  (local-set-key [(l)] 'help-go-back)   ; like Info-history-back
  (local-set-key [(control tab)] 'backward-button)

  (set (make-local-variable 'show-trailing-whitespace) nil)

  ;; Funktioniert nicht insofern dass dann die anderen highlightnings
  ;; verschwinden. Auch highlighht-regexp funktioniert in diesem Sinne nicht.
  ;;
  ;;(font-lock-add-keywords nil (list
  ;;    (list "\\`\\_<.+?\\_>" '(0 markup-title-2-face t))))
  )

(add-hook 'help-mode-hook 'my-help-mode-hook)

(defun help-mode-goto-src()
  (interactive)
  (goto-char (point-min))
  (forward-button 1)
  (push-button))

;;; finder mode
;; -------------------------------------------------------------------
(defun my-finder-mode-hook()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "(@\\*\\s-*\".*?\"\\s-*)" nil t)
      (linkd-mode))))

(add-hook 'finder-mode-hook 'my-finder-mode-hook)


;;; info mode
;; -------------------------------------------------------------------
(defun my-info-mode-hook()
  (require 'markup-faces)
  (define-key Info-mode-map [(control tab)] 'Info-prev-reference)
  (set (make-local-variable 'show-trailing-whitespace) nil)
  (font-lock-add-keywords nil (list
    (list "^[ \t]*--[ \t]*\\(\\w*\\)[ \t]*:[ \t]*\\([^ \t\n]+\\)" '(1 markup-emphasis-face t) '(2 markup-strong-face t)))))
(add-hook 'Info-mode-hook 'my-info-mode-hook)

;;; isearch minor mode
;; ----------------------------------------------------------------
(define-key isearch-mode-map [(meta w)]    'isearch-yank-sexp)
(define-key isearch-mode-map [(control e)] 'isearch-edit-string)

;;; font lock
;; --------------------------------------------------------------------
;; that's the way doxymacs suggests to call doxymacs-font-lock
;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode) (eq major-mode 'idl-mode))
;;       (require 'doxymacs)
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;;; dired mode
;; --------------------------------------------------------------------
(add-hook 'dired-mode-hook 'my-dired-mode-hook)

(defun my-dired-mode-hook ()
  (load "dired-x")
  (require 'dired-isearch)

  (setq case-fold-search t)
  (setq dired-deletion-confirmer (lambda(x)t))

  (local-set-key "E" 'dired-ediff-marked-files)
  (local-set-key [remap isearch-forward]  'dired-isearch-forward-regexp)
  (local-set-key [remap isearch-backward] 'dired-isearch-backward-regexp)
  (local-set-key [remap dired-do-delete] 'dired-do-delete-ext)
  (local-set-key "r" 'wdired-change-to-wdired-mode) ; suggested by wdired
  (local-set-key "*."               'dired-mark-extension-dwim) 
  (local-set-key "O"                'dired-open-in-external-app)
  (local-set-key [(control return)] 'project-dired-find-main-file)
  (local-set-key "*i"               'project-dired-mark-files))

;;; flyspell / flyspell timer (needs aspell)
;; --------------------------------------------------------------------
;; (add-hook 'flyspell-mode-hook 'flyspell-timer-ensure-idle-timer)
;; (put 'c++-mode 'flyspell-mode-predicate 'c-mode-common-flyspell-verify)
;; (put 'idl-mode 'flyspell-mode-predicate 'c-mode-common-flyspell-verify)
;; (put 'emacs-lisp-mode 'flyspell-mode-predicate 'c-mode-common-flyspell-verify)
;; (defun c-mode-common-flyspell-verify ()
;;   "Function used for `flyspell-generic-check-word-predicate' in
;;   C++-mode. Returns t to continue checking."
;;   (save-excursion
;;     (let ((case-fold-search nil)
;;           (f (get-text-property (point) 'face)))
;;       (forward-word -1)
;;       ; only spell check current word if all of the following is true:
;;       ; better to check too few words. Too many is annoying and takes longer.
;;       (and (memq f '(font-lock-string-face font-lock-comment-face font-lock-doc-face)) ; within a comment
;;            (looking-at "[A-Za-z][a-z]*\\>") ; The only upercase letter, if any, is the first. Only letters (no digits, no _)
;;            (looking-back "\\(^\\|\\s-\\)" (- (point) 1)) ; A whitespace char precedes the word, or word is first one on line
;;            ))))

;; (add-hook 'c++-mode-hook '(lambda () (setq flyspell-generic-check-word-predicate 'c-mode-common-flyspell-verify)))
;; (add-hook 'idl-mode-hook '(lambda () (setq flyspell-generic-check-word-predicate 'c-mode-common-flyspell-verify)))
;; (add-hook 'emacs-lisp-mode-hook '(lambda () (setq flyspell-generic-check-word-predicate 'c-mode-common-flyspell-verify)))

;;; (wo)?man 
;; ----------------------------------------------------------------------------
(defvar Man-heading2-regexp "\\s-+-")
  
(defun Man-outline-level ()
  (cond
   ((looking-at Man-heading-regexp) 1)
   ((looking-at Man-heading2-regexp) 2)
   (t 3)))

(defun my-man-mode-hook ()
  (setq outline-regexp
        (concat "\\(?:" Man-heading-regexp "\\|" Man-heading2-regexp "\\)"))
  (setq outline-level 'Man-outline-level))

(add-hook 'Man-mode-hook 'my-man-mode-hook)

;;; ibuffer
;; ----------------------------------------------------------------------------
(defun my-ibuffer-mode-hook ()
  (ibuffer-auto-mode 1)
  (define-key ibuffer-mode-map "a" 'ibuffer-goto-first))
(add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook)

(defun my-ibuffer-hook ()
  (ibuffer-goto-first))

(defun ibuffer-goto-first ()
  (interactive)
  (goto-char (point-min))
  (forward-line 3)
  (forward-char 4))
(add-hook 'ibuffer-hook 'my-ibuffer-hook)
 
;;; minibuffer
;; ----------------------------------------------------------------------------
;; (define-key minibuffer-local-filename-must-match-map [(backtab)] 'file-alias-minibuffer-complete)
(define-key minibuffer-local-completion-map [(backtab)] 'filealias-minibuffer-complete)

;; remember that minibuffer modes are not part of common-hook
(defun my-minibuffer-setup-hook ()
  (local-set-key "\C-y" 'yank)
  (local-set-key "\M-y" 'yank-pop)
  (local-set-key "\C-\M-y" 'yank-push)
  (setq truncate-lines nil) 		; for resize-mini-windows to work
  (delete-selection-mode t))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;;; shell
;; ----------------------------------------------------------------------------
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; pm
;; ----------------------------------------------------------------------------
(defun my-pm-hook ()
  (setq tab-width 4)
  (outline-minor-mode t)
  (local-set-key "\t" 'indent-line-or-region))
(add-hook 'pm-mode-hook 'my-pm-hook)

;;; inferior shell 
;; ----------------------------------------------------------------------------
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-hook 'comint-mode-hook 'my-comint-mode-hook)

(defun my-comint-mode-hook ()
  (local-set-key [(control d)] 'delete-char)) ;comint-mode redefines C-d with comint-delchar-or-maybe-eof, which doesn't respect delete-selection-mode
 
;;; mks
;; ----------------------------------------------------------------------------

(defun my-mks-bindings ()
  (local-set-key [f9] (make-sparse-keymap))
  (local-set-key [(f9)(f9)] 'mks-universal)
  (local-set-key [(f9)(b)] 'mks-blame)
  (local-set-key [(f9)(c)] 'mks-cat)
  (local-set-key [(f9)(d)] 'mks-diff)
  (local-set-key [(f9)(e)] 'mks-ediff)
  (local-set-key [(f9)(i)] 'mks-ci)
  (local-set-key [(f9)(f)] 'mks-find-working-file)
  (local-set-key [(f9)(l)] 'mks-log)
  (local-set-key [(f9)(L)] 'mks-log-single)
  (local-set-key [(f9)(o)] 'mks-co)
  (local-set-key [(f9)(r)] 'mks-revert)
  (local-set-key [(f9)(s)] 'mks-status)
  (local-set-key [(f9)(u)] 'mks-update))

(defun my-mks-common-mode-hook ()
  (my-mks-bindings)
  (local-set-key [(q)] 'kill-buffer)
  (local-set-key [(b)] 'mks-blame)
  (local-set-key [(c)] 'mks-cat)
  (local-set-key [(d)] 'mks-diff)
  (local-set-key [(e)] 'mks-ediff)
  (local-set-key [(f)] 'mks-find-working-file)
  (local-set-key [(l)] 'mks-log)
  (local-set-key [(L)] 'mks-log-single))

(defun my-mks-cat-mode-hook ()
  ;; (local-set-key [(control n)] 'mks-cat-next)
  ;; (local-set-key [(control p)] 'mks-cat-prev)
  )

;; there is no mks major-mode, so my-mks-bindings is not called within a
;; 'mks-mode-hook'. In contrast, my-mks-common-mode-hook is for mks's minor modes, which
;; *do* have a hook.
(add-hook 'mks-common-mode-hook 'my-mks-common-mode-hook)
(add-hook 'mks-cat-mode-hook 'my-mks-cat-mode-hook)

;;; logfile 
(defun my-logfile-mode-hook()
  (outline-minor-mode t)
  (view-mode 1)
  (local-set-key [f10] 'logfile-filter))
(add-hook 'logfile-mode-hook 'my-logfile-mode-hook)

;;; svn-status
(defun my-svn-status-hook ()
  ;; (unless (local-variable-p 'svn-status-hide-unmodified)
  ;;   (set (make-local-variable 'svn-status-hide-unmodified) t))
  ;; (unless (local-variable-p 'svn-status-hide-unknown)
  ;;   (set (make-local-variable 'svn-status-hide-unknown) t))
  )

(add-hook 'svn-status-mode-hook 'my-svn-status-hook)

;;; apt-sources
;; ----------------------------------------------------------------------------
(defun my-apt-sources-after-save-hook ()
  (when (and (eq major-mode 'apt-sources-mode) (y-or-n-p "update?"))
    (shell-command "gksudo -- apt-get -q update &")))

(add-hook 'after-save-hook 'my-apt-sources-after-save-hook)

;;; x-dict
;; ----------------------------------------------------------------------------
(defconst xdict-font-lock-keywords
  (list
     (cons "\\[.*?\\]" font-lock-semi-unimportant)
     (cons "|.*?|" font-lock-semi-unimportant)
     (cons "\\b\\(etw\\|sth\\|adj\\)\\." font-lock-semi-unimportant)
     (cons "\\<\\(to\\|the\\|der\\|die\\|das\\)\\>" font-lock-semi-unimportant)
     ))

(defun my-xdict-hook ()
  (set (make-local-variable 'font-lock-defaults)
       '(xdict-font-lock-keywords)))

(add-hook 'xdict-hook 'my-xdict-hook)

;;; hi-lock
;; ----------------------------------------------------------------------------
(defun my-hi-lock-mode-hook ()
  ;; or is it better to use eval-after-load?
  (define-key hi-lock-map "\C-xws" 'highlight-toggle-sexp-or-region)
  (define-key hi-lock-map "\C-xwu" 'unhighlight-all)
  (define-key hi-lock-map "\C-xwa" 'highlight-arguments)
  (define-key hi-lock-map "\C-xwA" 'highlight-arguments-uni)
  (define-key hi-lock-map "\C-xwm" 'highlight-members))

(add-hook 'hi-lock-mode-hook 'my-hi-lock-mode-hook)

;;; custom
;; ----------------------------------------------------------------------------
(defun my-Custom-mode-hook ()
  )

(add-hook 'Custom-mode-hook 'my-Custom-mode-hook)

;;; compilation
;; ----------------------------------------------------------------------------
(defun my-compilation-mode-hook ()
  (toggle-truncate-lines -1))

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)


;;; whitespace
;; ----------------------------------------------------------------------------
(defun my-whitespace-mode-hook ()
  ;; redundant to whitespace-style
  (setq whitespace-report-list
        (list
         (cons 'empty                   whitespace-empty-at-bob-regexp)
         (cons 'empty                   whitespace-empty-at-eob-regexp)
         (cons 'trailing                whitespace-trailing-regexp)
         (cons 'indentation             nil)
         (cons 'space-after-tab         nil))))

(add-hook 'whitespace-mode-hook 'my-whitespace-mode-hook)


;;; not really modes but other hooks 
;;; ===================================================================

(defun custom-file-before-save-hook ()
  (when (string= (expand-file-name (buffer-file-name)) (expand-file-name custom-file))
    (replace-cntrl-chars)))
(add-hook 'before-save-hook 'custom-file-before-save-hook t)


;;; common (all, text, programming, ...)
;;; ===================================================================
;; See also init.el for a summary of how to customize Emacs

(defun my-change-major-mode-after-body-hook ()
  (mode-message-start "my-change-major-mode-after-body-hook")
  (unless (string-match "minibuffer" (symbol-name major-mode))
    ;; nop yet
    )
  (mode-message-end "my-change-major-mode-after-body-hook"))

(add-hook 'change-major-mode-after-body-hook 'my-change-major-mode-after-body-hook)

(defun my-after-change-major-mode-hook ()
  (mode-message-start "my-after-change-major-mode-hook")
  (unless (string-match "minibuffer" (symbol-name major-mode))
    (when (is-edit-mode)
      (my-edit-mode-hook))
    (my-common-mode-bindings)

    ;; projects. Probably project.el should offer an project-hook.
    ;; nothing yet

    ;; the following is the last thing that is executed from all the mode
    ;; related hooks.

    ;; Make sure whitespace picks up the actual value of indent-tabs-mode /
    ;; tab-width. A better solution would probably be to modify whitespace.el
    ;; such that it accesses `whitespace-indent-tabs-mode' /
    ;; `whitespace-tab-width' via a function. Internaly that function either
    ;; returns `indent-tabs-mode' in the normal case or
    ;; `whitespace-indent-tabs-mode' in the case the user wants to overwrite it
    ;; with a whitespace specific value
    (when whitespace-mode
      (whitespace-mode 0)
      (whitespace-mode 1)))
  (mode-message-end "my-after-change-major-mode-hook"))

(add-hook 'after-change-major-mode-hook 'my-after-change-major-mode-hook)

;; Meant for all modes where the user freely can edit text -- even if it's
;; (currently) read only
(defun my-edit-mode-hook ()
  (mode-message-start "my-edit-mode-hook")
  (whitespace-mode t)
  (fci-mode t)
  (mode-message-end "my-edit-mode-hook"))

(defun my-text-mode-hook ()
  (mode-message-start "my-text-mode-hook")
  (mode-message-end "my-text-mode-hook"))

(add-hook 'text-mode-hook 'my-text-mode-hook)

(defun my-prog-mode-hook ()
  (mode-message-start "my-prog-mode-hook")
  (mode-message-end "my-prog-mode-hook"))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;;; mode-hooks.el ends here
