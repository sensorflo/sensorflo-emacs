(require 'tempo-ext)


;;; utility defuns / variables
;; -----------------------------------------------------------------------------
(defvar tempos-c++-open-brace-style 'behind-conditional
  "'new-line or 'behind-conditional")

(defvar tempos-c++-space-between-keyword-parenthesis t)

(defun tempos-c++-open-brace ()
  (indent-according-to-mode)
  (cond
   ((eq tempos-c++-open-brace-style 'new-line)
    (insert "\n{")
    (indent-for-tab-command) ;for some funny reason using (indent-according-to-mode) leads to infinite recursion
    (insert "\n")
    (indent-for-tab-command))
   (t ; 'behind-conditional is the default
    (insert " {\n"))))

(defun tempos-c++-close-brace ()
  (insert "}")
  (indent-according-to-mode)
  (if (not (or (eolp)
	       (save-excursion
		 (re-search-forward
		  "\\=\\s-*$" nil t))))
      (insert "\n")))

(defun tempos-c++-between-keyword-parenthesis ()
  (when tempos-c++-space-between-keyword-parenthesis    
    (insert " ")))

(defun tempos-c++-std (keyword &optional regex no-lws tag)
  (tempo-define-template
   (concat "c-" keyword)
   `((progn (tempo-entry ,regex) "") ,(unless no-lws 'lws)
     ,keyword (tempos-c++-between-keyword-parenthesis) "( " p " )"
     (tempos-c++-open-brace)
     r-or-blank-line>
     (tempos-c++-close-brace))
   tag))


;;; common structures
;; -----------------------------------------------------------------------------
(tempo-define-template "c-block"
 '( "{ " > n> r> n> "}" >))


;;; declartions
;; -----------------------------------------------------------------------------

(tempo-define-template "c-bool"
 '( (progn (tempo-entry "\\bi\\(n\\(t32\\sw*\\)?\\)?") "")
    "bool b" p " = " p ";" > )
 "bool")

(tempo-define-template "c-int32"
 '( (progn (tempo-entry "\\bi\\(n\\(t32\\sw*\\)?\\)?") "")
    "int32 n" p " = " p "0;" > )
 "int")

(tempo-define-template "c-uint32"
 '( (progn (tempo-entry "\\bu\\(i\\(nt32\\sw*\\)?\\)?") "")
    "uint32 n" p " = " p "0;" > )
 "uint")

(tempo-define-template "c-real64"
 '( ;(progn (tempo-entry "\\br\\(e\\(al64\\sw*\\)?\\)?") "")
    "real64 f" p " = " p "0.0;" > )
 "real")

(tempo-define-template "c-real64-ptr"
 '( ;(progn (tempo-entry "\\br\\(e\\(al64\\sw*\\)?\\)?") "")
    "real64* pf" p " = " p "0.0;" > )
 "preal")


;;; flow controll
;; -----------------------------------------------------------------------------


(tempos-c++-std "if" "\\bi\\(f\\sw*\\)?" nil "if")
(tempos-c++-std "else if" nil t "elif")
(tempos-c++-std "else" "\\bi\\(f\\sw*\\)?" t "else")
(tempos-c++-std "while" "\\bw\\(i\\sw*\\)?" t "while")

(tempo-define-template "c-for"
 '( (progn (tempo-entry "\\bf\\(o\\(r\\sw*\\)?\\)?") "") lws
   "for ( " p "; " p "; " p " )"
   (tempos-c++-open-brace)
   r-or-blank-line>
   (tempos-c++-close-brace) )
 "for")

(tempo-define-snippet "c-for-std"
 '( (progn (tempo-entry "\\bf\\(o\\(r\\sw*\\)?\\)?") "") lws
    "for ( " (p "type" type) " " (p "name" name) " = 0; "
      (s name) " < " (p "max" max) " ; "
      (s name) "++ )"
    (tempos-c++-open-brace)
    r-or-blank-line>
    (tempos-c++-close-brace)))

(tempo-define-template "c-for-std-2"
 '( (progn (tempo-entry "\\bf\\(o\\(r\\sw*\\)?\\)?") "") lws
   "for ( int i = 0 ; i < " p "; i++ )"
   (tempos-c++-open-brace)
   r-or-blank-line>
   (tempos-c++-close-brace)))

(tempo-define-template "c-for-iter"
  '( lws
     "for ( auto iter=" p ".begin() ; "
            "iter!=" p ".end() ; "
            "++iter )"
      (tempos-c++-open-brace)
      r-or-blank-line>
      (tempos-c++-close-brace)))

;; switch
(tempo-define-template "c-switch"
 '( lws
    "switch ( " p " )"
    (tempos-c++-open-brace)
    "case " p ": " > n>
    p > n>
    "break;" > n>
    > n>
    "case " p ": " > n>
    p > n>
    "break;" > n>    
    > n>
    "default :" > n>
    p > n>
    "break;" > n>
    (tempos-c++-close-brace))
 "switch")

;; case
(tempo-define-template "c-case"
 '( lws
    "case " p ":" >n
    r-or-blank-line>
    "break;" > %)
 "case")

;; default
(tempo-define-template "c-default"
 '( lws
    "default :" >n
    r-or-blank-line>
    "break;" > )
 "default")

;; do
(tempo-define-template "c-do"
 '( lws
    "do"
    (tempos-c++-open-brace)
    r-or-blank-line>
    (tempos-c++-close-brace) "while ( " p " );" > %)
 "do")

;; try
(tempo-define-template "c-try"
 '( lws
    "try"
    (tempos-c++-open-brace)
    r-or-blank-line>
    (tempos-c++-close-brace) n
    "catch ( " p " )"
    (tempos-c++-open-brace)
    p > n>
    (tempos-c++-close-brace))
 "try")

;; catch
(tempo-define-template "c-catch"
 '( lws
    "catch ( " p " )"
    (tempos-c++-open-brace)
    r-or-blank-line>
    (tempos-c++-close-brace))
 "catch")


;;; misc
;; -----------------------------------------------------------------------------

;; include
(tempo-define-template "c-include"
 '( &
    "#include \"" p ".h\"" >)
 "inc")

(tempo-define-template "c-include-system"
 '( &
    "#include <" p ".h>" >))

;; header file
(tempo-define-snippet "cpp-h-file"
  '("#ifndef " (upcase (tempo-lookup-named 'class)) "__H_\n" 
    "#define " (upcase (tempo-lookup-named 'class)) "__H_\n" 
    "\n" 
    "/** */\n" 
    "class " (p "classname: \n" class) ".\n"
    "{\n" 
    "  public:\n" 
    "    " (s class) "();\n" 
    "    ~" (s class) "();\n" 
    "\n" 
    "  private:\n" 
    "\n" 
    "};\n" 
    "\n"
    "#endif // " (upcase (tempo-lookup-named 'class)) "__H_\n"))


;; comment block
(tempo-define-template "c-comment-block"
 '( &    
    "/** " r> p "*/" > )
 "/**")

;; doxygen group
(tempo-define-template "c-member-group-named"
 '( lws 
    "/*! @name " p " */" >n
    "/*------------------------------------------------------------------*/" >n
    "//@{" >n
    r-or-blank-line>
    "//@}" >n
    > % ))

;; misc group
(tempo-define-template "c-member-group"
 '( &
    "/* miscellaneous " p " */" > n>
    "/*------------------------------------------------------------------*/" > ))

;; comment block
(tempo-define-template "c-if0"
 '( &    
    "#if 0" > n>
    r> n>
    "#endif" > )
 "")

;; (p "classname: \n" class)
;; (upcase (tempo-lookup-named 'class))

(tempo-define-snippet "c-delete"
 '( &    
    "if (" (p "ptr: " ptr)") {" > n>
    "delete " (s ptr) ";" > n>
    (s ptr) " = NULL;" > n>
    "}" > ))


;;; scratch tempo exentions
;; ----------------------------------------------------------------------
;; todo:
;; - as in VA snippet, typing space etc doesn't already insert template, but
;;   offers a list. Customizeable which version you want.
;;
;; problems:
;; - the space after if is also inserted. Now that i removed it from the
;; template, it is absent when expanded without abbrev
;;
;; solitions
;; - only one func for all keywords. That one looks at word + blanks before
;;   point, deletes them, and depending on the word calls the correct template.
;;   That would also be usefull with abbrev-mode off, but still wanting to
;;   exapand after having written e.g. "if " and then decide to expand it.

;; (define-abbrev c++-mode-abbrev-table "if"   t 'tempo-template-c-if) 
;; (define-abbrev c++-mode-abbrev-table "for"  t 'tempo-template-c-for-std)

;; insert " " is needed so unexpand-abbrev isn't confused with correct location
;; of point. It doesn't confuse tempo-template, because all templates do indent anyway.
(setq tempo-groups nil)
(setq tempo-act-group nil)
(setq tempo-act-group-act-tail nil)
(push (list 'tempo-template-c-for-std-2 'tempo-template-c-for-std 'tempo-template-c-for) tempo-groups)

(define-abbrev c++-mode-abbrev-table "do"      " " 'tempo-template-c-do)
(define-abbrev c++-mode-abbrev-table "while"   " " 'tempo-template-c-while)
(define-abbrev c++-mode-abbrev-table "switch"  " " 'tempo-template-c-switch)
(define-abbrev c++-mode-abbrev-table "case"    " " 'tempo-template-c-case)
(define-abbrev c++-mode-abbrev-table "default" " " 'tempo-template-c-default)
(define-abbrev c++-mode-abbrev-table "inc"     " " 'tempo-template-c-include)
(define-abbrev c++-mode-abbrev-table "include" " " 'tempo-template-c-include)

(define-abbrev c++-mode-abbrev-table "r"     " " 'tempo-template-c-return-std)
(define-abbrev c++-mode-abbrev-table "er"    " " 'tempo-template-c-early-return-std)
(define-abbrev c++-mode-abbrev-table "eret"  " " 'tempo-template-c-early-return-std)
(define-abbrev c++-mode-abbrev-table "et"    " " 'tempo-template-c-ethrow)

(provide 'tempos-c++)