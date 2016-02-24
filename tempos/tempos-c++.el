(require 'tempo-ext) ; https://gitorious.org/tempo-ext


;;; variables
;; -----------------------------------------------------------------------------
(defgroup tempos-c++ nil
  "Configuration of tempo templates for C/C++.

Note that indentation is configured by cc-mode."
  :link '(custom-group-link tempo)
  :link '(custom-manual "(ccmode) Customizing Indentation")
  :group 'tempo)

(defcustom tempos-c++-open-brace-style 'behind-conditional
  "To select where to put the open brace of body

Brace on a new line ('new-line):
if ( test )
{
  foo();
}

Brace behind conditional expression ('behind-conditional):
if ( test ) {
  foo;
}"
  :type '(choice (const :tag "Brace on a new line" new-line)
                 (const :tag "Brace behind conditional expression" behind-conditional))
  :group 'tempos-c++)

(defcustom tempos-c++-space-between-keyword-parenthesis t
  "To chooce whether or not a space is used before the condition's paranthesis.

No space (nil): if( ... ) ...
One space (t): if ( ...  ) ..."
  :type '(choice (const :tag "No space" nil)
                 (const :tag "One space" t))
  :group 'tempos-c++)

(defcustom tempos-c++-block-comment-style 'java-doc
  "Style for block (aka multiline) comments

plain-c++: /* ... */
java-doc: /** ... */
qt: /*! ... */"
  :type '(choice (const :tag "Plain C++: /*" plain-c++)
                 (const :tag "Java Doc: /**" java-doc)
                 (const :tag "Qt: /*!" qt))
  :group 'tempos-c++)


;;; utility defuns
;; -----------------------------------------------------------------------------
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
  (if tempos-c++-space-between-keyword-parenthesis
    (insert " ")
    ""))

(defun tempos-c++-std (keyword &optional regex no-lws tag)
  (tempo-define-template
   (concat "c-" (replace-regexp-in-string " " "-" keyword))
   `((progn (tempo-entry ,regex) "") ,(unless no-lws 'lws)
     ,keyword (tempos-c++-between-keyword-parenthesis) "( " p " )"
     (tempos-c++-open-brace)
     r-or-blank-line>
     (tempos-c++-close-brace))
   tag))

(defun tempos-c++-block-comment-start ()
  (cond
   ((when mark-active
      (save-excursion
        (when (> (point) (mark)) (exchange-point-and-mark))
        (or (not (looking-back "^\\s-*"))
            (progn (exchange-point-and-mark)
                   (not (looking-at "\\s-*$"))))))
    "/*")
   ((eq tempos-c++-block-comment-style 'plain-c++) "/*")
   ((eq tempos-c++-block-comment-style 'java-doc) "/**")
   ((eq tempos-c++-block-comment-style 'qt) "/*!")
   (t "")))



;;; common structures
;; -----------------------------------------------------------------------------
(tempo-define-template "c-block"
 '( "{" > n> r> n> "}" >))



;;; flow controll
;; -----------------------------------------------------------------------------
(tempos-c++-std "if" "\\bi\\(f\\sw*\\)?" nil "if")
(tempos-c++-std "else if" nil t "elif")
(tempos-c++-std "while" "\\bw\\(i\\sw*\\)?" t "while")

(tempo-define-template "c-else"
 '( (progn (tempo-entry "\\be\\(l\\(r\\sw*\\)?\\)?") "")
   "else"
   (tempos-c++-open-brace)
   r-or-blank-line>
   (tempos-c++-close-brace) )
 "else")

(tempo-define-template "c-for-range-based"
 '( (progn (tempo-entry "\\bf\\(o\\(r\\sw*\\)?\\)?") "") lws
    "for" (tempos-c++-between-keyword-parenthesis) "( const auto& " p " : " p " )"
   (tempos-c++-open-brace)
   r-or-blank-line>
   (tempos-c++-close-brace) )
 "for")

(tempo-define-template "c-for"
 '( (progn (tempo-entry "\\bf\\(o\\(r\\sw*\\)?\\)?") "") lws
    "for" (tempos-c++-between-keyword-parenthesis) "( " p "; " p "; " p " )"
   (tempos-c++-open-brace)
   r-or-blank-line>
   (tempos-c++-close-brace)))

(tempo-define-snippet "c-for-std"
 '( (progn (tempo-entry "\\bf\\(o\\(r\\sw*\\)?\\)?") "") lws
    "for" (tempos-c++-between-keyword-parenthesis) "( " (p "type" type) " " (p "name" name) " = 0; "
      (s name) " < " (p "max" max) " ; "
      (s name) "++ )"
    (tempos-c++-open-brace)
    r-or-blank-line>
    (tempos-c++-close-brace)))

(tempo-define-template "c-for-std-2"
 '( (progn (tempo-entry "\\bf\\(o\\(r\\sw*\\)?\\)?") "") lws
   "for" (tempos-c++-between-keyword-parenthesis) "( int i=0; i<" p "; ++i )"
   (tempos-c++-open-brace)
   r-or-blank-line>
   (tempos-c++-close-brace)))

(tempo-define-template "c-for-iter"
  '( lws
     "for" (tempos-c++-between-keyword-parenthesis) "( auto iter=" p ".begin(); "
            "iter!=" p ".end(); "
            "++iter )"
      (tempos-c++-open-brace)
      r-or-blank-line>
      (tempos-c++-close-brace)))

(tempo-ext-define-group "c-for" ("-range-based" "" "-std-2" "-iter"))

;; switch
(tempo-define-template "c-switch"
 '( lws
    "switch" (tempos-c++-between-keyword-parenthesis) "( " p " )"
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
    (tempos-c++-close-brace) "while" (tempos-c++-between-keyword-parenthesis) "( " p " );" > %)
 "do")

;; try
(tempo-define-template "c-try"
 '( lws
    "try"
    (tempos-c++-open-brace)
    r-or-blank-line>
    (tempos-c++-close-brace) n
    "catch" (tempos-c++-between-keyword-parenthesis) "( " p " )"
    (tempos-c++-open-brace)
    p > n>
    (tempos-c++-close-brace))
 "try")

;; catch
(tempo-define-template "c-catch"
 '( lws
    "catch" (tempos-c++-between-keyword-parenthesis) "( " p " )"
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
    (tempos-c++-block-comment-start) " */\n"
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

(tempo-define-snippet "c-delete-copy-ctor-and-assignment"
  '((c-class-name) "(const " (c-class-name) "&) = delete;" > n
    (c-class-name) "& operator=(const " (c-class-name) "&) = delete;" >))

;; comment block
(tempo-define-template "c-comment-block"
 '( (tempos-c++-block-comment-start) " " r> p " */" > ))

;; doxygen group
(tempo-define-template "c-member-group-named"
 '( lws
    (tempos-c++-block-comment-start) " @name " p " */" >n
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

(tempo-define-template "c-delete"
 '( lws
    "if" (tempos-c++-between-keyword-parenthesis) "(" ( p "ptr: " ptr ) ")"
    (tempos-c++-open-brace)
    "delete " (s ptr) ";" >n
    (s ptr) " = NULL;" >n
    (tempos-c++-close-brace)))

(provide 'tempos-c++)

;;; tempos-c++.el ends here
