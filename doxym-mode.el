;;; doxym-mode.el --- a major-mode for editing documentation written in doxygen markup language
;;
;; Copyright 2010 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: http://sensorflo-emacs.googlecode.com/svn/trunk/doxym-mode.el
;; Created: 2010
;; Version: 0.1
;; Keywords: wp doxygen
;; 
;; This file is not part of GNU Emacs.
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; 
;; Doxygen is a document generator for various programming languages.
;;
;; This mode is intended for doxygen pages, rather than doxygen comments
;; commenting single elements such as classes or members. So doxym-mode is
;; similar to latex-mode, just that the markup language is Doxygen, not
;; LaTeX.
;;
;; Todo:
;; - use existing stuff from latex modes that insert 'pictures' of math
;;   formulas, pictures of included pictures etc.
;; - spell checking only in 'real' text. I.e. not in
;;   - code, verbatim, blocks/inline sections
;;   - html elements / doxygen commands
;;   - comments
;;   - identifiers (camelcase words, words containing _, ...)
;; - paragraph definition. so fill-paragraph works correctly on list elements, ... 
;; - tell fill-paragraph not to break up certain constructs, such as the string
;;   in \ref id "bla bla".
;;
;; Bugs:
;;
;;; Variables:

(defgroup doxym nil
  "Support for documents written in doxygen markup language."
  :group 'wp)

(defcustom doxym-break-section-line nil
  "When true, the text of a section definition is displayed on a new line."
  :type 'boolean
  :group 'doxym)

(defconst doxym-break-section-string "\u00B6")

;;; Code:

(require 'markup-faces)

(defvar doxym-text 'default)
(defvar doxym-gen 'markup-gen-face)
(defvar doxym-title-0 'markup-title-0-face)
(defvar doxym-title-1 'markup-title-1-face)
(defvar doxym-title-2 'markup-title-2-face)
(defvar doxym-title-3 'markup-title-3-face)
(defvar doxym-title-4 'markup-title-4-face)
(defvar doxym-title-5 'markup-title-5-face)
(defvar doxym-emphasis 'markup-emphasis-face)
(defvar doxym-strong 'markup-strong-face)
(defvar doxym-verbatim 'markup-verbatim-face)
(defvar doxym-code 'markup-code-face)
(defvar doxym-passthrough 'markup-passthrough-face)
(defvar doxym-superscript 'markup-superscript-face)
(defvar doxym-subscript 'markup-subscript-face)
(defvar doxym-reference 'markup-reference-face)
(defvar doxym-bold 'markup-bold-face)
(defvar doxym-italic 'markup-italic-face)
(defvar doxym-typewriter 'markup-typewriter-face)

(defvar doxym-meta 'markup-meta-face)
(defvar doxym-delimiter 'markup-delimiter-face)
(defvar doxym-hide-delimiter 'markup-hide-delimiter-face )
(defvar doxym-anchor 'markup-anchor-face )
(defvar doxym-comment 'markup-comment-face )
(defvar doxym-list 'markup-list-face )
(defvar doxym-table 'markup-table-face )
(defvar doxym-table-row 'markup-table-row-face )
(defvar doxym-table-cell 'markup-table-cell-face )
(defvar doxym-replacement 'markup-replacement-face )
(defvar doxym-complex-replacement 'markup-complex-replacement-face )
(defvar doxym-secondary-text 'markup-secondary-text-face )
(defvar doxym-warning 'font-lock-warning-face)


(defvar doxym-mode-hook nil
  "Normal hook run when entering doxym Text mode.")

(defvar doxym-mode-abbrev-table nil
  "Abbrev table in use in doxym-mode buffers.")

(define-abbrev-table 'doxym-mode-abbrev-table ())

(defun doxym-make-string (str cnt)
  "As `make-string', but the argument is a string"
  (let ((result ""))
    (while (> cnt 0)
      (setq result (concat result str))
      (setq cnt (1- cnt)))
    result))

(defun doxym-from-to-quantifier (x from to)
  "Mimicks a lazy '{from,to}' quantifier operating on x."
  (concat
   (when (and from (> from 0))
     (doxym-make-string x from))
   (when to
     (if (> to from)
         (doxym-make-string (concat x "??") (- to from))
       (error "to must be greater or equal from")))))

;; from doctokenizer.l
;; WS  [ \t\r\n]
;; ID  "$"?[a-z_A-Z\x80-\xFF][a-z_A-Z0-9\x80-\xFF]*

;;; re - regular expressions

;; bug: actually the returned regex should also match a mixed case version
(defun doxym-re-upper-or-lowercase(regexp)
  "Returns a regex that matches the regexp either all uppercase or all lowercase."
  (concat "\\(?:" (downcase regexp) "\\|" (upcase regexp) "\\)"))

;; - \= is so we can also match two keywords which appear directly after
;; each another. Here the knowledge is used that font lock starts calling
;; re-search-forward always at the beginning of the line, or after a previous
;; match. Thus it's not possible that the search starts right after the
;; backslash in 'bla \<...'. Would it start there doxym-re-no-escape's regexp
;; would match because of its \=, altough it actually shoud not.
;;
;; todo: adapt adoc-mode likewise
(defun doxym-re-no-escape ()
  "Returns a regexp that matches before an unescaped meta character.
Intendet to use at the start of a bigger regexp before the
starting meta character."
  (concat
   "\\(?:^\\|\\=\\|[^\\@\n]\\)" 
   "\\(?:\\\\\\\\\\|\\\\@\\|@\\\\\\|@@\\)*"))

(defun doxym-re-enclosed-str (&optional max-lines)
  "Returns a regexp in a group that can match before an unquoted character.
Intendet to be used in a bigger regexp to match the string upto
an ending delimiter, e.g. to match the ... part in '<b>...</b>'."
  (let* ((first-line "\\(?:[^\\@\n]\\|[\\@].\\)*?")
         (next-line (concat "\\(?:\n" first-line "\\)")))
    (concat
     "\\(" first-line 
     (if (null max-lines)
         (concat next-line "*?")
       (doxym-from-to-quantifier next-line 0 (1- max-lines)))
     "\\)")))

;; unlike xml tags, the cmd name is case sensitive and all lowercase
(defun doxym-re-cmd (name)
  "Returns a regexp matching a doxygen cmd; save to be used as an
  expression. Does NOT check that it is not preceded by an
  escaping char."
  (concat "\\(?:[\\@]\\(?:" name "\\)\\b\\)"))

;; bug: spaces inbetween are not marked as multiline construct
;; todo: what exactly is a word? It seems doxygen uses different definitions depending on the context.
;; Tests with \b have shown, that 
;; - the following puntuation belongs to the 'word':  ~`!#$%^&*()-_|=+{}'<>/°:
;;   Maybe a reason is that like this filenames count as a word
;; - i.e. these belong NOT to a word: [],.;? 
;; - " ( : When one of those is encountered (must not be the first char of the
;;   word') everything up to the next " (that is ) ) belongs to the word too.
;;
;; - todo: # is also funny. It seems to be replaced by ::. Has that to do with
;;   auto link generation?
(defun doxym-re-word ()
  "Returns a regexp matching a doxygen word argument.
It has two groups: the later is for a 'problematic' ending, i.e.
when the word ends with !; it was probably not the users intend that the !
belongs to the word."
  (let* ((warning-punct "~`!#$%^&*_|=+{}'<>/:°")
	 (ok-chars "-a-zA-Z0-9_")	;those chars one commonly associates with a 'word'
	 (ok-alternatives (concat
			   "[" ok-chars "]"
			   "\\|"
			   ;; also doxygen allows only for one newline
			   "\\(?:\"[^\"\n]*\\(?:\n[^\"\n]*\\)?\"\\)"
			   "\\|"
			   ;; doxygen doesn allow newlines
			   ;; bugs:
			   ;; - doxygen only eats up one (...) construct, doxym-re-word can have multiple
			   ;; - doxygen doesnt allow (...) at the beginning, doxym-re-word everywhere
			   "\\(?:([^)\n]*)\\)"
			   ))
	 (warning-part (concat "[" warning-punct "]"))
	 (all-alternatives (concat warning-part "\\|" ok-alternatives)))
    (concat
     "\\(\\(?:" ok-alternatives "\\)*\\)"
     "\\(" warning-part "\\(?:" all-alternatives "\\)*" "\\)?")))

(defun doxym-re-id ()
  "Returns a regexp matching a doxygen id."
  "\\(?:[-a-zA-Z0-9_:]+\\)")

(defun doxym-re-endl ()
  "Returns a regexp that matches 'whites' at the end of a line."
  "\\(?:[ \t]*\\(?:<!--.*\\(?:-->[ \t]*\\)?\\)?$\\)")

;; from doctokenizer.l:
;; ATTRIB   {ID}{WS}*("="{WS}*(("\""[^\"]*"\"")|("'"[^\']*"'")|[^ \t\r\n'"><]+))?
(defun doxym-re-xml-attrib (&optional name group-val)
  "Returns a string that matches an HTML tag attribute."
  (concat (when (stringp name) (doxym-re-upper-or-lowercase name) "\\w+") "[ \t]*=[ \t]*"
          "\\(" (unless group-val "?:")
            "'[^']*?\\(?:\n[^']*?\\)*?'"
          "\\|"
            "\"[^\"]*?\\(?:\n[^\"]*?\\)*?\""
          "\\|"
            "[^ \t\r\n'\"><]+"
          "\\)"))

;; from doctokenizer.l:
;; HTMLTAG   "<"(("/")?){ID}({WS}+{ATTRIB})*{WS}*(("/")?)">" 
(defun doxym-re-xml-tag (name &optional attrib-name dont-look-at-escapes)
  "Returns a regexp matching an xml tag.
NAME is a string or a list of strings. "
  (concat (unless dont-look-at-escapes (doxym-re-no-escape))
          "\\(<"
          "\\(?:"
          (mapconcat
           (lambda (x) (doxym-re-upper-or-lowercase x))
           (if (stringp name) (list name) name)
           "\\|")
          "\\)"
          (when attrib-name (concat "\\(?:[ \t\n]*" (doxym-re-xml-attrib attrib-name t) "\\)*"))
          "[ \t\n]*>\\)"))

(defun doxym-kw-cmd (name &optional face)
  "Returns a regex matching a Doxygen cmd which takes no arguments."
  (list
   (concat (doxym-re-no-escape) "\\(" (doxym-re-cmd name) "\\)")
   `(1 ,(or face 'doxym-delimiter))))

;;; kw - font lock keyword
;;; mkl - make lambda usable as highligher function in font lock keywords

(defmacro doxym-kw-word (name face &optional del-face)
  "Returns a font lock keyword to hightlight a doxygen command
  with the given NAME wich expects a single word as argument.
  That word is highlighted with FACE, the doxygen command with
  DEL-FACE. When DEL-FACE is nil, doxym-hide-delimiter is used."
  `(list ,(concat
           (doxym-re-no-escape)
           "\\(" (doxym-re-cmd name) "\\)"
           "\\(?:[ \t\n]+\\)"
	   (doxym-re-word))
         '(1 ,(or del-face 'doxym-hide-delimiter))
         '(2 ,face)
         '(3 doxym-warning nil t)))

;; just guessing that doxygen does it like that. From the manual, there's no
;; such thing as a 'id' command, there's only 'word' commands.
(defmacro doxym-kw-id (name face &optional del-face)
  "As doxym-kw-word, however for contexts where the word is an id"
  `(list ,(concat
           (doxym-re-no-escape)
           "\\(" (doxym-re-cmd name) "\\)"
           "\\(?:[ \t\n]+\\)"
           "\\(" (doxym-re-id) "\\)")
         '(1 ,(or del-face 'doxym-hide-delimiter))
         '(2 ,face)))

(defmacro doxym-kw-id-opt-txt (name face txt-face &optional not-opt del-face)
  `(list ,(concat
           (doxym-re-no-escape)
           "\\(" (doxym-re-cmd name) "\\)"
           "[ \t\n]+"
           "\\(" (doxym-re-id) "\\)"
           "\\(?:"
             "[ \t\n]+"
	     "\\(?:"
               "\\(\"\\)\\([^\"\n]+\\)\\(\"\\)" 
    	       "\\|"
	       "\\(\"\\)\\(.*\\)\\(\n[^\"]*?\"\\)" ; 
	     "\\)"
           "\\)" (unless not-opt "?"))
         '(1 ,(or del-face 'doxym-hide-delimiter))
         '(2 ,face)

         '(3 ,(or del-face 'doxym-hide-delimiter) nil t) ; "
         '(4 ,txt-face nil t)                            ; link text
         '(5 ,(or del-face 'doxym-hide-delimiter) nil t) ; "

         '(6 ,(or del-face 'doxym-hide-delimiter) nil t) ; :
         '(7 ,txt-face nil t)                            ; (bogous) link text
         '(8 doxym-warning nil t)))                      ; normal text which is actually meant to be link text

(defmacro doxym-kw-id-line (name id-face &optional del-face)
  `(list ,(concat
           (doxym-re-no-escape)
           "\\(" (doxym-re-cmd name) "\\)[ \t]+" ; doxygen command
           "\\(" (doxym-re-id) "\\)"		 ; 1st arg: id
           "\\(.*?\\)" (doxym-re-endl))		 ; 2nd arg: up to end of line
         '(1 ,(or del-face 'doxym-hide-delimiter))
         '(2 ,id-face)))
     
(defmacro doxym-kw-line (name face &optional del-face)
  `(list ,(concat
           (doxym-re-no-escape)
           "\\(" (doxym-re-cmd name) "\\)[ \t]+" ; doxygen command
           "\\(.*?\\)" (doxym-re-endl))                          ; arg: up to end of line
         '(1 ,(or del-face 'doxym-hide-delimiter))
         '(2 ,face)))

;; id is optional. if left away, that single word is the text and the id. except
;; for page, there its only the id and the title text is empty.
(defun doxym-re-section(name)
  (concat
   (doxym-re-no-escape)
   "\\(" (doxym-re-cmd name) "\\)"	    ; 1: doxygen command
   "\\(?:[ \t]+\\(" (doxym-re-id) "\\)\\)?" ; 2: id
   "\\([ \t]+\\)"			    ; 3: 
   "\\(.+?\\)" (doxym-re-endl)))	    ; 4: sections's text

(defun doxym-mkl-section(name)
  `(lambda (end)
     (let* ((found (re-search-forward (doxym-re-section ,name) end t)))
       (when (and found doxym-break-section-line)
         (let* ((o (make-overlay (match-beginning 3) (match-end 3))))
           (overlay-put o 'before-string (concat doxym-break-section-string "\n")))) ; could also be text property via font lock
       found)))

(defun doxym-kw-section (name face)
  (list
   (doxym-mkl-section name)
   `(1 ,doxym-hide-delimiter)
   `(2 ,doxym-anchor nil t)
;   (list 3 (list 'face face 'invisible t))
   `(4 ,face)))

(defmacro doxym-kw-span (name del-start del-end face &optional del-arg del-face)
  `(list ,(concat (doxym-re-no-escape)
                  "\\(" (doxym-re-cmd name) (or del-arg "") del-start "\\)"
                  (doxym-re-enclosed-str)
                  "\\(" (doxym-re-cmd name) del-end "\\)")
         '(1 ,(or del-face 'doxym-delimiter))
         '(2 ,face)
         '(3 ,(or del-face 'doxym-delimiter))))

;; bug: multiline constructs not yet properly supported
;; todo: for dt/dd etc elements: name can also be a cons cell with start/end tag name
;; todo: correct case rules for html element names
;;
;; note that surprisingly doxygen allows that elements are across
;; paragraph boundaries (i.e. empty lines). It then creates 2
;; elements in the output, 1 for the end of the 1st paragraph, 1 for
;; the start of the 2nd paragraph.
;; 
;; However interleaving other elements, e.g. <b> and <i>, is as
;; expected not allowed.
(defun doxym-kw-xml-element (name face &optional del-face attrib-name attrib-face)
  "Returns a keyword that to highlight a xml element."
  (let* ((startname (if (consp name) (car name) name))
         (endname (if (consp name) (cdr name) (concat "/" name)))
         (start-face-tmp (if (consp del-face) (car del-face) del-face))
         (end-face-tmp (if (consp del-face) (cdr del-face) del-face))
         (start-face (or start-face-tmp 'doxym-hide-delimiter))
         (end-face (or end-face-tmp 'doxym-hide-delimiter)))
    (append
     (list
      (concat
       (doxym-re-xml-tag startname attrib-name)
       (doxym-re-enclosed-str)
       (doxym-re-xml-tag endname nil t)))
     (if attrib-name
         (list
          `(1 ,start-face)
          `(2 ,attrib-face append)
          `(3 ,face t)
          `(4 ,end-face))
       (list
        `(1 ,start-face)
        `(2 ,face append)
        `(3 ,end-face))))))

(defun doxym-kw-xml-tag(tag &optional face)
  (list
   (doxym-re-xml-tag tag)
   `(1 ,(or face 'doxym-delimiter))))
  
;; doxygen only supports a few character entities
;; http://www.stack.nl/~dimitri/doxygen/htmlcmds.html
(defun doxym-kw-entity ()
  (list
   (concat
    (doxym-re-no-escape) 
    "\\(?:"
    ;; doxygen's
    "\\(&\\(?:"
    "copy\\|tm\\|reg\\|lt\\|gt\\|amp\\|apos\\|quot\\|lsquo\\|rsquo\\|ldquo\\|"
    "rdquo\\|ndash\\|mdash\\|"
    "\\(?:A\\|E\\|I\\|O\\|U\\|Y\\|a\\|e\\|i\\|o\\|u\\|y\\)uml\\|"
    "\\(?:A\\|E\\|I\\|O\\|U\\|Y\\|a\\|e\\|i\\|o\\|u\\|y\\)acute\\|"
    "\\(?:A\\|E\\|I\\|O\\|U\\|a\\|e\\|i\\|o\\|u\\|y\\)grave\\|"
    "\\(?:A\\|E\\|I\\|O\\|U\\|a\\|e\\|i\\|o\\|u\\|y\\)circ\\|"
    "\\(?:A\\|N\\|O\\|a\\|n\\|o\\)tilde\\|"
    "szlig\\|\\(?:c\\|C\\)cedil\\|\\(?:a\\|A\\)ring\\|nbsp"
    "\\);\\)"
    ;;
    "\\|"
    ;; bogous
    "\\(&#?[a-zA-Z0-9]+;\\)"
    ;;
    "\\)"
    )
   '(1 doxym-replacement nil t)
   '(2 doxym-warning nil t)))

;; from doctokenizer.l (MAILADR):
;; [a-z_A-Z0-9.+-]+"@"[a-z_A-Z0-9-]+("."[a-z_A-Z0-9\-]+)+[a-z_A-Z0-9\-]+
(defun doxym-kw-email()
  (cons
   "\\b[a-z_A-Z0-9.+-]+@[a-z_A-Z0-9-]+\\(?:\\.[a-z_A-Z0-9\-]+\\)+[a-z_A-Z0-9\-]+"
   'adoc-reference)) 

;; from doctokenizer.l:
;; URLCHAR   [a-z_A-Z0-9\!\~\,\:\;\'\$\?\@\&\%\#\.\-\+\/\=]
;; URLMASK   ({URLCHAR}+([({]{URLCHAR}*[)}])?)* 
;; <St_Para>("http:"|"https:"|"ftp:"|"file:"|"news:"){URLMASK} { ...
;; 
;; doxym-extension: the last char of the url is warning highlighted, if its
;; likelely that the writer did not meant to be it part of the url, e.g. a '.'
;; (full stop).
(defun doxym-kw-url()
  (list
   (let (;; equal to URLCHAR, see above
         (c "[-a-z_A-Z0-9!~,:;'$?@&%#.+/=]")
         ;; last char of an url which makes common sense. Is subset of c.
         (lc "[a-z_A-Z0-9]")) 
     (concat
      "\\("
        "\\b\\(?:https?\\|ftp\\|file\\|news\\):"
        "\\(?:" c "+\\(?:[({]" c "*[)}]\\)?\\)*"
        "\\(?:" lc "\\|\\(" c "\\)\\)" ; the trick is that lc is testet before c
      "\\)"))
   '(1 doxym-reference)
   '(2 doxym-warning prepend t)))

(defun doxym-kw-image()
  (list
   (concat
    (doxym-re-no-escape)
    "\\(" (doxym-re-cmd "image") "\\)[ \t]+"  ; cmd
    "\\(\\w+\\)[ \t]+"                        ; format
    "\\([^ \t\n]+\\)"                         ; filename
    "\\(?:" (doxym-re-endl) "\\|[ \t]+\\)"    ; now end the line or use spaces to separate the next args   
    "\\(?:\\(\"\\)\\([^\"\n]*\\)\\(\"\\)\\)?" ; [caption]
    "\\(?:" (doxym-re-endl) "\\|[ \t]+\\)"    ; now end the line or use spaces to separate the next args   
    "\\(\\(?:width\\|height\\)=[^ \t\n]+\\)?" ; [size]
    )
   '(1 doxym-complex-replacement)       ; cmd
   '(2 doxym-hide-delimiter)            ; format
   '(3 doxym-delimiter)                 ; file
   '(4 doxym-hide-delimiter nil t)      ; "
   '(5 doxym-secondary-text nil t)      ; [caption]
   '(6 doxym-hide-delimiter nil t)      ; "
   '(7 doxym-delimiter nil t)))         ; [size]p

;; todo: copy from other modes how they relyable can match large multile
;; constructs, such as verb|...| in Latex
(defun doxym-kw-block(cmd)
  (list
   (concat
    "^[ \t]*\\(" (doxym-re-cmd cmd) "\\)[ \t]*\n"
    "\\(\\(?:.*\n\\)*?\\)"
    "[ \t]*\\(" (doxym-re-cmd (concat "end" cmd)) "\\)" (doxym-re-endl))
   '(1 doxym-hide-delimiter)
   '(2 doxym-code nil t)	
   '(3 doxym-hide-delimiter)))
  
(defun doxym-subscript-facespec ()
  `'(face ,doxym-subscript display (raise -0.3)))

(defun doxym-superscript-facespec ()
  `'(face ,doxym-superscript display (raise 0.3)))

(defun doxym-etags(&optional files tagfile)
  "Creates a TAGS file named TAGFILE out of FILES.
If FILES is nil, `buffer-file-name' is used. If TAGFILE is nil,
etags' defult it used."
  (interactive)
  (let ((re (concat
             "[ \\t]*[\\\\@]"
             "\\(?:"
             "\\(?:main\\)?page\\|"
             "\\(?:sub\\)*section\\|"
             "anchor\\|"
             "addindex\\|"
             "\\)"
             "[ \\1]*\\([-a-zA-Z0-9_]+\\)")))
    (shell-command (concat "etags --language=none --regex='/" re "/\\1/' " (buffer-file-name)))))

(defconst doxym-font-lock-keywords
  (list
   
   ;; comments
   ;; technically a comment, but semantically used to underline a heading. Doxym
   ;; has syntax highlighting, so this sort of highlightening is not needed.
   (list
    (concat (doxym-re-no-escape) "\\(/~-[ \t]*\\([-#=_]\\)\\2+[ \t]*/~\\)")
    '(1 doxym-hide-delimiter)) 
   (list
    (concat (doxym-re-no-escape) "\\(<!--[ \t]*\\([-#=_]\\)\\2+[ \t]*-->\\)")
    '(1 doxym-hide-delimiter)) 
   (list
    (concat (doxym-re-no-escape) "\\(/~-.*?/~\\)")
    '(1 doxym-comment))
   (list
    (concat (doxym-re-no-escape) "\\(<!--.*?\\(?:\n.*?\\)*?-->\\)")
    '(1 doxym-comment)) 
   ;; bufferstart upto first /** 
   (cons (concat
          "\\`"
          "\\(\\(?:.\\|\n\\)*?\\)"
          "\\(/\\*\\(\\*+\\|!\\)\\)")
         'doxym-comment) 
   ;; last */ up to buffer end
   (cons (concat
          "\\(\\*+/\\)"
          "\\([^/]*\\(/\\([^*]\\|\\*\\([^*!]\\|\\'\\)\\|\\'\\)[^/]*\\)*\\)"
          "\\'")
         'doxym-comment) 
   (cons (concat "\\(\\*+/\\)"
                 "\\(\\(?:.\\|\n\\)*?\\)"
                 "\\(/\\*\\(?:\\*+\\|!\\)\\)")
         'doxym-comment) 
   ;; (cons "/\\*\\*+\\|\\*+/" 'doxym-comment);

   ;; verbatim/code blocks
   (doxym-kw-block "verbatim")
   (doxym-kw-block "code")

   ;; math
   ;; math: in-text formulas
   (doxym-kw-span "f" "\\$" "\\$" doxym-passthrough) 
   ;; math: unnumbered & centered & on separate line
   (doxym-kw-span "f" "\\[" "\\]" doxym-passthrough) 
   ;; math: special environment
   (doxym-kw-span "f" "{" "}" doxym-passthrough "{[^}\n]*?}") 

   ;; 'preprocessor'
   (doxym-kw-word "cond" font-lock-preprocessor-face font-lock-preprocessor-face)
   (doxym-kw-cmd "endcond" font-lock-preprocessor-face)
   (doxym-kw-word "if" font-lock-preprocessor-face font-lock-preprocessor-face)
   (doxym-kw-cmd "endif" font-lock-preprocessor-face)

   ;; document structure
   ;; todo: actual title on a new line, i.e. only for display insert a newline
   (doxym-kw-line "mainpage" doxym-title-0)
   (doxym-kw-section "page" 'doxym-title-0)
   (doxym-kw-section "section" 'doxym-title-1)
   (doxym-kw-section "subsection" 'doxym-title-2)
   (doxym-kw-section "subsubsection" 'doxym-title-3)
   (doxym-kw-xml-element "h1" doxym-title-0) ;page
   (doxym-kw-xml-element "h2" doxym-title-1) ;section
   (doxym-kw-xml-element "h3" doxym-title-2)
   (doxym-kw-xml-element "h4" doxym-title-3)
   (doxym-kw-xml-element "h5" doxym-title-4)
   (doxym-kw-xml-element "h6" doxym-title-5)
   (doxym-kw-xml-tag "p" doxym-delimiter)

   ;; phrase formatting
   ;; 2) Concerning HTML, pre preserves spaces and is typically formatted using
   ;;    fixed with font. That would. However doxygen treats it very similar to the code element.
   ;; 3) It's unclear wheter \b is an alias for <B> or <STRONG>. Let's be nice
   ;;    and say <B>, which is better.
   (doxym-kw-word "b" doxym-strong) ; 3)
   (doxym-kw-word "e" doxym-emphasis)
   (doxym-kw-word "em" doxym-emphasis)
   (doxym-kw-word "c" doxym-code)
   (doxym-kw-xml-element "em" doxym-emphasis)
   (doxym-kw-xml-element "i" doxym-italic)
   (doxym-kw-xml-element "b" doxym-bold)
   (doxym-kw-xml-element "strong" doxym-strong)
   (doxym-kw-xml-element "tt" doxym-typewriter)
   (doxym-kw-xml-element "kbd" doxym-code)
   (doxym-kw-xml-element "code" doxym-code)
   (doxym-kw-xml-element "pre" doxym-code);2) 
   (doxym-kw-xml-element "var" doxym-code)
   (doxym-kw-xml-element "dfn" doxym-code)
   (doxym-kw-xml-element "small" doxym-gen doxym-delimiter)
   (doxym-kw-xml-element "sub" (doxym-subscript-facespec)) 
   (doxym-kw-xml-element "sup" (doxym-superscript-facespec))
   (doxym-kw-xml-element "center" doxym-gen doxym-delimiter)

   ;; admonition paragraphs
   (doxym-kw-cmd "note\\|warning\\|caution\\|attention" doxym-complex-replacement)
   (list (concat (doxym-re-no-escape) "\\([\\@]par\\)[ \t]+\\(.*?\\)" (doxym-re-endl))
         '(1 doxym-delimiter) '(2 doxym-gen))

   ;; other paragraphs
   (doxym-kw-cmd "authors?\\|version\\|date\\|copyright" doxym-replacement)
   
   ;; todo: error/warning face if reference text contains a line break
   (doxym-kw-id-opt-txt "ref" doxym-delimiter doxym-reference t)
   (doxym-kw-id "ref" doxym-reference)
   (cons (concat "[a-zA-Z0-9][a-zA-Z0-9_]+\\(?:#\\|::\\)"
        	 "[a-zA-Z0-9][a-zA-Z0-9_]+\\([ \t]*([^)\n]*?)\\)?")
         'doxym-reference)
   (list (concat
          "\\(#\\|::\\)\\([a-zA-Z0-9][a-zA-Z0-9_]+"
          "\\([ \t]*([^)\n]*?)\\)?\\)")
         '(1 doxym-hide-delimiter) '(2 doxym-reference))

   ;; bug: urls in href are also fontified with urls's regex
   (doxym-kw-xml-element "a" 'doxym-text 'doxym-hide-delimiter "name" 'doxym-anchor)
   (doxym-kw-xml-element "a" 'doxym-reference 'doxym-hide-delimiter "href" 'doxym-delimiter)
   ;; bug: its everything up to the end
   (doxym-kw-line "addindex" doxym-anchor)
   (doxym-kw-id "anchor" doxym-anchor)

   ;; references, links, index ...
   ;; must come after <a...> elements so the href="..." url is not overwritten
   (doxym-kw-email)
   (doxym-kw-url)

   ;; lists
   ;; 1) In general hide-delimiter would be ok, but for nested lists its not bad to
   ;; see the tags
   ;; 2) The point ending a list item
   (doxym-kw-xml-tag "/?[oud]l") ;1)
   (doxym-kw-xml-tag "li" doxym-list)
   (doxym-kw-xml-element (cons "dt" "/dt") doxym-gen (cons doxym-list doxym-hide-delimiter))
   (doxym-kw-xml-element (cons "dt" "dd") doxym-gen (cons doxym-list doxym-delimiter))
   (doxym-kw-xml-tag "dt")
   (doxym-kw-xml-tag "/\\(?:li\\|d[td]\\)" doxym-hide-delimiter)
   (list "^[ \t]*\\(-#?\\) +" '(1 doxym-list))
   (list (concat "^[ \t]*\\(\\.\\)[ \t]*$" (doxym-re-endl)) '(1 doxym-delimiter)) ;2)

   ;; tables
   (doxym-kw-xml-tag "/?table" doxym-table) 
   (doxym-kw-xml-tag "tr" doxym-table-row)
   (doxym-kw-xml-tag "t[dh]" doxym-table-cell)
   (doxym-kw-xml-tag "/t[dhr]" doxym-hide-delimiter)
   (doxym-kw-xml-element "caption" doxym-secondary-text)
   ;; problem is that end tag is eaten up and thus not fontified properly
   ;; (list (concat (doxym-re-xml-tag "th")
   ;; 		 (doxym-re-enclosed-str)
   ;; 		 (doxym-re-xml-tag "/t[hr]\\|/table\\|t[drh]"))
   ;; 	 '(1 doxym-table-cell)
   ;; 	 '(2 doxym-gen)
   ;; 	 '(3 doxym-table-cell))

   ;; bug: better HTML tolerance: caseinsensitive, spaces,...

   ;; misc
   ;; entitys ala &amp
   ;; \dotfile
   (doxym-kw-entity) 
   (doxym-kw-xml-tag "hr" 'doxym-complex-replacement)
   (doxym-kw-cmd "n")
   (doxym-kw-xml-tag "br")
   (doxym-kw-xml-tag "p")
   (doxym-kw-xml-tag "/p")
   (doxym-kw-image)

   ;; macro
   (list (concat (doxym-re-no-escape) "\\([\\@]\\w+[ \t]*{.*?}\\)") '(1 doxym-delimiter t))

   ;; escaped characters
   (list "\\([\\@]\\)[&$@\\#<>%\"]" '(1 doxym-hide-delimiter t)) 

   (doxym-kw-id-line "defgroup" doxym-anchor)
   (doxym-kw-id "ingroup" doxym-reference)
   
   ;; todo: this is nova specific. doxym shall provide a way so clients can
   ;; define custum doxygen keywords.
   (doxym-kw-cmd "toc" doxym-complex-replacement)

   ;; so things are nicely aligned
   ;; KLUDGE: actually don't do it in verbatim sections, since there the code
   ;; around probalby has doxym-verbatim, which might be (in buffer-face-mode)
   ;; of different width than doxym-fixed-pitch-text.
   (list "^\\([ \t]+\\)" '(1 doxym-meta t))

   ;; unknown commands
   ;; todo: make unknown's warning face, but provide a custom list of such wich
   ;; should be highlighted width delimiter face
   (list (concat (doxym-re-no-escape) "\\([\\@]\\w+\\)") '(1 doxym-delimiter)) ;; doxygen cmd
   (list (concat (doxym-re-no-escape) "\\(<[^>\n]*?>\\)") '(1 doxym-delimiter))  ;; html tag
   )
  "Keywords to highlight in doxym-mode.")

;;; miscellanous

;; 
(defun doxym-unfontify-region-function (beg end) 
  (when doxym-break-section-line (remove-overlays beg end))
  (font-lock-default-unfontify-region beg end))

(defun doxym-re-page-delimiter()
  (concat
   "\\(?:"
   ;; section stuff - todo: add html headings
   "[ \t]*\\\\\\(?:\\(?:main\\)?page\\|\\(?:sub\\)*section\\)\\b" 
   "\\)"
   ))

(defun doxym-re-paragraph-separate()
  (concat
   "\\(?:"
   ;; empty line
   "[ \t]*$" "\\|"
   ;; tables / lists
   "[ \t]*</?\\(?:table\\|dl\\|ul\\|ol\\)>" "\\|"
   ;; block stuff
   "[ \t]*\\\\\\(?:end\\)?\\(?:verbatim\\|code\\)\\b" "\\|"
   ;; point ending a list item
   "[ \t]*\\(\\.\\)[ \t]*$" 
   "\\)" ))

(defun doxym-re-paragraph-start()
  (concat
   "\\(?:"
   (doxym-re-paragraph-separate) "\\|"
   "\\(?:"
   ;; lets count section headings also as 'paragraphs'; i find that conventient.
   (doxym-re-page-delimiter) "\\|"
   ;; list/table items
   "[ \t]*<\\(?:td\\|tr\\|td\\|dt\\|dd\\|li\\)>" "\\|"
   "[ \t]*\\(-#?\\) +" 
   "\\)"
   "\\)"))

(defun doxym-toggle-ref()
  (interactive)
  (re-search-backward "<a\\b")
  (re-search-forward "<a\\s-+href\\s-*=\\s-*\"\\(?:[^/\"]*?/\\)?\\([^\"]+\\)\\.html\"\\s-*>\\s-*\\(.*?\\)\\s-*</a\\s-*>")
  (let ((str1 (match-string 1))
	(str2 (match-string 2)))
    (delete-region (match-beginning 0) (match-end 0))
    (insert "\\ref " str1)
    (when (> (length str2) 0)
      (insert " \"" str2 "\""))))

(defun doxym-mode-flyspell-verify () 
  "Function used for `flyspell-generic-check-word-predicate' in doxym mode."
  (save-excursion
    (forward-word -1)			
    (and (not (looking-back "[@\\]\\|</?\\|[@\\]ref\\(\\s-\\|\n\\)+"))
	 (let (case-fold-search) (looking-at "[a-zA-Z][a-z]*\\b")))))

;;;###autoload
(define-derived-mode doxym-mode text-mode "doxym"
  "Major mode for editing doxygen pages.
Turning on doxym mode runs the normal hook `doxym-mode-hook'."
  
  ;; syntax table
  ;; todo: a correct syntax table. But be carefullt that the regex \b still works as it should
  (modify-syntax-entry ?\$  ".")
  (modify-syntax-entry ?\@  ".")
  (modify-syntax-entry ?\{  ".")
  (modify-syntax-entry ?\}  ".")

  ;; comments
  (set (make-local-variable 'comment-column) 0)
  (set (make-local-variable 'comment-start) "<!--")
  (set (make-local-variable 'comment-end) "-->")
  (set (make-local-variable 'comment-start-skip) "\\(?:<!--\\|/~-\\)[ \t]*")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(?:/~\\([^-\n]\\|$\\)\\|-->\\)")
  
  ;; pages
  (set (make-local-variable 'page-delimiter) (doxym-re-page-delimiter)) 

  ;; paragraphs
  (set (make-local-variable 'paragraph-separate) (doxym-re-paragraph-separate)) 
  (set (make-local-variable 'paragraph-start) (doxym-re-paragraph-start))
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  
  ;; filladapt
  (when (boundp 'filladapt-token-table)
    (let ((bullet-regexp "</?\\(?:dt\\|dd\\|li\\|tr\\|td\\|th\\)>"))
      (unless (assoc bullet-regexp filladapt-token-table)
	(setq filladapt-token-table
	      (append filladapt-token-table
		      (list (list bullet-regexp 'bullet)))))))

  ;; font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(doxym-font-lock-keywords nil nil ((?_ . "w")) ))
  (make-local-variable 'font-lock-extra-managed-props)
  (setq font-lock-extra-managed-props (list 'display))
  (make-local-variable 'font-lock-unfontify-region-function)
  (setq font-lock-unfontify-region-function 'doxym-unfontify-region-function)
  
  ;; outline mode
  ;; todo: recognize also HTML headings
  (set (make-local-variable 'outline-regexp)
       (concat "[ \t]*" (doxym-re-cmd "\\(?:mainpage\\|page\\|\\(?:sub\\)\\{,2\\}section\\)")))
  
  ;; flyspell
  (setq flyspell-generic-check-word-predicate 'doxym-mode-flyspell-verify)
  

  (run-hooks 'doxym-mode-hook))

(provide 'doxym-mode)

;;; doxym-mode.el ends here
