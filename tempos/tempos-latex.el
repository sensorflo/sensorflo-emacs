(require 'tempo)
(require 'tempo-snippets)

;;; special chars

;;; inline text properties
(tempo-define-template "latex-verbatim"
 '( "\\verb|" r "|" )
 "verb")

(tempo-define-template "latex-bold"
 '( "\\textbf{" r "}" )
 "verb")

(tempo-define-template "latex-emph"
 '( "\\emph{" r "}" )
 "verb")

(tempo-define-template "latex-math"
 '( "$" r "$" )
 "verb")

(tempo-define-template "latex-text-in-math"
 '( "\\textrm{" r "}" )
 "verb")

;;; block text properties
(tempo-define-template "latex-verbatim-block"
 '( "\\begin{verbatim}" n
    r
    "\\end{verbatim}" n )
 "verbb")

(tempo-define-template "latex-math-block"
 '( "\\[" r "\\]" )
 "verb")

(tempo-define-template "latex-equation"
 '( "\\begin{equation}" n
    r
    "\\end{equation}" n )
 "verb")

;;; lists
(tempo-define-template "latex-enumeration"
 '( "\\begin{enumeration}" n
    r
    "\\end{enumeration}" n )
 "enum")

(tempo-define-template "latex-itemize"
 '( "\\begin{itemize}" n
    r
    "\\end{itemize}" n )
 "enum")

(tempo-define-template "latex-description"
 '( "\\begin{description}" n
    r
    "\\end{description}" n )
 "enum")

(tempo-define-template "latex-item"
 '( "\\item " )
 "item")

(tempo-define-template
 "latex-def-item"
 '( "\\item[" r "] " )
 "item")

;;; sections
(tempo-define-template "latex-h0"
  '("\\title{" r "}"))

(tempo-define-template "latex-h1"
  '("\\chapter{" r "}"))

(tempo-define-template "latex-h2"
  '("\\section{" r "}"))

(tempo-define-template "latex-h3"
  '("\\subsection{" r "}"))

(tempo-define-template "latex-h4"
  '("\\subsubsection{" r "}"))

(tempo-define-template "latex-h5"
  '("\\subsubsubsection{" r "}"))


;;; misc
(tempo-define-snippet "latex-env"
 '( "\\begin{" (p "env: \n" env) "}" n
    r
    "\\end{" (s env) "}" n ))

(tempo-define-template "latex-command"
 '( "\\" p "{" r "}"))

(provide 'tempos-latex)
