(require 'tempo)
(require 'tempo-snippets)

(tempo-define-template
 "bash-if"
 '( &
    "if [ " p " ]; then" > n>
    r> n>
    "fi" >)
 "if")

(tempo-define-template
 "bash-elif"
 '( &
    "elif [ " p " ]; then" > n> )
 "elif")

(tempo-define-template
 "bash-for"
 '( &
    "for (( " p " ; " p " ; " p " )); do" > n>
    r> n>
    "done" > )
 "for")

(tempo-define-template
 "bash-for-in"
 '( &
    "for " p " in " p " ; do" > n>
    r> n>
    "done" > )
 "forin")

(tempo-define-template
 "bash-while"
 '( "while " p " ; do" > n>
    r> n>
    "done" > )
 "while")

(tempo-define-template
 "bash-until"
 '( "until " p " ; do" > n>
    r> n>
    "done" >)
 "until")

(tempo-define-template
 "bash-def-sub"
 '( "function " p " {" > n>
    r> n>
    "}" >)
 "until")

(tempo-define-template
 "bash-case"
 '( "case " p " in" > n>
    p ")" > n>
    p > n>
    ";;" > n>
    p "*)" > n>
    p > n>
    ";;" > n>
    "esac" >)
 "case")

(tempo-define-template
 "bash-case-clause"
 '( p ")" > n>
    p > n>
    ";;" >))

(provide 'tempos-bash)