(require 'tempo)
(require 'tempo-snippets)

(tempo-define-template
 "py-for"
 '( & "for " p " in " p ":" n> p )
 "for")

(tempo-define-template
 "py-while"
 '( & "while " p ":" n> p )
 "for")

(tempo-define-template
 "py-def-func"
 '( & "def " p "(" p "):" n> "\"\"\"" p "\"\"\"" n> p )
 "deff")

(tempo-define-template
 "py-def-class"
 '( & "class " p  ":" n> p )
 "defc")

(provide 'tempos-python)