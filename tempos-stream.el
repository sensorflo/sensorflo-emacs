(require 'tempo)
(require 'tempo-snippets)

(tempo-define-template
 "stream-if"
 '( &
    "if [ " p " ]; then" > n>
    r> n>
    "fi" >)
 "if")

(provide 'tempos-stream)