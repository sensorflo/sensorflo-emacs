(require 'tempo)
(require 'tempo-snippets)

(tempo-define-template "bbcode-bold" '( "[b]" r "[/b]" ))
(tempo-define-template "bbcode-italic" '( "[i]" r "[/i]" ))
(tempo-define-template "bbcode-underline" '( "[u]" r "[/u]" ))

(tempo-define-template "bbcode-list-item" '( & "[*] " r > ))

(tempo-define-template
 "bbcode-ulist"
 '( & "[list]\n[*] " r "\n[*] " p "\n[*]" p "\n[/list]" ))

(tempo-define-template
 "bbcode-olist"
 '( & "[list=]\n[*] " r "\n[*] " p "\n[*] " p "\n[/list]" ))

(tempo-define-template
 "bbcode-code"
 '( "[code]" > n>
    r > n>
    "[/code]" > ))

(tempo-define-template
 "bbcode-quote"
 '( "[quote]" > n>
    r > n>
    "[/quote]" > ))

(provide 'tempos-bbcode)