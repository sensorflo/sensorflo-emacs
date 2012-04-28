(require 'tempo)
(require 'tempo-snippets)

(tempo-define-template
 "elisp-defun"
 '( & "(defun " p " (" p ") " n>
    "\"" p "\"" n>
    "(interactive)" n>
    "(let (" p ")" n>
    r> "))" > )
 "defun")

(tempo-define-template
 "elisp-if"
 '( "(if " p r ")")
 "if")

(tempo-define-template
 "elisp-while"
 '( "(while " p r ")")
 "while")

(tempo-define-template
 "elisp-let"
 '( "(let (" p ")" n> r> ")")
 "let")

(provide 'tempos-elisp)