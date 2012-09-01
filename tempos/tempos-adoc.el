(require 'tempo)
(require 'tempo-snippets)

;;; common used defuns/vars

;;; actual tempos 

(tempo-define-template "adoc-title-0" '( & "= " r))
(tempo-define-template "adoc-title-1" '( & "== " r ))
(tempo-define-template "adoc-title-2" '( & "=== " r ))
(tempo-define-template "adoc-title-3" '( & "==== " r ))
(tempo-define-template "adoc-title-4" '( & "===== " r ))

(tempo-define-template "adoc-emph" '( "_" r "_" ))
(tempo-define-template "adoc-strong" '( "*" r "*" ))
(tempo-define-template "adoc-code" '( "+" r "+" ))

(provide 'tempos-adoc)
