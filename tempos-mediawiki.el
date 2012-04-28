(require 'tempo)
(require 'tempo-snippets)

;;; common used defuns/vars

;;; actual tempos 

(tempo-define-template "mediawiki-title-0" '( & "= " r " =" ))
(tempo-define-template "mediawiki-title-1" '( & "== " r " ==" ))
(tempo-define-template "mediawiki-title-2" '( & "=== " r " ===" ))
(tempo-define-template "mediawiki-title-3" '( & "==== " r " ====" ))
(tempo-define-template "mediawiki-title-4" '( & "===== " r " =====" ))

(tempo-define-template "mediawiki-bold" '( "'''" r "'''" ))
(tempo-define-template "mediawiki-italic" '( "''" r "''" ))

(provide 'tempos-mediawiki)
