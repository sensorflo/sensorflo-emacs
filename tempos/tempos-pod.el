(require 'tempo)
(require 'tempo-snippets)

;;; special chars
(tempo-define-template "pod-escape"
 '( "E<" r ">" )
 "escape")


;;; inline text properties 
(tempo-define-template "pod-code"
 '( "C<" r ">" )
 "code")

(tempo-define-template "pod-bold"
 '( "B<" r ">" )
 "bold")

(tempo-define-template "pod-italic"
 '( "I<" r ">" )
 "italic")

(tempo-define-template "pod-file-name"
 '( "F<" r ">" )
 "file")

(tempo-define-template "pod-non-breaking-spaces"
 '( "S<" r ">" )
 "nsb")

;;; lists 
(tempo-define-template "pod-list"
 '( "=over" n
    r
    "=back" n )
 "list")

(tempo-define-template "pod-item"
 '( "=item " p > n>
    > n>
    r )
 "item")

;;; sections 
(tempo-define-template "pod-h1"
  '("=head1" r ))

(tempo-define-template "pod-h2"
  '("=head2" r ))

(tempo-define-template "pod-h3"
  '("=head3" r ))

(tempo-define-template "pod-h4"
  '("=head4" r ))


;;; misc 
(tempo-define-template "pod-command"
 '( p "<" r ">"))

(provide 'tempos-pod)