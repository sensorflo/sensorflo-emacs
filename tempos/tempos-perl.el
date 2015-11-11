(require 'tempo)
(require 'tempo-snippets)

(tempo-define-template
 "perl-def-sub"
 '( & "# " p n> "sub " p "(" p ") {" n>
      "my (" p ") = @_;" n>
      r> n>
      "}" > )
 "sub")

(tempo-define-template
 "perl-if"
 '( & "if (" p ") {" > n> r> n> "} " > )
 "if")

(tempo-define-template
 "perl-elsif"
 '( & "elsif (" p ") {" > n> r> n> "} " > )
 "else")

(tempo-define-template
 "perl-else"
 '( & "else {" n> r> n> "} " > )
 "else")

(tempo-define-template
 "perl-while"
 '( & "while (" p ") {" n> r> n> "} " > )
 "while")

(tempo-define-template
 "perl-for"
 '( & "for (" p ") {" n> r> n> "} " > )
 "for")

(tempo-define-template
 "perl-continue"
 '( & "continue {" n> r> n> "} " > )
 "continue")

(tempo-define-snippet
 "perl-pod"
 '( &
"__END__

=pod

=head1 NAME

B<" (P "name : " name) "> - " p "

=head1 SYNOPSIS

B<" (s name) "> " p "

=head1 DESCRIPTION

" p "

=head1 OPTIONS

" p "

=over 4

=item B<" p "> I<" p ">

" p "

=back

=head1 EXAMPLES

" p "

=head1 AUTHOR

Written by Florian Kaufmann

=head1 COPYRIGHT

Copyright (C) 2008 Free Software Foundation, Inc. License GPLv3+: GNU GPL
version 3 or later <http://gnu.org/licenses/gpl.html>

This is free software: you are free to change and redistribute it. There is NO
WARRANTY, to the extent permitted by law.

=cut" ))

(provide 'tempos-perl)
