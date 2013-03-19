AsciiDoc (http://www.methods.co.nz/asciidoc/) is a text document format for
writing short documents, articles, books and UNIX man pages. AsciiDoc files
can be translated to HTML and DocBook markups.

This is just a first version which works not too bad for my small uses of
AsciiDoc. It's mostly about syntax highlighting. I still like to play a lot
and thus it's not stable at all.

I actually would like to improve it, but realistically will invest
my time in other activities.

Installation:

Installation is as usual, so if you are proficient with Emacs you don't need
to read this.

1. Copy this file to a directory in `load-path'. To add a specific directory
   to the load path, add this to your initialization file (~/.emacs or ~/_emacs):
   (add-to-list 'load-path "mypath")

2. Add either of the two following lines to your initialization file:
   a)  (autoload 'adoc-mode "adoc-mode")
   b)  (require 'adoc-mode)
   The first only loads adoc mode when necessary, the 2nd always during
   startup of Emacs.

3. To use adoc mode, call adoc-mode after you opened an AsciiDoc file
   M-x adoc-mode

Each of the following is optional

* Byte compile this file (adoc-mode.el) for faster startup:
  M-x byte-compile

* According to AsciiDoc manual, '.txt' is the standard file extension for
  AsciiDoc files. Add the following to your initialization file to open all
  '.txt' files with adoc-mode as major mode automatically:
  (add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))

* If your default face is a fixed pitch (monospace) face, but in AsciiDoc
  files you liked to have normal text with a variable pitch face,
  `buffer-face-mode' is for you:
  (add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))


Todo:
- Fontlock
  - make font-lock regexps based upon AsciiDoc configuration file, or
    make them configurable in a way similar to that configuration file
  - respect font-lock-maximum-decoration
- Other common emacs functionality/features
  - indent functions
  - imenu / outline / hideshow
  - tags tables for anchors, indixes, bibliography items, titles, ...
  - Make 'compilation', i.e. translating into the desired output format more
    conventient
  - tempo-templates
  - spell check shall ignore text that is not part of the output
  - supply a regexp for magic-mode-alist
  - Is there something that would remove hard newlines within a paragraph,
    but just for display, so the paragraph uses the whole buffer length.
  - are there generic base packages to handle lists / tables?
  - a readonly view mode where commands for navigation are on short key
    bindings like alphanum letters
  - study what other markup modes like rst offer
- AsciiDoc related features
  - Two (or gruadualy fading) display modes: one emphasises to see the
    AsciiDoc source text, the other emphasises to see how the output will
    look like.
  - invisible text property could be used to hide meta characters

Bugs:
- delimited blocks are supported, but not well at all
- Most regexps for highlighting can spawn at most over two lines.
- font-lock's multi line capabilities are not used well enough. At least 2
  line spawns should be covered - replace all .*? by .*?\\(?:\n.*?\\)??
