(require 'tempo)
(require 'tempo-snippets)

;;; common used defuns/vars

(tempo-define-template
 "doxy-page"
 '( "\\page sec_" r ))

(defvar doxy-sec-prefix-base nil
  "")

(defun doxym-sec-prefix(base)
  (concat "sec_" base "_"))

(defun doxy-sec-prefix()
  (when (null doxy-sec-prefix-base)
    (setq doxy-sec-prefix-base (read-string "section prefix: ")))
  (doxym-sec-prefix doxy-sec-prefix-base))

;;; actual tempos 

(tempo-define-template
 "doxy-section"
 '( "\\section " (doxy-sec-prefix) (p "id" id) " " r ))

(tempo-define-template
 "doxy-subsection"
 '( "\\subsection " (doxy-sec-prefix) (p "id" id) " " r ))

(tempo-define-template
 "doxy-subsubsection"
 '( "\\subsubsection " (doxy-sec-prefix) (p "id" id) " " r ))

(tempo-define-template
 "doxy-verbatim"
 '( & "\\verbatim" > n>
    r > n>
    "\\endverbatim" > ))

(tempo-define-template
 "doxy-code"
 '( & "\\code" > n>
    r > n>
    "\\endcode" > ))

(provide 'tempos-doxy)
