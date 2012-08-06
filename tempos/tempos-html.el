;; tailored for html elements used in Doxygen documents


;;; common used defuns/vars
(require 'tempo)
(require 'tempo-snippets)

(tempo-define-snippet
 "html-element"
 '( "<" (p "name" name) ">" r "</" (s name) ">"))

;; for tags ala </table> or <br/> its easy to just additionaly press /
(tempo-define-template
 "html-tag"
 '( "<" r ">"))

(tempo-define-template
 "html-anchor"
 '( "<a name=\"" r  "\"></a>"))

(tempo-define-template
 "html-ref"
 '( "<a href=\"" p "\">" r "</a>"))

(defun tempos-html-element(tag &optional content-on-newline)
  (tempo-define-template
   (concat "html-" tag)
   (if content-on-newline
       `("<" ,tag ">" > n> r> n> "</" ,tag ">")
     `("<" ,tag ">" r "</" ,tag ">"))))

(defun tempos-html-tag(tag)
  (tempo-define-template
   (concat "html-" tag)
   `("<" ,tag ">")))

;;; actual tempos 

(tempos-html-element "var") 
(tempos-html-element "code") 
(tempos-html-element "em") 
(tempos-html-element "i") 
(tempos-html-element "strong") 
(tempos-html-element "b")
(tempos-html-element "pre") 
(tempos-html-element "h1") 
(tempos-html-element "h2") 
(tempos-html-element "h3") 
(tempos-html-element "h4") 
(tempos-html-element "h5") 
(tempos-html-element "table" t) 
(tempos-html-tag "td") 
(tempos-html-tag "th") 
(tempos-html-element "ol" t) 
(tempos-html-element "ul" t) 
(tempos-html-element "dl" t) 
(tempos-html-tag "li")
(tempos-html-tag "dt") 
(tempos-html-tag "dd")

(tempo-define-template "html-ul+" '( "<ul>\n<li>" p "\n<li>" p "\n<li>" p "\n</ul>"))
(tempo-define-template "html-ol+" '( "<ol>\n<li>" p "\n<li>" p "\n<li>" p "\n</ol>"))
(tempo-define-template "html-dl+" '( "<dl>\n<dt>" p "\n<dd>" p "\n<dt>" p "\n<dd>" p "\n<dt>" p "\n<dd>" p "\n</dl>"))
(tempo-define-template "html-dt+" '( "<dt>" p > n> "<dd>"))

(tempo-define-template "html-table+" '( "<table>\n<tr><td>" p "<td>" p "\n<tr><td>" p "<td>" p "\n<tr><td>" p "<td>" p "\n</table>"))
(tempo-define-template "html-tr+" '( "<tr><td>" p "<td>" p))


(provide 'tempos-html)