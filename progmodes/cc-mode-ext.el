;;; cc-mode-ext.el --- extensions to cc mode
;;
;; Copyright 2008-2012 Florian Kaufmann <sensorflo@gmail.com>
;; 
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; 
;; This file is not part of GNU Emacs.
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;; Commentary:
;; 
;;

;;; Variables:
(require 'cc-mode)

(defvar c-class-name-alist nil
  "Maps filename no classname.
Typically used for occasions where the filename does not exactly
correspond to the contained class name. The filename is without
the directory part and without suffix. Thus for
'~/src/foo/bar.cpp' it would be 'bar'.")


;;; Code:

;;; moving

(defun c-beginning-of-defun-ext (&optional stay ignore-defuns-comment)
  "Move backward to the beginning of a defun definition,
  inclusive its leading comment if any."
  (interactive)
  (if (c-src-buffer-p)

      ;; defun definition
      (let ((saved-point (point)))
        (c-forward-comments) ;; skip possible leading comments which belong to this defun
        (end-of-line) 
        (c-beginning-of-defun)
        (unless ignore-defuns-comment
          (c-backward-single-comment-ext))
        (when (and (not stay) (equal saved-point (point)))
          (while (c-backward-single-comment))
          (backward-char)
          (c-beginning-of-defun-ext)))

    ;; defun declaration
    (let ((saved-point (point)) ;; because delimiter is before 'beginning-of-defin', its possible to arrived
          delim-pos) 
      (re-search-backward "\\(public\\|private\\|protected\\)\\s-*:\\|;\\|{") ;; should not be within comment syntax
      (setq delim-pos (point))
      (end-of-line)
      (c-forward-comments)
      (unless ignore-defuns-comment
        (c-backward-single-comment-ext))
      (when (and (not stay) (equal saved-point (point)))
        (goto-char delim-pos)
        (c-beginning-of-defun-ext)))))

(defun c-end-of-defun-ext ()
   "Goto beginning of the line following this/next defun's
   declartion. 'This' ends at the closing ;"
  (interactive)
  (if (c-src-buffer-p)
      ;; defun definition
      (progn
        (c-end-of-defun)
        (when (looking-at "\\s-*$")
          (forward-line 1)))

    ;; defun declaration
    (c-forward-single-comment)
    (re-search-forward ";") ;; should not be within comment syntax
    (forward-line 1)))

(defun c-beginning-of-defun-body ()
  (interactive)
  (c-beginning-of-defun-ext t)
  (c-forward-comments)
  (re-search-forward "{")
  (c-skip-ws-forward))

(defun c-beginning-of-defun-param-list ()
  (interactive)
  (c-beginning-of-defun-ext t)
  (c-forward-comments)
  (re-search-forward "(")
  (c-skip-ws-forward))

(defun c-backward-single-comment-ext ()
  (interactive)
  (c-backward-single-comment)
  (c-skip-ws-forward)      
  (beginning-of-line))  

(defun c-forward-defun-name(&optional stay)
  "Move to this defun's name.
  'This' starts at } of previous defun and ends at } of this
  defun. If already at defuns name, go to next defun's
  name."
  (interactive )
  (let ((saved-point (point)))
    (push-mark (point))
    
    ;; move to this defun's name. A name followed by ( is either a constructor
    ;; which is ok, or a macro in which case the defun name is the first
    ;; argument of the macr
    (c-beginning-of-defun-ext t)
    (c-forward-comments)    
    (if (and (looking-at "\\w+\\s-*(") (not (looking-at (c-class-name))))
	(progn
	  (down-list)
	  (when (looking-at "\\s-") (re-search-forward "\\S-")))
      (progn
	(re-search-forward "(")
	(goto-char (match-beginning 0))
	(forward-sexp -1)))
    
    ;; if we where there already from the beginning
    (when (and (not stay) (equal saved-point (point)))
      (c-end-of-defun-ext)       ; move past 'scope' of this defun
      (c-forward-defun-name))))

(defun c-goto-other-defun (&optional arg)
  (interactive "P")
  (setq current-prefix-arg nil)
  (if (not arg)
      (ff-get-other-file nil)
    (let ((defunname (c-defun-name)))
      (ff-get-other-file nil)
      (c-goto-specific-defun-name defunname nil t))))

(defun c-goto-declaration ()
  (interactive)
  (let (name
        end
        saved-point (point))
    (save-excursion
      (unless (looking-at "\\b") (forward-sexp))
      (setq end (point))
      (backward-sexp)
      (setq name (buffer-substring-no-properties (point) end)))
    (cond
     ;; class member
     ((string-match "\\b[cms]+_" name)
      (ff-get-other-file)
      (goto-char (point-min))
      (re-search-forward (concat "\\b" name "\\b *;")))
     ;; method parameter
     ((string-match "\\b[iox]+_" name)
      (c-forward-defun-name)
      (re-search-forward (concat "\\b" name "\\b *[,)]")))     
     ;; pc ao dispatch enum
     ((string-match "\\beMSG_" name)
      (goto-char 0)
      (re-search-forward (concat "\\bcase\\s-+" name)))
     ;; else asume it to be a member method
     (t
      (c-goto-specific-defun-name name nil t)))))

;; todo: let user choose wheter this class or the class point is on (well, for the later probably
;; using tags or the like is better)
(defun c-goto-class-declaration ()
  (interactive)
  (if (c-src-buffer-p) (ff-get-other-file))
  (beginning-of-buffer)
  (re-search-forward (concat "^[ \t]*class[ \t]+" (regexp-quote (c-class-name)) "\\b")))

(defun c-goto-specific-defun-name (defun-name &optional search-from-point exact)
  (interactive "sMethod name: ")
  (let ((saved-point (point))
        (found)
        (searched-defun-name))
    (if (not search-from-point)
        (goto-char (point-min)))
    (setq searched-defun-name
          (if exact (concat "\\<" defun-name "\\>") (concat "\\<\\w*?" defun-name "\\w*\\>")))
    (if (c-src-buffer-p)
        (setq found (re-search-forward (concat (c-class-name) " *:: *" searched-defun-name " *(") nil t)) ; cpp
      (setq found (re-search-forward (concat searched-defun-name " *(") nil t))) ; h
    (if (not found)
        (progn
          (goto-char saved-point)
          (error "No more occurences found")))
    (backward-char)                                      ; (
    (backward-sexp)                                      ; the defun name
    (if (and search-from-point (eq saved-point (point)))
        (c-goto-specific-defun-name defun-name t exact))))

;;; marking / narrowing

(defun c-mark-function-incl-comment ()
  "Put mark at end of this defun definition, point at beginning inclusive comment.
   The defun marked is the one that contains point or follows point."
  (interactive)
  (let ((saved-point (point)))
    (c-end-of-defun-ext)
    (push-mark (point))
    (goto-char saved-point)
    (c-beginning-of-defun-ext t)
    (setq mark-active t)))

(defun c-mark-defun-name ()
  "Mark current or next defuns name"
  (interactive)
  (c-forward-defun-name t)
  (mark-sexp))

(defun c-mark-whole-comment ()
  "Marks the current or next comment"
  (interactive)
  (if mark-active (re-search-backward "/\\*\\*" ) )
  (re-search-backward "/\\*\\*" )
  (push-mark (point))
  (re-search-forward "\\*/" )
  (setq mark-active t))

(defun c-mark-whole-comment ()
  "Marks the current or next comment"
  (interactive)
  (if mark-active (re-search-backward "/\\*\\*" ) )
  (re-search-backward "/\\*\\*" )
  (push-mark (point))
  (re-search-forward "\\*/" )
  (setq mark-active t))

(defun c-mark-whole-statement ()
  (interactive)
  (c-beginning-of-statement-1)
  (push-mark (point))
  (c-end-of-statement)
  (setq mark-active t))

(defun c-mark-block ()
  (interactive)
  (backward-up-list)
  (let ((saved-point (point)))
    (c-skip-ws-backward)
    (when (looking-back ")")
      (backward-list))
    (c-beginning-of-statement 1)
    (push-mark-command (point))
    (goto-char saved-point))
  (forward-list)
  (setq mark-active t))

(defun c-mark-param-list ()
  (interactive)
  (c-forward-defun-name t)
  (forward-list)
  (backward-list)
  (mark-sexp))

(defun c-mark-signature ()
  (interactive)
  (c-beginning-of-defun-ext t t)
  (push-mark-command (point))
  (forward-list)
  (re-search-forward (if (c-src-buffer-p) "\n" ";"))
  (goto-char (match-beginning 0))
  (c-skip-ws-backward)
  (setq mark-active t))

(defun c-narrow-to-function-incl-comment ()
  "Make text outside current function definition inclusive its
  leading comment, if any, invisible. The defun visible is the
  one that contains point or follows point."
  (interactive)
  (let ((saved-point (point))
        defun-end)
    (save-excursion
      (widen)
      (c-end-of-defun-ext)
      (setq defun-end (point))
      (goto-char saved-point)
      (c-beginning-of-defun-ext t)
      (narrow-to-region (point) defun-end))))

(defun c-narrow-to-block ()
  (interactive)
  (save-excursion
    (c-mark-block)
    (mark-whole-lines)
    (narrow-to-region (point) (mark))))


;;; bla bli bla blu

(defun c-looking-at-empty-comment ()
  "return t if looking at an empty singleline comment."
  (looking-at "\\s-*\\(/\\*+\\s-*\\*+/\\|//+\\)\\s-*$"))

(defun c-src-buffer-p ()
  "Return t if buffer contains definitions, nil if it contains
  declarations, an error occures if it contains other stuff."
  (interactive)
  (let ((ext (if buffer-file-name
		 (file-name-extension buffer-file-name)
	       (file-name-extension (buffer-name)))))
    (cond
     ((and ext (string-match "^[ch]pp$" ext)) t)
     ((and ext (string-match "^\\(h\\|idl\\)$" ext)) nil)
     (t (message "c-src-buffer-p: unknown file extension, assuming its a source buffer")))))

(defun c-class-name ()
  "Returns the class name of the class defined/declared in this buffer"
  (let ((switch-buffer (c-src-buffer-p))
        name)
    (when switch-buffer
      (ff-get-other-file))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let* ((case-fold-search t)
	       (fn-stem (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
	  (setq name (cdr (assoc fn-stem c-class-name-alist))) 
          (unless name
	    (when (re-search-forward (concat "^class\\s-*\\(c?" fn-stem "\\)") nil t)
	      (setq name (match-string-no-properties 1)))
	  (unless name
            (when (re-search-forward "^class\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)\\(?:\\s-\\|\n\\)*[:{]")
	      (setq name (match-string-no-properties 1))))))))
    (when switch-buffer
      (ff-get-other-file))
    (when (null name)
      (error "no class name found"))
    name))

;; todo: probably its better to use info from tags
(defun c-file-name-of-class (class-name)
  "Returns the file name which declares the given CLASS-NAME"
  (concat (substring class-name 1) ".h"))

(defun c-base-class-names ()
  "Returns a list of class names from which this class derives"
  (save-excursion
    (c-goto-class-declaration)
    (let* (bases
	   (whites "\\(?:[ \t\n]+\\)")
	   (span-comment "\\(?:/\\*[^*]*\\(?:\\*[^/][^*]*\\)*\\*/\\)")
	   (line-comment "\\(?://.*\n\\)")
	   (comment (concat "\\(?:" span-comment "\\|" line-comment "\\)"))
	   (tmp (concat "\\(?:\\(?:" whites "\\|" comment "\\)")) 
	   (skip+ (concat tmp "+\\)"))	;exclusive empty string
	   (skip* (concat tmp "*\\)"))	;inclusive empty string
	   (identifier "\\(?:[_A-Za-z][_A-Za-z0-9]+\\)"))
      (re-search-forward (concat "\\=" skip* "\\([:{]\\)" skip*))
      (setq stop (string= (match-string 1) "{"))
      (while (not stop)
      	(if (re-search-forward (concat "\\=" skip* "\\(?:public\\|private\\|protected\\)" skip+ "\\(?:virtual" skip+ "\\)?" "\\(" identifier "\\)" skip* ",?" ) nil t)
      	    (add-to-list 'bases (match-string-no-properties 1))
      	  (setq stop t)))
      bases)))

(defun c-derived-class-names ()
  "Returns a list of class names which derive from this class"
  nil)

(defun c-defun-name ()
  (interactive)
  (save-excursion
    (let (beginning)
      (c-forward-defun-name t)
      (setq beginning (point))
      (forward-sexp)
      (buffer-substring-no-properties beginning (point)))))

(defun c-move-comment-dec-to-def()
  "Strips a comment from the declartion point is at and insert
  the comment at the definition of the defun, if the current
  declarion defun comment is
  empty."
  (interactive)
  (let (decl-comment-start
        decl-comment-end        
        defunname
        comment
        beginning-of-defun
        beginning-of-comment
        decl-has-comment)
    
    ; get defunname 
    (setq defunname (c-defun-name))
    
    ; get comment. if its empty or inexistent, delete it and exit
    (c-beginning-of-defun-ext)
    (set-mark (point))
    (c-forward-single-comment)
    (c-skip-ws-backward)
    (exchange-point-and-mark)
    (when (or (<= (mark) (point)) (c-looking-at-empty-comment))
      (mark-whole-lines)
      (unless (y-or-n-p "Comment is empty. I'll delete it. Ok? ")
        (error "Aborted by user"))
      (delete-region (point) (mark))
      (setq mark-active nil)
      (throw 'skip t))
    
    ; ask user wheter he really wants to copy
    (mark-whole-lines)
    (setq mark-active t)
    (setq comment (buffer-substring-no-properties (point) (mark)))
    (unless (y-or-n-p "I'll copy this comment. Ok? ")
      (throw 'skip t))
         
     ; goto declaration buffer and search declaration of defun
    (ff-get-other-file)
    (setq mark-active nil)
    (goto-char (point-min))
    (c-goto-specific-defun-name defunname nil t)

    ; delete any existing comment. If its nonempty, querry user.
    (beginning-of-line)
    (setq beginning-of-defun (point))
    (setq decl-has-comment (c-backward-single-comment))
    (c-skip-ws-forward)    
    (when decl-has-comment
      (setq beginning-of-comment (point))
      (set-mark (point))
      (goto-char beginning-of-defun)
      (unless (c-looking-at-empty-comment)        
        (unless (y-or-n-p "Replace the existing non-empty comment? ")
            (throw 'skip t)))
      (delete-region (point) beginning-of-comment))

    ; insert comment copied from header and indent it
    (set-mark (point))
    (insert comment)
    (setq mark-active t)
    (c-indent-line-or-region)
    (setq mark-active nil)

    ; delete comment in header file
    (ff-get-other-file)
    (delete-region (point) (mark))
    (setq mark-active nil)))

(defun c-move-comment-dec-to-def-buffer ()
  (interactive)
  (let ()
    (while t
        (catch 'skip (c-move-comment-dec-to-def))
        (if (c-src-buffer-p) (ff-get-other-file))
        (c-forward-defun-name)
        (c-forward-defun-name))))

;; todo: contains Dragon specific stuff, extract that into dragon.el.
(defun insert-class-name-dwim ()
  (interactive)
  (save-excursion
    (cond
     ((save-excursion
	(beginning-of-line)
	(looking-at "\\s-*\\(\\w\\|_\\)*TRACE"))
      (beginning-of-line)
      (re-search-forward "::")
      (cond
       ((looking-at "\\(\\w\\|_\\)*::")
	(delete-region (word-beginning-position) (word-end-position))
	(delete-region (point) (progn (forward-word) (point))))
       ((looking-back "::\\(\\w\\|_\\)*")
	(delete-region (word-beginning-position) (word-end-position))
	(delete-region (point) (progn (backward-word) (point)))))
      (insert-class-and-defun-name))

     ((save-excursion
	(beginning-of-line)
	(looking-at "\\s-*\\(\\w\\|_\\)*EASSERT"))
      (beginning-of-line)      
      (when (re-search-forward "_T(\"")
	(when (looking-at "\\(\\w\\|_\\)*::\\(\\w\\|_\\)*")
	  (delete-region (match-beginning 0) (match-end 0)))
	(insert-class-and-defun-name)))
     
     ((save-excursion
	(beginning-of-line)
	(looking-at "EHRESULT\\s-+\\(\\(?:\\w\\|_\\)*\\)\\s-*::\\s-*\\(?:\\w\\|_\\)"))
      (goto-char (match-beginning 1))
      (delete-region (match-beginning 1) (match-end 1))
      (insert-class-name))

     ((save-excursion
	(beginning-of-line)
	(looking-at "\\s-*\\(?:WRITE\\|READ\\)_METHOD_IMPL\\s-*(\\s-*\\(.*?\\)\\_>"))
      (goto-char (match-beginning 1))
      (delete-region (match-beginning 1) (match-end 1))
      (insert-class-name))

     ((looking-at "\\(\\w\\|_\\)*::")
      (delete-region (word-beginning-position) (word-end-position))
      (insert-class-name))
     
     ((looking-back "::\\(\\w\\|_\\)*")
      (backward-word (if (looking-at "\\_<") 1 2))
      (delete-region (point) (word-end-position))
      (insert-class-name))
     
     (t
      (insert-class-name)))))

(defun insert-class-name ()
  (interactive)
  (insert (c-class-name)))

(defun insert-class-name-with-scope ()
  (interactive)
  (insert (c-class-name) "::"))

(defun insert-class-and-defun-name ()
  (interactive)
  (insert (c-class-name) "::" (c-defun-name)))

(defun insert-defun-name ()
  (interactive)
  (insert (c-defun-name)))

(defun insert-base-class-name-with-scope ()
  (interactive)
  (let ((bases (c-base-class-names)))
    (if (null bases)
	(error "No base classes"))
    (insert (car (c-base-class-names)) "::")))

(defun c-copy-param-list ()
  (interactive)
  (let (param-list)

    ;; copy param list from current method
    (save-excursion
      (c-mark-param-list)
      (setq param-list (buffer-substring-no-properties (mark) (point))))

    ;; if we are in source file, insert default arguments extracted from respective comments
    ;; if we are in header file, strip default arguments
    (if (c-src-buffer-p)
      (setq param-list (replace-regexp-in-string "\\s-*/\\*\\s-*=\\s-*\\(.*?\\)\\s-*\\*/\\s-*\\([,)]\\)" " = \\1 \\2" param-list))
      (setq param-list (replace-regexp-in-string "\\s-*=\\s-*\\([^,)]*?\\)\\s-*\\([,)]\\)" " /* = \\1 */\\2" param-list)))
    
    ;; delete old param list of same method in other file (source/header)
    (c-goto-other-defun t)
    (c-mark-param-list)
    (delete-region (mark) (point))
  
    ;; insert new param list
    (insert param-list)
    (setq mark-active t)
    (indent-region (mark) (point))))

(defun c-copy-signature ()
  "Copy signature of current method to 'c-goto-other-defun', so
  both sides are consistent."
  (interactive)
  (let (signature)

    (save-excursion
      (c-mark-signature)
      (setq signature (buffer-substring-no-properties (mark) (point))))

    ;; if we are in source file, insert default arguments extracted from respective comments
    ;; if we are in header file, strip default arguments
    (if (c-src-buffer-p)
      (setq signature (replace-regexp-in-string "\\s-*/\\*\\s-*=\\s-*\\(.*?\\)\\s-*\\*/\\s-*\\([,)]\\)" " = \\1 \\2" signature))
      (setq signature (replace-regexp-in-string "\\s-*=\\s-*\\([^,)]*?\\)\\s-*\\([,)]\\)" " /* = \\1 */\\2" signature)))
    
    (if (c-src-buffer-p)
      (setq signature (replace-regexp-in-string "/\\*+\\s-*virtual\\s-*\\*+/" "virtual" signature))
      (setq signature (replace-regexp-in-string "virtual" "/* virtual */" signature)))

    (if (c-src-buffer-p)
      (setq signature (replace-regexp-in-string (concat (c-class-name) "::") "" signature))
      (setq signature (replace-regexp-in-string (c-defun-name) (concat (c-class-name) "::" (c-defun-name)) signature)))

    ;; delete old param list of same method in other file (source/header)
    (c-goto-other-defun t)
    (c-mark-signature)
    (delete-region (mark) (point))
  
    ;; insert new param list
    (insert signature)
    (setq mark-active t)
    (indent-region (mark) (point))))

(defun c-copy-signature-buffer ()
  (interactive)
  (let ()
    (while t
        (y-or-n-p "I'll copy this signature. Ok? ")
        (catch 'skip (c-copy-signature))
        (y-or-n-p "Thats what I've done. Ok? ")
        (if (c-src-buffer-p) (ff-get-other-file))
        (c-forward-defun-name))))

(defun c-defun-height ()
  "Returns the height in lines the current defun occupies,
  i.e. the number of lines the current defun
  occupies."
  (save-excursion
    (c-mark-function-incl-comment)
    (count-lines (point) (mark))))

(defun c-recenter-defun-or-region ()
  "If region is not active, recenters current defun, see
  c-recenter-defun. If region is not active, recenter region, see
  c-recenter-region. To recenter point, make a small region,
  e.g. just one char."
  
  (interactive)
  (if mark-active
      (c-recenter-region)
    (c-recenter-defun)))

(defun c-recenter-defun ()
  "Recenters current defun in the middle of the window. Point
  remains at its position."
  (interactive)
  (let ((saved-point (point))
        margin)
    (setq margin (/ (- (window-body-height) (c-defun-height)) 2))
    (c-beginning-of-defun-ext t)
    (if (> margin 0)
          (recenter margin)
        (set-window-start (selected-window) (point)))
    (if (> (window-body-height) (count-lines (point) saved-point))
        (goto-char saved-point)
      (forward-line (- (window-body-height) 1)))))

(defun c-recenter-region ()
  (interactive)
  (let ((exchanged-p nil)
        (region-height (count-lines (point) (mark)))
        margin)
    (setq margin (/ (- (window-body-height) region-height) 2))
    (when (< margin 0) (setq margin 0))
    (when (> (point) (mark))
      (exchange-point-and-mark)
      (setq exchanged-p t))
    (recenter margin)
    (when exchanged-p
      (exchange-point-and-mark))))

(defun c-align-function-call ()
  (interactive)
  (align-regexp (point) (mark) "\\(\\s-*\\)," 1 0 t)
  (align-regexp (point) (mark) ",\\(\\s-*\\)" 1 1 t))

;; TODO
;; 1) if parentheses ( ) before opening { is several lines long, all lines must be deleted
;; 2) if removed block is and "if", then make trailing "else if" to and "if"
;; 3) if removed block is and "else", let the preceding "}" stay
;; 4) The following doesnt make sense. the code block must be either moved to the beginning or the end of this if/elseif/else sequence. Ask user.
;;  if removed block is an "else if", let preceding "}" stay and let the optional trailing "else ..." stay
(defun remove-parentheses ()
  (interactive)
  (let ((open-paren-pos) 
        (close-paren-pos) ;; pos of closing paranthesis
        (delstart)) ;; start pos of region to be deleted
    
    ;; get position of enclosing opening- and closing-paranthesis 
    (unless (looking-at "[[({]")
      (backward-up-list))
    (setq open-paren-pos (point))
    (forward-sexp)
    (setq close-paren-pos (point))
    
    ;; curly braces on different lines, i.e. C function blocks, are a special case
    (if (and (equal (char-before) ?} ) (/= 0 (count-lines open-paren-pos close-paren-pos)))
        (progn 
          ;; remove all white space around clocing }. If a blank line would
          ;; result, delete it.
          (backward-char 1)
          (re-search-backward "^\\|\\S-" )
          (re-search-forward "\\s-*}\\s-*")
          (replace-match "")
          (move-beginning-of-line 1)
          (if (looking-at "$") (delete-char 1))
              
          ;; remove everything from start of line to opening { and remove
          ;; trailing white spaces. If a blank line would result, delete it
          (goto-char open-paren-pos)
          (move-beginning-of-line 1)
          (re-search-forward ".*{\\s-*")
          (replace-match "")
          (move-beginning-of-line 1)
          (if (looking-at "$") (delete-char 1)))
      
      ;; delete the two parentheses
      (progn 
        (backward-delete-char 1)
        (goto-char open-paren-pos)
        (delete-char 1)))
    
    ;; indent the region that was enclosed by the parentheses
    (set-mark close-paren-pos)
    (setq mark-active t)
    (indent-for-tab-command)))

(defun c-strip(str)
  str)

(defun c-toggle-ternary-op()
  (interactive)
  (c-beginning-of-statement-1)
  (re-search-forward "\\=\\(.*?=\\|return\\)?\\(\\(?:.\\|\n\\)*?\\)\\?\\(\\(?:.\\|\n\\)*?\\):\\(\\(?:.\\|\n\\)*?\\);")
  (let ((start (c-strip (match-string 1)))
	(condition (c-strip (match-string 2)))
	(a (c-strip (match-string 3)))
	(b (c-strip (match-string 4))))

    (insert (concat "if ( " condition ") {\nreturn " a ";\n} else {\nreturn" b ";\n}"))
    
  ))

(defun c-electric-left-brace ()
  (interactive)
  (if (looking-at "[ \t]*$")
      (tempo-template-c-block)
    (insert ?{)))

(defvar c-doc-comment-char ?*
  "Char to be used afer '/*'")

(defun c-toggle-comment-style ()
  ""
  (interactive)
  (re-search-forward "\\s-*" (line-end-position) t) 
  (re-search-backward "//\\|/\\*" (line-beginning-position) t)
  (cond
   ((not (looking-back "^\\s-*" (line-beginning-position)))
    (re-search-forward "//\\(.*?\\)\\s-*$" (line-end-position) t)
    (let ((str (match-string 1)))
      (delete-region (match-beginning 0) (match-end 0))
      (beginning-of-line)
      (open-line 1)
      (insert "/*" c-doc-comment-char str " */")
      (indent-for-tab-command)))
   ((looking-at "//")
    (replace-regexp "//\\(.*?\\)\\s-*$"
                    (concat "/*" (string c-doc-comment-char) "\\1 */")
                    nil (point) (line-end-position)))
   ((looking-at "/\\*")
    (replace-regexp "/\\*[*!]*\\(.*?\\)\\**/" "//\\1" nil (point) (line-end-position)))))

(defun c-convert-ifndef-to-pragma-once ()
  (interactive)
  (let (found-match)
    (save-restriction
      (widen)
      (goto-char 0)
      (when (looking-at
             "\\`[ \t\n]*\\(#ifndef[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*\n#define[ \t]*\\2.*$\\)")
        (save-match-data
          (when (re-search-forward "^[ \t]*#endif\\(?:[ \t]*//.*?\n\\)?[ \t\n]*\\'")
            (replace-match "")
            (setq found-match t)))
        (when found-match
          (replace-match "#pragma once" t t nil 1))))))

;; (defun c-font-lock-invalid-string ()
;;   "Replacement of the original `c-font-lock-invalid-string'.
;; The original one wronly fontifies double quotes when the whole
;; string is fontified with an not font-lock-string-face."
;;   nil)

(provide 'cc-mode-ext)
;;; cc-mode-ext.el ends here