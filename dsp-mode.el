(defvar dsp-mode-hook nil)

(defvar dsp-mode-map nil "Keymap for dsp-mode.")
(unless dsp-mode-map
  (setq dsp-mode-map (make-sparse-keymap)))

(defconst dsp-mode-font-lock-keywords
  (list
   ;; comment / semantic
   (list "\\(#\\)\\s-*\\(Begin\\|End\\|Name\\|PROP\\|ADD\\|SUBTRACT\\|TARGTYPE\\)\\b" '(1 font-lock-comment-face) '(2 font-lock-keyword-face)) 
   (cons "#.*$" 'font-lock-comment-face)
   
   ;; message stuff
   (list "^\\s-*\\(!MESSAGE\\)\\(.*$\\)" '(1 font-lock-preprocessor-face) '(2 font-lock-string-face))
   (list "\\b\\(echo\\)\\b\\(.*$\\)" '(1 font-lock-keyword-face) '(2 font-lock-string-face)) 

   ;; preprocessor
   (cons "^\\s-*!\\sw+" 'font-lock-preprocessor-face)
   
   ;; string / terminal
   (cons "\".*?\"" 'font-lock-string-face)
   
   ;; Definition / Label
   (list "^\\s-*\\(\\(\\sw\\|_\\)+\\)\\s-*=" '(1 font-lock-variable-name-face))
   (list "^\\s-*\\(:\\sw+\\)\\s-*$" '(1 font-lock-variable-name-face))
   
   ;; keywords
   (cons "\\b\\(if\\|not\\|goto\\)\\b" 'font-lock-keyword-face)
   ))

(defun dsp-mode()
  (interactive)
  (kill-all-local-variables)
  
  (use-local-map dsp-mode-map)  
  
  (set (make-local-variable 'paragraph-start) "\\s-*\\w+\\s-*=.*$\\|\\s-*$")
  (set (make-local-variable 'paragraph-separate)  "\\s-*$")
        
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#\\s-*")
  
  (set (make-local-variable 'font-lock-defaults)
       '(dsp-mode-font-lock-keywords t nil nil nil))
  
  (setq major-mode 'dsp-mode
        mode-name "dsp")

  (local-set-key [(control j)] 'backward-char)
  (local-set-key [(control l)] 'forward-char)
  (run-hooks 'dsp-mode-hook))

;; KLUDGE: only add if not already in there
(add-to-list 'auto-mode-alist '( "\\.dsp\\'" . dsp-mode))

(provide 'dsp-mode)


