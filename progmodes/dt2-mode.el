;;; dt2-mode.el --- a major-mode for editing Indel's dt2 files in emacs

;;; Variables

(defconst dt2-mode-version "0.1")

(defvar dt2-mode-hook nil
  "Normal hook run when entering dt2 mode.")

;;; Code

(defconst dt2-re-string "\"[^\"\\\n]*\\(?:\\.[^\"\\\n]*\\)*\"" )

(defconst dt2-font-lock-keywords
  (list
   (cons "^[ \t]*//!.*$" font-lock-keyword-face)
   (cons "//.*$" font-lock-comment-face)
   (list "^[ \t]*\\([-A-Z]*\\)[ \t]*$" '(1 font-lock-variable-name-face))
   (list (concat "^[ \t]*Name[ \t]*;[^;]*?;[ \t]*\\(" dt2-re-string "\\)") '(1 font-lock-function-name-face))
   (cons dt2-re-string font-lock-string-face)
   (cons "\\b0x[a-fA-F0-9]+\\b" font-lock-constant-face)
   (cons "\\b[-+]?[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\b" font-lock-constant-face)
   ))

;;;###autoload
(define-derived-mode dt2-mode conf-mode "dt2"
  "Major mode for editing dt2 files.
Turning on dt2 mode runs the normal hook `dt2-mode-hook'."
  (interactive)
  
  ;; syntax table
  (modify-syntax-entry ?\" "\"")
  (modify-syntax-entry ?\_ "w")
  
  ;; comments
  (set (make-local-variable 'comment-column) 0)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "//[ \t]*")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\n")
  
  ;; outline
  ;; (set (make-local-variable 'outline-regexp) "^[ \t]*Name[ \t]*;[^;]*?;[ \t]*\\(" dt2-re-string "\\)")
  ;; (set (make-local-variable 'outline-level) (lambda () 1))

  ;; misc
  (set (make-local-variable 'require-final-newline) t)
  
  ;; font lock
  (set (make-local-variable 'font-lock-defaults) '(dt2-font-lock-keywords t))
  
  ;; 
  (run-hooks 'dt2-mode-hook))

(provide 'dt2-mode)

;;; dt2-mode.el ends here
