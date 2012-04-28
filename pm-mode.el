;;; pm-mode.el --- a major-mode for editing personality files in emacs
;;
;;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; 
;;; Commentary:
;; 
;;; Variables:

(defconst pm-mode-version "0.1")

(defvar pm-mode-hook nil
  "Normal hook run when entering pm mode.")

(defvar pm-bold 'bold)

;;; Code

(defconst pm-font-lock-keywords
  (list
   (list "^\\s-*\\(\\w+\\)" '(1 font-lock-keyword-face))
   (list "\\bOBJ_DEF\\s-*,\\s-*\\(\\w+\\)" '(1 font-lock-variable-name-face))
   (list "^#+\\([^#]+\\)#+ *$" '(1 pm-bold append))
   ))

(defun pm-indent-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let* (name
           (il (cond
                ;; OBJ_TOP
                ((looking-at "\\(\\s-*\\)OBJ_TOPO\\s-*,\\s-*\\w+\\s-*,\\s-*\\(\\w+\\)")
                 (setq name (match-string 2))
                 (save-match-data
                   (beginning-of-line)
                   (forward-line -1)
                   (while (and (looking-at "\\s-*\\($\\|#\\)") (not (bobp)))
                     (forward-line -1))
                   (if (bobp)
                       0
                     ;; BUG: the parent def might be further above than just one line, i.e.
                     ;; other OBJ_TOP might be inbetween
                     (if (looking-at (concat "\\(\\s-*\\)OBJ_TOPO\\s-*,\\s-*" name "\\b"))
                         (1+ (/ (length (match-string 1)) tab-width))
                         0))))
                ;; OBJ_DEF
                ((looking-at "\\(\\s-*\\)OBJ_DEF\\b") 0)
                ;; an non blank line
                ((looking-at "\\(\\s-*\\)\\S-") 1)
                ;; a blank line
                (t nil))) ; do nothing
           (ws (when il (make-string (* il tab-width) ?\ ))))
      (when ws (replace-match ws nil nil nil 1)))))

;;;###autoload
(define-derived-mode pm-mode text-mode "pm"
  "Major mode for editing pm files.
Turning on pm mode runs the normal hook `pm-mode-hook'."
  (interactive)
  
  ;; syntax table
  (modify-syntax-entry ?\_ "w")
  (modify-syntax-entry ?\# "<")
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?\r ">")
  
  ;; comments
  (set (make-local-variable 'comment-column) 0)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#\\s-*")
  (set (make-local-variable 'comment-end-skip) "\\s-*\n")
  
  ;; TODO: make paragraphs using blank lines / OBJ_DEF 
  ;; paragraphs
  ;;   (set (make-local-variable 'paragraph-separate) xxx)
  ;;   (set (make-local-variable 'paragraph-start) xxx )
  ;;   (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  
  ;; misc
  (set (make-local-variable 'require-final-newline) t)
  
  ;; font lock
  (set (make-local-variable 'font-lock-defaults) '(pm-font-lock-keywords))
  
  ;; indent
  (set (make-local-variable 'indent-line-function) 'pm-indent-line)

  ;; outline mode
  (let ((l1 " *#+[^#]+#+ *$")
        (l2 "\\s-*OBJ_DEF"))
    (set (make-local-variable 'outline-regexp)
         (concat "\\(" l1 "\\)\\|\\(" l2 "\\)"))
    (set (make-local-variable 'outline-level)
         (lambda()
           (cond ((looking-at l1) 1)
                 ((looking-at l2) 2)))))
  
  ;; 
  (run-hooks 'pm-mode-hook))

(provide 'pm-mode)

;;; pm-mode.el ends here
