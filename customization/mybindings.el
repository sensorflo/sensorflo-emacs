;;; mybindings.el --- sensorflo's key bindings
;;
;;; Commentary:
;;
;; Sensorflo's extension's and overwritting-changes to Emacs' bindings library.
;; See also Elisp info manual, chapter 'Key Binding Conventions'.
;;
;; Free in vanilla Emacs:
;;   C-most punctuation of top + 2nd row
;;   C-<
;;   C-'  (now mark-word)
;;   C-,  (now prefix map)
;;   C-.  (now prefix map)
;;
;; Peudo free:
;;   C-m, C-i, C-[  (maps to ret/tab/esc)
;;   C-t only transpose-chars
;;   C-z only iconify-or-deiconify-frame
;;   C-_ C-/ undo is already on C-x u
;;   C-\ toggle-input-method
;;
;; Emacs keybinding guidelines summary:
;;   C-c ctrlchar/digit/{ } < > : ;   : major modes
;;   C-c other punct : minor modes
;;   C-c letter and (without C-c) F5-F9 : the only keys free for users


;;; Code:
(require 'repeatable)

;;; redifine existing binding with semtantically similar functionality
(global-set-key [remap isearch-forward]         'isearch-forward-regexp)
(global-set-key [remap isearch-backward]        'isearch-backward-regexp)
(global-set-key [remap list-buffers]            'ibuffer)
(global-set-key [remap save-some-buffers]       'save-some-buffers-no-query)
(global-set-key [remap scroll-up]               'scroll-up-block)
(global-set-key [remap scroll-down]             'scroll-down-block)
(global-set-key [remap open-line]               'open-line-above)
(global-set-key [remap capitalize-word]         'capitalize-dwim)
(global-set-key [remap capitalize-region]       'capitalize-dwim)
; (global-set-key [remap downcase-word]           'downcase-dwim) ; no use since covered by forward-word below
(global-set-key [remap downcase-region]         'downcase-dwim)
(global-set-key [remap upcase-word]             'upcase-dwim)
(global-set-key [remap upcase-region]           'upcase-dwim)
(global-set-key [remap down-list]               'down-list-ext)
(global-set-key [remap backward-up-list]        'backward-up-list-ext)
(global-set-key [remap mark-sexp]               'mark-sexp-ext)
(global-set-key [remap set-mark-command]        'toggle-mark)
(global-set-key [remap transpose-sexps]         'transpose-sexps-ext)
(global-set-key [remap shell-command-on-region] 'shell-command-on-region-ext)
(global-set-key [remap shell-command]           'shell-command-ext)
(global-set-key [remap just-one-space]          'just-one-space-ext)
(global-set-key [remap delete-blank-lines]      'delete-blank-lines-ext)
(global-set-key [remap kill-line]               'kill-line-ext)
(global-set-key [remap kill-buffer]             'kill-buffer-ext)
(global-set-key [remap toggle-read-only]        'toggle-read-only-ext)
(global-set-key [remap indent-for-tab-command]  'indent-for-tab-command-ext)
(global-set-key [remap indent-rigidly]          'indent-rigidly-ext)
(global-set-key [remap fill-paragraph]          'fill-paragraph-dwim)
(global-set-key [remap yank]                    'yank-ext)
(global-set-key [remap yank-pop]                'yank-pop-ext) ;see also yank-push below
(global-set-key [remap comment-dwim]            'comment-dwim-ext)


;;; redefine existing bindings with semantically different functionality
(global-set-key [(control l)]      'forward-char)         ; recenter-top-bottom     -> nowhere (use c-recenter-defun-or-region)
(global-set-key [(meta l)]         'forward-word)         ; downcase-word           -> nowhere
(global-set-key [(control meta l)] 'forward-sexp)         ; reposition-window       -> nowhere
(global-set-key [(meta m)]          (make-sparse-keymap)) ; back-to-indentation     -> nowhere (use beginning-of-line-dwim)
(global-set-key [(control f)]       (make-sparse-keymap)) ; forward-char            -> C-l
(global-set-key [C-M-left]          'move-text-backward)  ; forward-sexp            : is alwo on other keys
(global-set-key [C-M-right]         'move-text-forward)   ; backward-sexp           : is also on other keys


;;; add new bindings
;; (prefix-keymaps M-m / C-f were created above)
(global-set-key [(control ?\')]           'mark-word)     ; by default on M-@
(global-set-key [(control meta ?\')]      'mark-comment-dwim)
(global-set-key [(control f)(control f)]  'ffe-find-file)
(global-set-key [(control f)(control o)]  'ffe-find-other-file)
(global-set-key [(control f)(f)]          'globalff)
(global-set-key [(control f)(control r)]  'query-replace-regexp)
(global-set-key [(control f)(control k)]  'execute-extended-command)
(autoload 'doremi-buffers+ "doremi-cmd.el")
(global-set-key [(control f)(control b)]  'doremi-buffers+) ; replaces next-buffer-ext / previous-buffer-ext
(global-set-key [(control f)(control s)]  'grep-find-ext)
(global-set-key [(meta m)(meta m)]        'kmacro-start-stop-macro-ext)
(global-set-key [(control meta y)]        'yank-push-ext) ; see also yank-pop above
(global-set-key [(control c)(w)] (make-sparse-keymap))
(global-set-key [(control c)(w)(b)]  'windmove-left)
(global-set-key [(control c)(w)(l)]  'windmove-right)
(global-set-key [(control c)(w)(p)]  'windmove-up)
(global-set-key [(control c)(w)(n)]  'windmove-down)
(global-set-key [(control x)(E)] 'eval-last-sexp-to-kill-ring)
(global-set-key [(control x)(control j)] 'dired-jump)
(global-set-key [(control ?\;)] 'ace-jump-mode)
(global-set-key [C-M-up] 'move-text-up)
;; (global-set-key [(meta p)] 'tempo-backward-mark)
;; (global-set-key [(meta n)] 'tempo-forward-mark)
(global-set-key [C-M-down] 'move-text-down)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key [backtab] 'format-buffer-or-region)

(global-set-key "\C-xw" (make-sparse-keymap))
(global-set-key "\C-xws" 'highlight-toggle-sexp-or-region)
(global-set-key "\C-xwu" 'unhighlight-all)
(global-set-key "\C-xwa" 'highlight-arguments)
(global-set-key "\C-xwA" 'highlight-arguments-uni)
(global-set-key "\C-xwm" 'highlight-members)

(global-set-key [f7]   'recompile)
(global-set-key [f10]  'ediff-show-registry)

(define-key ctl-x-r-map "e" 'extract-rectangle)
(define-key ctl-x-r-map "w" 'copy-rectangle-as-kill)
(define-key ctl-x-r-map "T" 'string-insert-rectangle) ; C-x r t is string-rectangle

;; note that projectile-global-mode must be on for the binding to be 'active'
(helm-projectile-on) ;; only sets bindings, see its function help
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)


;;; repeatable
(repeatable-command-advice forward-page)
(repeatable-command-advice backward-page)
(repeatable-command-advice next-error)
(repeatable-command-advice previous-error)
;set-mark-command-repeat-pop = t


;;; my-common-mode-bindings
;; Maybe theres a function returning local key map, so a generic method including
;; global map is possible
;; ------------------------------------------------------------------
(defun my-common-mode-bindings ()
  (local-set-key [(control f)]       (make-sparse-keymap))

  ;; redefine with similar functionality
  (local-set-key [remap mark-paragraph]          'mark-paragraph-ext)
  (local-set-key [remap newline]                 'indent-new-comment-line)
  (local-set-key [remap move-beginning-of-line]  'beginning-of-line-dwim )
  (local-set-key [remap move-end-of-line]        'end-of-line-dwim       )

  ;; add new bindings "near" similar funcionality
  (local-set-key [(control meta backspace)] 'backward-kill-sexp) ; M-DEL is backward-kill-word

  ;; new bindings
  (local-set-key [(control return)]        'open-line-below) ; Actually C-e C-j or C-n C-o would be convenient enough
  (local-set-key [(control f)(control d)]  'duplicate-line-or-region)
  (local-set-key [(control f)(control c)]  'concat-line)
  (local-set-key [(control f)(control m)]  'mark-whole-lines)
  (local-set-key [(control f)(control ? )] 'replace-by-space)
  ;; (local-set-key [(meta s)]                'project-grep-find-ext)
  ;; (local-set-key [(control meta s)]        'project-grep-find-sexp-at-point)
  (local-set-key [(control meta n)]        'next-error)
  (local-set-key [(control meta p)]        'previous-error)
  (local-set-key [(control f)(control ?,)] 'compilation-ext-previous-file)
  (local-set-key [(control f)(control ?.)] 'compilation-ext-next-file))


;;; mybindings.el ends here
