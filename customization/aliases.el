;;; aliases.el --- sensorflo's aliases
;;
;;; Variables:

;;; misc commands: ...
(defalias 'bkr 'browse-kill-ring)
(defalias 'ir 'indent-region) 		; also on C-M-\
(defalias 'fr 'fill-region)
(defalias 'ufp 'unfill-paragraph)
(defalias 'gf 'grep-find)
(defalias 'fgd 'find-grep-dired)
(defalias 'fd 'find-dired)
(defalias 'cg 'customize-group)
(defalias 'cv 'customize-variable)       ; with custimize buffer
(defalias 'csv 'customize-save-variable)  ; with minibuffer, save for future sessions
(defalias 'csev 'customize-set-variable)  ; with minibuffer, only for this session
(defalias 'cf 'customize-face)
(defalias 'sv 'set-variable)
(defalias 'lsk 'local-set-key)
(defalias 'gsk 'global-set-key)
(defalias 'fb 'font-lock-fontify-buffer)
(defalias 'ar 'align-regexp)
(defalias 'fl 'find-library)
(defalias 'll 'load-library)
(defalias 'ff 'find-file) ; C-x f is quick, but sometimes my mind thinks I ought to start with M-x f(ind)...
(defalias 'ffl 'find-file-literally)
(defalias 'id 'insert-date)
(defalias 'iid 'insert-initials-date)
(defalias 'url 'browse-url-at-point)
(defalias 'urlr 'browse-url-of-region)
(defalias 'afc 'c-align-function-call)
(defalias 'rbs 'replace-by-space)
(defalias 'dq 'xdict-query)
(defalias 'er 'eval-region)
(defalias 'isb 'ispell-buffer)
(defalias 'isr 'ispell-region)
(defalias 'isw 'ispell-word)
(defalias 'cib 'clone-indirect-buffer)
(defalias 'ckrs 'clipboard-kill-ring-save)
(defalias 'fc 'finder-commentary)
(defalias 'rvb 'revert-buffer)
(defalias 'rnb 'rename-buffer-ext)
(defalias 'rnu 'rename-uniquely)
(defalias 'rp 'remove-parentheses)
(defalias 'tw 'transpose-windows)
(defalias 'sc 'shell-command-ext)	; also on M-!
(defalias 'dm 'deactivate-mark)
(defalias 'dam 'deactivate-mark)
(defalias 'hl 'hide-lines)
(defalias 'sb 'speedbar)
(defalias 'vtt 'visit-tags-table)
(defalias 'o 'occur)
(defalias 'sa 'ffe-show-abbrevs)
(defalias 'ac 'adoc-calc)
(defalias 'gc 'goto-char)
(defalias 'ss 'svn-status)
(defalias 'svn 'svn-status-wd)
(defalias 'mc 'mark-comment)
(defalias 'cbfn 'copy-buffer-file-name-as-kill)
(defalias 'mb 'make-backup)
(defalias 'sj 'senator-jump)
(defalias 'rb 're-builder)
(defalias 'reb 're-builder)
(defalias 'mws 'mediawiki-site)
(defalias 'mwo 'mediawiki-open)
(defalias 'cm 'chmod)
(defalias 'fsb 'flyspell-buffer)
(defalias 'wc 'whitespace-cleanup)
(defalias 'ufal 'update-file-autoloads)

;;; toggle: t...
(defalias 'ttl 'toggle-truncate-lines)
(defalias 'tww 'toggle-word-wrap)
(defalias 'tbc 'toggle-background-color)
(defalias 'wet 'whitespace-ext-toggle)

;;; list: l...
(defalias 'lp 'list-processes)
(defalias 'lcd 'list-colors-display)
(defalias 'lfd 'list-faces-display)
(defalias 'lb 'list-bookmarks)

;;; apropos: a...
(defalias 'a 'apropos)
(defalias 'aval 'apropos-value)
(defalias 'avar 'apropos-variable)
(defalias 'al 'apropos-library)
(defalias 'am 'apropos-mode)

;;; describe: d...
(defalias 'df 'describe-face)
(defalias 'dcg 'describe-custom-group)
(defalias 'dmm 'describe-minor-mode)	;!! clash
(defalias 'ds 'describe-symbol)
(defalias 'dtp 'describe-text-properties)
(defalias 'dc 'describe-char)

;;; file: ...file
(defalias 'bcf 'byte-compile-file)
(defalias 'lf 'load-file)
(defalias 'rnf 'rename-file)
(defalias 'sfm 'set-file-modes) ; see also cm=chmod

;;; modes: ...m
(defalias 'elm 'emacs-lisp-mode)
(defalias 'tm 'text-mode)
(defalias 'adm 'adoc-mode)
(defalias 'dmm 'doxym-mode)
(defalias 'flm 'font-lock-mode)
(defalias 'omm 'outline-minor-mode)
(defalias 'bfm 'buffer-face-mode)
(defalias 'mbm 'menu-bar-mode)
(defalias 'tbm 'tool-bar-mode)
(defalias 'sbm 'scroll-bar-mode)
(defalias 'hlp 'highlight-parentheses-mode)
(defalias 'ssm 'shell-script-mode)
(defalias 'wm 'whitespace-mode)
(defalias 'vm 'view-mode)
(defalias 'afm 'auto-fill-mode)
(defalias 'fam 'filladapt-mode)
(defalias 'nm 'normal-mode)
(defalias 'mm 'makefile-mode)
(defalias 'mgm 'makefile-gmake-mode)
(defalias 'gmm 'makefile-gmake-mode)
(defalias 'dsm 'delete-selection-mode)
(defalias 'bbcm 'bbcode)
(defalias 'mwm 'mediawiki-mode)
(defalias 'rstm 'rst-mode)
(defalias 'rem 'rst-mode)
(defalias 'vlm 'visual-line-mode)
(defalias 'ldm 'linkd-mode)
(defalias 'hlm 'hi-lock-mode)
(defalias 'fspm 'flyspell-prog-mode)
(defalias 'fpm 'flyspell-prog-mode)
(defalias 'fsm 'flyspell-mode)
(defalias 'fm 'flyspell-mode)
(defalias 'lnm 'line-number-mode)

;;; marker pen: mp...
(defalias 'mp0 'markerpen0)
(defalias 'mp1 'markerpen1)
(defalias 'mp2 'markerpen2)
(defalias 'mp3 'markerpen3)
(defalias 'mpmr 'markerpen-mark-region)
(defalias 'mpcr 'markerpen-clear-region)

;;; hide show: hs...
(defalias 'hshl 'hs-hide-level)
(defalias 'hshb 'hs-hide-block)
(defalias 'hssb 'hs-show-block)
(defalias 'hssa 'hs-show-all)

;;; (e)diff
;;  <d>iff <m>erge | <r>egions <b>uffers <f>iles <d>irectories | <r>evision | <3>
(defalias 'edr 'ediff-regions-wordwise) ;; diff
(defalias 'edcf 'ediff-current-file)
(defalias 'edf 'ediff-files)
(defalias 'edb 'ediff-buffers)
(defalias 'edd 'ediff-directories)
(defalias 'edf3 'ediff-files3) ;; diff 3
(defalias 'edb3 'ediff-buffers3)
(defalias 'edd3 'ediff-directories3)
(defalias 'emf3 'ediff-merge-files-with-ancestor) ;; merge 3
(defalias 'emb3 'ediff-merge-buffers-with-ancestor)
(defalias 'emd3 'ediff-merge-directories-with-ancestor)
(defalias 'edfr 'ediff-revision)        ;; diff revisions
(defalias 'eddr 'ediff-directory-revisions)
(defalias 'emfr 'ediff-merge-revisions-with-ancestor) ;; merge revisions
(defalias 'emdr 'ediff-merge-directory-revisions-with-ancestor)

;;; icicles: i...
(defalias 'ick 'icicle-complete-keys)

;;; aliases.el ends here
