;;; loaddefs-site-lisp.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (dired-do-relsymlink dired-jump-other-window dired-jump)
;;;;;;  "dired-x" "dired-x.el" (22083 9147 983042 652000))
;;; Generated autoloads from dired-x.el

(autoload 'dired-jump "dired-x" "\
Jump to dired buffer corresponding to current buffer.
If in a file, dired the current directory and move to file's line.
If in Dired already, pop up a level and goto old directory's line.
In case the proper dired file line cannot be found, refresh the dired
buffer and try again.
When OTHER-WINDOW is non-nil, jump to dired buffer in other window.
Interactively with prefix argument, read FILE-NAME and
move to its line in dired.

\(fn &optional OTHER-WINDOW FILE-NAME)" t nil)

(autoload 'dired-jump-other-window "dired-x" "\
Like \\[dired-jump] (`dired-jump') but in other window.

\(fn &optional FILE-NAME)" t nil)

(autoload 'dired-do-relsymlink "dired-x" "\
Relative symlink all marked (or next ARG) files into a directory.
Otherwise make a relative symbolic link to the current file.
This creates relative symbolic links like

    foo -> ../bar/foo

not absolute ones like

    foo -> /ugly/file/name/that/may/change/any/day/bar/foo

For absolute symlinks, use \\[dired-do-symlink].

\(fn &optional ARG)" t nil)

;;;***

(provide 'loaddefs-site-lisp)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs-site-lisp.el ends here
