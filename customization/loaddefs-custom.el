;;; loaddefs-custom.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "../../src/ef/src/IDE-support/Emacs/ef-mode"
;;;;;;  "../../src/ef/src/IDE-support/Emacs/ef-mode.el" (21713 61094
;;;;;;  848089 270000))
;;; Generated autoloads from ../../src/ef/src/IDE-support/Emacs/ef-mode.el

(autoload 'ef-mode "../../src/ef/src/IDE-support/Emacs/ef-mode" "\
Major mode for editing EF files.
Turning on EF mode runs the normal hook `ef-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads (gstack-mode) "../misc/gstack-mode/gstack-mode"
;;;;;;  "../misc/gstack-mode/gstack-mode.el" (22093 63435 410499
;;;;;;  631000))
;;; Generated autoloads from ../misc/gstack-mode/gstack-mode.el

(autoload 'gstack-mode "gstack-mode" "\
A major mode for viewing gstack output

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "/gstack[^/.]*\\'" 'gstack-mode))

;;;***

;;;### (autoloads nil "../misc/find-file-ext/find-file-ext" "../misc/find-file-ext/find-file-ext.el"
;;;;;;  (22087 27588 972149 28000))
;;; Generated autoloads from ../misc/find-file-ext/find-file-ext.el

(autoload 'ffe-find-file "../misc/find-file-ext/find-file-ext" "\
As find-file, however the file to be opened is given as an
  abbreviation.

\(fn ABBREV-PATH)" t nil)

(autoload 'ffe-show-abbrevs "../misc/find-file-ext/find-file-ext" "\
Shows in the echo area the list of abbrevs for the current
file. If there exists no map for the current directory, do
nothing.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../misc/hi-lock-ext" "../misc/hi-lock-ext.el"
;;;;;;  (20741 14150 0 0))
;;; Generated autoloads from ../misc/hi-lock-ext.el

(autoload 'highlight-toggle-sexp-or-region "../misc/hi-lock-ext" "\
Toggle highlighting of sexp-at-point or region.

It's the region if mark is active, it's the sexp at point
otherwise. If it is already highlighted, unhighlight it. The face
used for highlighting is the first unused face of
`highlight-face-list'

\(fn)" t nil)

(autoload 'highlight-arguments "../misc/hi-lock-ext" "\
Highlights all arguments of current C/C++ method, each in a
  different face. This is done by calling
  highlight-toggle-sexp-or-region for each argument of the
  method.

\(fn)" t nil)

(autoload 'highlight-members "../misc/hi-lock-ext" "\
Highlights all member identifiers, i.e. all identifiers that
  follow the naming ESEC guidelines for class members.

\(fn)" t nil)

(autoload 'highlight-arguments-uni "../misc/hi-lock-ext" "\
Highlights all argument identifiers (i.e. identifiers that follow the naming ESEC guidelines

\(fn)" t nil)

(autoload 'highlight-locals "../misc/hi-lock-ext" "\
Highlights all locals identifiers - however with a imprecise regex

\(fn)" t nil)

(autoload 'unhighlight-all "../misc/hi-lock-ext" "\
Unhighlight all highlightened sexp in current buffer

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../misc/kmacro-ext" "../misc/kmacro-ext.el"
;;;;;;  (22087 27269 357197 804000))
;;; Generated autoloads from ../misc/kmacro-ext.el

(autoload 'kmacro-start-stop-macro-ext "../misc/kmacro-ext" "\
If currently defining a macro, end it, else start defining a new.
For how exactly the macro is ended, see
`end-and-global-set-key-kbd-macro'.

\(fn)" t nil)

(autoload 'end-and-global-set-key-kbd-macro "../misc/kmacro-ext" "\
Ends recording of a kbd-macro and lets you assign the new macro a local key sequence.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "../misc/multiple-cursors/mc-edit-lines" "../misc/multiple-cursors/mc-edit-lines.el"
;;;;;;  (21137 60452 341375 985000))
;;; Generated autoloads from ../misc/multiple-cursors/mc-edit-lines.el

(autoload 'mc/edit-lines "../misc/multiple-cursors/mc-edit-lines" "\
Add one cursor to each line of the active region.
Starts from mark and moves in straight down or up towards the
line point is on.

\(fn)" t nil)

(autoload 'mc/edit-ends-of-lines "../misc/multiple-cursors/mc-edit-lines" "\
Add one cursor to the end of each line in the active region.

\(fn)" t nil)

(autoload 'mc/edit-beginnings-of-lines "../misc/multiple-cursors/mc-edit-lines" "\
Add one cursor to the beginning of each line in the active region.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modified-site-lisp/batch-mode" "../modified-site-lisp/batch-mode.el"
;;;;;;  (20741 14140 0 0))
;;; Generated autoloads from ../modified-site-lisp/batch-mode.el

(autoload 'batch-mode "../modified-site-lisp/batch-mode" "\
Major mode for editing batch scripts.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modified-site-lisp/find-dired+" "../modified-site-lisp/find-dired+.el"
;;;;;;  (20741 14140 0 0))
;;; Generated autoloads from ../modified-site-lisp/find-dired+.el

(defvar find-dired-hook nil "\
*Hook to be run at the end of each `find-dired' execution.")

(defvar find-dired-default-fn (and (fboundp 'symbol-name-nearest-point) 'symbol-name-nearest-point) "\
*Function of 0 args called to provide default input for \\[find-dired],
\\[find-name-dired], and  \\[find-grep-dired].

Some reasonable choices:
`word-nearest-point', `symbol-name-nearest-point', `sexp-nearest-point'.

If this is nil, then no default input is provided.")

(defconst find-ls-option (cond ((eq system-type 'berkeley-unix) '("-ls" . "-gilsb")) ((eq system-type 'windows-nt) (cons "-ls" dired-listing-switches)) (t '("-exec ls -ld {} \\;" . "-ld"))) "\
*Description of the option to `find' to produce an `ls -l'-type listing.
This is a cons of two strings (FIND-OPTION . LS-SWITCHES).  FIND-OPTION
gives the option (or options) to `find' that produce the desired output.
LS-SWITCHES is a list of `ls' switches to tell dired how to parse the output.")

(autoload 'find-dired "../modified-site-lisp/find-dired+" "\
Run `find' and put its output in a buffer in Dired Mode.
Then run `find-dired-hook' and `dired-after-readin-hook'.
The `find' command run (after changing into DIR) is:

    find . \\( ARGS \\) -ls

\(fn DIR ARGS)" t nil)

(autoload 'find-name-dired "../modified-site-lisp/find-dired+" "\
Search directory DIR recursively for files matching globbing PATTERN,
and run `dired' on those files.  PATTERN may use shell wildcards, and
it need not be quoted.  It is not an Emacs regexp.
The command run (after changing into DIR) is: find . -name 'PATTERN' -ls

\(fn DIR PATTERN)" t nil)

(autoload 'find-grep-dired "../modified-site-lisp/find-dired+" "\
Find files in DIR containing a regexp REGEXP.
The output is in a Dired buffer.
The `find' command run (after changing into DIR) is:

    find . -exec grep -s REGEXP {} \\; -ls

Thus REGEXP can also contain additional grep options.

\(fn DIR REGEXP)" t nil)

(autoload 'find-dired-filter "../modified-site-lisp/find-dired+" "\
Filter for \\[find-dired] processes.
PROC is the process.
STRING is the string to insert.

\(fn PROC STRING)" nil nil)

(autoload 'find-dired-sentinel "../modified-site-lisp/find-dired+" "\
Sentinel for \\[find-dired] processes.
PROC is the process.
STATE is the state of process PROC.

\(fn PROC STATE)" nil nil)

;;;***

;;;### (autoloads nil "../modified-site-lisp/find-dired-" "../modified-site-lisp/find-dired-.el"
;;;;;;  (20741 14140 0 0))
;;; Generated autoloads from ../modified-site-lisp/find-dired-.el

(defvar find-args-history nil "\
Minibuffer input history of args for `find-dired'.")

;;;***

;;;### (autoloads nil "../modified-site-lisp/ido" "../modified-site-lisp/ido.el"
;;;;;;  (20741 14140 0 0))
;;; Generated autoloads from ../modified-site-lisp/ido.el

(defvar ido-mode nil "\
Determines for which functional group (buffer and files) ido behavior
should be enabled.  The following values are possible:
- `buffer': Turn only on ido buffer behavior (switching, killing,
  displaying...)
- `file': Turn only on ido file behavior (finding, writing, inserting...)
- `both': Turn on ido buffer and file behavior.
- `nil': Turn off any ido switching.

Setting this variable directly does not take effect;
use either \\[customize] or the function `ido-mode'.")

(custom-autoload 'ido-mode "../modified-site-lisp/ido" nil)

(autoload 'ido-mode "../modified-site-lisp/ido" "\
Toggle ido speed-ups on or off.
With ARG, turn ido speed-up on if arg is positive, off otherwise.
Turning on ido-mode will remap (via a minor-mode keymap) the default
keybindings for the `find-file' and `switch-to-buffer' families of
commands to the ido versions of these functions.
However, if ARG arg equals 'files, remap only commands for files, or
if it equals 'buffers, remap only commands for buffer switching.
This function also adds a hook to the minibuffer.

\(fn &optional ARG)" t nil)

(autoload 'ido-switch-buffer "../modified-site-lisp/ido" "\
Switch to another buffer.
The buffer is displayed according to `ido-default-buffer-method' -- the
default is to show it in the same window, unless it is already visible
in another frame.

As you type in a string, all of the buffers matching the string are
displayed if substring-matching is used (default).  Look at
`ido-enable-prefix' and `ido-toggle-prefix'.  When you have found the
buffer you want, it can then be selected.  As you type, most keys have
their normal keybindings, except for the following: \\<ido-buffer-completion-map>

RET Select the buffer at the front of the list of matches.  If the
list is empty, possibly prompt to create new buffer.

\\[ido-select-text] Select the current prompt as the buffer.
If no buffer is found, prompt for a new one.

\\[ido-next-match] Put the first element at the end of the list.
\\[ido-prev-match] Put the last element at the start of the list.
\\[ido-complete] Complete a common suffix to the current string that
matches all buffers.  If there is only one match, select that buffer.
If there is no common suffix, show a list of all matching buffers
in a separate window.
\\[ido-edit-input] Edit input string.
\\[ido-fallback-command] Fallback to non-ido version of current command.
\\[ido-toggle-regexp] Toggle regexp searching.
\\[ido-toggle-prefix] Toggle between substring and prefix matching.
\\[ido-toggle-case] Toggle case-sensitive searching of buffer names.
\\[ido-completion-help] Show list of matching buffers in separate window.
\\[ido-enter-find-file] Drop into `ido-find-file'.
\\[ido-kill-buffer-at-head] Kill buffer at head of buffer list.
\\[ido-toggle-ignore] Toggle ignoring buffers listed in `ido-ignore-buffers'.

\(fn)" t nil)

(autoload 'ido-switch-buffer-other-window "../modified-site-lisp/ido" "\
Switch to another buffer and show it in another window.
The buffer name is selected interactively by typing a substring.
For details of keybindings, see `ido-switch-buffer'.

\(fn)" t nil)

(autoload 'ido-display-buffer "../modified-site-lisp/ido" "\
Display a buffer in another window but don't select it.
The buffer name is selected interactively by typing a substring.
For details of keybindings, see `ido-switch-buffer'.

\(fn)" t nil)

(autoload 'ido-kill-buffer "../modified-site-lisp/ido" "\
Kill a buffer.
The buffer name is selected interactively by typing a substring.
For details of keybindings, see `ido-switch-buffer'.

\(fn)" t nil)

(autoload 'ido-insert-buffer "../modified-site-lisp/ido" "\
Insert contents of a buffer in current buffer after point.
The buffer name is selected interactively by typing a substring.
For details of keybindings, see `ido-switch-buffer'.

\(fn)" t nil)

(autoload 'ido-switch-buffer-other-frame "../modified-site-lisp/ido" "\
Switch to another buffer and show it in another frame.
The buffer name is selected interactively by typing a substring.
For details of keybindings, see `ido-switch-buffer'.

\(fn)" t nil)

(autoload 'ido-find-file-in-dir "../modified-site-lisp/ido" "\
Switch to another file starting from DIR.

\(fn DIR)" t nil)

(autoload 'ido-find-file "../modified-site-lisp/ido" "\
Edit file with name obtained via minibuffer.
The file is displayed according to `ido-default-file-method' -- the
default is to show it in the same window, unless it is already
visible in another frame.

The file name is selected interactively by typing a substring.  As you
type in a string, all of the filenames matching the string are displayed
if substring-matching is used (default).  Look at `ido-enable-prefix' and
`ido-toggle-prefix'.  When you have found the filename you want, it can
then be selected.  As you type, most keys have their normal keybindings,
except for the following: \\<ido-file-completion-map>

RET Select the file at the front of the list of matches.  If the
list is empty, possibly prompt to create new file.

\\[ido-select-text] Select the current prompt as the buffer or file.
If no buffer or file is found, prompt for a new one.

\\[ido-next-match] Put the first element at the end of the list.
\\[ido-prev-match] Put the last element at the start of the list.
\\[ido-complete] Complete a common suffix to the current string that
matches all files.  If there is only one match, select that file.
If there is no common suffix, show a list of all matching files
in a separate window.
\\[ido-edit-input] Edit input string (including directory).
\\[ido-prev-work-directory] or \\[ido-next-work-directory] go to previous/next directory in work directory history.
\\[ido-merge-work-directories] search for file in the work directory history.
\\[ido-forget-work-directory] removes current directory from the work directory history.
\\[ido-prev-work-file] or \\[ido-next-work-file] cycle through the work file history.
\\[ido-wide-find-file-or-pop-dir] and \\[ido-wide-find-dir-or-delete-dir] prompts and uses find to locate files or directories.
\\[ido-make-directory] prompts for a directory to create in current directory.
\\[ido-fallback-command] Fallback to non-ido version of current command.
\\[ido-toggle-regexp] Toggle regexp searching.
\\[ido-toggle-prefix] Toggle between substring and prefix matching.
\\[ido-toggle-case] Toggle case-sensitive searching of file names.
\\[ido-toggle-vc] Toggle version control for this file.
\\[ido-toggle-literal] Toggle literal reading of this file.
\\[ido-completion-help] Show list of matching files in separate window.
\\[ido-toggle-ignore] Toggle ignoring files listed in `ido-ignore-files'.

\(fn)" t nil)

(autoload 'ido-find-file-other-window "../modified-site-lisp/ido" "\
Switch to another file and show it in another window.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'.

\(fn)" t nil)

(autoload 'ido-find-alternate-file "../modified-site-lisp/ido" "\
Switch to another file and show it in another window.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'.

\(fn)" t nil)

(autoload 'ido-find-file-read-only "../modified-site-lisp/ido" "\
Edit file read-only with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'.

\(fn)" t nil)

(autoload 'ido-find-file-read-only-other-window "../modified-site-lisp/ido" "\
Edit file read-only in other window with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'.

\(fn)" t nil)

(autoload 'ido-find-file-read-only-other-frame "../modified-site-lisp/ido" "\
Edit file read-only in other frame with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'.

\(fn)" t nil)

(autoload 'ido-display-file "../modified-site-lisp/ido" "\
Display a file in another window but don't select it.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'.

\(fn)" t nil)

(autoload 'ido-find-file-other-frame "../modified-site-lisp/ido" "\
Switch to another file and show it in another frame.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'.

\(fn)" t nil)

(autoload 'ido-write-file "../modified-site-lisp/ido" "\
Write current buffer to a file.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'.

\(fn)" t nil)

(autoload 'ido-insert-file "../modified-site-lisp/ido" "\
Insert contents of file in current buffer.
The file name is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'.

\(fn)" t nil)

(autoload 'ido-dired "../modified-site-lisp/ido" "\
Call `dired' the ido way.
The directory is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'.

\(fn)" t nil)

(autoload 'ido-read-buffer "../modified-site-lisp/ido" "\
Ido replacement for the built-in `read-buffer'.
Return the name of a buffer selected.
PROMPT is the prompt to give to the user.  DEFAULT if given is the default
buffer to be selected, which will go to the front of the list.
If REQUIRE-MATCH is non-nil, an existing buffer must be selected.

\(fn PROMPT &optional DEFAULT REQUIRE-MATCH)" nil nil)

(autoload 'ido-read-file-name "../modified-site-lisp/ido" "\
Ido replacement for the built-in `read-file-name'.
Read file name, prompting with PROMPT and completing in directory DIR.
See `read-file-name' for additional parameters.

\(fn PROMPT &optional DIR DEFAULT-FILENAME MUSTMATCH INITIAL PREDICATE)" nil nil)

(autoload 'ido-read-directory-name "../modified-site-lisp/ido" "\
Ido replacement for the built-in `read-directory-name'.
Read directory name, prompting with PROMPT and completing in directory DIR.
See `read-directory-name' for additional parameters.

\(fn PROMPT &optional DIR DEFAULT-DIRNAME MUSTMATCH INITIAL)" nil nil)

(autoload 'ido-completing-read "../modified-site-lisp/ido" "\
Ido replacement for the built-in `completing-read'.
Read a string in the minibuffer with ido-style completion.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
CHOICES is a list of strings which are the possible completions.
PREDICATE is currently ignored; it is included to be compatible
 with `completing-read'.
If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless
 the input is (or completes to) an element of CHOICES or is null.
 If the input is null, `ido-completing-read' returns DEF, or an empty
 string if DEF is nil, regardless of the value of REQUIRE-MATCH.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially,
 with point positioned at the end.
HIST, if non-nil, specifies a history list.
DEF, if non-nil, is the default value.

\(fn PROMPT CHOICES &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF)" nil nil)

;;;***

;;;### (autoloads nil "../modified-site-lisp/linkd" "../modified-site-lisp/linkd.el"
;;;;;;  (20741 14140 0 0))
;;; Generated autoloads from ../modified-site-lisp/linkd.el

(autoload 'linkd-version "../modified-site-lisp/linkd" "\
Display Linkd version.

\(fn)" t nil)

(autoload 'linkd-back "../modified-site-lisp/linkd" "\
Return to the buffer being viewed before the last link was followed.

\(fn)" t nil)

(autoload 'linkd-follow-at-point "../modified-site-lisp/linkd" "\
Follow the link at point.

\(fn)" t nil)

(autoload 'linkd-next-link "../modified-site-lisp/linkd" "\
Move point to the next link, if any.

\(fn)" t nil)

(autoload 'linkd-previous-link "../modified-site-lisp/linkd" "\
Move point to the previous link, if any.

\(fn)" t nil)

(autoload 'linkd-insert-single-arg-link "../modified-site-lisp/linkd" "\
Insert a link containing ARGUMENT.

\(fn TYPE-STRING ARGUMENT)" nil nil)

(autoload 'linkd-insert-tag "../modified-site-lisp/linkd" "\
Insert a tag.

\(fn TAG-NAME)" t nil)

(autoload 'linkd-insert-star "../modified-site-lisp/linkd" "\
Insert a star.

\(fn STAR-NAME)" t nil)

(autoload 'linkd-insert-wiki "../modified-site-lisp/linkd" "\
Insert a wiki link.

\(fn WIKI-NAME)" t nil)

(autoload 'linkd-insert-lisp "../modified-site-lisp/linkd" "\
Insert a Lisp sexp.

\(fn SEXP)" t nil)

(autoload 'linkd-insert-link "../modified-site-lisp/linkd" "\
Insert a link.
Optional arg TYPE is the link type.
Optional arg CURRENT-VALUES is a property list of current values.

\(fn &optional TYPE CURRENT-VALUES)" t nil)

(autoload 'linkd-edit-link-at-point "../modified-site-lisp/linkd" "\
Edit the Linkd link at point.

\(fn)" t nil)

(autoload 'linkd-export-default "../modified-site-lisp/linkd" "\
Export the current buffer with default settings to all available formats.

\(fn)" t nil)

(autoload 'linkd-latex-export "../modified-site-lisp/linkd" "\
Render a buffer as a LaTeX book chapter.

\(fn)" t nil)

(autoload 'linkd-wiki-find-page "../modified-site-lisp/linkd" "\
Find Linkd wiki page named PAGE-NAME.

\(fn PAGE-NAME)" t nil)

;;;***

;;;### (autoloads nil "../modified-site-lisp/move-text" "../modified-site-lisp/move-text.el"
;;;;;;  (21143 32520 841501 109000))
;;; Generated autoloads from ../modified-site-lisp/move-text.el

(autoload 'move-text-down "../modified-site-lisp/move-text" "\
Move region (transient-mark-mode active) or current line
  ARG lines down.

\(fn ARG)" t nil)

(autoload 'move-text-forward "../modified-site-lisp/move-text" "\
Move region (transient-mark-mode active) ARG columns forward.

\(fn ARG)" t nil)

(autoload 'move-text-up "../modified-site-lisp/move-text" "\
Move region (transient-mark-mode active) or current line
  ARG lines up.

\(fn ARG)" t nil)

(autoload 'move-text-backward "../modified-site-lisp/move-text" "\
Move region (transient-mark-mode active) ARG columns backward.

\(fn ARG)" t nil)

(autoload 'move-text-default-bindings "../modified-site-lisp/move-text" "\
Bind `move-text-up' and `move-text-down' to M-up and M-down.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "../modified-site-lisp/sunrise-commander" "../modified-site-lisp/sunrise-commander.el"
;;;;;;  (20741 14140 0 0))
;;; Generated autoloads from ../modified-site-lisp/sunrise-commander.el

(autoload 'sunrise "../modified-site-lisp/sunrise-commander" "\
Start the Sunrise Commander.
If LEFT-DIRECTORY is given, the left window will display that
directory (same for RIGHT-DIRECTORY). Specifying nil for any of
these values uses the default, ie. $HOME.

\(fn &optional LEFT-DIRECTORY RIGHT-DIRECTORY FILENAME)" t nil)

(autoload 'sunrise-cd "../modified-site-lisp/sunrise-commander" "\
Run Sunrise but give it the current directory to use.

\(fn)" t nil)

(autoload 'sr-dired "../modified-site-lisp/sunrise-commander" "\
Visit the given directory in `sr-mode'.

\(fn DIRECTORY &optional SWITCHES)" t nil)

;;;***

;;;### (autoloads nil "../modified-site-lisp/x-dict" "../modified-site-lisp/x-dict.el"
;;;;;;  (21741 63343 798084 765000))
;;; Generated autoloads from ../modified-site-lisp/x-dict.el

(autoload 'xdict-query "../modified-site-lisp/x-dict" "\
Query dict.leo.org for WORD.
This calls my python script x-dict (it can be found at: http://www.xsteve.at/prg/python)

\(fn WORD)" t nil)

(autoload 'xdict-query-with-word-at-point "../modified-site-lisp/x-dict" "\
Run `xdict-query' for the word at point.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../progmodes/autoexp-mode/autoexp-mode" "../progmodes/autoexp-mode/autoexp-mode.el"
;;;;;;  (21136 63505 250986 522000))
;;; Generated autoloads from ../progmodes/autoexp-mode/autoexp-mode.el

(autoload 'autoexp-mode "../progmodes/autoexp-mode/autoexp-mode" "\
Major mode for editing msvc's autoexp.dat files.
Turning on autoexp mode runs the normal hook `autoexp-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../progmodes/cppkoans-mode/cppkoans-mode"
;;;;;;  "../progmodes/cppkoans-mode/cppkoans-mode.el" (21607 47483
;;;;;;  800670 183000))
;;; Generated autoloads from ../progmodes/cppkoans-mode/cppkoans-mode.el

(autoload 'cppkoans-koans-buffer-p "../progmodes/cppkoans-mode/cppkoans-mode" "\
Return t if current buffer is part of koans of cppkoans, nil otherwise.
It does so using an heuristic.

\(fn)" t nil)

(autoload 'cppkoans-mode "../progmodes/cppkoans-mode/cppkoans-mode" "\
See (finder-commentary \"cppkoans-mode\")

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "../progmodes/doxymacs/doxymacs" "../progmodes/doxymacs/doxymacs.el"
;;;;;;  (21644 25668 948733 312000))
;;; Generated autoloads from ../progmodes/doxymacs/doxymacs.el

(autoload 'doxymacs-mode "../progmodes/doxymacs/doxymacs" "\
Minor mode for using/creating Doxygen documentation.
To submit a problem report, request a feature or get support, please
visit doxymacs' homepage at http://doxymacs.sourceforge.net/.

To see what version of doxymacs you are running, enter
`\\[doxymacs-version]'.

In order for `doxymacs-lookup' to work you will need to customise the
variable `doxymacs-doxygen-dirs'.

Key bindings:
\\{doxymacs-mode-map}

\(fn &optional ARG)" t nil)

(or (assoc 'doxymacs-mode minor-mode-alist) (setq minor-mode-alist (cons '(doxymacs-mode " doxy") minor-mode-alist)))

;;;***

;;;### (autoloads nil "../progmodes/ebnf-mode" "../progmodes/ebnf-mode.el"
;;;;;;  (20741 14140 0 0))
;;; Generated autoloads from ../progmodes/ebnf-mode.el

(autoload 'ebnf-mode "../progmodes/ebnf-mode" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "../progmodes/git-irb-mode" "../progmodes/git-irb-mode.el"
;;;;;;  (21143 32520 845501 109000))
;;; Generated autoloads from ../progmodes/git-irb-mode.el

(autoload 'git-irb-mode "../progmodes/git-irb-mode" "\
A major-mode for editing git's interative rebase files.

See (finder-commentary \"git-irb-mode\").

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../progmodes/pm-mode" "../progmodes/pm-mode.el"
;;;;;;  (20741 14140 0 0))
;;; Generated autoloads from ../progmodes/pm-mode.el

(autoload 'pm-mode "../progmodes/pm-mode" "\
Major mode for editing pm files.
Turning on pm mode runs the normal hook `pm-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../progmodes/rl-mode" "../progmodes/rl-mode.el"
;;;;;;  (20741 14140 0 0))
;;; Generated autoloads from ../progmodes/rl-mode.el

(autoload 'rl-mode "../progmodes/rl-mode" "\
Major mode for editing readline init files.
Turning on rl mode runs the normal hook `rl-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../progmodes/sln-mode/sln-mode" "../progmodes/sln-mode/sln-mode.el"
;;;;;;  (21136 63673 742985 68000))
;;; Generated autoloads from ../progmodes/sln-mode/sln-mode.el

(autoload 'sln-mode "../progmodes/sln-mode/sln-mode" "\
Major mode for editing msvc's *.sln files.
Turning on sln mode runs the normal hook `sln-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../progmodes/stream-mode" "../progmodes/stream-mode.el"
;;;;;;  (20741 14140 0 0))
;;; Generated autoloads from ../progmodes/stream-mode.el

(autoload 'stream-mode "../progmodes/stream-mode" "\
Major mode for editing stream files.
Turning on stream mode runs the normal hook `stream-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../progmodes/yas-mode" "../progmodes/yas-mode.el"
;;;;;;  (21600 31949 243781 802000))
;;; Generated autoloads from ../progmodes/yas-mode.el

(autoload 'yas-mode "../progmodes/yas-mode" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "../progmodes/zimbu-mode/zimbu-mode" "../progmodes/zimbu-mode/zimbu-mode.el"
;;;;;;  (21309 54890 61575 417000))
;;; Generated autoloads from ../progmodes/zimbu-mode/zimbu-mode.el

(autoload 'zimbu-mode "../progmodes/zimbu-mode/zimbu-mode" "\
Major mode for editing Zimbu code.
Turning on zimbu mode runs the normal hook `zimbu-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../projects/xentis" "../projects/xentis.el"
;;;;;;  (22102 56367 261745 53000))
;;; Generated autoloads from ../projects/xentis.el

(defvar xentis-file-name-regex "/src/xentis[^/]*/" "\
Files matching this regexp belong to xentis project")

(add-to-list 'file-coding-system-alist (list xentis-file-name-regex 'iso-latin-1-unix))

(add-hook 'change-major-mode-after-body-hook 'xentis-hook)

(add-hook 'c-mode-common-hook 'xentis-c-mode-common-hook)

(autoload 'xentis-hook "../projects/xentis" "\


\(fn)" nil nil)

(autoload 'xentis-c-mode-common-hook "../projects/xentis" "\


\(fn)" nil nil)

(autoload 'xentis-before-save-hook "../projects/xentis" "\


\(fn)" nil nil)

(add-hook 'before-save-hook 'xentis-before-save-hook t)

;;;***

;;;### (autoloads nil "../site-lisp/fill-column-indicator" "../site-lisp/fill-column-indicator.el"
;;;;;;  (21143 32520 849501 109000))
;;; Generated autoloads from ../site-lisp/fill-column-indicator.el

(autoload 'fci-mode "../site-lisp/fill-column-indicator" "\
Toggle fci-mode on and off.
Fci-mode indicates the location of the fill column by drawing a
thin line (a `rule') at the fill column.

With prefix ARG, turn fci-mode on if and only if ARG is positive.

The following options control the appearance of the fill-column
rule: `fci-rule-column', `fci-rule-width', `fci-rule-color',
`fci-rule-use-dashes', `fci-dash-pattern', `fci-rule-character',
and `fci-rule-character-color'.  For further options, see the
Customization menu or the package file.  (See the latter for tips
on troubleshooting.)

\(fn &optional ARG)" t nil)

(autoload 'turn-on-fci-mode "../site-lisp/fill-column-indicator" "\
Turn on fci-mode unconditionally.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../site-lisp/llvm-mode" "../site-lisp/llvm-mode.el"
;;;;;;  (21782 62671 405803 88000))
;;; Generated autoloads from ../site-lisp/llvm-mode.el

(autoload 'llvm-mode "../site-lisp/llvm-mode" "\
Major mode for editing LLVM source files.
\\{llvm-mode-map}
  Runs `llvm-mode-hook' on startup.

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons (purecopy "\\.ll\\'") 'llvm-mode))

;;;***

;;;### (autoloads nil "../site-lisp/ws-trim" "../site-lisp/ws-trim.el"
;;;;;;  (22076 56323 553770 320000))
;;; Generated autoloads from ../site-lisp/ws-trim.el

(defvar ws-trim-method-hook '(ws-trim-leading ws-trim-trailing) "\
*The kind of trimming done by the WS Trim mode and functions.
A single or a list of functions which are run on each line that's
getting trimmed.  Supplied trim functions:

`ws-trim-trailing'        Delete trailing whitespace.
`ws-trim-leading-spaces'  Replace unnecessary leading spaces with tabs.
`ws-trim-leading-tabs'    Replace leading tabs with spaces.
`ws-trim-leading'         Replace leading tabs or spaces according to
                          `indent-tabs-mode'.  If it's nil, leading
                          tabs are replaced with spaces, otherwise
                          it's the other way around.
`ws-trim-tabs'            Replace all tabs with spaces.

This is a perfectly normal hook run by `run-hooks' and custom
functions can of course be used.  There's no inherent restriction to
just whitespace trimming either, for that matter.  Each function
should modify the current line and leave point somewhere on it.")

(autoload 'ws-trim-line "../site-lisp/ws-trim" "\
Trim whitespace on the current line.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead.

\(fn ARG)" t nil)

(autoload 'ws-trim-region "../site-lisp/ws-trim" "\
Trim whitespace on each line in the region.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead.

\(fn ARG)" t nil)

(autoload 'ws-trim-buffer "../site-lisp/ws-trim" "\
Trim whitespace on each line in the buffer.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead.

\(fn ARG)" t nil)

(defvar ws-trim-mode nil "\
If non-nil, WS Trim mode is active.
This mode automatically trims whitespace on text lines.  The kind of
trimming is specified by the hook `ws-trim-method-hook'.  You can
either trim every line in the buffer or just the lines you edit
manually, see the variable `ws-trim-level' for details.  This mode
runs the hook `ws-trim-mode-hook' when activated.

Please note that there are other common functions, e.g. `indent-to',
`newline-and-indent' (often bound to LFD or RET), `fill-paragraph',
and the variable `indent-tabs-mode', that also trims whitespace in
various circumstances.  They are entirely independent of this mode.

To automatically enable WS Trim mode in any major mode, put
`turn-on-ws-trim' in the major mode's hook, e.g. in your .emacs:

  (add-hook 'emacs-lisp-mode-hook 'turn-on-ws-trim)

You can also activate WS Trim mode automagically in all modes where
it's likely to be useful by putting the following in .emacs:

  (global-ws-trim-mode t)

Exactly when WS Trim is activated are by default controlled by a
heuristic, see the function `ws-trim-mode-heuristic' for details.  You
can get more control over the process through the variable
`global-ws-trim-modes'.

This variable automatically becomes buffer local when modified.  It
should not be set directly; use the commands `ws-trim-mode' or
`turn-on-ws-trim' instead.")

(defvar ws-trim-level 0 "\
*How thorough automatic whitespace trimming should be in WS Trim mode.
If 3 or greater, all lines in the buffer are kept trimmed at all
times (if the buffer is modifiable).
If 2, all lines in the buffer are trimmed when the buffer is modified
for the first time.
If 1, only modified lines are trimmed.
If 0, only single modified lines are trimmed, i.e. operations that
modify more than one line don't cause any trimming (newline is an
exception).

The current line is never trimmed on any level, unless the buffer is
about to be written.  In that case the current line is treated as any
other line.

The default level is 0, which is very restrictive.  This is
particularly useful when you edit files which are compared with diff
\(e.g. for patches), because parts that you don't change manually are
kept unchanged.  You can also do block operations over several lines
without risking strange side effects (e.g. paste patches into mails).

This variable automatically becomes buffer local when changed.  Use
the function `set-default' to set the value it defaults to in all new
buffers.  If you want even more control it's best to put a suitable
function onto `ws-trim-mode-hook'.  Changes of `ws-trim-level' might
not take effect immediately; it's best set when the mode is
initialized.")

(defvar ws-trim-mode-line-string " Trim" "\
*Modeline string for WS Trim mode.
Set to nil to remove the modeline indicator for ws-trim.")

(defvar ws-trim-mode-hook nil "\
A normal hook which is run when WS Trim mode is turned on.
This hook is run by `run-hooks' and can therefore be buffer local.

Some care might be necessary when putting functions on this hook due
to the somewhat strange circumstances under which it's run.
Specifically, anything put here might indirectly be run from
`post-command-hook' or `find-file-hooks'.  Don't worry about it if you
just want to do something simple, e.g. setting some variables.")

(autoload 'turn-on-ws-trim "../site-lisp/ws-trim" "\
Unconditionally turn on WS Trim mode.
See the variable `ws-trim-mode' for further info on this mode.

\(fn)" t nil)

(autoload 'ws-trim-mode "../site-lisp/ws-trim" "\
Toggle WS Trim mode, which automatically trims whitespace on lines.
A positive prefix argument turns the mode on, any other prefix turns
it off.

See the variable docstring for details about this mode.

\(fn &optional ARG)" t nil)

(defvar global-ws-trim-mode nil "\
If non-nil, automagically turn on WS Trim mode in many major modes.
How it's done is controlled by the variable `ws-trim-global-modes'.

This variable should not be changed directly; use the command
`global-ws-trim-mode' instead.")

(defvar ws-trim-global-modes 'guess "\
*Controls which major modes should have WS Trim mode turned on.
Global WS Trim mode must first be activated, which is done by the
command `global-ws-trim-mode'.

If nil, no modes turn on WS Trim mode.
If t, all modes turn on WS Trim mode.
If `guess', then a heuristic is used to determine whether WS Trim mode
should be activated in the mode in question.  See
`ws-trim-mode-heuristic' for details.
If a list, then all modes whose `major-mode' symbol names matches some
entry in it turn on WS Trim mode.
If a list begins with `not', all modes but the ones mentioned turn on
WS Trim mode.
If a list begins with `guess', then the remaining elements must in
turn be lists as above.  All modes not specified in any of these lists
will use the heuristic.  E.g:

  (setq ws-trim-global-modes '(guess (Info-mode) (not c-mode c++-mode)))

turns on WS Trim in Info-mode (God knows why), off in C mode and
C++ mode, and uses the heuristic for all other modes.")

(autoload 'global-ws-trim-mode "../site-lisp/ws-trim" "\
Toggle Global WS Trim mode.
A positive prefix argument turns the mode on, any other prefix turns
it off.

When this mode is active, WS Trim mode is automagically turned on or
off in buffers depending on their major modes.  The behavior is
controlled by the `ws-trim-global-modes' variable.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "../textmodes/adoc-mode/adoc-mode" "../textmodes/adoc-mode/adoc-mode.el"
;;;;;;  (21607 29924 702512 772000))
;;; Generated autoloads from ../textmodes/adoc-mode/adoc-mode.el

(autoload 'adoc-mode "../textmodes/adoc-mode/adoc-mode" "\
Major mode for editing AsciiDoc text files.
Turning on Adoc mode runs the normal hook `adoc-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../textmodes/bbcode/bbcode" "../textmodes/bbcode/bbcode.el"
;;;;;;  (20741 14150 0 0))
;;; Generated autoloads from ../textmodes/bbcode/bbcode.el

(autoload 'bbcode "../textmodes/bbcode/bbcode" "\
Major mode for viewing log files.
Turning on bbcode mode runs the normal hook `bbcode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../textmodes/doxym-mode/doxym-mode" "../textmodes/doxym-mode/doxym-mode.el"
;;;;;;  (20741 14150 0 0))
;;; Generated autoloads from ../textmodes/doxym-mode/doxym-mode.el

(autoload 'doxym-mode "../textmodes/doxym-mode/doxym-mode" "\
Major mode for editing doxygen pages.
Turning on doxym mode runs the normal hook `doxym-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../textmodes/mediawiki/mediawiki" "../textmodes/mediawiki/mediawiki.el"
;;;;;;  (21741 59562 150053 40000))
;;; Generated autoloads from ../textmodes/mediawiki/mediawiki.el

(autoload 'mediawiki-draft "../textmodes/mediawiki/mediawiki" "\
Open a temporary buffer in wikipedia mode for editing an wikipedia
 draft, which an arbitrary piece of data. After finishing the editing
 either use C-c C-k \\[mediawiki-draft-buffer] to send the data into
 the mediawiki-draft-data-file, or send  the buffer using C-x C-s
\\[mediawiki-save]  and insert it later into a wikipedia article.

\(fn)" t nil)

(autoload 'mediawiki-draft-page "../textmodes/mediawiki/mediawiki" "\


\(fn)" t nil)

(autoload 'mediawiki-draft-buffer "../textmodes/mediawiki/mediawiki" "\
Mediawiki-draft-buffer sends the contents of the current (temporary)
buffer to the mediawiki-draft-buffer, see the variable
mediawiki-draft-data-file.

\(fn)" t nil)

(autoload 'mediawiki-mode "../textmodes/mediawiki/mediawiki" "\
Major mode for editing articles written in the markup language
used by Mediawiki.

Wikipedia articles are usually unfilled: newline characters are not
used for breaking paragraphs into lines. Unfortunately, Emacs does not
handle word wrapping yet. As a workaround, wikipedia-mode turns on
longlines-mode automatically. In case something goes wrong, the
following commands may come in handy:

\\[mediawiki-fill-article] fills the buffer.
\\[mediawiki-unfill-article] unfills the buffer.

Be warned that function can be dead  slow, better use mediawiki-unfill-paragraph-or-region.
\\[mediawiki-unfill-paragraph-or-region] unfills the paragraph
\\[mediawiki-unfill-paragraph-simple] doehe same but simpler.

The following commands put in markup structures.
\\[mediawiki-insert-strong-emphasis] inserts italics
\\[mediawiki-insert-bold] inserts bold text
\\[mediawiki-insert-italics] italics
\\[mediawiki-insert-header] header
\\[mediawiki-insert-link] inserts a link

The following commands are also defined:
\\[mediawiki-insert-user] inserts user name
\\[mediawiki-insert-signature] inserts ~~~~
\\[mediawiki-insert-enumerate] inserts enumerate type structures
\\[mediawiki-insert-itemize] inserts itemize type structures
\\[mediawiki-insert-hline] inserts a hline

The draft functionality
\\[mediawiki-draft]
\\[mediawiki-draft-region]
\\[mediawiki-draft-view-draft]
\\[mediawiki-draft-page]
\\[mediawiki-draft-buffer]

Replying and sending functionality
\\[mediawiki-reply-at-point-simple]
\\[mediawiki-draft-reply]

The register functionality
\\[mediawiki-copy-page-to-register]
\\[defun mediawiki-insert-page-to-register]

Some simple editing commands.
\\[mediawiki-enhance-indent]
\\[mediawiki-yank-prefix]
\\[mediawiki-unfill-paragraph-or-region]

\\[mediawiki-terminate-paragraph]     starts a new list item or paragraph in a context-aware manner.
\\[mediawiki-next-header]     moves to the next (sub)section header.
\\[mediawiki-prev-header]     moves to the previous (sub)section header.

\(fn)" t nil)

;;;***

(provide 'loaddefs-custom)
;; Local Variables:
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs-custom.el ends here
