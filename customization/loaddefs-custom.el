;;; loaddefs-custom.el --- automatically extracted autoloads
;;
;;; Code:


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

;;;### (autoloads nil "../modified-site-lisp/whitespace" "../modified-site-lisp/whitespace.el"
;;;;;;  (21143 32744 357499 181000))
;;; Generated autoloads from ../modified-site-lisp/whitespace.el

(autoload 'whitespace-mode "../modified-site-lisp/whitespace" "\
Toggle whitespace minor mode visualization (\"ws\" on modeline).

If ARG is null, toggle whitespace visualization.
If ARG is a number greater than zero, turn on visualization;
otherwise, turn off visualization.

See also `whitespace-style', `whitespace-newline' and
`whitespace-display-mappings'.

\(fn &optional ARG)" t nil)

(autoload 'whitespace-newline-mode "../modified-site-lisp/whitespace" "\
Toggle NEWLINE minor mode visualization (\"nl\" on modeline).

If ARG is null, toggle NEWLINE visualization.
If ARG is a number greater than zero, turn on visualization;
otherwise, turn off visualization.

Use `whitespace-newline-mode' only for NEWLINE visualization
exclusively.  For other visualizations, including NEWLINE
visualization together with (HARD) SPACEs and/or TABs, please,
use `whitespace-mode'.

See also `whitespace-newline' and `whitespace-display-mappings'.

\(fn &optional ARG)" t nil)

(defvar global-whitespace-mode nil "\
Non-nil if Global-Whitespace mode is enabled.
See the command `global-whitespace-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-whitespace-mode'.")

(custom-autoload 'global-whitespace-mode "../modified-site-lisp/whitespace" nil)

(autoload 'global-whitespace-mode "../modified-site-lisp/whitespace" "\
Toggle whitespace global minor mode visualization (\"WS\" on modeline).

If ARG is null, toggle whitespace visualization.
If ARG is a number greater than zero, turn on visualization;
otherwise, turn off visualization.

See also `whitespace-style', `whitespace-newline' and
`whitespace-display-mappings'.

\(fn &optional ARG)" t nil)

(defvar global-whitespace-newline-mode nil "\
Non-nil if Global-Whitespace-Newline mode is enabled.
See the command `global-whitespace-newline-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-whitespace-newline-mode'.")

(custom-autoload 'global-whitespace-newline-mode "../modified-site-lisp/whitespace" nil)

(autoload 'global-whitespace-newline-mode "../modified-site-lisp/whitespace" "\
Toggle NEWLINE global minor mode visualization (\"NL\" on modeline).

If ARG is null, toggle NEWLINE visualization.
If ARG is a number greater than zero, turn on visualization;
otherwise, turn off visualization.

Use `global-whitespace-newline-mode' only for NEWLINE
visualization exclusively.  For other visualizations, including
NEWLINE visualization together with (HARD) SPACEs and/or TABs,
please, use `global-whitespace-mode'.

See also `whitespace-newline' and `whitespace-display-mappings'.

\(fn &optional ARG)" t nil)

(autoload 'whitespace-toggle-options "../modified-site-lisp/whitespace" "\
Toggle local `whitespace-mode' options.

If local whitespace-mode is off, toggle the option given by ARG
and turn on local whitespace-mode.

If local whitespace-mode is on, toggle the option given by ARG
and restart local whitespace-mode.

Interactively, it reads one of the following chars:

  CHAR	MEANING
  (VIA FACES)
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   l	toggle \"long lines\" visualization
   L	toggle \"long lines\" tail visualization
   n	toggle NEWLINE visualization
   e	toggle empty line at bob and/or eob visualization
   C-i	toggle indentation SPACEs visualization (via `indent-tabs-mode')
   I	toggle indentation SPACEs visualization
   i	toggle indentation TABs visualization
   C-a	toggle SPACEs after TAB visualization (via `indent-tabs-mode')
   A	toggle SPACEs after TAB: SPACEs visualization
   a	toggle SPACEs after TAB: TABs visualization
   C-b	toggle SPACEs before TAB visualization (via `indent-tabs-mode')
   B	toggle SPACEs before TAB: SPACEs visualization
   b	toggle SPACEs before TAB: TABs visualization

  (VIA DISPLAY TABLE)
   T	toggle TAB visualization
   S	toggle SPACEs before TAB visualization
   N	toggle NEWLINE visualization

   x	restore `whitespace-style' value
   ?	display brief help

Non-interactively, ARG should be a symbol or a list of symbols.
The valid symbols are:

   tabs			toggle TAB visualization
   spaces		toggle SPACE and HARD SPACE visualization
   trailing		toggle trailing blanks visualization
   lines		toggle \"long lines\" visualization
   lines-tail		toggle \"long lines\" tail visualization
   newline		toggle NEWLINE visualization
   empty		toggle empty line at bob and/or eob visualization
   indentation		toggle indentation SPACEs visualization
   indentation::tab	toggle indentation SPACEs visualization
   indentation::space	toggle indentation TABs visualization
   space-after-tab		toggle SPACEs after TAB visualization
   space-after-tab::tab		toggle SPACEs after TAB: SPACEs visualization
   space-after-tab::space	toggle SPACEs after TAB: TABs visualization
   space-before-tab		toggle SPACEs before TAB visualization
   space-before-tab::tab	toggle SPACEs before TAB: SPACEs visualization
   space-before-tab::space	toggle SPACEs before TAB: TABs visualization

   tab-mark		toggle TAB visualization
   space-mark		toggle SPACEs before TAB visualization
   newline-mark		toggle NEWLINE visualization

   whitespace-style	restore `whitespace-style' value

See `whitespace-style' and `indent-tabs-mode' for documentation.

\(fn ARG)" t nil)

(autoload 'global-whitespace-toggle-options "../modified-site-lisp/whitespace" "\
Toggle global `whitespace-mode' options.

If global whitespace-mode is off, toggle the option given by ARG
and turn on global whitespace-mode.

If global whitespace-mode is on, toggle the option given by ARG
and restart global whitespace-mode.

Interactively, it accepts one of the following chars:

  CHAR	MEANING
  (VIA FACES)
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   l	toggle \"long lines\" visualization
   L	toggle \"long lines\" tail visualization
   n	toggle NEWLINE visualization
   e	toggle empty line at bob and/or eob visualization
   C-i	toggle indentation SPACEs visualization (via `indent-tabs-mode')
   I	toggle indentation SPACEs visualization
   i	toggle indentation TABs visualization
   C-a	toggle SPACEs after TAB visualization (via `indent-tabs-mode')
   A	toggle SPACEs after TAB: SPACEs visualization
   a	toggle SPACEs after TAB: TABs visualization
   C-b	toggle SPACEs before TAB visualization (via `indent-tabs-mode')
   B	toggle SPACEs before TAB: SPACEs visualization
   b	toggle SPACEs before TAB: TABs visualization

  (VIA DISPLAY TABLE)
   T	toggle TAB visualization
   S	toggle SPACEs before TAB visualization
   N	toggle NEWLINE visualization

   x	restore `whitespace-style' value
   ?	display brief help

Non-interactively, ARG should be a symbol or a list of symbols.
The valid symbols are:

   tabs			toggle TAB visualization
   spaces		toggle SPACE and HARD SPACE visualization
   trailing		toggle trailing blanks visualization
   lines		toggle \"long lines\" visualization
   lines-tail		toggle \"long lines\" tail visualization
   newline		toggle NEWLINE visualization
   empty		toggle empty line at bob and/or eob visualization
   indentation		toggle indentation SPACEs visualization
   indentation::tab	toggle indentation SPACEs visualization
   indentation::space	toggle indentation TABs visualization
   space-after-tab		toggle SPACEs after TAB visualization
   space-after-tab::tab		toggle SPACEs after TAB: SPACEs visualization
   space-after-tab::space	toggle SPACEs after TAB: TABs visualization
   space-before-tab		toggle SPACEs before TAB visualization
   space-before-tab::tab	toggle SPACEs before TAB: SPACEs visualization
   space-before-tab::space	toggle SPACEs before TAB: TABs visualization

   tab-mark		toggle TAB visualization
   space-mark		toggle SPACEs before TAB visualization
   newline-mark		toggle NEWLINE visualization

   whitespace-style	restore `whitespace-style' value

See `whitespace-style' and `indent-tabs-mode' for documentation.

\(fn ARG)" t nil)

(autoload 'whitespace-cleanup "../modified-site-lisp/whitespace" "\
Cleanup some blank problems in all buffer or at region.

It usually applies to the whole buffer, but in transient mark
mode when the mark is active, it applies to the region.  It also
applies to the region when it is not in transient mark mode, the
mark is active and \\[universal-argument] was pressed just before
calling `whitespace-cleanup' interactively.

See also `whitespace-cleanup-region'.

The problems cleaned up are:

1. empty lines at beginning of buffer.
2. empty lines at end of buffer.
   If `whitespace-style' includes the value `empty', remove all
   empty lines at beginning and/or end of buffer.

3. 8 or more SPACEs at beginning of line.
   If `whitespace-style' includes the value `indentation':
   replace 8 or more SPACEs at beginning of line by TABs, if
   `indent-tabs-mode' is non-nil; otherwise, replace TABs by
   SPACEs.
   If `whitespace-style' includes the value `indentation::tab',
   replace 8 or more SPACEs at beginning of line by TABs.
   If `whitespace-style' includes the value `indentation::space',
   replace TABs by SPACEs.

4. SPACEs before TAB.
   If `whitespace-style' includes the value `space-before-tab':
   replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style' includes the value
   `space-before-tab::tab', replace SPACEs by TABs.
   If `whitespace-style' includes the value
   `space-before-tab::space', replace TABs by SPACEs.

5. SPACEs or TABs at end of line.
   If `whitespace-style' includes the value `trailing', remove
   all SPACEs or TABs at end of line.

6. 8 or more SPACEs after TAB.
   If `whitespace-style' includes the value `space-after-tab':
   replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style' includes the value
   `space-after-tab::tab', replace SPACEs by TABs.
   If `whitespace-style' includes the value
   `space-after-tab::space', replace TABs by SPACEs.

See `whitespace-style', `indent-tabs-mode' and `tab-width' for
documentation.

\(fn)" t nil)

(autoload 'whitespace-cleanup-region "../modified-site-lisp/whitespace" "\
Cleanup some blank problems at region.

The problems cleaned up are:

1. 8 or more SPACEs at beginning of line.
   If `whitespace-style' includes the value `indentation':
   replace 8 or more SPACEs at beginning of line by TABs, if
   `indent-tabs-mode' is non-nil; otherwise, replace TABs by
   SPACEs.
   If `whitespace-style' includes the value `indentation::tab',
   replace 8 or more SPACEs at beginning of line by TABs.
   If `whitespace-style' includes the value `indentation::space',
   replace TABs by SPACEs.

2. SPACEs before TAB.
   If `whitespace-style' includes the value `space-before-tab':
   replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style' includes the value
   `space-before-tab::tab', replace SPACEs by TABs.
   If `whitespace-style' includes the value
   `space-before-tab::space', replace TABs by SPACEs.

3. SPACEs or TABs at end of line.
   If `whitespace-style' includes the value `trailing', remove
   all SPACEs or TABs at end of line.

4. 8 or more SPACEs after TAB.
   If `whitespace-style' includes the value `space-after-tab':
   replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style' includes the value
   `space-after-tab::tab', replace SPACEs by TABs.
   If `whitespace-style' includes the value
   `space-after-tab::space', replace TABs by SPACEs.

See `whitespace-style', `indent-tabs-mode' and `tab-width' for
documentation.

\(fn START END)" t nil)

(autoload 'whitespace-report "../modified-site-lisp/whitespace" "\
Report some whitespace problems in buffer.

Return nil if there is no whitespace problem; otherwise, return
non-nil.

If FORCE is non-nil or \\[universal-argument] was pressed just
before calling `whitespace-report' interactively, it forces
`whitespace-style' to have:

   empty
   trailing
   indentation
   space-before-tab
   space-after-tab

If REPORT-IF-BOGUS is non-nil, it reports only when there are any
whitespace problems in buffer.

Report if some of the following whitespace problems exist:

* If `indent-tabs-mode' is non-nil:
   empty		1. empty lines at beginning of buffer.
   empty		2. empty lines at end of buffer.
   trailing		3. SPACEs or TABs at end of line.
   indentation		4. 8 or more SPACEs at beginning of line.
   space-before-tab	5. SPACEs before TAB.
   space-after-tab	6. 8 or more SPACEs after TAB.

* If `indent-tabs-mode' is nil:
   empty		1. empty lines at beginning of buffer.
   empty		2. empty lines at end of buffer.
   trailing		3. SPACEs or TABs at end of line.
   indentation		4. TABS at beginning of line.
   space-before-tab	5. SPACEs before TAB.
   space-after-tab	6. 8 or more SPACEs after TAB.

See `whitespace-style' for documentation.
See also `whitespace-cleanup' and `whitespace-cleanup-region' for
cleaning up these problems.

\(fn &optional FORCE REPORT-IF-BOGUS)" t nil)

(autoload 'whitespace-report-region "../modified-site-lisp/whitespace" "\
Report some whitespace problems in a region.

Return nil if there is no whitespace problem; otherwise, return
non-nil.

If FORCE is non-nil or \\[universal-argument] was pressed just
before calling `whitespace-report-region' interactively, it
forces `whitespace-style' to have:

   empty
   indentation
   space-before-tab
   trailing
   space-after-tab

If REPORT-IF-BOGUS is non-nil, it reports only when there are any
whitespace problems in buffer.

Report if some of the following whitespace problems exist:

* If `indent-tabs-mode' is non-nil:
   empty		1. empty lines at beginning of buffer.
   empty		2. empty lines at end of buffer.
   trailing		3. SPACEs or TABs at end of line.
   indentation		4. 8 or more SPACEs at beginning of line.
   space-before-tab	5. SPACEs before TAB.
   space-after-tab	6. 8 or more SPACEs after TAB.

* If `indent-tabs-mode' is nil:
   empty		1. empty lines at beginning of buffer.
   empty		2. empty lines at end of buffer.
   trailing		3. SPACEs or TABs at end of line.
   indentation		4. TABS at beginning of line.
   space-before-tab	5. SPACEs before TAB.
   space-after-tab	6. 8 or more SPACEs after TAB.

See `whitespace-style' for documentation.
See also `whitespace-cleanup' and `whitespace-cleanup-region' for
cleaning up these problems.

\(fn START END &optional FORCE REPORT-IF-BOGUS)" t nil)

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
;;;;;;  (20991 65242 618083 464000))
;;; Generated autoloads from ../progmodes/doxymacs/doxymacs.el

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
;;;;;;  (21607 47531 620568 584000))
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

;;;***

(provide 'loaddefs-custom)
;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs-custom.el ends here
