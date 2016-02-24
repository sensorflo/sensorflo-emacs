;;; loaddefs-local-site-lisp.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "ace-jump-mode" "ace-jump-mode.el" (21600 29556
;;;;;;  411761 728000))
;;; Generated autoloads from ace-jump-mode.el

(autoload 'ace-jump-mode "ace-jump-mode" "\
AceJump mode is a minor mode for you to quick jump to a
position in the curret view.
   There is three submode now:
     `ace-jump-char-mode'
     `ace-jump-word-mode'
     `ace-jump-line-mode'

You can specify the sequence about which mode should enter
by customize `ace-jump-mode-submode-list'.

If you do not want to query char for word mode, you can change
`ace-jump-word-mode-use-query-char' to nil.

If you don't like the default move keys, you can change it by
setting `ace-jump-mode-move-keys'.

You can constrol whether use the case sensitive via
`ace-jump-mode-case-fold'.

\(fn &optional PREFIX)" t nil)

;;;***

;;;### (autoloads nil "bashdb" "bashdb.el" (20741 14138 0 0))
;;; Generated autoloads from bashdb.el

(autoload 'bashdb "bashdb" "\
Run bashdb on program FILE in buffer *bashdb-cmd-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

You can specify a target script and its arguments like:

  bash YOUR-SCRIPT ARGUMENT...

or
  
  bashdb YOUR-SCRIPT ARGUMENT...

Generally the former one works fine. The later one may be useful if
you have not installed bashdb yet or you have installed bashdb to the
place where Bash doesn't expect

The custom variable `gud-bashdb-command-name' sets the pattern used
to invoke bashdb.

If `bashdb-many-windows' is nil (the default value) then bashdb just
starts with two windows: one displaying the GUD buffer and the
other with the source file with the main routine of the inferior.

If `bashdb-many-windows' is t, regardless of the value of the layout
below will appear.

+----------------------------------------------------------------------+
|                               GDB Toolbar                            |
+-----------------------------------+----------------------------------+
| GUD buffer (I/O of GDB)                                              |
|                                                                      |
|                                                                      |
|                                                                      |
+-----------------------------------+----------------------------------+
| Source buffer                                                        |
|                                                                      |
+-----------------------------------+----------------------------------+
| Stack buffer                      | Breakpoints buffer               |
| RET  bashdb-goto-stack-frame      | SPC bashdb-toggle-breakpoint     |
|                                   | RET bashdb-goto-breakpoint       |
|                                   | D   bashdb-delete-breakpoint     |
+-----------------------------------+----------------------------------+
.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads nil "col-highlight" "col-highlight.el" (20741 14138
;;;;;;  0 0))
;;; Generated autoloads from col-highlight.el

(let ((loads (get 'column-highlight 'custom-loads))) (if (member '"col-highlight" loads) nil (put 'column-highlight 'custom-loads (cons '"col-highlight" loads))))

(defvar col-highlight-vline-face-flag t "\
*Non-nil means `column-highlight-mode' uses `col-highlight-face'.
nil means that it uses `vline-face'.")

(custom-autoload 'col-highlight-vline-face-flag "col-highlight" t)

(defvar col-highlight-period 1 "\
*Number of seconds to highlight the current column.")

(custom-autoload 'col-highlight-period "col-highlight" t)

(defvar col-highlight-overlay-priority 300 "\
*Priority to use for overlays in `vline-overlay-table'.
A higher priority can make the vline highlighting appear on top of
other overlays that might exist.")

(custom-autoload 'col-highlight-overlay-priority "col-highlight" t)

(defface col-highlight '((t (:background "SlateGray3"))) "\
*Face for current-column highlighting by `column-highlight-mode'.
Not used if `col-highlight-vline-face-flag' is nil." :group (quote column-highlight) :group (quote faces))

(defvar column-highlight-mode nil "\
Non-nil if Column-Highlight mode is enabled.
See the command `column-highlight-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `column-highlight-mode'.")

(custom-autoload 'column-highlight-mode "col-highlight" nil)

(autoload 'column-highlight-mode "col-highlight" "\
Toggle highlighting the current column.
With ARG, turn column highlighting on if and only if ARG is positive.

Column-Highlight mode uses the functions
`col-highlight-unhighlight' and `col-highlight-highlight'
on `pre-command-hook' and `post-command-hook'.

\(fn &optional ARG)" t nil)

(defalias 'toggle-highlight-column-when-idle 'col-highlight-toggle-when-idle)

(autoload 'col-highlight-toggle-when-idle "col-highlight" "\
Turn on or off highlighting the current column when Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off.

\(fn &optional ARG)" t nil)

(autoload 'col-highlight-set-interval "col-highlight" "\
Set the delay before highlighting current column when Emacs is idle.
Whenever Emacs has been idle for N seconds, the current column is
highlighted using the face that is the value of variable
`col-highlight-face'.

To turn on or off automatically highlighting the current column
when Emacs is idle, use `\\[toggle-highlight-column-when-idle].

\(fn N)" t nil)

(defalias 'flash-column-highlight 'col-highlight-flash)

(autoload 'col-highlight-flash "col-highlight" "\
Highlight the current column for `col-highlight-period' seconds.
With a prefix ARG, highlight for that many seconds.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "company-mode" "company-mode.el" (20741 14138
;;;;;;  0 0))
;;; Generated autoloads from company-mode.el

(autoload 'company-mode "company-mode" "\


\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "compilation-recenter-end" "compilation-recenter-end.el"
;;;;;;  (20741 14138 0 0))
;;; Generated autoloads from compilation-recenter-end.el

(autoload 'compilation-recenter-end-enable "compilation-recenter-end" "\
Enable recentring of compilation windows at finish.
This function adds `compilation-recenter-end-at-finish' to
`compilation-finish-functions' (for Emacs 21 and up) or sets it
into `compilation-finish-function' (otherwise).  This is a global
change, affecting all compilation-mode buffers.

If you want multiple finish functions and only have an old Emacs
with the single `compilation-finish-function', you might try your
own defvar of `compilation-finish-functions' and set the single
function to call those.  `compilation-recenter-end-enable' here
will notice any `compilation-finish-functions' and use that.

\(fn)" nil nil)
 (custom-add-option 'compilation-mode-hook 'compilation-recenter-end-enable)

;;;***

;;;### (autoloads nil "crosshairs" "crosshairs.el" (20741 14138 0
;;;;;;  0))
;;; Generated autoloads from crosshairs.el

(let ((loads (get 'crosshairs 'custom-loads))) (if (member '"crosshairs" loads) nil (put 'crosshairs 'custom-loads (cons '"crosshairs" loads))))

(defvar crosshairs-mode nil "\
Non-nil if Crosshairs mode is enabled.
See the command `crosshairs-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `crosshairs-mode'.")

(custom-autoload 'crosshairs-mode "crosshairs" nil)

(autoload 'crosshairs-mode "crosshairs" "\
Toggle highlighting the current line and column.
With ARG, turn highlighting on if and only if ARG is positive.

\(fn &optional ARG)" t nil)

(defalias 'toggle-crosshairs-when-idle 'crosshairs-toggle-when-idle)

(autoload 'crosshairs-toggle-when-idle "crosshairs" "\
Toggle highlighting the current line and column when Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off.
You can use commands `col-highlight-set-interval' and
`hl-line-when-idle-interval' to change the idle times.

\(fn &optional ARG)" t nil)

(defalias 'flash-crosshairs 'crosshairs-flash)

(autoload 'crosshairs-flash "crosshairs" "\
Highlight the current line and column temporarily.
Highlight the line for `hl-line-flash-show-period' and the column for
`column-show-period' seconds.  With prefix argument SECONDS, highlight
both for SECONDS seconds.

\(fn &optional SECONDS)" t nil)

(autoload 'crosshairs "crosshairs" "\
Highlight current position with crosshairs.
With no prefix arg, highlighting turns off at the next command.
With a prefix arg, highlighting stays on until you toggle it off using
`crosshairs-mode'.

\(fn &optional MODALP)" t nil)

(autoload 'crosshairs-highlight "crosshairs" "\
Echo current position and highlight it with crosshairs.
If optional arg MODE is `line-only', then highlight only the line.
If optional arg MODE is `col-only', then highlight only the column.
 Interactively:
  A non-negative prefix argument uses MODE `line-only'.
  A negative prefix argument uses MODE `col-only'.

Optional arg NOMSG non-nil means show no message.

If the current buffer is not the same as the value of `orig-buff',
then indicate the buffer, as well as the position.  Variable
`orig-buff' is not bound here; if you want to take advantage of this
feature in your code, then bind it.

Return current position as a marker.

\(fn &optional MODE NOMSG)" t nil)

(autoload 'crosshairs-unhighlight "crosshairs" "\
Turn off crosshairs highlighting of current position.
Optional arg nil means do nothing if this event is a frame switch.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "dired-sort-menu" "dired-sort-menu.el" (20741
;;;;;;  14138 0 0))
;;; Generated autoloads from dired-sort-menu.el

(let ((loads (get 'dired-sort-menu 'custom-loads))) (if (member '"dired-sort-menu" loads) nil (put 'dired-sort-menu 'custom-loads (cons '"dired-sort-menu" loads))))

(autoload 'dired-sort-menu-popup "dired-sort-menu" "\
Pop up and run \"Sort By\" menu for dired mode *in EVENT window*.

\(fn EVENT)" t nil)

(autoload 'dired-sort-menu-toggle-ignore-case "dired-sort-menu" "\
Toggle ls-lisp switch `ls-lisp-ignore-case' and update buffer.

\(fn)" t nil)

(autoload 'dired-sort-menu-toggle-dirs-first "dired-sort-menu" "\
Toggle ls-lisp switch `ls-lisp-dirs-first' and update buffer.

\(fn)" t nil)

(autoload 'dired-sort-menu-toggle-reverse "dired-sort-menu" "\
Toggle ls -r switch and update buffer.
Does not affect other sort switches.

\(fn)" t nil)

(autoload 'dired-sort-menu-toggle-recursive "dired-sort-menu" "\
Toggle ls -R switch and update buffer.
Does not affect other sort switches.

\(fn)" t nil)

(autoload 'dired-sort-menu-swap-config "dired-sort-menu" "\
Swap saved and current `dired' sort configuration.

\(fn)" t nil)

(autoload 'dired-sort-dialogue "dired-sort-menu" "\
A static dialogue version of the Dired sort menu.
This command *must* be run in the Dired buffer!

\(fn)" t nil)

;;;***

;;;### (autoloads nil "doremi" "doremi.el" (20741 14138 0 0))
;;; Generated autoloads from doremi.el

(let ((loads (get 'doremi 'custom-loads))) (if (member '"doremi" loads) nil (put 'doremi 'custom-loads (cons '"doremi" loads))))

(defvar doremi-up-keys '(up) "\
*Keys (events) associated with one direction of adjusting by `doremi'.
The other direction is associated with `doremi-down-keys'.

The value must be a list of keyboard events: characters or symbols.
For example, a list element might be `?' or `prior'.")

(custom-autoload 'doremi-up-keys "doremi" t)

(defvar doremi-down-keys '(down) "\
*Keys (events) associated with one direction of adjusting by `doremi'.
The other direction is associated with `doremi-up-keys'.

The value must be a list of keyboard events: characters or symbols.
For example, a list element might be `?' or `next'.")

(custom-autoload 'doremi-down-keys "doremi" t)

(defvar doremi-boost-up-keys '(M-up) "\
*Like `doremi-up-keys', but increments by `doremi-boost-scale-factor'.

The value must be a list of keyboard events: characters or symbols.
For example, a list element might be `?\360' or `S-prior'.")

(custom-autoload 'doremi-boost-up-keys "doremi" t)

(defvar doremi-boost-down-keys '(M-down) "\
*Like `doremi-down-keys', but increments by `doremi-boost-scale-factor'.

The value must be a list of keyboard events: characters or symbols.
For example, a list element might be `?\356' or `S-next'.")

(custom-autoload 'doremi-boost-down-keys "doremi" t)

(defvar doremi-boost-scale-factor 10 "\
*Factor to boost incremental change of numerical properties.
Using `doremi-boost-up-keys' or `doremi-boost-down-keys', instead of
`doremi-up-keys' or `doremi-down-keys' means that the increment is
this many times larger.  Using a modifier key with the mouse wheel has
the same effect as using `doremi-boost-up-keys' or
`doremi-boost-down-keys'.")

(custom-autoload 'doremi-boost-scale-factor "doremi" t)

;;;***

;;;### (autoloads nil "doremi-cmd" "doremi-cmd.el" (20741 14138 0
;;;;;;  0))
;;; Generated autoloads from doremi-cmd.el

(let ((loads (get 'doremi-misc-commands 'custom-loads))) (if (member '"doremi-cmd" loads) nil (put 'doremi-misc-commands 'custom-loads (cons '"doremi-cmd" loads))))

(defvar doremi-color-themes nil "\
*List of color themes to cycle through using `doremi-color-themes+'.")

(custom-autoload 'doremi-color-themes "doremi-cmd" t)

(autoload 'doremi-color-themes+ "doremi-cmd" "\
Successively cycle among color themes.
The themes used for cycling are those in option `doremi-color-themes'.

You can use `C-g' to quit and cancel changes made so far.
Alternatively, after using `doremi-color-themes+' you can use
`color-theme-select' and choose pseudo-theme `[Reset]' - that does the
same thing.  Note that in either case, some things might not be
restored.

\(fn)" t nil)

(autoload 'doremi-bookmarks+ "doremi-cmd" "\
Successively cycle among all bookmarks.

\(fn)" t nil)

(autoload 'doremi-buffers+ "doremi-cmd" "\
Successively cycle among all existing buffers.
You can use `C-g' to quit and return to the original buffer.

\(fn)" t nil)

(autoload 'doremi-marks+ "doremi-cmd" "\
Successively cycle among all marks in the `mark-ring'.
You can use `C-g' to quit and return to the original position.
If library `crosshairs.el' is used, highlight each visited mark
position temporarily.

\(fn)" t nil)

(autoload 'doremi-global-marks+ "doremi-cmd" "\
Successively cycle among all marks in the `global-mark-ring'.
You can use `C-g' to quit and return to the original position.
If library `crosshairs.el' is used, highlight each visited mark
position temporarily.

\(fn)" t nil)

(autoload 'doremi-window-height+ "doremi-cmd" "\
Change height of WINDOW incrementally.
INCREMENT is the size increment.
WINDOW is selected.  WINDOW defaults to the selected window.

\(fn &optional INCREMENT WINDOW)" t nil)

(autoload 'doremi-window-width+ "doremi-cmd" "\
Change width of WINDOW incrementally.
INCREMENT is the size increment.
WINDOW is selected.  WINDOW defaults to the selected window.

\(fn &optional INCREMENT WINDOW)" t nil)

;;;***

;;;### (autoloads nil "doremi-mac" "doremi-mac.el" (20741 14138 0
;;;;;;  0))
;;; Generated autoloads from doremi-mac.el

(defvar define-doremi-before-hook nil "\
*Normal hook (list of functions) run before `doremi' is run.
See `run-hooks'.")

(custom-autoload 'define-doremi-before-hook "doremi-mac" t)

(defvar define-doremi-after-hook nil "\
*Normal hook (list of functions) run after `doremi' is run.
See `run-hooks'.")

(custom-autoload 'define-doremi-after-hook "doremi-mac" t)

(autoload 'define-doremi "doremi-mac" "\
Define a Do Re Mi command.
CMD-NAME is the name of the command, to be prefixed by `doremi-'.
DOC-STRING is the documentation string for the new command.
CMD-MENU-NAME is the menu name for the command (a string).

The other arguments are as for command `doremi', except that the
`doremi' increment argument is not an argument to
`define-doremi'. The new command has a single, optional argument,
INCREMENT, provided interactively by the prefix argument.

\(fn CMD-NAME DOC-STRING CMD-MENU-NAME SETTER-FN INIT-VAL &optional GROW-FN-P ENUM ALLOW-NEW-P)" nil t)

;;;***

;;;### (autoloads nil "dos" "dos.el" (20741 14140 0 0))
;;; Generated autoloads from dos.el

(autoload 'dos-mode "dos" "\
Major mode for editing Dos scripts.

The `dos-help-mode' command shows this page.

Start a new script from `dos-template' or `dos-template-mini'. Navigate between
sections using `dos-outline', `imenu', or `outline-minor-mode'. Use `dos-sep' to
save keystrokes. Read help for Dos command with `dos-help-cmd'. Run script using
`dos-run' and `dos-run-args'.

\\{dos-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "eproject" "eproject.el" (20741 14138 0 0))
;;; Generated autoloads from eproject.el
(require 'eproject)

;;;***

;;;### (autoloads nil "ert" "ert.el" (21050 7404 518163 760000))
;;; Generated autoloads from ert.el

(autoload 'ert-deftest "ert" "\
Define NAME (a symbol) as a test.

BODY is evaluated as a `progn' when the test is run.  It should
signal a condition on failure or just return if the test passes.

`should', `should-not' and `should-error' are useful for
assertions in BODY.

Use `ert' to run tests interactively.

Tests that are expected to fail can be marked as such
using :expected-result.  See `ert-test-result-type-p' for a
description of valid values for RESULT-TYPE.

\(fn NAME () [DOCSTRING] [:expected-result RESULT-TYPE] [:tags '(TAG...)] BODY...)" nil (quote macro))

(put 'ert-deftest 'lisp-indent-function 2)

(put 'ert-info 'lisp-indent-function 1)

(autoload 'ert-run-tests-batch "ert" "\
Run the tests specified by SELECTOR, printing results to the terminal.

SELECTOR works as described in `ert-select-tests', except if
SELECTOR is nil, in which case all tests rather than none will be
run; this makes the command line \"emacs -batch -l my-tests.el -f
ert-run-tests-batch-and-exit\" useful.

Returns the stats object.

\(fn &optional SELECTOR)" nil nil)

(autoload 'ert-run-tests-batch-and-exit "ert" "\
Like `ert-run-tests-batch', but exits Emacs when done.

The exit status will be 0 if all test results were as expected, 1
on unexpected results, or 2 if the tool detected an error outside
of the tests (e.g. invalid SELECTOR or bug in the code that runs
the tests).

\(fn &optional SELECTOR)" nil nil)

(autoload 'ert-run-tests-interactively "ert" "\
Run the tests specified by SELECTOR and display the results in a buffer.

SELECTOR works as described in `ert-select-tests'.
OUTPUT-BUFFER-NAME and MESSAGE-FN should normally be nil; they
are used for automated self-tests and specify which buffer to use
and how to display message.

\(fn SELECTOR &optional OUTPUT-BUFFER-NAME MESSAGE-FN)" t nil)

(defalias 'ert 'ert-run-tests-interactively)

(autoload 'ert-describe-test "ert" "\
Display the documentation for TEST-OR-TEST-NAME (a symbol or ert-test).

\(fn TEST-OR-TEST-NAME)" t nil)

;;;***

;;;### (autoloads nil "fill-column-indicator" "fill-column-indicator.el"
;;;;;;  (21143 32520 849501 109000))
;;; Generated autoloads from fill-column-indicator.el

(autoload 'fci-mode "fill-column-indicator" "\
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

(autoload 'turn-on-fci-mode "fill-column-indicator" "\
Turn on fci-mode unconditionally.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "fshell" "fshell.el" (20741 14138 0 0))
;;; Generated autoloads from fshell.el

(defvar fshell-default-make-new-shell nil "\
*If non-`nil', reverse the meaning of prefix arg to \\[fshell]")

(defvar fshell-make-shell-hook nil "\
*Forms to run in a new shell buffer just before the process is started.")

(defvar fshell-buffer-name "*shell*" "\
*Buffer name for shell process.")
 (add-hook 'same-window-regexps "^\\*shell\\*\\(\\|<[0-9]+>\\)")

(autoload 'fshell "fshell" "\
Run an inferior shell, with I/O through buffer *shell*.
The actual name of the buffer can be specified with the variable
`fshell-buffer-name', but for the sake of brevity the default will be used
in the examples below.

If buffer exists but shell process is not running, make new shell.

If buffer exists and shell process is running, just switch to buffer
 named \"*shell*\".
If an explicit numeric prefix argument is given (or this function is called
  from lisp with a numeric argument), switch to the buffer *shell*<prefix>,
  e.g. \"*shell*<2>\".  If there is no process in that buffer, start one.
If a prefix argument is given but it is not a number, create a new buffer
  and start a shell process in it.  This is the same as calling the function
  from lisp with an argument of `t'.

The previous paragraph describes the behavior of this function whenever it
is called from lisp.  If it is called interactively and the variable
`fshell-default-make-new-shell' is non-`nil', then the meaning of
non-numeric prefix arguments is reversed,
i.e. typing `\\[fshell]' without a prefix argument creates a new shell,
and `\\[universal-argument] \\[fshell]' would switch to the buffer \"*shell*\".

Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-sh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

Type \\[describe-mode] in the shell buffer for a list of commands.

\(fn &optional PREFIX)" t nil)

;;;***

;;;### (autoloads (gitattributes-mode) "gitattributes-mode" "gitattributes-mode.el"
;;;;;;  (22080 25480 169740 28000))
;;; Generated autoloads from gitattributes-mode.el

(autoload 'gitattributes-mode "gitattributes-mode" "\
A major mode for editing .gitattributes files.
\\{gitattributes-mode-map}

\(fn)" t nil)

(dolist (pattern '("/\\.gitattributes\\'" "/\\.git/info/attributes\\'" "/git/attributes\\'")) (add-to-list 'auto-mode-alist (cons pattern #'gitattributes-mode)))

;;;***

;;;### (autoloads (gitconfig-mode) "gitconfig-mode" "gitconfig-mode.el"
;;;;;;  (22080 25469 196393 913000))
;;; Generated autoloads from gitconfig-mode.el

(autoload 'gitconfig-mode "gitconfig-mode" "\
A major mode for editing .gitconfig files.

\(fn)" t nil)

(dolist (pattern '("/\\.gitconfig\\'" "/\\.git/config\\'" "/git/config\\'" "/\\.gitmodules\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gitconfig-mode)))

;;;***

;;;### (autoloads (gitignore-mode) "gitignore-mode" "gitignore-mode.el"
;;;;;;  (22080 25454 689326 200000))
;;; Generated autoloads from gitignore-mode.el

(autoload 'gitignore-mode "gitignore-mode" "\
A major mode for editing .gitignore files.

\(fn)" t nil)

(dolist (pattern (list "/\\.gitignore\\'" "/\\.git/info/exclude\\'" "/git/ignore\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gitignore-mode)))

;;;***

;;;### (autoloads nil "god-mode" "god-mode.el" (21136 61243 131006
;;;;;;  40000))
;;; Generated autoloads from god-mode.el

(autoload 'god-mode "god-mode" "\
Toggle global God mode.

\(fn)" t nil)

(autoload 'god-local-mode "god-mode" "\
Minor mode for running commands.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "hexrgb" "hexrgb.el" (20741 14138 0 0))
;;; Generated autoloads from hexrgb.el

(eval-and-compile (defun hexrgb-canonicalize-defined-colors (list) "Copy of LIST with color names canonicalized.\nLIST is a list of color names (strings).\nCanonical names are lowercase, with no whitespace.\nThere are no duplicate names." (let ((tail list) this new) (while tail (setq this (car tail) this (hexrgb-delete-whitespace-from-string (downcase this) 0 (length this))) (unless (member this new) (push this new)) (pop tail)) (nreverse new))) (defun hexrgb-delete-whitespace-from-string (string &optional from to) "Remove whitespace from substring of STRING from FROM to TO.\nIf FROM is nil, then start at the beginning of STRING (FROM = 0).\nIf TO is nil, then end at the end of STRING (TO = length of STRING).\nFROM and TO are zero-based indexes into STRING.\nCharacter FROM is affected (possibly deleted).  Character TO is not." (setq from (or from 0) to (or to (length string))) (with-temp-buffer (insert string) (goto-char (+ from (point-min))) (let ((count from) char) (while (and (not (eobp)) (< count to)) (setq char (char-after)) (if (memq char '(32 9 10)) (delete-char 1) (forward-char 1)) (setq count (1+ count))) (buffer-string)))))

(defconst hexrgb-defined-colors (eval-when-compile (and window-system (x-defined-colors))) "\
List of all supported colors.")

(defconst hexrgb-defined-colors-no-dups (eval-when-compile (and window-system (hexrgb-canonicalize-defined-colors (x-defined-colors)))) "\
List of all supported color names, with no duplicates.
Names are all lowercase, without any spaces.")

(defconst hexrgb-defined-colors-alist (eval-when-compile (and window-system (mapcar #'list (x-defined-colors)))) "\
Alist of all supported color names, for use in completion.
See also `hexrgb-defined-colors-no-dups-alist', which is the same
thing, but without any duplicates, such as \"light blue\" and
\"LightBlue\".")

(defconst hexrgb-defined-colors-no-dups-alist (eval-when-compile (and window-system (mapcar #'list (hexrgb-canonicalize-defined-colors (x-defined-colors))))) "\
Alist of all supported color names, with no duplicates, for completion.
Names are all lowercase, without any spaces.")

(defvar hexrgb-canonicalize-defined-colors-flag t "\
*Non-nil means remove duplicate color names.
Names are considered duplicates if they are the same when abstracting
from whitespace and letter case.")

(custom-autoload 'hexrgb-canonicalize-defined-colors-flag "hexrgb" t)

(autoload 'hexrgb-read-color "hexrgb" "\
Read a color name or hex RGB hexadecimal color value #RRRRGGGGBBBB.
Completion is available for color names, but not for RGB hex strings.
If you input an RGB hex string, it must have the form #XXXXXXXXXXXX or
XXXXXXXXXXXX, where each X is a hex digit.  The number of Xs must be a
multiple of 3, with the same number of Xs for each of red, green, and
blue.  The order is red, green, blue.

Color names that are normally considered equivalent are canonicalized:
They are lowercased, whitespace is removed, and duplicates are
eliminated.  E.g. \"LightBlue\" and \"light blue\" are both replaced
by \"lightblue\".  If you do not want this behavior, but want to
choose names that might contain whitespace or uppercase letters, then
customize option `hexrgb-canonicalize-defined-colors-flag' to nil.

In addition to standard color names and RGB hex values, the following
are available as color candidates.  In each case, the corresponding
color is used.

* `*copied foreground*'  - last copied foreground, if available
* `*copied background*'  - last copied background, if available
* `*mouse-2 foreground*' - foreground where you click `mouse-2'
* `*mouse-2 background*' - background where you click `mouse-2'
* `*point foreground*'   - foreground under the cursor
* `*point background*'   - background under the cursor

\(You can copy a color using eyedropper commands such as
`eyedrop-pick-foreground-at-mouse'.)

Optional arg PROMPT is the prompt - nil means use a default prompt.

Checks input to be sure it represents a valid color.  If not, raises
an error (but see exception for empty input with non-nil
ALLOW-EMPTY-NAME-P).

Interactively, or with optional arg CONVERT-TO-RGB-P non-nil, converts
an input color name to an RGB hex string.  Returns the RGB hex string.

Optional arg ALLOW-EMPTY-NAME-P controls what happens if you enter an
empty color name (that is, you just hit `RET').  If non-nil, then
`hexrgb-read-color' returns an empty color name, \"\".  If nil, then
it raises an error.  Calling programs must test for \"\" if
ALLOW-EMPTY-NAME-P is non-nil.  They can then perform an appropriate
action in case of empty input.

Interactively, or with non-nil MSGP, show color name in the echo area.

\(fn &optional PROMPT CONVERT-TO-RGB-P ALLOW-EMPTY-NAME-P MSGP)" t nil)

(autoload 'hexrgb-complement "hexrgb" "\
Return the color that is the complement of COLOR.
Non-interactively, non-nil optional arg MSG-P means show a message
with the complement.

\(fn COLOR &optional MSG-P)" t nil)

(autoload 'hexrgb-hue "hexrgb" "\
Return the hue component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\".

\(fn COLOR)" t nil)

(autoload 'hexrgb-saturation "hexrgb" "\
Return the saturation component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\".

\(fn COLOR)" t nil)

(autoload 'hexrgb-value "hexrgb" "\
Return the value component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\".

\(fn COLOR)" t nil)

(autoload 'hexrgb-red "hexrgb" "\
Return the red component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\".

\(fn COLOR)" t nil)

(autoload 'hexrgb-green "hexrgb" "\
Return the green component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\".

\(fn COLOR)" t nil)

(autoload 'hexrgb-blue "hexrgb" "\
Return the blue component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\".

\(fn COLOR)" t nil)

;;;***

;;;### (autoloads nil "highlight" "highlight.el" (20741 14138 0 0))
;;; Generated autoloads from highlight.el

(when (fboundp 'next-single-char-property-change) (defcustom hlt-act-on-any-face-flag nil "*Non-nil means highlight actions apply to all text with a face.\nNil means that they apply only to text that has been highlighted.\nConsult the doc for particular actions, to see if they are affected by\nthis option." :type 'boolean :group 'editing :group 'convenience :group 'wp :group 'faces))

(defvar hlt-max-region-no-warning 100000 "\
*Max size (chars) of region to highlight without confirmation.
This is used only for highlighting of a regexp, which can be slow.")

(custom-autoload 'hlt-max-region-no-warning "highlight" t)

(defvar hlt-use-overlays-flag 'only "\
*Non-nil means use overlays to highlight; nil means use text properties.
This value also affects some actions, such as unhighlighting, for text
that is highlighted.  If the value is `only' (the default value), then
those actions only affect overlay highlighting.  Otherwise, they
affect both kinds of highlighting.")

(custom-autoload 'hlt-use-overlays-flag "highlight" t)

(autoload 'hlt-choose-default-face "highlight" "\
Choose a face for highlighting.

\(fn FACE)" t nil)

(autoload 'hlt-highlighter "highlight" "\
Highlight the text you drag the mouse over.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose a different face.

\(fn START-EVENT)" t nil)

(autoload 'hlt-eraser "highlight" "\
Erase highlights that you click or drag the mouse over.
If `hlt-use-overlays-flag' is non-nil, then overlay highlighting is
removed for the last face that was used for highlighting.  (You can
use command `hlt-choose-default-face' first to choose a different
face.)  If `hlt-use-overlays-flag' is not `only', then text-property
highlighting is removed for *ALL* faces (not just highlighting faces).
This means, in particular, that a value of nil erases both overlays
for the last face and text properties for all faces.

Note: When text properties are affected, this is like using an eraser:
only characters you drag over lose their faces.  But when overlays are
affected, an overlay is erased as soon as any part of it is touched.
You need not drag over the entire overlay to delete it, and there is
no way to erase only part of it.

\(fn START-EVENT)" t nil)

(autoload 'hlt-highlight "highlight" "\
Highlight region, regexp (PREFIX +), or unhighlight region (PREFIX -).
PREFIX arg non-negative means `hlt-highlight-regexp-region'
PREFIX arg negative means `hlt-unhighlight-region'
PREFIX arg nil means `hlt-highlight-region'.
If the region is not active or it is empty, then use the whole buffer.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose a different face.

\(fn &optional PREFIX)" t nil)

(autoload 'hlt-highlight-region "highlight" "\
Highlight the region or new input.
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional 3rd arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional 4th arg MSG-P non-nil means to display a progress message.
Optional 5th arg MOUSE-P non-nil means use the `mouse-face' property,
 not the `face' property.
Interactively, MOUSE-P is provided by the prefix arg.

If the region is not active or it is empty, then:
 - If `hlt-use-overlays-flag' is non-nil, apply FACE to the
   entire buffer.  If MOUSE-P is non-nil, use the `mouse-face'
   property; otherwise, use the `face' property.
 - Else, if MOUSE-P is non-nil, then apply FACE as the `mouse-face'
   property to the whole buffer.
 - Else, if interactive, apply FACE to the next character you type,
   and add FACE to the facemenu menu.
 - Else, apply FACE as the `face' property to the whole buffer.

\(fn &optional START END FACE MSG-P MOUSE-P)" t nil)

(autoload 'hlt-highlight-regexp-region "highlight" "\
Highlight regular expression REGEXP in region.
If the region is not active or it is empty, then use the whole buffer.
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional 4th arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional 5th arg MSG-P:
  t means to treat this as an interactive call when deciding to
    display all messages.
  non-nil & non-t means to display only error and warning messages.
Optional 6th arg MOUSE-P non-nil means to use `mouse-face' property,
  not `face'.  Interactively, this is provided by the prefix arg.
Optional 7th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)

\(fn &optional START END REGEXP FACE MSG-P MOUSE-P NTH)" t nil)

(autoload 'hlt-highlight-regexp-to-end "highlight" "\
Highlight text after cursor that matches REGEXP.
Optional 2nd arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional 3rd arg MSG-P non-nil means display a progress message.
Optional 4th arg MOUSE-P non-nil means to use `mouse-face' property,
  not `face'.  Interactively, this is provided by the prefix arg.
Optional 5th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)

\(fn REGEXP &optional FACE MSG-P MOUSE-P NTH)" t nil)

(autoload 'hlt-unhighlight-region "highlight" "\
Remove all highlighting in region.
If the region is not active or it is empty, then use the whole buffer.
If `hlt-use-overlays-flag' is non-nil, then overlay highlighting is
removed.  If `hlt-use-overlays-flag' is not `only', then text-property
highlighting is removed.  This means, in particular, that a value of
nil removes both overlays and text properties.

Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional 3rd arg FACE non-nil means delete only highlighting that uses
  FACE.  Nil means delete all highlighting.
Optional 4th argument MSG-P non-nil means display a progress message.
Optional 5th arg MOUSE-P non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSE-P is provided by the prefix arg.

\(fn &optional START END FACE MSG-P MOUSE-P)" t nil)

(autoload 'hlt-unhighlight-region-for-face "highlight" "\
Remove highlighting that uses FACE in region.
Same as `hlt-unhighlight-region', but removes only highlighting
that uses FACE.  Interactively, you are prompted for the face.

This works only for overlay highlighting, not text-property
highlighting.

Optional arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional arg MOUSE-P non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSE-P is provided by the prefix arg.

\(fn &optional FACE START END MOUSE-P)" t nil)

(autoload 'hlt-replace-highlight-face "highlight" "\
Replace OLD-FACE by NEW-FACE in all highlights in the region.
If the region is not active or it is empty, then use the whole buffer.
With a prefix argument, replace OLD-FACE as the `mouse-face' property,
 not the `face' property.
Other arguments:
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional 5th argument MSG-P non-nil means display a progress message.
Optional 6th arg MOUSE-P non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSE-P is provided by the prefix arg.

This works only for overlay highlighting, not text-property
highlighting.

\(fn OLD-FACE NEW-FACE &optional START END MSG-P MOUSE-P)" t nil)

(autoload 'hlt-highlight-single-quotations "highlight" "\
Highlight single-quoted text in the region.
This means, for example, commands and keys between `'s: `foobar'.
If the region is not active or it is empty, then use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a different face.

\(fn &optional FACE)" t nil)

(autoload 'hlt-mouse-face-each-line "highlight" "\
Put `mouse-face' on each line of buffer in region.
If the region is active and not empty, then limit mouse-face
highlighting to the region.  Otherwise, use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a different face.
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional arg MSG-P non-nil means display a progress message.

\(fn &optional START END FACE MSG-P)" t nil)

(when (and (featurep 'icicles) (fboundp 'next-single-char-property-change)) (icicle-define-command hlt-choose-faces "Choose a list of face names.\nOption `hlt-act-on-any-face-flag' determines whether only highlighting\nfaces in the buffer are candidates.  The list of names (strings) is\nreturned." (lambda (name) (push name face-names)) "Choose face (`RET' when done): " (mapcar #'icicle-make-face-candidate (if hlt-act-on-any-face-flag (face-list) (hlt-highlight-faces-in-buffer (point-min) (point-max)))) nil t nil (cond ((boundp 'face-name-history) 'face-name-history) ((boundp 'icicle-face-name-history) 'icicle-face-name-history) (t 'face-name-history)) nil nil ((face-names nil)) nil nil (prog1 (setq face-names (delete "" face-names)) (when (interactive-p) (message "Faces: %S" face-names)))) (icicle-define-command hlt-choose-invisible-faces "Choose a list of face names from those currently invisible.\nOption `hlt-act-on-any-face-flag' determines whether only highlighting\nfaces in the buffer are candidates.  The list of names (strings) is\nreturned." (lambda (name) (push name face-names)) "Choose face (`RET' when done): " (mapcar #'icicle-make-face-candidate (icicle-delete-if-not (lambda (x) (memq x buffer-invisibility-spec)) (if hlt-act-on-any-face-flag (face-list) (hlt-highlight-faces-in-buffer (point-min) (point-max))))) nil t nil (cond ((boundp 'face-name-history) 'face-name-history) ((boundp 'icicle-face-name-history) 'icicle-face-name-history) (t 'face-name-history)) nil nil ((face-names nil)) nil nil (prog1 (setq face-names (delete "" face-names)) (when (interactive-p) (message "Faces: %S" face-names)))) (icicle-define-command hlt-choose-visible-faces "Choose a list of face names from those currently visible.\nOption `hlt-act-on-any-face-flag' determines whether only highlighting\nfaces in the buffer are candidates.  The list of names (strings) is\nreturned." (lambda (name) (push name face-names)) "Choose face (`RET' when done): " (mapcar #'icicle-make-face-candidate (icicle-delete-if (lambda (x) (memq x buffer-invisibility-spec)) (if hlt-act-on-any-face-flag (face-list) (hlt-highlight-faces-in-buffer (point-min) (point-max))))) nil t nil (cond ((boundp 'face-name-history) 'face-name-history) ((boundp 'icicle-face-name-history) 'icicle-face-name-history) (t 'face-name-history)) nil nil ((face-names nil)) nil nil (prog1 (setq face-names (delete "" face-names)) (when (interactive-p) (message "Faces: %S" face-names)))) (defun hlt-show-only (&optional start end faces) "Show only the faces you choose, hiding all others.\nNon-nil `hlt-act-on-any-face-flag' means choose from among all\nfaces.  Nil means choose only from among faces used to highlight.\n\nWhen choosing faces, completion and cycling are available. During\ncycling, these keys with prefix `C-' act on the current face name:\n\n`C-mouse-2', `C-RET' - Choose current face candidate only\n`C-down'  - Choose, then move to next prefix-completion candidate\n`C-up'    - Choose, then move to previous prefix-completion candidate\n`C-next'  - Choose, then move to next apropos-completion candidate\n`C-prior' - Choose, then move to previous apropos-completion candidate\n`C-!'     - Choose *all* matching face names" (interactive `(,@(hlt-region-or-buffer-limits) ,(mapcar #'intern (hlt-choose-faces)))) (dolist (face (if hlt-act-on-any-face-flag (face-list) (hlt-highlight-faces-in-buffer start end))) (if (memq face faces) (hlt-show-default-face face) (hlt-hide-default-face start end face)))) (defun hlt-hide-only (&optional start end faces) "hide only the faces you choose, showing all others.\nNon-nil `hlt-act-on-any-face-flag' means choose from among all\nfaces.  Nil means choose only from among faces used to highlight.\n\nWhen choosing faces, completion and cycling are available. During\ncycling, these keys with prefix `C-' act on the current face name:\n\n`C-mouse-2', `C-RET' - Choose current face candidate only\n`C-down'  - Choose, then move to next prefix-completion candidate\n`C-up'    - Choose, then move to previous prefix-completion candidate\n`C-next'  - Choose, then move to next apropos-completion candidate\n`C-prior' - Choose, then move to previous apropos-completion candidate\n`C-!'     - Choose *all* matching face names" (interactive `(,@(hlt-region-or-buffer-limits) ,(mapcar #'intern (hlt-choose-faces)))) (dolist (face (if hlt-act-on-any-face-flag (face-list) (hlt-highlight-faces-in-buffer start end))) (if (memq face faces) (hlt-hide-default-face start end face) (hlt-show-default-face face)))) (defun hlt-show (faces) "Show invisible faces that you choose.  Do nothing to other faces.\nNon-nil `hlt-act-on-any-face-flag' means choose from among all\ninvisible faces.  Nil means choose only from among invisible faces\nused to highlight.\n\nWhen choosing faces, completion and cycling are available. During\ncycling, these keys with prefix `C-' act on the current face name:\n\n`C-mouse-2', `C-RET' - Choose current face candidate only\n`C-down'  - Choose, then move to next prefix-completion candidate\n`C-up'    - Choose, then move to previous prefix-completion candidate\n`C-next'  - Choose, then move to next apropos-completion candidate\n`C-prior' - Choose, then move to previous apropos-completion candidate\n`C-!'     - Choose *all* matching face names" (interactive (list (let ((fs (icicle-delete-if-not (lambda (x) (memq x buffer-invisibility-spec)) (if hlt-act-on-any-face-flag (face-list) (hlt-highlight-faces-in-buffer (point-min) (point-max)))))) (if fs (mapcar #'intern (hlt-choose-invisible-faces)) (error "No%s faces are invisible" (if hlt-act-on-any-face-flag "" " highlight")))))) (dolist (face faces) (hlt-show-default-face face))) (defun hlt-hide (&optional start end faces) "Hide visible faces that you choose.  Do nothing to other faces.\nNon-nil `hlt-act-on-any-face-flag' means choose from among all\nvisible faces.  Nil means choose only from among visible faces used to\nhighlight.\n\nWhen choosing faces, completion and cycling are available. During\ncycling, these keys with prefix `C-' act on the current face name:\n\n`C-mouse-2', `C-RET' - Choose current face candidate only\n`C-down'  - Choose, then move to next prefix-completion candidate\n`C-up'    - Choose, then move to previous prefix-completion candidate\n`C-next'  - Choose, then move to next apropos-completion candidate\n`C-prior' - Choose, then move to previous apropos-completion candidate\n`C-!'     - Choose *all* matching face names" (interactive `(,@(hlt-region-or-buffer-limits) ,(mapcar #'intern (hlt-choose-faces)))) (dolist (face faces) (hlt-hide-default-face start end face))))

(when (fboundp 'next-single-char-property-change) (defun hlt-show-default-face (face) "Show FACE, by default, the default highlighting face.\nWith a prefix argument, prompt for the highlighting face to show.\nOtherwise, show the last face used for highlighting.\n You can also use command `hlt-choose-default-face' to choose a different face." (interactive (list (if current-prefix-arg (read-face-name "Show highlighting face: ") hlt-last-face))) (hlt-listify-invisibility-spec) (remove-from-invisibility-spec face)) (defun hlt-listify-invisibility-spec nil "Convert `buffer-invisibility-spec' to list form.\nIf it is already a list, do nothing.\nIf it is t, set it to a list of all `invisible' spec values in the buffer.\nThat is, for each character in the buffer that has property `invisible',\nthe invisibility criteria specified by that value are accumulated." (unless (listp buffer-invisibility-spec) (setq buffer-invisibility-spec nil) (let ((start (point-min)) (end (point-max)) spec) (dolist (ov (overlays-in start end)) (when (setq spec (overlay-get ov 'invisible)) (unless (listp spec) (setq spec (list spec))) (setq buffer-invisibility-spec (hlt-set-union spec buffer-invisibility-spec)))) (while (< start end) (when (setq spec (get-text-property start 'invisible)) (unless (listp spec) (setq spec (list spec))) (setq buffer-invisibility-spec (hlt-set-union spec buffer-invisibility-spec))) (setq start (1+ start))))) buffer-invisibility-spec) (defun hlt-set-union (list1 list2) "Combine LIST1 and LIST2 using a set-union operation.\nThe result list contains all items that appear in either LIST1 or\nLIST2.  This is a non-destructive function; it copies the data if\nnecessary." (cond ((null list1) list2) ((null list2) list1) ((equal list1 list2) list1) (t (or (>= (length list1) (length list2)) (setq list1 (prog1 list2 (setq list2 list1)))) (while list2 (unless (member (car list2) list1) (setq list1 (cons (car list2) list1))) (setq list2 (cdr list2))) list1))) (defun hlt-hide-default-face (&optional start end face) "Hide the last face used for highlighting.\nWith a prefix argument, prompt for the highlighting face to hide,\n instead.  You can also use command `hlt-choose-default-face' to\n choose a different face.\n\nIf `hlt-act-on-any-face-flag' is non-nil, then the face to be hidden\ncan be any face you choose.  Otherwise, it must be a face that has\nbeen used for highlighting.\n\nHiding a face at some location means two things:\n1) setting its `invisible' property there, making it susceptible to\n   being hidden by `buffer-invisibility-spec', and\n2) adding it to `buffer-invisibility-spec', so that it is hidden.\n\nThis command hides all text with the specified face that has the\n`invisible' property, throughout the entire buffer.  However, it only\nadds the `invisible' property to text with an overlay or text\nproperty, depending on `hlt-use-overlays-flag', and it only does so\nwithin the region, if the region is active.\n\nNon-interactively:\nFACE is the face to hide. It defaults to the last highlighting face.\nSTART and END are the limits of the area to act on. They default to\n  the region limits." (interactive `(,@(hlt-region-or-buffer-limits) ,(if current-prefix-arg (read-face-name "Hide highlighting face: ") hlt-last-face))) (unless (and start end) (let ((start-end (hlt-region-or-buffer-limits))) (setq start (car start-end) end (cadr start-end)))) (hlt-listify-invisibility-spec) (save-excursion (save-window-excursion (goto-char start) (let ((zone-beg start) zone-end zone) (while (and zone-beg (< zone-beg end)) (setq zone (hlt-next-highlight zone-beg end face nil nil 'no-error-msg) zone-beg (car zone) zone-end (cdr zone)) (when hlt-use-overlays-flag (let ((overlays (overlays-at zone-beg))) (while overlays (when (and (or hlt-act-on-any-face-flag (eq face (overlay-get (car overlays) 'hlt-highlight))) (eq face (overlay-get (car overlays) 'face))) (overlay-put (car overlays) 'invisible (hlt-add-listifying (overlay-get (car overlays) 'invisible) face))) (when overlays (setq overlays (cdr overlays)))))) (when (and (not (eq hlt-use-overlays-flag 'only)) (or hlt-act-on-any-face-flag (eq face (get-text-property (point) 'hlt-highlight))) (eq face (get-text-property (point) 'face))) (put-text-property zone-beg zone-end 'invisible (hlt-add-listifying (get-text-property zone-beg 'invisible) face))) (hlt-add-to-invisibility-spec face)))))) (defun hlt-add-to-invisibility-spec (element) "Add ELEMENT to `buffer-invisibility-spec'.\nSee documentation for `buffer-invisibility-spec' for the kind of elements\nthat can be added." (when (eq buffer-invisibility-spec t) (setq buffer-invisibility-spec (list t))) (add-to-list 'buffer-invisibility-spec element)) (defun hlt-add-listifying (orig-val val-to-add) "Add VAL-TO-ADD to list ORIG-VAL, listifying ORIG-VAL first if needed." (unless (listp orig-val) (setq orig-val (list orig-val))) (add-to-list 'orig-val val-to-add) orig-val) (defun hlt-next-highlight (&optional start end face mouse-p backward-p no-error-p) "Go to the next highlight in FACE.\nInteractively, FACE is the last face used for highlighting, but\nyou can use command `hlt-choose-default-face' to choose a different face.\n\nIf `hlt-act-on-any-face-flag' is non-nil, then the target face can be\nany face you choose.  Otherwise, it must be a face that has been used\nfor highlighting.\n\nIf `hlt-use-overlays-flag' is non-nil, then overlay highlighting is\ntargeted.  If `hlt-use-overlays-flag' is not `only', then\ntext-property highlighting is targeted.  This means, in particular,\nthat a value of nil targets both overlays and text properties.\n\nIf the region is active and not empty, then limit movement to the\nregion.  Otherwise, use the whole buffer.\nWhen called non-interactively:\n\n - non-nil argument NO-ERROR-P means do not raise an error if no\n   highlight with FACE is found, and leave point at END.\n\n - Return a cons of the limits of the text starting at point that has\n   property `hlt-highlight' of value FACE: (BEGIN-FACE . END-FACE), where\n   BEGIN-FACE is point and END-FACE is the first position just after\n   value FACE ends." (interactive `(,@(hlt-region-or-buffer-limits) nil ,current-prefix-arg)) (unless (and start end) (let ((start-end (hlt-region-or-buffer-limits))) (setq start (car start-end) end (cadr start-end)))) (if face (setq hlt-last-face face) (setq face hlt-last-face)) (when backward-p (setq end (prog1 start (setq start end)))) (let ((face-found nil) (orig-point (point)) (beg start)) (while (and (not (if backward-p (bobp) (eobp))) (not (eq face face-found)) (not (= beg end))) (save-restriction (narrow-to-region beg end) (setq beg (if backward-p (goto-char (previous-single-char-property-change (point) (if mouse-p 'mouse-face 'face) nil (point-min))) (goto-char (next-single-char-property-change (point) (if mouse-p 'mouse-face 'face) nil (point-max)))))) (when hlt-use-overlays-flag (let ((overlays (overlays-at (point)))) (while overlays (when (and (or hlt-act-on-any-face-flag (eq face (overlay-get (car overlays) 'hlt-highlight))) (eq face (overlay-get (car overlays) 'face))) (setq face-found face overlays nil)) (when overlays (setq overlays (cdr overlays)))))) (when (and (not face-found) (not (eq hlt-use-overlays-flag 'only)) (or hlt-act-on-any-face-flag (eq face (get-text-property (point) 'hlt-highlight))) (eq face (get-text-property (point) 'face))) (setq face-found face))) (unless (or (and (eq face face-found) (not (eq (point) orig-point))) no-error-p) (goto-char orig-point) (error "No %s highlight with face `%s'" (if backward-p "previous" "next") face))) (unless (interactive-p) (cons (point) (next-single-char-property-change (point) (if mouse-p 'mouse-face 'face) nil (if backward-p start end))))) (defun hlt-previous-highlight (&optional start end face mouse-p no-error-p) "Go to the previous highlight in the last face used for highlighting.\nThis is the same as `hlt-previous-highlight', except movement is backward." (interactive `(,@(hlt-region-or-buffer-limits) nil ,current-prefix-arg)) (unless (and start end) (let ((start-end (hlt-region-or-buffer-limits))) (setq start (car start-end) end (cadr start-end)))) (hlt-next-highlight start end face mouse-p t no-error-p)) (defun hlt-highlight-faces-in-buffer (start end) "List of highlighting faces in current buffer between START and END.\nThis includes faces used in overlays and as text properties.\nOnly highlighting faces are included, that is, faces associated with a\n`hlt-highlight' property." (save-excursion (save-window-excursion (let ((faces nil) (beg start) face) (setq end (min end (point-max))) (goto-char beg) (while (< beg end) (save-restriction (narrow-to-region beg end) (setq beg (goto-char (next-single-char-property-change (point) 'face nil (point-max))))) (when (setq face (get-text-property (point) 'hlt-highlight)) (add-to-list 'faces face)) (let ((overlays (overlays-at (point)))) (while overlays (when (and (overlay-get (car overlays) 'hlt-highlight) (setq face (overlay-get (car overlays) 'face))) (add-to-list 'faces face) (setq overlays nil)) (when overlays (setq overlays (cdr overlays)))))) faces)))) (defun hlt-toggle-act-on-any-face-flag nil "Toggle `hlt-act-on-any-face-flag'." (interactive) (setq hlt-act-on-any-face-flag (not hlt-act-on-any-face-flag)) (message (if hlt-act-on-any-face-flag "Highlight actions now apply to any face, not just a highlighting face" "Highlight actions now apply only to a highlighting face"))))

;;;***

;;;### (autoloads nil "highlight-parentheses" "highlight-parentheses.el"
;;;;;;  (20741 14138 0 0))
;;; Generated autoloads from highlight-parentheses.el

(autoload 'highlight-parentheses-mode "highlight-parentheses" "\
Minor mode to highlight the surrounding parentheses.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "highlight-symbol" "highlight-symbol.el" (20741
;;;;;;  14138 0 0))
;;; Generated autoloads from highlight-symbol.el

(autoload 'highlight-symbol-mode "highlight-symbol" "\
Minor mode that highlights the symbol under point throughout the buffer.
Highlighting takes place after `highlight-symbol-idle-delay'.

\(fn &optional ARG)" t nil)

(autoload 'highlight-symbol-at-point "highlight-symbol" "\
Toggle highlighting of the symbol at point.
This highlights or unhighlights the symbol at point using the first
element in of `highlight-symbol-faces'.

\(fn)" t nil)

(autoload 'highlight-symbol-remove-all "highlight-symbol" "\
Remove symbol highlighting in buffer.

\(fn)" t nil)

(autoload 'highlight-symbol-next "highlight-symbol" "\
Jump to the next location of the symbol at point within the function.

\(fn)" t nil)

(autoload 'highlight-symbol-prev "highlight-symbol" "\
Jump to the previous location of the symbol at point within the function.

\(fn)" t nil)

(autoload 'highlight-symbol-next-in-defun "highlight-symbol" "\
Jump to the next location of the symbol at point within the defun.

\(fn)" t nil)

(autoload 'highlight-symbol-prev-in-defun "highlight-symbol" "\
Jump to the previous location of the symbol at point within the defun.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "hl-line+" "hl-line+.el" (20741 14138 0 0))
;;; Generated autoloads from hl-line+.el

(defface hl-line '((t (:background "SlateGray3"))) "\
*Face to use for `hl-line-face'." :group (quote hl-line))

(defvar hl-line-flash-show-period 1 "\
*Number of seconds for `hl-line-flash' to highlight the line.")

(custom-autoload 'hl-line-flash-show-period "hl-line+" t)

(defvar hl-line-inhibit-highlighting-for-modes nil "\
*Modes where highlighting is inhibited for `hl-line-highlight-now'.
A list of `major-mode' values (symbols).")

(custom-autoload 'hl-line-inhibit-highlighting-for-modes "hl-line+" t)

(defvar hl-line-overlay-priority 300 "\
*Priority to use for `hl-line-overlay' and `global-hl-line-overlay'.
A higher priority can make the hl-line highlighting appear on top of
other overlays that might exist.")

(custom-autoload 'hl-line-overlay-priority "hl-line+" t)

(defalias 'toggle-hl-line-when-idle 'hl-line-toggle-when-idle)

(autoload 'hl-line-toggle-when-idle "hl-line+" "\
Turn on or off using `global-hl-line-mode' when Emacs is idle.
When on, use `global-hl-line-mode' whenever Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off.

\(fn &optional ARG)" t nil)

(autoload 'hl-line-when-idle-interval "hl-line+" "\
Set wait until using `global-hl-line-mode' when Emacs is idle.
Whenever Emacs is idle for this many seconds, `global-hl-line-mode'
will be turned on.

To turn on or off using `global-hl-line-mode' when idle,
use `\\[toggle-hl-line-when-idle].

\(fn SECS)" t nil)

(defalias 'flash-line-highlight 'hl-line-flash)

(autoload 'hl-line-flash "hl-line+" "\
Highlight the current line for `hl-line-flash-show-period' seconds.
With a prefix argument, highlight for that many seconds.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "icomplete+" "icomplete+.el" (20741 14138 0
;;;;;;  0))
;;; Generated autoloads from icomplete+.el

(let ((loads (get 'Icomplete-Plus 'custom-loads))) (if (member '"icomplete+" loads) nil (put 'Icomplete-Plus 'custom-loads (cons '"icomplete+" loads))))

(defface icompletep-choices '((((background dark)) (:foreground "Snow4")) (t (:foreground "DarkBlue"))) "\
*Face for minibuffer reminder of possible completion suffixes." :group (quote Icomplete-Plus))

(defface icompletep-determined '((t (:foreground "SeaGreen"))) "\
*Face for minibuffer reminder of possible completion prefix." :group (quote Icomplete-Plus))

(defface icompletep-nb-candidates '((((background dark)) (:foreground "SpringGreen")) (t (:foreground "DarkMagenta"))) "\
*Face for minibuffer reminder of number of completion candidates.
This has no effect unless library `icicles.el' is being used." :group (quote Icomplete-Plus))

(defface icompletep-keys '((t (:foreground "Red"))) "\
*Face for minibuffer reminder of possible completion key bindings." :group (quote Icomplete-Plus))

;;;***

;;;### (autoloads nil "igrep" "igrep.el" (20741 14138 0 0))
;;; Generated autoloads from igrep.el

(autoload 'igrep-insinuate "igrep" "\
Define `grep' aliases for the corresponding `igrep' commands.
With a prefix arg, OVERRIDE the current `grep' command definitions.

\(fn &optional OVERRIDE)" t nil)

(autoload 'igrep "igrep" "\
*Run `grep` PROGRAM to match REGEX in FILES.
The output is displayed in the *igrep* buffer, which `\\[next-error]' and
`\\[compile-goto-error]' parse to find each line of matched text.

PROGRAM may be nil, in which case it defaults to `igrep-program'.

REGEX is automatically quoted by `shell-quote-argument'.

FILES is either a file name pattern (automatically quoted by
`shell-quote-wildcard-pattern', then expanded by the `shell-file-name' shell),
or a list of file name patterns.

Optional OPTIONS is also passed to PROGRAM; it defaults to `igrep-options'.

If a prefix argument (`\\[universal-argument]') is given when called interactively,
or if `igrep-read-options' is set, OPTIONS is read from the minibuffer.

If two prefix arguments (`\\[universal-argument] \\[universal-argument]') are given when called interactively,
or if `igrep-read-multiple-files' is set, FILES is read from the minibuffer
multiple times.

If three prefix arguments (`\\[universal-argument] \\[universal-argument] \\[universal-argument]') are given when called interactively,
or if `igrep-read-options' and `igrep-read-multiple-files' are set,
OPTIONS is read and FILES is read multiple times.

If `igrep-find' is non-nil, the directory or directories
containing FILES is recursively searched for files whose name matches
the file name component of FILES (and whose contents match REGEX).

\(fn PROGRAM REGEX FILES &optional OPTIONS)" t nil)

(autoload 'igrep-find "igrep" "\
*Run `grep` via `find`; see `igrep' and `igrep-find'.
All IGREP-ARGS (including prefix arguments, when called interactively)
are handled by `igrep'.

\(fn &rest IGREP-ARGS)" t nil)

(autoload 'igrep-visited-files "igrep" "\
*Run `grep` PROGRAM to match REGEX (with optional OPTIONS) on all visited files.
See `\\[igrep]'.

\(fn PROGRAM REGEX &optional OPTIONS)" t nil)

(autoload 'dired-do-igrep "igrep" "\
*Search the marked (or next prefix ARG) files.
See `\\[igrep]' for a description of PROGRAM, REGEX, and OPTIONS.

\(fn PROGRAM REGEX &optional OPTIONS ARG)" t nil)

(autoload 'dired-do-igrep-find "igrep" "\
*Run `grep` on the marked (or next prefix ARG) directories.
See `\\[igrep]' for a description of PROGRAM, REGEX, and OPTIONS.

\(fn PROGRAM REGEX &optional OPTIONS ARG)" t nil)

(autoload 'Buffer-menu-igrep "igrep" "\
*Run `grep` on the files visited in buffers marked with '>'.
See `\\[igrep]' for a description of PROGRAM, REGEX, and OPTIONS.

\(fn PROGRAM REGEX &optional OPTIONS)" t nil)

;;;***

;;;### (autoloads nil "iproject" "iproject.el" (20741 14138 0 0))
;;; Generated autoloads from iproject.el

(autoload 'iproject-new "iproject" "\
Create a iproject buffer named NAME with a `default-directory' set to ROOT-FOLDER.

\(fn NAME ROOT-FOLDER)" t nil)

(autoload 'iproject-key-binding "iproject" "\
Setup some global key-bindings.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "javascript" "javascript.el" (20741 14138 0
;;;;;;  0))
;;; Generated autoloads from javascript.el

(autoload 'javascript-mode "javascript" "\
Major mode for editing JavaScript source text.

Key bindings:

\\{javascript-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "kill-ring-search" "kill-ring-search.el" (20741
;;;;;;  14138 0 0))
;;; Generated autoloads from kill-ring-search.el

(autoload 'kill-ring-search "kill-ring-search" "\
Search the kill ring in the minibuffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "lacarte" "lacarte.el" (20741 14138 0 0))
;;; Generated autoloads from lacarte.el

(let ((loads (get 'lacarte 'custom-loads))) (if (member '"lacarte" loads) nil (put 'lacarte 'custom-loads (cons '"lacarte" loads))))

(defvar lacarte-convert-menu-item-function nil "\
*Function to call to convert a menu item.
Used by `lacarte-execute-menu-command'.  A typical use would be to
remove the `&' characters used in MS Windows menus to define keyboard
accelerators.  See `lacarte-remove-w32-keybd-accelerators'.")

(custom-autoload 'lacarte-convert-menu-item-function "lacarte" t)

(autoload 'lacarte-execute-command "lacarte" "\
Execute a menu-bar menu command or an ordinary command.
Type a menu item or a command name.  Completion is available.
With a prefix arg, only menu items are available.
Completion is not case-sensitive.  However, if you use Icicles, then
you can use `C-A' in the minibuffer to toggle case-sensitivity.

If you use Icicles, then you can also sort the completion candidates
in different ways, using `C-,'.  With Icicles, by default menu items
are sorted before non-menu commands, and menu items are highlighted
using face `icicle-special-candidate'.

\(fn &optional NO-COMMANDS-P)" t nil)

(autoload 'lacarte-execute-menu-command "lacarte" "\
Execute a menu-bar menu command.
Type a menu item.  Completion is available.
Completion is not case-sensitive.  However, if you use Icicles, then
you can use `C-A' in the minibuffer to toggle case-sensitivity.
If you use Icicles, then you can also sort the completion candidates
in different ways, using `C-,'.

\(fn)" t nil)

;;;***

;;;### (autoloads (mediawiki-mode mediawiki-draft-buffer mediawiki-draft-page
;;;;;;  mediawiki-draft) "mediawiki" "mediawiki.el" (22082 5496 891638
;;;;;;  619000))
;;; Generated autoloads from mediawiki.el

(autoload 'mediawiki-draft "mediawiki" "\
Open a temporary buffer in wikipedia mode for editing an wikipedia
 draft, which an arbitrary piece of data. After finishing the editing
 either use C-c C-k \\[mediawiki-draft-buffer] to send the data into
 the mediawiki-draft-data-file, or send  the buffer using C-x C-s
\\[mediawiki-save]  and insert it later into a wikipedia article.

\(fn)" t nil)

(autoload 'mediawiki-draft-page "mediawiki" "\


\(fn)" t nil)

(autoload 'mediawiki-draft-buffer "mediawiki" "\
Mediawiki-draft-buffer sends the contents of the current (temporary)
buffer to the mediawiki-draft-buffer, see the variable
mediawiki-draft-data-file.

\(fn)" t nil)

(autoload 'mediawiki-mode "mediawiki" "\
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

;;;### (autoloads nil "misc/magit/lisp/git-commit" "../misc/magit/lisp/git-commit.el"
;;;;;;  (22081 4590 776517 377000))
;;; Generated autoloads from ../misc/magit/lisp/git-commit.el

(defvar global-git-commit-mode t "\
Non-nil if Global-Git-Commit mode is enabled.
See the command `global-git-commit-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-commit-mode'.")

(custom-autoload 'global-git-commit-mode "misc/magit/lisp/git-commit" nil)

(autoload 'global-git-commit-mode "misc/magit/lisp/git-commit" "\
Edit Git commit messages.
This global mode arranges for `git-commit-setup' to be called
when a Git commit message file is opened.  That usually happens
when Git uses the Emacsclient as $GIT_EDITOR to have the user
provide such a commit message.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "misc/magit/lisp/git-rebase" "../misc/magit/lisp/git-rebase.el"
;;;;;;  (22218 60319 650835 250000))
;;; Generated autoloads from ../misc/magit/lisp/git-rebase.el

(autoload 'git-rebase-mode "misc/magit/lisp/git-rebase" "\
Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

\(fn)" t nil)

(defconst git-rebase-filename-regexp "/git-rebase-todo\\'")

(add-to-list 'auto-mode-alist (cons git-rebase-filename-regexp 'git-irb-mode))

;;;***

;;;### (autoloads nil "misc/magit/lisp/magit" "../misc/magit/lisp/magit.el"
;;;;;;  (22081 4590 780516 585000))
;;; Generated autoloads from ../misc/magit/lisp/magit.el

(autoload 'magit-status "misc/magit/lisp/magit" "\
Show the status of the current Git repository in a buffer.
With a prefix argument prompt for a repository to be shown.
With two prefix arguments prompt for an arbitrary directory.
If that directory isn't the root of an existing repository
then offer to initialize it as a new repository.

\(fn &optional DIRECTORY)" t nil)

(autoload 'magit-status-internal "misc/magit/lisp/magit" "\


\(fn DIRECTORY)" nil nil)
 (autoload 'magit-show-refs-popup "magit" nil t)

(autoload 'magit-show-refs-head "misc/magit/lisp/magit" "\
List and compare references in a dedicated buffer.
Refs are compared with `HEAD'.

\(fn &optional ARGS)" t nil)

(autoload 'magit-show-refs-current "misc/magit/lisp/magit" "\
List and compare references in a dedicated buffer.
Refs are compared with the current branch or `HEAD' if
it is detached.

\(fn &optional ARGS)" t nil)

(autoload 'magit-show-refs "misc/magit/lisp/magit" "\
List and compare references in a dedicated buffer.
Refs are compared with a branch read form the user.

\(fn &optional REF ARGS)" t nil)

(autoload 'magit-find-file "misc/magit/lisp/magit" "\
View FILE from REV.
Switch to a buffer visiting blob REV:FILE,
creating one if none already exists.

\(fn REV FILE)" t nil)

(autoload 'magit-find-file-other-window "misc/magit/lisp/magit" "\
View FILE from REV, in another window.
Like `magit-find-file', but create a new window or reuse an
existing one.

\(fn REV FILE)" t nil)

(autoload 'magit-dired-jump "misc/magit/lisp/magit" "\
Visit file at point using Dired.
With a prefix argument, visit in other window.  If there
is no file at point then instead visit `default-directory'.

\(fn &optional OTHER-WINDOW)" t nil)

(autoload 'magit-checkout-file "misc/magit/lisp/magit" "\
Checkout FILE from REV.

\(fn REV FILE)" t nil)

(autoload 'magit-init "misc/magit/lisp/magit" "\
Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally.

\(fn DIRECTORY)" t nil)
 (autoload 'magit-branch-popup "magit" nil t)

(autoload 'magit-checkout "misc/magit/lisp/magit" "\
Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch then that becomes the current
branch.  If it is something else then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.

\(git checkout REVISION).

\(fn REVISION)" t nil)

(autoload 'magit-branch-and-checkout "misc/magit/lisp/magit" "\
Create and checkout BRANCH at branch or revision START-POINT.

\(git checkout [ARGS] -b BRANCH START-POINT).

\(fn BRANCH START-POINT &optional ARGS)" t nil)

(autoload 'magit-branch-spinoff "misc/magit/lisp/magit" "\
Create new branch from the unpushed commits.

Create and checkout a new branch starting at and tracking the
current branch.  That branch in turn is reset to the last commit
it shares with its upstream.  If the current branch has no
upstream or no unpushed commits, then the new branch is created
anyway and the previously current branch is not touched.

This is useful to create a feature branch after work has already
began on the old branch (likely but not necessarily \"master\").

\(fn BRANCH &rest ARGS)" t nil)

(autoload 'magit-branch-reset "misc/magit/lisp/magit" "\
Reset a branch to the tip of another branch or any other commit.

When resetting to another branch, then also set that branch as
the upstream of the branch being reset.

When the branch being reset is the current branch, then do a
hard reset.  If there are any uncommitted changes, then the user
has to confirming the reset because those changes would be lost.

This is useful when you have started work on a feature branch but
realize it's all crap and want to start over.

\(fn BRANCH TO &optional ARGS)" t nil)

(autoload 'magit-branch-delete "misc/magit/lisp/magit" "\
Delete one or multiple branches.
If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point.

\(fn BRANCHES &optional FORCE)" t nil)

(autoload 'magit-branch-set-upstream "misc/magit/lisp/magit" "\
Change the UPSTREAM branch of BRANCH.

\(fn BRANCH UPSTREAM)" t nil)

(autoload 'magit-branch-unset-upstream "misc/magit/lisp/magit" "\
Unset the upstream branch of BRANCH.

\(fn BRANCH)" t nil)

(autoload 'magit-branch-rename "misc/magit/lisp/magit" "\
Rename branch OLD to NEW.
With prefix, forces the rename even if NEW already exists.

\(git branch -m|-M OLD NEW).

\(fn OLD NEW &optional FORCE)" t nil)

(autoload 'magit-branch-edit-description "misc/magit/lisp/magit" "\
Edit the description of BRANCH.

\(fn BRANCH)" t nil)
 (autoload 'magit-merge-popup "magit" nil t)

(autoload 'magit-merge "misc/magit/lisp/magit" "\
Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

\(git merge --no-edit|--no-commit [ARGS] REV)

\(fn REV &optional ARGS NOCOMMIT)" t nil)

(autoload 'magit-merge-editmsg "misc/magit/lisp/magit" "\
Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.

\(git merge --edit [ARGS] rev)

\(fn REV &optional ARGS)" t nil)

(autoload 'magit-merge-nocommit "misc/magit/lisp/magit" "\
Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.

\(git merge --no-commit [ARGS] rev)

\(fn REV &optional ARGS)" t nil)

(autoload 'magit-merge-preview "misc/magit/lisp/magit" "\
Preview result of merging REV into the current branch.

\(fn REV)" t nil)

(autoload 'magit-merge-abort "misc/magit/lisp/magit" "\
Abort the current merge operation.

\(git merge --abort)

\(fn)" t nil)

(autoload 'magit-reset-index "misc/magit/lisp/magit" "\
Reset the index to COMMIT.
Keep the head and working tree as-is, so if COMMIT refers to the
head this effectivley unstages all changes.

\(git reset COMMIT)

\(fn COMMIT)" t nil)

(autoload 'magit-reset "misc/magit/lisp/magit" "\
Reset the head and index to COMMIT, but not the working tree.
With a prefix argument also reset the working tree.

\(git reset --mixed|--hard COMMIT)

\(fn COMMIT &optional HARD)" t nil)

(autoload 'magit-reset-head "misc/magit/lisp/magit" "\
Reset the head and index to COMMIT, but not the working tree.

\(git reset --mixed COMMIT)

\(fn COMMIT)" t nil)

(autoload 'magit-reset-soft "misc/magit/lisp/magit" "\
Reset the head to COMMIT, but not the index and working tree.

\(git reset --soft REVISION)

\(fn COMMIT)" t nil)

(autoload 'magit-reset-hard "misc/magit/lisp/magit" "\
Reset the head, index, and working tree to COMMIT.

\(git reset --hard REVISION)

\(fn COMMIT)" t nil)
 (autoload 'magit-tag-popup "magit" nil t)

(autoload 'magit-tag "misc/magit/lisp/magit" "\
Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.

\(git tag [--annotate] NAME REV)

\(fn NAME REV &optional ARGS)" t nil)

(autoload 'magit-tag-delete "misc/magit/lisp/magit" "\
Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.

\(git tag -d TAGS)

\(fn TAGS)" t nil)
 (autoload 'magit-notes-popup "magit" nil t)
 (autoload 'magit-submodule-popup "magit" nil t)

(autoload 'magit-submodule-add "misc/magit/lisp/magit" "\
Add the repository at URL as a submodule.
Optional PATH is the path to the submodule relative to the root
of the superproject. If it is nil then the path is determined
based on URL.

\(fn URL &optional PATH)" t nil)

(autoload 'magit-submodule-setup "misc/magit/lisp/magit" "\
Clone and register missing submodules and checkout appropriate commits.

\(fn)" t nil)

(autoload 'magit-submodule-init "misc/magit/lisp/magit" "\
Register submodules listed in \".gitmodules\" into \".git/config\".

\(fn)" t nil)

(autoload 'magit-submodule-update "misc/magit/lisp/magit" "\
Clone missing submodules and checkout appropriate commits.
With a prefix argument also register submodules in \".git/config\".

\(fn &optional INIT)" t nil)

(autoload 'magit-submodule-sync "misc/magit/lisp/magit" "\
Update each submodule's remote URL according to \".gitmodules\".

\(fn)" t nil)

(autoload 'magit-submodule-fetch "misc/magit/lisp/magit" "\
Fetch all submodules.
With a prefix argument fetch all remotes.

\(fn &optional ALL)" t nil)

(autoload 'magit-submodule-deinit "misc/magit/lisp/magit" "\
Unregister the submodule at PATH.

\(fn PATH)" t nil)

(defvar global-magit-file-mode nil "\
Non-nil if Global-Magit-File mode is enabled.
See the command `global-magit-file-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-magit-file-mode'.")

(custom-autoload 'global-magit-file-mode "misc/magit/lisp/magit" nil)

(autoload 'global-magit-file-mode "misc/magit/lisp/magit" "\
Toggle Magit-File mode in all buffers.
With prefix ARG, enable Global-Magit-File mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-File mode is enabled in all buffers where
`magit-file-mode-turn-on' would do it.
See `magit-file-mode' for more information on Magit-File mode.

\(fn &optional ARG)" t nil)
 (autoload 'magit-dispatch-popup "magit" nil t)
 (autoload 'magit-run-popup "magit" nil t)

(autoload 'magit-git-command "misc/magit/lisp/magit" "\
Execute a Git subcommand asynchronously, displaying the output.
With a prefix argument run Git in the root of the current
repository, otherwise in `default-directory'.

\(fn ARGS DIRECTORY)" t nil)

(autoload 'magit-git-command-topdir "misc/magit/lisp/magit" "\
Execute a Git subcommand asynchronously, displaying the output.
Run Git in the top-level directory of the current repository.

\(fn)" t nil)

(autoload 'magit-shell-command "misc/magit/lisp/magit" "\
Execute a shell command asynchronously, displaying the output.
With a prefix argument run the command in the root of the current
repository, otherwise in `default-directory'.

\(fn ARGS DIRECTORY)" t nil)

(autoload 'magit-shell-command-topdir "misc/magit/lisp/magit" "\
Execute a shell command asynchronously, displaying the output.
Run the command in the top-level directory of the current repository.

\(fn)" t nil)

(autoload 'magit-version "misc/magit/lisp/magit" "\
Return the version of Magit currently in use.
When called interactive also show the used versions of Magit,
Git, and Emacs in the echo area.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "misc/magit/lisp/magit-apply" "../misc/magit/lisp/magit-apply.el"
;;;;;;  (22081 4590 776517 377000))
;;; Generated autoloads from ../misc/magit/lisp/magit-apply.el

(autoload 'magit-stage-file "misc/magit/lisp/magit-apply" "\
Stage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation.

\(fn FILE)" t nil)

(autoload 'magit-stage-modified "misc/magit/lisp/magit-apply" "\
Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.
\('git add --update|--all .').

\(fn &optional ALL)" t nil)

(autoload 'magit-unstage-file "misc/magit/lisp/magit-apply" "\
Unstage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be unstaged.  Otherwise unstage the file at point
without requiring confirmation.

\(fn FILE)" t nil)

(autoload 'magit-unstage-all "misc/magit/lisp/magit-apply" "\
Remove all changes from the staging area.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "misc/magit/lisp/magit-bisect" "../misc/magit/lisp/magit-bisect.el"
;;;;;;  (22081 4590 776517 377000))
;;; Generated autoloads from ../misc/magit/lisp/magit-bisect.el
 (autoload 'magit-bisect-popup "magit-bisect" nil t)

(autoload 'magit-bisect-start "misc/magit/lisp/magit-bisect" "\
Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a know
good and a bad commit.  To move the session forward use the
other actions from the bisect popup (\\<magit-status-mode-map>\\[magit-bisect-popup]).

\(fn BAD GOOD)" t nil)

(autoload 'magit-bisect-reset "misc/magit/lisp/magit-bisect" "\
After bisecting, cleanup bisection state and return to original `HEAD'.

\(fn)" t nil)

(autoload 'magit-bisect-good "misc/magit/lisp/magit-bisect" "\
While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question.

\(fn)" t nil)

(autoload 'magit-bisect-bad "misc/magit/lisp/magit-bisect" "\
While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question.

\(fn)" t nil)

(autoload 'magit-bisect-skip "misc/magit/lisp/magit-bisect" "\
While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one.

\(fn)" t nil)

(autoload 'magit-bisect-run "misc/magit/lisp/magit-bisect" "\
Bisect automatically by running commands after each step.

Unlike `git bisect run' this can be used before bisecting has
begun.  In that case it behaves like `git bisect start; git
bisect run'.

\(fn CMDLINE &optional BAD GOOD)" t nil)

;;;***

;;;### (autoloads nil "misc/magit/lisp/magit-blame" "../misc/magit/lisp/magit-blame.el"
;;;;;;  (22081 4590 776517 377000))
;;; Generated autoloads from ../misc/magit/lisp/magit-blame.el
 (autoload 'magit-blame-popup "magit-blame" nil t)

(autoload 'magit-blame "misc/magit/lisp/magit-blame" "\
Display edit history of FILE up to REVISION.

Interactively blame the file being visited in the current buffer.
If the buffer visits a revision of that file, then blame up to
that revision, otherwise blame the file's full history, including
uncommitted changes.

If Magit-Blame mode is already turned on then blame recursively, by
visiting REVISION:FILE (using `magit-find-file'), where revision
is the revision before the revision that added the lines at
point.

ARGS is a list of additional arguments to pass to `git blame';
only arguments available from `magit-blame-popup' should be used.

\(fn REVISION FILE &optional ARGS)" t nil)

;;;***

;;;### (autoloads nil "misc/magit/lisp/magit-commit" "../misc/magit/lisp/magit-commit.el"
;;;;;;  (22081 4590 776517 377000))
;;; Generated autoloads from ../misc/magit/lisp/magit-commit.el

(autoload 'magit-commit "misc/magit/lisp/magit-commit" "\
Create a new commit on HEAD.
With a prefix argument amend to the commit at HEAD instead.

\(git commit [--amend] ARGS)

\(fn &optional ARGS)" t nil)

(autoload 'magit-commit-amend "misc/magit/lisp/magit-commit" "\
Amend the last commit.

\(git commit --amend ARGS)

\(fn &optional ARGS)" t nil)

(autoload 'magit-commit-extend "misc/magit/lisp/magit-commit" "\
Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.  
\(git commit
--amend --no-edit)

\(fn &optional ARGS OVERRIDE-DATE)" t nil)

(autoload 'magit-commit-reword "misc/magit/lisp/magit-commit" "\
Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.

\(git commit --amend --only)

\(fn &optional ARGS OVERRIDE-DATE)" t nil)

(autoload 'magit-commit-fixup "misc/magit/lisp/magit-commit" "\
Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-squash "misc/magit/lisp/magit-commit" "\
Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-augment "misc/magit/lisp/magit-commit" "\
Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-instant-fixup "misc/magit/lisp/magit-commit" "\
Create a fixup commit targeting COMMIT and instantly rebase.

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-instant-squash "misc/magit/lisp/magit-commit" "\
Create a squash commit targeting COMMIT and instantly rebase.

\(fn &optional COMMIT ARGS)" t nil)

;;;***

;;;### (autoloads nil "misc/magit/lisp/magit-diff" "../misc/magit/lisp/magit-diff.el"
;;;;;;  (22081 4590 776517 377000))
;;; Generated autoloads from ../misc/magit/lisp/magit-diff.el

(autoload 'magit-diff-dwim "misc/magit/lisp/magit-diff" "\
Show changes for the thing at point.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff "misc/magit/lisp/magit-diff" "\
Show differences between two commits.

REV-OR-RANGE should be a RANGE or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range).

\(fn REV-OR-RANGE &optional ARGS FILES)" t nil)

(autoload 'magit-diff-working-tree "misc/magit/lisp/magit-diff" "\
Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer.

\(fn &optional REV ARGS FILES)" t nil)

(autoload 'magit-diff-staged "misc/magit/lisp/magit-diff" "\
Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer.

\(fn &optional REV ARGS FILES)" t nil)

(autoload 'magit-diff-unstaged "misc/magit/lisp/magit-diff" "\
Show changes between the working tree and the index.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff-unpushed "misc/magit/lisp/magit-diff" "\
Show unpushed changes.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff-unpulled "misc/magit/lisp/magit-diff" "\
Show unpulled changes.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff-while-committing "misc/magit/lisp/magit-diff" "\
While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be commited.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff-paths "misc/magit/lisp/magit-diff" "\
Show changes between any two files on disk.

\(fn A B)" t nil)

(autoload 'magit-show-commit "misc/magit/lisp/magit-diff" "\
Show the revision at point.
If there is no revision at point or with a prefix argument prompt
for a revision.

\(fn REV &optional ARGS FILES MODULE)" t nil)

;;;***

;;;### (autoloads nil "misc/magit/lisp/magit-ediff" "../misc/magit/lisp/magit-ediff.el"
;;;;;;  (22081 4590 776517 377000))
;;; Generated autoloads from ../misc/magit/lisp/magit-ediff.el
 (autoload 'magit-ediff-popup "magit-ediff" nil t)

(autoload 'magit-ediff-resolve "misc/magit/lisp/magit-ediff" "\
Resolve outstanding conflicts in FILE using Ediff.
FILE has to be relative to the top directory of the repository.

In the rare event that you want to manually resolve all
conflicts, including those already resolved by Git, use
`ediff-merge-revisions-with-ancestor'.

\(fn FILE)" t nil)

(autoload 'magit-ediff-stage "misc/magit/lisp/magit-ediff" "\
Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'magit-ediff-compare "misc/magit/lisp/magit-ediff" "\
Compare REVA:FILEA with REVB:FILEB using Ediff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range).

\(fn REVA REVB FILEA FILEB)" t nil)

(autoload 'magit-ediff-dwim "misc/magit/lisp/magit-ediff" "\
Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `magit-ediff-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run.

\(fn)" t nil)

(autoload 'magit-ediff-show-staged "misc/magit/lisp/magit-ediff" "\
Show staged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'magit-ediff-show-unstaged "misc/magit/lisp/magit-ediff" "\
Show unstaged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'magit-ediff-show-working-tree "misc/magit/lisp/magit-ediff" "\
Show changes between HEAD and working tree using Ediff.
FILE must be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'magit-ediff-show-commit "misc/magit/lisp/magit-ediff" "\
Show changes introduced by COMMIT using Ediff.

\(fn COMMIT)" t nil)

;;;***

;;;### (autoloads nil "misc/magit/lisp/magit-extras" "../misc/magit/lisp/magit-extras.el"
;;;;;;  (22081 4590 776517 377000))
;;; Generated autoloads from ../misc/magit/lisp/magit-extras.el

(autoload 'magit-run-git-gui "misc/magit/lisp/magit-extras" "\
Run `git gui' for the current git repository.

\(fn)" t nil)

(autoload 'magit-run-git-gui-blame "misc/magit/lisp/magit-extras" "\
Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the HEAD, with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on.

\(fn COMMIT FILENAME &optional LINENUM)" t nil)

(autoload 'magit-run-gitk "misc/magit/lisp/magit-extras" "\
Run `gitk' in the current repository.

\(fn)" t nil)

(autoload 'magit-run-gitk-branches "misc/magit/lisp/magit-extras" "\
Run `gitk --branches' in the current repository.

\(fn)" t nil)

(autoload 'magit-run-gitk-all "misc/magit/lisp/magit-extras" "\
Run `gitk --all' in the current repository.

\(fn)" t nil)

(autoload 'magit-clean "misc/magit/lisp/magit-extras" "\
Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.

\(git clean -f -d [-x|-X])

\(fn &optional ARG)" t nil)

(autoload 'magit-gitignore "misc/magit/lisp/magit-extras" "\
Instruct Git to ignore FILE-OR-PATTERN.
With a prefix argument only ignore locally.

\(fn FILE-OR-PATTERN &optional LOCAL)" t nil)

(autoload 'magit-gitignore-locally "misc/magit/lisp/magit-extras" "\
Instruct Git to locally ignore FILE-OR-PATTERN.

\(fn FILE-OR-PATTERN)" t nil)

(autoload 'magit-add-change-log-entry "misc/magit/lisp/magit-extras" "\
Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer.

\(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t nil)

(autoload 'magit-add-change-log-entry-other-window "misc/magit/lisp/magit-extras" "\
Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Magit buffer instead of
on a position in a file-visiting buffer.

\(fn &optional WHOAMI FILE-NAME)" t nil)

;;;***

;;;### (autoloads nil "misc/magit/lisp/magit-log" "../misc/magit/lisp/magit-log.el"
;;;;;;  (22081 4590 780516 585000))
;;; Generated autoloads from ../misc/magit/lisp/magit-log.el

(autoload 'magit-log-current "misc/magit/lisp/magit-log" "\
Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer.

\(fn REVS &optional ARGS FILES)" t nil)

(autoload 'magit-log "misc/magit/lisp/magit-log" "\
Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates.

\(fn REVS &optional ARGS FILES)" t nil)

(autoload 'magit-log-head "misc/magit/lisp/magit-log" "\
Show log for `HEAD'.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-branches "misc/magit/lisp/magit-log" "\
Show log for all local branches and `HEAD'.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-all-branches "misc/magit/lisp/magit-log" "\
Show log for all local and remote branches and `HEAD'.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-all "misc/magit/lisp/magit-log" "\
Show log for all references and `HEAD'.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-buffer-file "misc/magit/lisp/magit-log" "\
Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is part of
`magit-log-arguments', then follow renames.

\(fn &optional FOLLOW BEG END)" t nil)

(autoload 'magit-reflog-current "misc/magit/lisp/magit-log" "\
Display the reflog of the current branch.

\(fn)" t nil)

(autoload 'magit-reflog "misc/magit/lisp/magit-log" "\
Display the reflog of a branch.

\(fn REF)" t nil)

(autoload 'magit-reflog-head "misc/magit/lisp/magit-log" "\
Display the `HEAD' reflog.

\(fn)" t nil)

(autoload 'magit-cherry "misc/magit/lisp/magit-log" "\
Show commits in a branch that are not merged in the upstream branch.

\(fn HEAD UPSTREAM)" t nil)

;;;***

;;;### (autoloads nil "misc/magit/lisp/magit-remote" "../misc/magit/lisp/magit-remote.el"
;;;;;;  (22081 4590 780516 585000))
;;; Generated autoloads from ../misc/magit/lisp/magit-remote.el

(autoload 'magit-clone "misc/magit/lisp/magit-remote" "\
Clone the REPOSITORY to DIRECTORY.
Then show the status buffer for the new repository.

\(fn REPOSITORY DIRECTORY)" t nil)
 (autoload 'magit-remote-popup "magit-remote" nil t)

(autoload 'magit-remote-add "misc/magit/lisp/magit-remote" "\
Add a remote named REMOTE and fetch it.

\(fn REMOTE URL)" t nil)

(autoload 'magit-remote-rename "misc/magit/lisp/magit-remote" "\
Rename the remote named OLD to NEW.

\(fn OLD NEW)" t nil)

(autoload 'magit-remote-set-url "misc/magit/lisp/magit-remote" "\
Change the url of the remote named REMOTE to URL.

\(fn REMOTE URL)" t nil)

(autoload 'magit-remote-remove "misc/magit/lisp/magit-remote" "\
Delete the remote named REMOTE.

\(fn REMOTE)" t nil)
 (autoload 'magit-fetch-popup "magit-remote" nil t)

(autoload 'magit-fetch-current "misc/magit/lisp/magit-remote" "\
Fetch from the upstream repository of the current branch.
If `HEAD' is detached or if the upstream is not configured,
then read the remote.

\(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-fetch "misc/magit/lisp/magit-remote" "\
Fetch from another repository.

\(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-fetch-all "misc/magit/lisp/magit-remote" "\
Fetch from all configured remotes.

\(fn &optional ARGS)" t nil)
 (autoload 'magit-pull-popup "magit-remote" nil t)

(autoload 'magit-pull-current "misc/magit/lisp/magit-remote" "\
Fetch and merge into current branch.

\(fn REMOTE BRANCH &optional ARGS)" t nil)

(autoload 'magit-pull "misc/magit/lisp/magit-remote" "\
Fetch from another repository and merge a fetched branch.

\(fn REMOTE BRANCH &optional ARGS)" t nil)
 (autoload 'magit-push-popup "magit-remote" nil t)

(autoload 'magit-push-current "misc/magit/lisp/magit-remote" "\
Push the current branch to its upstream branch.
If the upstream isn't set, then read the remote branch.

If `magit-push-always-verify' is not nil, however, always read
the remote branch.

\(fn BRANCH REMOTE &optional REMOTE-BRANCH ARGS)" t nil)

(autoload 'magit-push "misc/magit/lisp/magit-remote" "\
Push a branch to its upstream branch.
If the upstream isn't set, then read the remote branch.

If `magit-push-always-verify' is not nil, however, always read
the remote branch.

\(fn BRANCH REMOTE &optional REMOTE-BRANCH ARGS)" t nil)

(autoload 'magit-push-elsewhere "misc/magit/lisp/magit-remote" "\
Push a branch or commit to some remote branch.
Read the local and remote branch.

\(fn BRANCH REMOTE REMOTE-BRANCH &optional ARGS)" t nil)

(autoload 'magit-push-quickly "misc/magit/lisp/magit-remote" "\
Push the current branch to some remote.
When the Git variable `magit.pushRemote' is set, then push to
that remote.  If that variable is undefined or the remote does
not exist, then push to \"origin\".  If that also doesn't exist
then raise an error.  The local branch is pushed to the remote
branch with the same name.

\(fn &optional ARGS)" t nil)

(autoload 'magit-push-implicitly "misc/magit/lisp/magit-remote" "\
Push without explicitly specifing what to push.
This runs `git push -v'.  What is being pushed depends on various
Git variables as described in the `git-push(1)' and `git-config(1)'
manpages.

\(fn &optional ARGS)" t nil)

(autoload 'magit-push-matching "misc/magit/lisp/magit-remote" "\
Push all matching branches to another repository.
If multiple remotes exit, then read one from the user.
If just one exists, use that without requiring confirmation.

\(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-tag "misc/magit/lisp/magit-remote" "\
Push a tag to another repository.

\(fn TAG REMOTE &optional ARGS)" t nil)
 (autoload 'magit-patch-popup "magit-remote" nil t)

(autoload 'magit-format-patch "misc/magit/lisp/magit-remote" "\
Create patches for the commits in RANGE.
When a single commit is given for RANGE, create a patch for the
changes introduced by that commit (unlike 'git format-patch'
which creates patches for all commits that are reachable from
HEAD but not from the specified commit).

\(fn RANGE ARGS)" t nil)

(autoload 'magit-request-pull "misc/magit/lisp/magit-remote" "\
Request upstream to pull from you public repository.

URL is the url of your publically accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit.

\(fn URL START END)" t nil)

;;;***

;;;### (autoloads nil "misc/magit/lisp/magit-sequence" "../misc/magit/lisp/magit-sequence.el"
;;;;;;  (22081 4590 780516 585000))
;;; Generated autoloads from ../misc/magit/lisp/magit-sequence.el

(autoload 'magit-sequencer-continue "misc/magit/lisp/magit-sequence" "\
Resume the current cherry-pick or revert sequence.

\(fn)" t nil)

(autoload 'magit-sequencer-skip "misc/magit/lisp/magit-sequence" "\
Skip the stopped at commit during a cherry-pick or revert sequence.

\(fn)" t nil)

(autoload 'magit-sequencer-abort "misc/magit/lisp/magit-sequence" "\
Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started.

\(fn)" t nil)
 (autoload 'magit-cherry-pick-popup "magit-sequence" nil t)

(autoload 'magit-cherry-pick "misc/magit/lisp/magit-sequence" "\
Cherry-pick COMMIT.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting.

\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-cherry-apply "misc/magit/lisp/magit-sequence" "\
Apply the changes in COMMIT but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting.

\(fn COMMIT &optional ARGS)" t nil)
 (autoload 'magit-revert-popup "magit-sequence" nil t)

(autoload 'magit-revert "misc/magit/lisp/magit-sequence" "\
Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-revert-no-commit "misc/magit/lisp/magit-sequence" "\
Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

\(fn COMMIT &optional ARGS)" t nil)
 (autoload 'magit-am-popup "magit-sequence" nil t)

(autoload 'magit-am-apply-patches "misc/magit/lisp/magit-sequence" "\
Apply the patches FILES.

\(fn &optional FILES ARGS)" t nil)

(autoload 'magit-am-apply-maildir "misc/magit/lisp/magit-sequence" "\
Apply the patches from MAILDIR.

\(fn &optional MAILDIR ARGS)" t nil)

(autoload 'magit-am-continue "misc/magit/lisp/magit-sequence" "\
Resume the current patch applying sequence.

\(fn)" t nil)

(autoload 'magit-am-skip "misc/magit/lisp/magit-sequence" "\
Skip the stopped at patch during a patch applying sequence.

\(fn)" t nil)

(autoload 'magit-am-abort "misc/magit/lisp/magit-sequence" "\
Abort the current patch applying sequence.
This discards all changes made since the sequence started.

\(fn)" t nil)
 (autoload 'magit-rebase-popup "magit-sequence" nil t)

(autoload 'magit-rebase "misc/magit/lisp/magit-sequence" "\
Start a non-interactive rebase sequence.
All commits not in UPSTREAM are rebased.

\(fn UPSTREAM &optional ARGS)" t nil)

(autoload 'magit-rebase-subset "misc/magit/lisp/magit-sequence" "\
Start a non-interactive rebase sequence.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits.

\(fn NEWBASE START &optional ARGS)" t nil)

(autoload 'magit-rebase-interactive "misc/magit/lisp/magit-sequence" "\
Start an interactive rebase sequence.

\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-rebase-unpushed "misc/magit/lisp/magit-sequence" "\
Start an interactive rebase sequence of all unpushed commits.

\(fn &optional ARGS)" t nil)

(autoload 'magit-rebase-autosquash "misc/magit/lisp/magit-sequence" "\
Combine squash and fixup commits with their intended targets.

\(fn &optional ARGS)" t nil)

(autoload 'magit-rebase-edit-commit "misc/magit/lisp/magit-sequence" "\
Edit a single older commit using rebase.

\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-rebase-reword-commit "misc/magit/lisp/magit-sequence" "\
Reword a single older commit using rebase.

\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-rebase-continue "misc/magit/lisp/magit-sequence" "\
Restart the current rebasing operation.

\(fn)" t nil)

(autoload 'magit-rebase-skip "misc/magit/lisp/magit-sequence" "\
Skip the current commit and restart the current rebase operation.

\(fn)" t nil)

(autoload 'magit-rebase-edit "misc/magit/lisp/magit-sequence" "\
Edit the todo list of the current rebase operation.

\(fn)" t nil)

(autoload 'magit-rebase-abort "misc/magit/lisp/magit-sequence" "\
Abort the current rebase operation, restoring the original branch.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "misc/magit/lisp/magit-stash" "../misc/magit/lisp/magit-stash.el"
;;;;;;  (22081 4590 780516 585000))
;;; Generated autoloads from ../misc/magit/lisp/magit-stash.el
 (autoload 'magit-stash-popup "magit-stash" nil t)

(autoload 'magit-stash "misc/magit/lisp/magit-stash" "\
Create a stash of the index and working tree.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-index "misc/magit/lisp/magit-stash" "\
Create a stash of the index only.
Unstaged and untracked changes are not stashed.

\(fn MESSAGE)" t nil)

(autoload 'magit-stash-worktree "misc/magit/lisp/magit-stash" "\
Create a stash of the working tree only.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-keep-index "misc/magit/lisp/magit-stash" "\
Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-snapshot "misc/magit/lisp/magit-stash" "\
Create a snapshot of the index and working tree.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-snapshot-index "misc/magit/lisp/magit-stash" "\
Create a snapshot of the index only.
Unstaged and untracked changes are not stashed.

\(fn)" t nil)

(autoload 'magit-snapshot-worktree "misc/magit/lisp/magit-stash" "\
Create a snapshot of the working tree only.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-apply "misc/magit/lisp/magit-stash" "\
Apply a stash to the working tree.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index.

\(fn STASH)" t nil)

(autoload 'magit-stash-drop "misc/magit/lisp/magit-stash" "\
Remove a stash from the stash list.
When the region is active offer to drop all contained stashes.

\(fn STASH)" t nil)

(autoload 'magit-stash-clear "misc/magit/lisp/magit-stash" "\
Remove all stashes saved in REF's reflog by deleting REF.

\(fn REF)" t nil)

(autoload 'magit-stash-branch "misc/magit/lisp/magit-stash" "\
Create and checkout a new BRANCH from STASH.

\(fn STASH BRANCH)" t nil)

(autoload 'magit-stash-format-patch "misc/magit/lisp/magit-stash" "\
Create a patch from STASH

\(fn STASH)" t nil)

(autoload 'magit-stash-list "misc/magit/lisp/magit-stash" "\
List all stashes in a buffer.

\(fn)" t nil)

(autoload 'magit-stash-show "misc/magit/lisp/magit-stash" "\
Show all diffs of a stash in a buffer.

\(fn STASH &optional ARGS FILES)" t nil)

;;;***

;;;### (autoloads nil "misc/magit/lisp/magit-wip" "../misc/magit/lisp/magit-wip.el"
;;;;;;  (22081 4590 780516 585000))
;;; Generated autoloads from ../misc/magit/lisp/magit-wip.el

(defvar magit-wip-after-save-mode nil "\
Non-nil if Magit-Wip-After-Save mode is enabled.
See the command `magit-wip-after-save-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-after-save-mode'.")

(custom-autoload 'magit-wip-after-save-mode "misc/magit/lisp/magit-wip" nil)

(autoload 'magit-wip-after-save-mode "misc/magit/lisp/magit-wip" "\
Toggle Magit-Wip-After-Save-Local mode in all buffers.
With prefix ARG, enable Magit-Wip-After-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Wip-After-Save-Local mode is enabled in all buffers where
`magit-wip-after-save-local-mode-turn-on' would do it.
See `magit-wip-after-save-local-mode' for more information on Magit-Wip-After-Save-Local mode.

\(fn &optional ARG)" t nil)

(defvar magit-wip-after-apply-mode nil "\
Non-nil if Magit-Wip-After-Apply mode is enabled.
See the command `magit-wip-after-apply-mode' for a description of this minor mode.")

(custom-autoload 'magit-wip-after-apply-mode "misc/magit/lisp/magit-wip" nil)

(autoload 'magit-wip-after-apply-mode "misc/magit/lisp/magit-wip" "\
Commit to work-in-progress refs.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index.

\(fn &optional ARG)" t nil)

(defvar magit-wip-before-change-mode nil "\
Non-nil if Magit-Wip-Before-Change mode is enabled.
See the command `magit-wip-before-change-mode' for a description of this minor mode.")

(custom-autoload 'magit-wip-before-change-mode "misc/magit/lisp/magit-wip" nil)

(autoload 'magit-wip-before-change-mode "misc/magit/lisp/magit-wip" "\
Commit to work-in-progress refs before certain destructive changes.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "mode-compile" "mode-compile.el" (20741 14138
;;;;;;  0 0))
;;; Generated autoloads from mode-compile.el

(defvar mode-compile-make-program "make" "\
*The `make' program used to process makefiles.

If you have GNU make installed with name \"gmake\" use it.")

(custom-autoload 'mode-compile-make-program "mode-compile" t)

(defvar mode-compile-ignore-makefile-backups t "\
*Tell mode compile to ignore makefiles backup files when selecting the Makefile to use.")

(custom-autoload 'mode-compile-ignore-makefile-backups "mode-compile" t)

(defvar mode-compile-default-make-options "-k" "\
Default options to give to `make'.")

(defvar mode-compile-make-options (eval mode-compile-default-make-options) "\
*Options to give to `make'.
This could be any form evaluating to a string.

Some people asked me a way to modify the make options everytime a
compilation command is launched, do that:
 (defun my-mode-compile-ask-make-options()
   \"*Hook called by mode-compile, asking for make options.\"
   (interactive)
   (read-string \"Make options: \"
                mode-compile-default-make-options))
 (setq mode-compile-make-options
           'my-mode-compile-ask-make-options)")

(custom-autoload 'mode-compile-make-options "mode-compile" t)

(defvar mode-compile-prefered-default-makerule 'none "\
*Default makerule you would like to see in minibuffer as a default choice
when selecting the make rule to build.

Possible values are:
'none    -- let mode-compile deciding for you.
'all     -- try hard to show you the \"all\" rule.
'default -- try hard to show you the \"default\" rule.
'file    -- try to show you the name of the file which will be
            result of compilation.
The 'none action is taken as default is something fail.")

(custom-autoload 'mode-compile-prefered-default-makerule "mode-compile" t)

(defvar mode-compile-ignore-makerule-regexp nil "\
*Makefile rules which must be ignored when building completion list.

For example if you want to remove all `files rules' set
it to: \"\\\\.\\\\([aoc]\\\\|s[ao][.0-9]*\\\\)\". ")

(custom-autoload 'mode-compile-ignore-makerule-regexp "mode-compile" t)

(defvar mode-compile-save-all-p nil "\
*Non-nil means save ALL the modified buffers without asking
before launching compilation command.")

(custom-autoload 'mode-compile-save-all-p "mode-compile" t)

(defvar mode-compile-always-save-buffer-p nil "\
*Non-nil means save the current buffer without asking
before launching compilation command.")

(custom-autoload 'mode-compile-always-save-buffer-p "mode-compile" t)

(defvar mode-compile-never-edit-command-p nil "\
*Non-nil means never ask to user to edit the compile command.")

(custom-autoload 'mode-compile-never-edit-command-p "mode-compile" t)

(defvar mode-compile-other-frame-p nil "\
*Non-nil means compile in another frame.

A new Emacs FRAME is created and the compilation command is executed
in this other frame.  To specify the frame parameters see also
variable `mode-compile-frame-parameters-alist'.")

(custom-autoload 'mode-compile-other-frame-p "mode-compile" t)

(defvar mode-compile-before-compile-hook nil "\
Hook to be run before compile command is executed
when `mode-compile' is invoked.")

(custom-autoload 'mode-compile-before-compile-hook "mode-compile" t)

(defvar mode-compile-after-compile-hook nil "\
Hook to be run after compile command is executed
when `mode-compile' is invoked.")

(custom-autoload 'mode-compile-after-compile-hook "mode-compile" t)

(defvar mode-compile-before-kill-hook nil "\
Hook to be run before killing compile command is executed
when `mode-compile-kill' is invoked.")

(custom-autoload 'mode-compile-before-kill-hook "mode-compile" t)

(defvar mode-compile-after-kill-hook nil "\
Hook to be run after killing compile command is executed
when `mode-compile-kill' is invoked.")

(custom-autoload 'mode-compile-after-kill-hook "mode-compile" t)

(defvar mode-compile-choosen-compiler nil "\
*Global variable containing the name of the compiler
which will be used for compiling without makefile.

 Could be used in combination with
 (cc|c++|ada|f77)-default-compiler-options
to automaticaly choose the compiler specific options.

example:
 (defun my-compiler-get-options()
   (cond
    ((string= mode-compile-choosen-compiler \"gcc\")
      \"-Wall -pedantic-errors\")
    ((string= mode-compile-choosen-compiler \"cc\")
      \"cc options whatever they are...\")
    (t
     (message \"Don't know this compiler: %s\" mode-compile-choosen-compiler)
     (read-string
      (format \"Options for %s compiler: \" mode-compile-choosen-compiler)))))

  (setq cc-default-compiler-options 'my-compiler-get-options)")

(defvar mode-compile-expert-p nil "\
*Non nil means `mode-compile' will not speaks too much.

See also variable variable mode-compile-reading-time.")

(custom-autoload 'mode-compile-expert-p "mode-compile" t)

(defvar mode-compile-reading-time 1 "\
*Seconds to wait in verbose mode after printing a message.

In verbose mode mode-compile print too much messages that it is
allmost impossible to read them. Just setting this delay leave you the
time to read all the messages. If you don't want any delay set it to
`0'.

See also function sit-for.")

(custom-autoload 'mode-compile-reading-time "mode-compile" t)

(defvar emacs-lisp-byte-compile-dir-interactive-p t "\
*Non-nil means when byte-compiling a directory ask for each file
needing to be recompiled or not.")

(custom-autoload 'emacs-lisp-byte-compile-dir-interactive-p "mode-compile" t)

(defconst mode-compile-version "2.28" "\
Current version of mode-compile package.

mode-compile.el,v 2.28 2003/04/01 13:52:47 boubaker Exp
Please send bugs-fixes/contributions/comments to boubaker@cena.fr")

(autoload 'mode-compile-submit-bug-report "mode-compile" "\
*Submit via mail a bug report on mode-compile v2.27.

\(fn)" t nil)

(autoload 'mode-compile "mode-compile" "\
*Compile the file in the current buffer with a dynamically built command.

The command is built according to the current major mode the function
was invoked from.

Running this command preceded by universal-argument (\\[universal-argument])
allows remote compilation, the user is prompted for a host name to run the
compilation command on.

Currently know how to compile in:
 `c-mode' ,              -- function cc-compile.
 `java-mode' ,           -- function java-compile.
 `c++-mode',             -- function c++-compile.
 `ada-mode',             -- function ada-compile.
 `fortran-mode',         -- function f77-compile.
 `emacs-lisp-mode'       -- function elisp-compile.
 `lisp-interaction-mode' -- function elisp-compile.
 `makefile-mode'         -- function makefile-compile.
 `dired-mode'            -- function dired-compile.
 `sh-mode'               -- function sh-compile.
 `csh-mode'              -- function csh-compile.
 `zsh-mode'              -- function zsh-compile.
 `perl-mode'             -- function perl-compile.
 `cperl-mode'            -- function perl-compile.
 `tcl-mode'              -- function tcl-compile.
 `python-mode'           -- function python-compile.
 `ruby-mode'             -- function ruby-compile.
 `fundamental-mode'      -- function guess-compile.
 `text-mode'             -- function guess-compile.
 `indented-text-mode'    -- function guess-compile.
 `compilation-mode'      -- function default-compile.
 The function `guess-compile' is called when mode is unknown.

The variable `mode-compile-modes-alist' contain description of known
modes.  The hooks variables `mode-compile-before-compile-hook' and
`mode-compile-after-compile-hook' are run just before and after
invoking the compile command of the mode.

Use the command `mode-compile-kill' (\\[mode-compile-kill]) to abort a
running compilation.

Bound on \\[mode-compile].

\(fn &optional REMOTE-HOST)" t nil)

(autoload 'mode-compile-kill "mode-compile" "\
*Kill the running compilation launched by `mode-compile' (\\[mode-compile]) command.

The compilation command is killed according to the current major mode
the function was invoked from.

Currently know how to kill compilations from:
 `c-mode' ,              -- function kill-compilation.
 `java-mode' ,           -- function kill-compilation.
 `c++-mode' ,            -- function kill-compilation.
 `ada-mode' ,            -- function kill-compilation.
 `fortran-mode' ,        -- function kill-compilation.
 `emacs-lisp-mode'       -- function keyboard-quit.
 `lisp-interaction-mode' -- function keyboard-quit.
 `makefile-mode'         -- function kill-compilation.
 `dired-mode'            -- function kill-compilation.
 `sh-mode'               -- function kill-compilation.
 `csh-mode'              -- function kill-compilation.
 `zsh-mode'              -- function kill-compilation.
 `perl-mode'             -- function kill-compilation.
 `cperl-mode'            -- function kill-compilation.
 `tcl-mode'              -- function kill-compilation.
 `python-mode'           -- function kill-compilation.
 `ruby-mode'             -- function kill-compilation.
 `fundamental-mode'      -- Bound dynamically.
 `text-mode'             -- Bound dynamically.
 `indented-text-mode'    -- Bound dynamically.
 `compilation-mode'      -- function kill-compilation.

The variable `mode-compile-modes-alist' contain description of ALL
known modes.  The hooks variables `mode-compile-before-kill-hook' and
`mode-compile-after-kill-hook' are run just before and after invoking
the kill compile command of the mode.

Bound on \\[mode-compile-kill].

\(fn)" t nil)

;;;***

;;;### (autoloads nil "oddmuse" "oddmuse.el" (20741 14138 0 0))
;;; Generated autoloads from oddmuse.el

(autoload 'oddmuse-toggle-minor "oddmuse" "\
Toggle minor mode state.

\(fn &optional ARG)" t nil)

(autoload 'oddmuse-edit "oddmuse" "\
Edit a page on a wiki.
WIKI is the name of the wiki as defined in `oddmuse-wikis',
PAGENAME is the pagename of the page you want to edit.
Use a prefix argument to force a reload of the page.

\(fn WIKI PAGENAME)" t nil)

(autoload 'oddmuse-follow "oddmuse" "\
Figure out what page we need to visit
and call `oddmuse-edit' on it.

\(fn ARG)" t nil)

(autoload 'oddmuse-post "oddmuse" "\
Post the current buffer to the current wiki.
The current wiki is taken from `oddmuse-wiki'.

\(fn SUMMARY)" t nil)

(autoload 'oddmuse-revert "oddmuse" "\
Revert this oddmuse page.

\(fn)" t nil)

(autoload 'oddmuse-insert-pagename "oddmuse" "\
Insert a PAGENAME of current wiki with completion.

\(fn PAGENAME)" t nil)

(autoload 'oddmuse-redirect "oddmuse" "\
Insert a #REDIRECT directive to `pagename' for current `oddmuse-page-name'.

\(fn PAGENAME)" t nil)

(autoload 'oddmuse-delete "oddmuse" "\
Add a page name to the DeletedPage stack.
User will be asked to confirm it is correct.

\(fn SUMMARY)" t nil)

(autoload 'emacswiki-post "oddmuse" "\
Post the current buffer to the EmacsWiki.
If this command is invoked interactively: with prefix argument, prompts pagename,
otherwise set pagename as basename of `buffer-file-name'.

This command is intended to post current EmacsLisp program easily.

\(fn &optional PAGENAME SUMMARY)" t nil)

(autoload 'oddmuse-browse-page "oddmuse" "\
Ask a WWW browser to load an oddmuse page.
WIKI is the name of the wiki as defined in `oddmuse-wikis',
PAGENAME is the pagename of the page you want to browse.

\(fn WIKI PAGENAME)" t nil)

(autoload 'oddmuse-browse-this-page "oddmuse" "\
Ask a WWW browser to load current oddmuse page.

\(fn)" t nil)

(autoload 'oddmuse-kill-url "oddmuse" "\
Make the URL of current oddmuse page the latest kill in the kill ring.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "pi-tempo-abbrev" "pi-tempo-abbrev.el" (20741
;;;;;;  14138 0 0))
;;; Generated autoloads from pi-tempo-abbrev.el

(autoload 'tempo-abbrev-add "pi-tempo-abbrev" "\
* Construct tempo-template and insert abbrev in 'table'.
- HOOKS is a list of hook name for which the abbrev-table will be available.
- TABLE is a symbol, the name of the table which will create.
- TAGLIST is a symbol, the name of the tag list that tag of tempolist
should be added to.
- tempolist, a list, has this form:
'((\"TAG1\" (ELEMENTS))
 (\"TAG2\" (ELEMENTS))
 ...)
where TAG* is the abbrevation and ELEMENTS is the definition of tempo-template
as describes in the documentation of `tempo-define-template'.
Here an example:
\(tempo-abbrev-add
 '(lisp-mode-hook emacs-lisp-mode-hook) ;; List of modes where the following table will use.
 'tempo-abbrev-lisp-table    ;; Table name (* MUST BE UNIQUE *).
 'tempo-abbrev-lisp-tagslist ;; Name of the variable where the tag list should be added.
 '((\"lambda\" (> \"lambda (\" p \")\" n> p > \")\" > %))
   (\"defun\" (> \"defun \" p \" (\" p \")\" n> \"\\\"\" p \"\\\"\" n> r \")\" > %))))

\(fn HOOKS TABLE TAGLIST TEMPOLIST)" nil nil)

(autoload 'tempo-abbrev-change-table "pi-tempo-abbrev" "\
* Change local-abbrev-table or set it to TABLE.

\(fn &optional TABLE)" t nil)

;;;***

;;;### (autoloads nil "progmodes/graphviz-dot-mode/graphviz-dot-mode"
;;;;;;  "../progmodes/graphviz-dot-mode/graphviz-dot-mode.el" (22112
;;;;;;  16669 66915 472000))
;;; Generated autoloads from ../progmodes/graphviz-dot-mode/graphviz-dot-mode.el

(autoload 'graphviz-dot-mode "progmodes/graphviz-dot-mode/graphviz-dot-mode" "\
Major mode for the dot language. \\<graphviz-dot-mode-map>
TAB indents for graph lines.

\\[graphviz-dot-indent-graph]	- Indentation function.
\\[graphviz-dot-preview]	- Previews graph in a buffer.
\\[graphviz-dot-view]	- Views graph in an external viewer.
\\[graphviz-dot-indent-line]	- Indents current line of code.
\\[graphviz-dot-complete-word]	- Completes the current word.
\\[electric-graphviz-dot-terminate-line]	- Electric newline.
\\[electric-graphviz-dot-open-brace]	- Electric open braces.
\\[electric-graphviz-dot-close-brace]	- Electric close braces.
\\[electric-graphviz-dot-semi]	- Electric semi colons.

Variables specific to this mode:

  graphviz-dot-dot-program            (default `dot')
       Location of the dot program.
  graphviz-dot-view-command           (default `doted %s')
       Command to run when `graphviz-dot-view' is executed.
  graphviz-dot-view-edit-command      (default nil)
       If the user should be asked to edit the view command.
  graphviz-dot-save-before-view       (default t)
       Automatically save current buffer berore `graphviz-dot-view'.
  graphviz-dot-preview-extension      (default `png')
       File type to use for `graphviz-dot-preview'.
  graphviz-dot-auto-indent-on-newline (default t)
       Whether to run `electric-graphviz-dot-terminate-line' when
       newline is entered.
  graphviz-dot-auto-indent-on-braces (default t)
       Whether to run `electric-graphviz-dot-open-brace' and
       `electric-graphviz-dot-close-brace' when braces are
       entered.
  graphviz-dot-auto-indent-on-semi (default t)
       Whether to run `electric-graphviz-dot-semi' when semi colon
       is typed.
  graphviz-dot-toggle-completions  (default nil)
       If completions should be displayed in the buffer instead of a
       completion buffer when \\[graphviz-dot-complete-word] is
       pressed repeatedly.

This mode can be customized by running \\[graphviz-dot-customize].

Turning on Graphviz Dot mode calls the value of the variable
`graphviz-dot-mode-hook' with no args, if that value is non-nil.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))

;;;***

;;;### (autoloads ((quote info-mode)) "progmodes/info-mode/info-mode"
;;;;;;  "../progmodes/info-mode/info-mode.el" (22092 10972 892316
;;;;;;  321000))
;;; Generated autoloads from ../progmodes/info-mode/info-mode.el

(autoload 'info-mode "info-mode" "\
A mode for info files

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.info\\'" 'info-mode))

;;;***

;;;### (autoloads nil "project-buffer-mode" "project-buffer-mode.el"
;;;;;;  (20741 14136 0 0))
;;; Generated autoloads from project-buffer-mode.el

(autoload 'project-buffer-find-file "project-buffer-mode" "\
Create a `project-buffer-mode' buffer based on the content of FILENAME.

\(fn FILENAME)" t nil)

;;;***

;;;### (autoloads nil "psvn" "psvn.el" (20741 14138 0 0))
;;; Generated autoloads from psvn.el

(autoload 'svn-checkout "psvn" "\
Run svn checkout REPOS-URL PATH.

\(fn REPOS-URL PATH)" t nil)
 (defalias 'svn-examine 'svn-status)

(autoload 'svn-status "psvn" "\
Examine the status of Subversion working copy in directory DIR.
If ARG is -, allow editing of the parameters. One could add -N to
run svn status non recursively to make it faster.
For every other non nil ARG pass the -u argument to `svn status', which
asks svn to connect to the repository and check to see if there are updates
there.

If there is no .svn directory, examine if there is CVS and run
`cvs-examine'. Otherwise ask if to run `dired'.

\(fn DIR &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "rect-mark" "rect-mark.el" (20741 14138 0 0))
;;; Generated autoloads from rect-mark.el
 (define-key ctl-x-map "r\C-@" 'rm-set-mark)
 (define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
 (define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
 (define-key ctl-x-map "r\C-w" 'rm-kill-region)
 (define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)
 (define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)

(autoload 'rm-example-picture-mode-bindings "rect-mark" "\
Example rect-mark keyboard and mouse bindings for picture mode.

\(fn)" nil nil)

(autoload 'rm-set-mark "rect-mark" "\
Set mark like `set-mark-command' but anticipates a rectangle.
This arranges for the rectangular region between point and mark
to be highlighted using the same face that is used to highlight
the region in `transient-mark-mode'.  This special state lasts only
until the mark is deactivated, usually by executing a text-modifying
command like \\[kill-rectangle], by inserting text, or by typing \\[keyboard-quit].

With optional argument FORCE, arrange for tabs to be expanded and
for spaces to inserted as necessary to keep the region perfectly
rectangular.  This is the default in `picture-mode'.

\(fn FORCE)" t nil)

(autoload 'rm-exchange-point-and-mark "rect-mark" "\
Like `exchange-point-and-mark' but treats region as a rectangle.
See `rm-set-mark' for more details.

With optional argument FORCE, tabs are expanded and spaces are
inserted as necessary to keep the region perfectly rectangular.
This is the default in `picture-mode'.

\(fn FORCE)" t nil)

(autoload 'rm-kill-region "rect-mark" "\
Like kill-rectangle except the rectangle is also saved in the kill ring.
Since rectangles are not ordinary text, the killed rectangle is saved
in the kill ring as a series of lines, one for each row of the rectangle.
The rectangle is also saved as the killed rectangle so it is available for
insertion with yank-rectangle.

\(fn START END)" t nil)

(autoload 'rm-kill-ring-save "rect-mark" "\
Copies the region like rm-kill-region would but the rectangle isn't killed.

\(fn START END)" t nil)

(autoload 'rm-mouse-drag-region "rect-mark" "\
Highlight a rectangular region of text as the the mouse is dragged over it.
This must be bound to a button-down mouse event.

\(fn START-EVENT)" t nil)

;;;***

;;;### (autoloads nil "rubydb2x" "rubydb2x.el" (20741 14138 0 0))
;;; Generated autoloads from rubydb2x.el

(autoload 'rubydb "rubydb2x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads nil "rubydb3x" "rubydb3x.el" (20741 14138 0 0))
;;; Generated autoloads from rubydb3x.el

(autoload 'rubydb "rubydb3x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads nil "rust-mode" "rust-mode.el" (21636 4995 643853
;;;;;;  888000))
;;; Generated autoloads from rust-mode.el

(autoload 'rust-mode "rust-mode" "\
Major mode for Rust code.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;;***

;;;### (autoloads nil "scilab" "scilab.el" (20741 14138 0 0))
;;; Generated autoloads from scilab.el

(defvar scilab-command "scilab" "\
Command to run Scilab. scilab must be in the path.
The option and the name of the file, will be added to this string.")

(defvar scilab-last-buffer-compiled nil "\
Buffer which was last compiled.")

(defvar scilab-zap-file nil "\
Temporary file name used for text being sent as input to Scilab.
Should be a simple file name with no extension or directory specification.")

(defvar scilab-directory "." "\
*Directory in which temporary files are left.
You can make this /tmp if your functions has no relative directories in it.")

(defvar scilab-offer-save t "\
*If non-nil, ask about saving modified buffers before \\[scilab-file] is run.")

(defvar scilab-last-temp-file nil "\
Latest temporary file generated by \\[scilab-region] and \\[scilab-compile-buffer].
Deleted when the \\[scilab-region] or \\[scilab-compile-buffer] is next run, or when the
scilab-shell goes away.")

(defvar scilab-trailer nil "\
String appended after the end of a region sent to scilab by \\[scilab-region].")

;;;***

;;;### (autoloads nil "screen-lines" "screen-lines.el" (20741 14138
;;;;;;  0 0))
;;; Generated autoloads from screen-lines.el

(autoload 'screen-lines-mode "screen-lines" "\
Toggle Screen Lines minor mode for the current buffer.
With ARG, turn the mode on if ARG is positive, off otherwise.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-screen-lines-mode "screen-lines" "\
Turn on Screen Lines minor mode for the current buffer.

\(fn)" t nil)

(autoload 'turn-off-screen-lines-mode "screen-lines" "\
Turn off Screen Lines minor mode for the current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "semantic-bnf" "semantic-bnf.el" (20741 14138
;;;;;;  0 0))
;;; Generated autoloads from semantic-bnf.el

(defalias 'bnf-mode 'semantic-bnf-mode)

(autoload 'semantic-bnf-mode "semantic-bnf" "\
Initialize a buffer for editing BNF code.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "sidebrain-browse" "sidebrain-browse.el" (20741
;;;;;;  14138 0 0))
;;; Generated autoloads from sidebrain-browse.el

(autoload 'sidebrain-browse-tasks "sidebrain-browse" "\
Browse the task queue, and perhaps select a task.
With optional argument, display all groups and projects, even when empty.
Further args are SPECIFIC-GROUPS SPECIFIC-PROJECTS and
START-AT-GROUP START-AT-PROJECT START-AT-TASK.
The SPECIFIC- arguments give alternative lists of groups and projects,
and if you specify SPECIFIC-PROJECTS there should only be one group
in SPECIFIC-GROUPS, and that group should contain those projects.
The START-AT- arguments are used to set the initial position of the
cursor; if they are not given, it starts on the first task.

\(fn &optional SHOW-ALL SPECIFIC-GROUPS SPECIFIC-PROJECTS START-AT-GROUP START-AT-PROJECT START-AT-TASK)" t nil)

(autoload 'sidebrain-mail-tasks "sidebrain-browse" "\
Send selected tasks in the mail.
Should normally be used from sidebrain-browse-tasks-mode.

\(fn RECIPIENT)" t nil)

(autoload 'sidebrain-extract-tasks-from-mail "sidebrain-browse" "\
Parse the current buffer as a mail message, and pick up any tasks described in it.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "sidebrain-commands" "sidebrain-commands.el"
;;;;;;  (20741 14138 0 0))
;;; Generated autoloads from sidebrain-commands.el

(autoload 'sidebrain-begin-task "sidebrain-commands" "\
Begin a task described as TASK-STRING.

\(fn TASK-STRING &optional SAME-PROJECT)" t nil)

(autoload 'sidebrain-end-task "sidebrain-commands" "\
End the topmost task on the stack.
With optional arg non-nil, don't update the display.
Second optional is functions to use instead of those on sidebrain-record-task-hook.

\(fn &optional NO-DISPLAY FILTER-FUNCTIONS)" t nil)

(autoload 'sidebrain-abandon-task "sidebrain-commands" "\
End the topmost task on the stack.
With optional arg non-nil, don't update the display.

\(fn &optional NO-DISPLAY)" t nil)

(autoload 'sidebrain-reminder "sidebrain-commands" "\
Enter a reminder described as REMINDER-STRING.
If FILE and LINE are given, mark the reminder as having come from there.
If PROJECT-GROUP and PROJECT are given, enter the reminder in that project.
These may be given as names to look up in the appropriate lists,
or conses of name and list, for efficiency if entering reminders in bulk
from a program.

\(fn REMINDER-STRING &optional FILE LINE PROJECT-GROUP PROJECT)" t nil)

(autoload 'sidebrain-suspend-task "sidebrain-commands" "\
Suspend the current task stack.

With optional argument non-nil, don't ask for a name under which to suspend it,
but use the base task of the stack.

Second optional argument warns that we are being called from
sidebrain-resume-task, or similar.

Otherwise, suspending a \"special\" task (one whose project-group is
called \"special\") will resume the task that was active before it.

Returns the name under which it was suspended.

\(fn &optional NO-EDIT RESUMING-ANOTHER)" t nil)

(autoload 'sidebrain-resume-task "sidebrain-commands" "\
Resume the stack described as TASK-NAME.
If there was a task active, it is suspended.
Returns the label of the suspended task, if there was one.
With optional second argument non-nil, don't ask about the name under which to suspend
the suspended task, but use the base task of that stack.
If TASK-NAME is empty, the task stack is left empty.

\(fn TASK-NAME &optional NO-EDIT PROJECT-GROUP PROJECT)" t nil)

(autoload 'sidebrain-delete-task-stack "sidebrain-commands" "\
Delete TASK stack from PROJECT.
TASK may be a task label, task defstruct, or a cons of the two.

\(fn TASK PROJECT)" t nil)

(autoload 'sidebrain-delete-current-task-stack "sidebrain-commands" "\
Delete the current task stack.

\(fn)" t nil)

(autoload 'sidebrain-observe "sidebrain-commands" "\
Observe OBSERVATION.

\(fn OBSERVATION)" t nil)

;;;***

;;;### (autoloads nil "sidebrain-display" "sidebrain-display.el"
;;;;;;  (20741 14138 0 0))
;;; Generated autoloads from sidebrain-display.el

(autoload 'sidebrain-display "sidebrain-display" "\
Display the current task stack, etc.
Creates the buffer as needed.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "sidebrain-todo" "sidebrain-todo.el" (20741
;;;;;;  14140 0 0))
;;; Generated autoloads from sidebrain-todo.el

(autoload 'sidebrain-read-todo-from-comments "sidebrain-todo" "\
Make sidebrain reminders from todo comments in the current buffer.
They become part of the project (and project group) determined thus:
  First, sidebrain-determine-project-hook is tried (which see)
  If none of those match, sidebrain-file-projects is scanned (which see)
  If none of those match:
    if sidebrain-use-default-project is set, a default is used;
    otherwise, the user is prompted for them.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "sidebrain-xml" "sidebrain-xml.el" (20741 14138
;;;;;;  0 0))
;;; Generated autoloads from sidebrain-xml.el

(autoload 'sidebrain-save-to-file "sidebrain-xml" "\
Save the sidebrain data to the file named in sidebrain-file-name.

\(fn)" t nil)

(autoload 'sidebrain-load-from-file "sidebrain-xml" "\
Load the sidebrain data from the file named in sidebrain-file-name.
If it has not been saved since we saved it, don't do it, so as not to
disturb the existing data structures, unless optional argument non-nil.

\(fn &optional FORCE)" t nil)

;;;***

;;;### (autoloads nil "ssh-config-mode" "ssh-config-mode.el" (21565
;;;;;;  12945 516027 758000))
;;; Generated autoloads from ssh-config-mode.el

(autoload 'ssh-config-mode "ssh-config-mode" "\
Major mode for fontifiying ssh config files.

\\{ssh-config-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "subedit" "subedit.el" (20741 14136 0 0))
;;; Generated autoloads from subedit.el

(autoload 'subedit-edit-region "subedit" "\
Copy text between point and mark to a subedit buffer whose mode is MODE.

The copied lines in the region may all start with PREFIX and/or
end with SUFFIX; in that case, the prefix and the suffix will be
stripped after copying.  PREFIX and SUFFIX may contain preceding
and/or trailing whitespace characters; if a line in the region
does not start with PREFIX and/or end with SUFFIX because of the
whitespace characters, it will be still treated as starting with
PREFIX and/or ending with SUFFIX, and the prefix and/or
suffix (the ones after removing the whitespace characters) will
be stripped as well.

Empty lines are always treated as starting with any prefixes and
ending with any suffixes.

The subedit buffer is for editing text in the region in
MODE (either major mode or minor mode).  Later, changes in the
subedit buffer can be copied back to the region with
`subedit-commit', and the stripped prefix and suffix (if exist)
will be added back.

\(fn BEG END PREFIX SUFFIX MODE)" t nil)

(autoload 'subedit-commit "subedit" "\
Copy changes to the subedit buffer back to the region the text is copied from.
This will remove the subedit buffer from its original buffer's
`subedit-buffer-region-alist' and kill it too.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "synonyms" "synonyms.el" (20741 14138 0 0))
;;; Generated autoloads from synonyms.el

(let ((loads (get 'Synonyms 'custom-loads))) (if (member '"synonyms" loads) nil (put 'Synonyms 'custom-loads (cons '"synonyms" loads))))

(defface synonyms-heading '((((background dark)) (:foreground "Yellow")) (t (:foreground "Blue"))) "\
*Face for different synonym types." :group (quote Synonyms) :group (quote faces))

(defface synonyms-search-text '((t (:foreground "Red"))) "\
*Face for the term whose synonyms were sought." :group (quote Synonyms) :group (quote faces))

(defface synonyms-link '((((background dark)) (:foreground "Yellow" :underline t)) (t (:foreground "Blue" :underline t))) "\
*Face for history links." :group (quote Synonyms) :group (quote faces))

(defface synonyms-mouse-face '((((background dark)) (:background "DarkCyan")) (t (:background "Cyan"))) "\
*Mouse face for the term whose synonyms were sought." :group (quote Synonyms) :group (quote faces))

(defvar synonyms-append-result-flag nil "\
*t means that `synonyms' appends search result to previous results.
No other value, besides t, has this effect.

This can be overridden by using a negative prefix argument,
for example, `M--'.  If you use `C-u C-u', then both this and
`synonyms-match-more-flag' are overridden.")

(custom-autoload 'synonyms-append-result-flag "synonyms" t)

(defvar synonyms-cache-file "" "\
*Location to write cache file containing synonyms.
Written to save the list of synonyms used for completion.
This is an absolute (complete-path) location, including the file name.")

(custom-autoload 'synonyms-cache-file "synonyms" t)

(defvar synonyms-file "" "\
*Location of thesaurus file `mthesaur.txt'.
This is an absolute (complete-path) location, including the file name.")

(custom-autoload 'synonyms-file "synonyms" t)

(defvar synonyms-fill-column 80 "\
*Synonyms* buffer text is wrapped (filled) to this many columns.")

(custom-autoload 'synonyms-fill-column "synonyms" t)

(defvar synonyms-match-more-flag nil "\
*t means additional thesaurus entries can be matched by `synonyms'.
No other value, besides t, has this effect.

A value of t means two things:
 1) Input can match parts of synonyms, in addition to whole synonyms.
 2) All synonyms are shown, even if input matches a thesaurus entry.

This can be overridden by using a positive prefix argument,
  for example, `C-u'.  If you use `C-u C-u', then both this and
`synonyms-append-result-flag' are overridden.")

(custom-autoload 'synonyms-match-more-flag "synonyms" t)

(defvar synonyms-mode-hook nil "\
*Normal hook run when entering Thesaurus mode.")

(custom-autoload 'synonyms-mode-hook "synonyms" t)

(defvar synonyms-use-cygwin-flag nil "\
*Non-nil means to double backslashes in arguments to `call-process'.
There is apparently a bug in the Emacs (at least versions 20-22) C
code that implements function `call-process' on MS Windows.  When
using native Windows Emacs with Cygwin commands, such as `grep', the C
code removes a level of backslashes, so string arguments supplied to
`call-process' need to have twice as many backslashes as they should
need.  If you are using Emacs on Windows and Cygwin `grep', then you
probably will want to use a non-nil value for
`synonyms-use-cygwin-flag'.")

(custom-autoload 'synonyms-use-cygwin-flag "synonyms" t)

(defvar synonyms-dictionary-url "http://dictionary.reference.com/search?q=" "\
*URL of a Web dictionary lookup.  Text to look up is appended to this.
See also `synonyms-dictionaries-url'.")

(custom-autoload 'synonyms-dictionary-url "synonyms" t)

(defvar synonyms-dictionary-alternate-url "http://www.onelook.com/?ls=b&w=" "\
*URL of a Web dictionary lookup.  Text to look up is appended to this.
The default value, \"http://www.onelook.com/?ls=b&w=\" lets you use `?'
and `*' as wildcards in the terms you look up.  These are not used as
regexp wildcards, however.  `?' stands for any single character, and
`*' stands for any sequence of characters.  In terms of regexp syntax,
`?' here is equivalent to the regexp `.', and `*' is equivalent to the
regexp `.*'.  See http://www.onelook.com/?c=faq#patterns for more
information on the allowed wildcard patterns.
See also `synonyms-dictionary-url'.")

(custom-autoload 'synonyms-dictionary-alternate-url "synonyms" t)

(autoload 'synonyms-mode "synonyms" "\
Major mode for browsing thesaurus entries (synonyms).
Like Text mode but with these additional key bindings:

 \\<synonyms-mode-map>\\[synonyms-mouse],     \\[synonyms-no-read],     \\[synonyms] - Look up synonyms for a word or phrase
 \\[synonyms-mouse-match-more],   \\[synonyms-match-more]   - Like \\[synonyms-no-read], but try to match more terms
 \\[synonyms-mouse-append-result],   \\[synonyms-append-result]   - Like \\[synonyms-no-read], but add result to previous result
 \\[synonyms-mouse-match-more+append-result], \\[synonyms-match-more+append-result] - Like \\[synonyms-match-more] and \\[synonyms-append-result] combined

 \\[scroll-up] - Scroll down through the buffer of synonyms
 \\[scroll-down] - Scroll up through the buffer of synonyms
 \\[describe-mode]   - Display this help
 \\[quit-window]   - Quit Synonyms mode

Of the various key bindings that look up synonyms, the most flexible
is \\[synonyms] - it prompts you for the search string to match.  This
can be a regular expression (regexp).  The other lookup bindings are
for convenience - just click.

In Synonyms mode, Transient Mark mode is enabled.

Options `synonyms-match-more-flag' and `synonyms-append-result-flag'
affect synonym matching and the results.  For convenience, \\[synonyms-mouse-match-more],
\\[synonyms-mouse-append-result], and \\[synonyms-mouse-match-more+append-result] toggle the effect of those options for the
duration of the command.

Note that even though Synonyms mode is similar to Text mode, buffer
`*Synonyms*' is read-only, by default - use `C-x C-q' to toggle.

Turning on Synonyms mode runs the normal hooks `text-mode-hook' and
`synonyms-mode-hook' (in that order).

\(fn)" t nil)

(autoload 'synonyms-ensure-synonyms-read-from-cache "synonyms" "\
Ensure synonyms are in `synonyms-obarray', from `synonyms-cache-file'.
If this file does not yet exist, then it and the obarray are created.
Creating the obarray for the first time takes 2-3 minutes.
This does nothing if the obarray is already complete.

\(fn)" t nil)

(autoload 'synonyms-make-obarray "synonyms" "\
Fill `synonyms-obarray' with the available synonyms.

\(fn)" t nil)

(autoload 'synonyms-write-synonyms-to-cache "synonyms" "\
Write synonyms in `synonyms-obarray' to file `synonyms-cache-file'.

\(fn)" t nil)

(autoload 'synonyms-no-read "synonyms" "\
Same as command `synonyms', but uses the default input text (regexp).

\(fn ARG)" t nil)

(autoload 'synonyms-match-more "synonyms" "\
Same as using `synonyms' with `synonyms-match-more-flag' = t.

\(fn)" t nil)

(autoload 'synonyms-match-more-no-read "synonyms" "\
Same as using `synonyms' with `synonyms-match-more-flag' = t.

\(fn ARG)" t nil)

(autoload 'synonyms-append-result "synonyms" "\
Same as using `synonyms' with `synonyms-append-result-flag' = t.

\(fn)" t nil)

(autoload 'synonyms-append-result-no-read "synonyms" "\
Same as using `synonyms' with `synonyms-append-result-flag' = t.

\(fn ARG)" t nil)

(autoload 'synonyms-match-more+append-result "synonyms" "\
Like `synonyms-match-more-flag' = `synonyms-append-result-flag' = t.

\(fn)" t nil)

(autoload 'synonyms-match-more+append-result-no-read "synonyms" "\
Like `synonyms-match-more-flag' = `synonyms-append-result-flag' = t.

\(fn ARG)" t nil)

(autoload 'synonyms-mouse "synonyms" "\
Show synonyms that match a regular expression (e.g. a word or phrase).
The regexp to match is the synonym or region clicked with mouse-2.  If
the region is active, but a synonym elsewhere is clicked, that synonym
is used, not the selected text.

You can either click a listed synonym, to see its synonyms, or select
one or more words and click the selection, to see matching synonyms.
To quickly select a series of words: double-click mouse-1 to select
the first word, then click mouse-3 to extend the selection to the last
word.

Selection is useful when you want to see synonyms of a similar term.
For example, instead of clicking the listed synonym `bleeding heart', you
might select `heart' and click that.

The prefix argument acts the same as for command `synonyms'.

If you click a history link with mouse-2, previously retrieved search
results are revisited.

\(fn EVENT ARG)" t nil)

(autoload 'synonyms-mouse-match-more "synonyms" "\
Same as `synonyms-mouse' with `synonyms-match-more-flag' = t.

\(fn EVENT ARG)" t nil)

(autoload 'synonyms-mouse-append-result "synonyms" "\
Same as `synonyms-mouse' with `synonyms-append-result-flag' = t.

\(fn EVENT ARG)" t nil)

(autoload 'synonyms-mouse-match-more+append-result "synonyms" "\
Like `synonyms-match-more-flag' = `synonyms-append-result-flag' = t.

\(fn EVENT ARG)" t nil)

(autoload 'synonyms-history-backward "synonyms" "\
Run `synonyms' on a previous argument, moving backward in the history.
A prefix argument has the same meaning as for command `synonyms'.

\(fn ARG)" t nil)

(autoload 'synonyms-history-forward "synonyms" "\
Run `synonyms' on a previous argument, moving forward in the history.
A prefix argument has the same meaning as for command `synonyms'.

\(fn ARG)" t nil)

(defalias 'dictionary-definition 'synonyms-definition)

(autoload 'synonyms-definition "synonyms" "\
Look up the definition of a word or phrase using online dictionaries.
The dictionary used is `synonyms-dictionary-url'.
With prefix arg, look up the definition in the alternate dictionary,
`synonyms-dictionary-alternate-url'.

\(fn SEARCH-TEXT ALTERNATE-P)" t nil)

(autoload 'synonyms-definition-no-read "synonyms" "\
Look up the definition of a word or phrase using online dictionaries.
The dictionary used is `synonyms-dictionary-url'.
With prefix arg, look up the definition in the alternate dictionary,
`synonyms-dictionary-alternate-url'.

\(fn ALTERNATE-P)" t nil)

(autoload 'synonyms-definition-mouse "synonyms" "\
Look up the definition of a word or phrase using online dictionaries.
The dictionary used is `synonyms-dictionary-url'.
With prefix arg, look up the definition in the alternate dictionary,
`synonyms-dictionary-alternate-url'.

\(fn EVENT ALTERNATE-P)" t nil)

;;;***

;;;### (autoloads nil "template" "template.el" (21636 46444 934708
;;;;;;  834000))
;;; Generated autoloads from template.el

(autoload 'template-single-comment "template" "\
Decorate the comment in the current line with dashes and alike.
The line must be a comment-only line or must contain a comment ending by
eol.  That is, jump to the end of the current line and insert the dashes
and the final comment end-string up-to the fill position.  Prefix
argument ARG and `template-comment-specification' determines the comment
style to use.  The length of the resulting line is determined by
`template-max-column' and `template-max-column-with-end'.

\(fn &optional ARG)" t nil)

(autoload 'template-block-comment "template" "\
Decorate the current block of comment-only lines with dashes and alike.
That is, surround the the contiguous comment-only lines around point
with extra lines containing dashes and alike and to put the correct
number of newlines around the block.

Barf if the comment syntax at point has a non-empty `comment-end' or if
point is not in a comment-only line.

A block comment consists of all neighboring lines which start with
spaces and `comment-start'.  If `comment-start' is a string of length 1,
the number of repetitions of `comment-start' must be the same or larger
than in the line where the command is invoked from, too.

Prefix argument ARG and `template-comment-specification' determines the
comment style to use.  The length of the separator line is determined by
`template-max-column'.

This command can also be used with point in an empty line after a block
comment.  A second invocation of this command directly after a
successful invocation deletes the remaining empty lines from the current
line on.

\(fn &optional ARG)" t nil)

(autoload 'template-update-header "template" "\
Replace old file name in header with current file name.
If SHOW is t, just return region of the filename or nil.  Otherwise,
replace filename if possible and signal an error if SHOW is nil and
there is no filename in the header.  See `template-header-lines' and
`template-header-regexp-alist'.

\(fn &optional SHOW)" t nil)

(autoload 'template-expand-template "template" "\
Expand template file TEMPLATE and insert result in current buffer.
Using a template for inserting some text consists of:
  1. Template derivation: suggest a reasonable template file to the user
     according to `buffer-file-name', see `template-derivation-alist'.
  2. Template insertion: insert the template file at point into the
     current buffer.
  3.. as steps 6.. of `template-new-file'.

\(fn TEMPLATE)" t nil)

(autoload 'template-new-file "template" "\
Open a new file FILE by using a TEMPLATE.
Using a template for creating a new file consists of, steps 1 to 3 are
only executed when called interactively:
  1. Prompt for the name of the new file.
  2. Template derivation: suggest a reasonable template file to the user
     see `template-derivation-alist'.
  3. File name refinement: e.g., if the given file name is \"exercise\"
     and there are two files \"exercise1.tex\" and \"exercise2.tex\" in
     the same directory and if we have a template \"exercise.tex.tpl\",
     the file name is refined to \"exercise3.tex\".  This is turned off
     when \\[template-new-file] is called with a prefix argument.
  4. Template insertion: insert the template file into the empty buffer.
  5. Read per-template expansion definition section starting at
     `template-definition-start' and delete it.
  6. Display :before message in `template-message-buffer'.
  7. Execute pre-expansion commands defined in the definition section.
  8. Set local variables defined in the definition section.
  9. Expansion: expand the expansion forms (text matched by
     `template-expansion-regexp') They are defined in the definition
     section, in `template-expansion-alist', or provided by default, see
     `template-expansion-regexp' and `template-register-regexp'.
 10. Execute post-expansion commands defined in the definition section.
 11. Run `normal-mode' and functions in `find-file-hooks'.
 12. Update header according to `template-update-header' with argument
    `if-exists'.
 13. Display :after message in `template-message-buffer'.
 14. Report: display a temporary message at point defined in the
     definition section and an automatically generated message in the
     minibuffer area, see `template-message-timeout'.

If optional WITH-UNDO is non-nil, store corresponding changes in
`buffer-undo-list'.  If FILE is nil, the buffer for FILE has already
been created and the accessible part will be replaced by the expanded
template.  If TEMPLATE is nil (empty input when called interactively),
do not use a template.

\(fn FILE TEMPLATE &optional WITH-UNDO)" t nil)


;;;***

;;;### (autoloads nil "tempo-snippets" "tempo-snippets.el" (20741
;;;;;;  14138 0 0))
;;; Generated autoloads from tempo-snippets.el

(autoload 'tempo-snippets-clear-all "tempo-snippets" "\
Clear all tempo-snippet overlays.

\(fn)" t nil)

(autoload 'tempo-snippets-clear-oldest "tempo-snippets" "\
Clear the oldest tempo-snippet overlays.

\(fn)" t nil)

(autoload 'tempo-snippets-clear-latest "tempo-snippets" "\
Clear the latest tempo-snippet overlays.

\(fn)" t nil)

(autoload 'tempo-snippets-previous-field "tempo-snippets" "\
Jump to the previous editable tempo-snippet field.
You can also use `tempo-forward-mark', which will include more points of
interest.

\(fn &optional ARG)" t nil)

(autoload 'tempo-snippets-next-field "tempo-snippets" "\
Jump to the next editable tempo-snippet field.
You can also use `tempo-backward-mark', which will include more points of
interest.

\(fn &optional ARG)" t nil)

(autoload 'tempo-define-snippet "tempo-snippets" "\
`tempo-snippets' version of `tempo-define-template'.
Use with the same arguments as `tempo-define-template'.  The resulting template
will prompt for input right in the buffer instead of the minibuffer.

\(fn NAME ELEMENTS &optional TAG DOCUMENTATION TAGLIST)" nil nil)

(autoload 'tempo-snippets-insert-template "tempo-snippets" "\
`tempo-snippets' version of `tempo-insert-template.'

\(fn TEMPLATE ON-REGION)" nil nil)

(autoload 'tempo-snippets-complete-tag "tempo-snippets" "\
`tempo-snippets' version of `tempo-complete-tag.'

\(fn &optional SILENT)" t nil)

;;;***

;;;### (autoloads nil "tempo-x" "tempo-x.el" (20812 5108 358647 406000))
;;; Generated autoloads from tempo-x.el

(autoload 'tempo-x-space "tempo-x" "\
Expand tempo if complete in `tempo-local-tags' or insert space.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "undo-tree" "undo-tree.el" (20741 14138 0 0))
;;; Generated autoloads from undo-tree.el

(autoload 'undo-tree-mode "undo-tree" "\
Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-map}

\(fn &optional ARG)" t nil)

(defvar global-undo-tree-mode nil "\
Non-nil if Global-Undo-Tree mode is enabled.
See the command `global-undo-tree-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.")

(custom-autoload 'global-undo-tree-mode "undo-tree" nil)

(autoload 'global-undo-tree-mode "undo-tree" "\
Toggle Undo-Tree mode in all buffers.
With prefix ARG, enable Global-Undo-Tree mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Undo-Tree mode is enabled in all buffers where
`turn-on-undo-tree-mode' would do it.
See `undo-tree-mode' for more information on Undo-Tree mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "vc-ediff" "vc-ediff.el" (20741 14138 0 0))
;;; Generated autoloads from vc-ediff.el

(autoload 'vc-ediff "vc-ediff" "\
Like `vc-diff' but performs an `ediff' rather than a `diff'.

\(fn ARG)" t nil)

(autoload 'vc-ediff-menubar-init "vc-ediff" "\
Change the vc `Diff Against Last' from `vc-diff' to `vc-ediff'.
This would normally be put in your `init.el' file.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "vcdb" "vcdb.el" (20741 14138 0 0))
;;; Generated autoloads from vcdb.el

(autoload 'vcdb "vcdb" "\
Run vcdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads nil "vline" "vline.el" (20741 14138 0 0))
;;; Generated autoloads from vline.el

(autoload 'vline-mode "vline" "\
Display vertical line mode.

\(fn &optional ARG)" t nil)

(defvar vline-global-mode nil "\
Non-nil if Vline-Global mode is enabled.
See the command `vline-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vline-global-mode'.")

(custom-autoload 'vline-global-mode "vline" nil)

(autoload 'vline-global-mode "vline" "\
Toggle Vline mode in all buffers.
With prefix ARG, enable Vline-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Vline mode is enabled in all buffers where
`(lambda nil (unless (minibufferp) (vline-mode 1)))' would do it.
See `vline-mode' for more information on Vline mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "widget-demo" "widget-demo.el" (20741 14138
;;;;;;  0 0))
;;; Generated autoloads from widget-demo.el

(autoload 'widget-demo "widget-demo" "\
Show widget demo.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "winring" "winring.el" (20741 14138 0 0))
;;; Generated autoloads from winring.el

(autoload 'winring-new-configuration "winring" "\
Save the current window configuration and create an empty new one.
The buffer shown in the new empty configuration is defined by
`winring-new-config-buffer-name'.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name.

\(fn &optional ARG)" t nil)

(autoload 'winring-duplicate-configuration "winring" "\
Push the current window configuration on the ring, and duplicate it.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name.

\(fn &optional ARG)" t nil)

(autoload 'winring-next-configuration "winring" "\
Switch to the next window configuration for this frame.

\(fn)" t nil)

(autoload 'winring-prev-configuration "winring" "\
Switch to the previous window configuration for this frame.

\(fn)" t nil)

(autoload 'winring-jump-to-configuration "winring" "\
Go to the named window configuration.

\(fn)" t nil)

(autoload 'winring-delete-configuration "winring" "\
Delete the current configuration and switch to the next one.
With \\[universal-argument] prompt for named configuration to delete.

\(fn &optional ARG)" t nil)

(autoload 'winring-rename-configuration "winring" "\
Rename the current configuration to NAME.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads nil "xml-parse" "xml-parse.el" (20741 14140 0 0))
;;; Generated autoloads from xml-parse.el

(autoload 'read-xml "xml-parse" "\
Parse XML data at point into a Lisp structure.
See `insert-xml' for a description of the format of this structure.
Point is left at the end of the XML structure read.

\(fn &optional PROGRESS-CALLBACK)" nil nil)

(autoload 'insert-xml "xml-parse" "\
Insert DATA, a recursive Lisp structure, at point as XML.
DATA has the form:

  ENTRY       ::=  (TAG CHILD*)
  CHILD       ::=  STRING | ENTRY
  TAG         ::=  TAG_NAME | (TAG_NAME ATTR+)
  ATTR        ::=  (ATTR_NAME . ATTR_VALUE)
  TAG_NAME    ::=  STRING
  ATTR_NAME   ::=  STRING
  ATTR_VALUE  ::=  STRING

If ADD-NEWLINES is non-nil, newlines and indentation will be added to
make the data user-friendly.

If PUBLIC and SYSTEM are non-nil, a !DOCTYPE tag will be added at the
top of the document to identify it as an XML document.

DEPTH is normally for internal use only, and controls the depth of the
indentation.

\(fn DATA &optional ADD-NEWLINES PUBLIC SYSTEM DEPTH RET-DEPTH)" nil nil)

(autoload 'xml-reformat-tags "xml-parse" "\
If point is on the open bracket of an XML tag, reformat that tree.
Note that this only works if the opening tag starts at column 0.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "yasnippet-bundle" "yasnippet-bundle.el" (20741
;;;;;;  14138 0 0))
;;; Generated autoloads from yasnippet-bundle.el

(defvar yas/root-directory nil "\
Root directory that stores the snippets for each major mode.

If you set this from your .emacs, can also be a list of strings,
for multiple root directories. If you make this a list, the first
element is always the user-created snippets directory. Other
directories are used for bulk reloading of all snippets using
`yas/reload-all'")

(custom-autoload 'yas/root-directory "yasnippet-bundle" nil)

(autoload 'yas/minor-mode "yasnippet-bundle" "\
Toggle YASnippet mode.

When YASnippet mode is enabled, the `tas/trigger-key' key expands
snippets of code depending on the mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

You can customize the key through `yas/trigger-key'.

Key bindings:
\\{yas/minor-mode-map}

\(fn &optional ARG)" t nil)
(require 'yasnippet-bundle)

;;;***

;;;### (autoloads nil "yatexenv" "yatexenv.el" (20741 14138 0 0))
;;; Generated autoloads from yatexenv.el

(autoload 'YaTeX-what-column "yatexenv" "\
Show which kind of column the current position is belonging to.

\(fn)" t nil)

(autoload 'YaTeX-intelligent-newline "yatexenv" "\
Insert newline and environment-specific entry.
`\\item'	for some itemizing environment,
`\\> \\> \\'	for tabbing environemnt,
`& & \\ hline'	for tabular environment.

\(fn ARG)" t nil)

(autoload 'YaTeX-indent-line-equation "yatexenv" "\
Indent a line in equation family.

\(fn)" nil nil)

(autoload 'YaTeX-goto-corresponding-leftright "yatexenv" "\
Go to corresponding left or ight.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "yatexhlp" "yatexhlp.el" (20741 14138 0 0))
;;; Generated autoloads from yatexhlp.el

(autoload 'YaTeX-apropos "yatexhlp" "\


\(fn KEY)" t nil)

(autoload 'YaTeX-help "yatexhlp" "\
Show help buffer of LaTeX/TeX commands or macros.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "yatexlib" "yatexlib.el" (20741 14138 0 0))
;;; Generated autoloads from yatexlib.el

(autoload 'YaTeX-define-key "yatexlib" "\
Define key on YaTeX-prefix-map.

\(fn KEY BINDING &optional MAP)" nil nil)

(autoload 'YaTeX-local-table-symbol "yatexlib" "\
Return the lisp symbol which keeps local completion table of SYMBOL.

\(fn SYMBOL)" nil nil)

(autoload 'YaTeX-sync-local-table "yatexlib" "\
Synchronize local variable SYMBOL.
Copy its corresponding directory dependent completion table to SYMBOL.

\(fn SYMBOL)" nil nil)

(autoload 'YaTeX-read-user-completion-table "yatexlib" "\
Append user completion table of LaTeX macros

\(fn &optional FORCETOREAD)" t nil)

(autoload 'YaTeX-reload-dictionary "yatexlib" "\
Reload local dictionary.
Use this function after editing ./.yatexrc.

\(fn)" t nil)

(autoload 'YaTeX-lookup-table "yatexlib" "\
Lookup WORD in completion table whose type is TYPE.
This function refers the symbol tmp-TYPE-table, user-TYPE-table, TYPE-table.
Typically, TYPE is one of 'env, 'section, 'fontsize, 'singlecmd.

\(fn WORD TYPE)" nil nil)

(autoload 'YaTeX-update-table "yatexlib" "\
Update completion table if the car of VALLIST is not in current tables.
Second argument DEFAULT-TABLE is the quoted symbol of default completion
table, third argument USER-TABLE is user table which will be saved in
YaTeX-user-completion-table, fourth argument LOCAL-TABLE should have the
completion which is valid during current Emacs's session.  If you
want to make LOCAL-TABLE valid longer span (but restrict in this directory)
create the file in current directory which has the same name with
YaTeX-user-completion-table.

\(fn VALLIST DEFAULT-TABLE USER-TABLE LOCAL-TABLE)" nil nil)

(autoload 'YaTeX-cplread-with-learning "yatexlib" "\
Completing read with learning.
Do a completing read with prompt PROM.  Completion table is what
DEFAULT-TABLE, USER-TABLE, LOCAL table are appended in reverse order.
Note that these tables are passed by the symbol.
Optional arguments PRED, REQMATH and INIT are passed to completing-read
as its arguments PREDICATE, REQUIRE-MATCH and INITIAL-INPUT respectively.
If optional 8th argument HSYM, history symbol, is passed, use it as
history list variable.

\(fn PROM DEFAULT-TABLE USER-TABLE LOCAL-TABLE &optional PRED REQMATCH INIT HSYM)" nil nil)

(autoload 'YaTeX-update-dictionary "yatexlib" "\


\(fn FILE SYMBOL &optional TYPE)" nil nil)

(autoload 'YaTeX-define-begend-key-normal "yatexlib" "\
Define short cut YaTeX-make-begin-end key.

\(fn KEY ENV &optional MAP)" nil nil)

(autoload 'YaTeX-define-begend-region-key "yatexlib" "\
Define short cut YaTeX-make-begin-end-region key.

\(fn KEY ENV &optional MAP)" nil nil)

(autoload 'YaTeX-define-begend-key "yatexlib" "\
Define short cut key for begin type completion both for normal
and region mode.  To customize YaTeX, user should use this function.

\(fn KEY ENV &optional MAP)" nil nil)

(autoload 'YaTeX-search-active-forward "yatexlib" "\
Search STRING which is not commented out by CMNTRX.
Optional arguments after BOUND, ERR, CNT are passed literally to search-forward
or search-backward.
Optional sixth argument FUNC changes search-function.

\(fn STRING CMNTRX &optional BOUND ERR CNT FUNC)" nil nil)

(autoload 'YaTeX-switch-to-buffer "yatexlib" "\
Switch to buffer if buffer exists, find file if not.
Optional second arg SETBUF t make use set-buffer instead of switch-to-buffer.

\(fn FILE &optional SETBUF)" t nil)

(autoload 'YaTeX-switch-to-buffer-other-window "yatexlib" "\
Switch to buffer if buffer exists, find file if not.

\(fn FILE)" t nil)

(autoload 'YaTeX-replace-format "yatexlib" "\
In STRING, replace first appearance of FORMAT to REPL as if
function `format' does.  FORMAT does not contain `%'

\(fn STRING FORMAT REPL)" nil nil)

(autoload 'YaTeX-replace-formats "yatexlib" "\


\(fn STRING REPLACE-LIST)" nil nil)

(autoload 'YaTeX-replace-format-args "yatexlib" "\
Translate the argument mark #1, #2, ... #n in the STRING into the
corresponding real arguments ARGS.

\(fn STRING &rest ARGS)" nil nil)

(autoload 'rindex "yatexlib" "\


\(fn STRING CHAR)" nil nil)

(autoload 'point-beginning-of-line "yatexlib" "\


\(fn)" nil nil)

(autoload 'point-end-of-line "yatexlib" "\


\(fn)" nil nil)

(autoload 'YaTeX-showup-buffer "yatexlib" "\
Make BUFFER show up in certain window (but current window)
that gives the maximum value by the FUNC.  FUNC should take an argument
of its window object.  Non-nil for optional third argument SELECT selects
that window.  This function never selects minibuffer window.

\(fn BUFFER &optional FUNC SELECT)" nil nil)

(autoload 'split-window-calculate-height "yatexlib" "\
Split current window wight specified HEIGHT.
If HEIGHT is number, make a new window that has HEIGHT lines.
If HEIGHT is string, make a new window that occupies HEIGT % of screen height.
Otherwise split window conventionally.

\(fn HEIGHT)" nil nil)

(autoload 'YaTeX-window-list "yatexlib" "\


\(fn)" nil nil)

(autoload 'substitute-all-key-definition "yatexlib" "\
Replace recursively OLDDEF with NEWDEF for any keys in KEYMAP now
defined as OLDDEF. In other words, OLDDEF is replaced with NEWDEF
where ever it appears.

\(fn OLDDEF NEWDEF KEYMAP)" nil nil)

(autoload 'YaTeX-match-string "yatexlib" "\
Return (buffer-substring (match-beginning n) (match-beginning m)).

\(fn N &optional M)" nil nil)

(autoload 'YaTeX-minibuffer-complete "yatexlib" "\
Complete in minibuffer.
  If the symbol 'delim is bound and is string, its value is assumed to be
the character class of delimiters.  Completion will be performed on
the last field separated by those delimiters.
  If the symbol 'quick is bound and is 't, when the try-completion results
in t, exit minibuffer immediately.

\(fn)" t nil)

(autoload 'completing-read-with-history "yatexlib" "\
Completing read with general history: gmhist, Emacs-19.

\(fn PROMPT TABLE &optional PREDICATE MUST-MATCH INITIAL HSYM)" nil nil)

(autoload 'read-from-minibuffer-with-history "yatexlib" "\
Read from minibuffer with general history: gmhist, Emacs-19.

\(fn PROMPT &optional INIT MAP READ HSYM)" nil nil)

(autoload 'read-string-with-history "yatexlib" "\
Read string with history: gmhist(Emacs-18) and Emacs-19.

\(fn PROMPT &optional INIT HSYM)" nil nil)

(fset 'YaTeX-rassoc (if (and nil (fboundp 'rassoc) (subrp (symbol-function 'rassoc))) (symbol-function 'rassoc) #'(lambda (key list) (let ((l list)) (catch 'found (while l (if (equal key (cdr (car l))) (throw 'found (car l))) (setq l (cdr l))))))))

(autoload 'YaTeX-delete1 "yatexlib" "\
Delete

\(fn ELT LIST)" nil nil)

(autoload 'YaTeX-switch-to-window "yatexlib" "\
Switch to windows.el's window decided by last pressed key.

\(fn)" t nil)

(autoload 'YaTeX-reindent "yatexlib" "\
Remove current indentation and reindento to COL column.

\(fn COL)" nil nil)

;;;***

;;;### (autoloads nil "yatexmth" "yatexmth.el" (21636 47123 62714
;;;;;;  523000))
;;; Generated autoloads from yatexmth.el

(autoload 'YaTeX-toggle-math-mode "yatexmth" "\


\(fn &optional ARG)" t nil)

(autoload 'YaTeX-goto-corresponding-paren "yatexmth" "\
Go to corresponding mathematical parentheses.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "yatexsec" "yatexsec.el" (20741 14136 0 0))
;;; Generated autoloads from yatexsec.el

(autoload 'YaTeX-read-section-in-minibuffer "yatexsec" "\


\(fn PROMPT TABLE &optional DEFAULT DELIM)" t nil)

(autoload 'YaTeX-make-section-with-overview "yatexsec" "\
Input sectining command with previous overview.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("auto-complete.el" "auto-install.el" "autopair.el"
;;;;;;  "balanced.el" "bazaar.el" "bison-mode.el" "camelCase.el"
;;;;;;  "camelcase-mode.el" "chm-view.el" "color-grep.el" "comment.el"
;;;;;;  "company-bundled-completions.el" "cursor-chg.el" "cygwin-mount.el"
;;;;;;  "dbg-test.el" "dired-explore.el" "dired-isearch.el" "dired-search.el"
;;;;;;  "doc-mode.el" "doxygen.el" "elk-test.el" "eol-conversion.el"
;;;;;;  "eproject-config.el" "eproject-pkg.el" "find-files.el" "flex-mode.el"
;;;;;;  "flyspell-timer.el" "gdb-highlight.el" "globalff.el" "gtypist-mode.el"
;;;;;;  "hide-lines.el" "hidesearch.el" "html-font.el" "html-helper-mode.el"
;;;;;;  "htmlr.el" "inf-ruby.el" "isearch-outline.el"
;;;;;;  "keywiz.el" "list-register.el" "llvm-mode.el" "make-regexp.el"
;;;;;;  "markerpen.el" "mk-project.el" "no-word.el" "out-xtra.el"
;;;;;;  "palette.el" "paredit.el" "perlnow.el" "popup-kill-ring.el"
;;;;;;  "popup.el" "pos-tip.el" "powerkey.el" "powershell-mode.el"
;;;;;;  "project-local-variables.el" "regex-tool.el" "ri.el" "ring+.el"
;;;;;;  "ruby-electric.el" "ruby-style.el" "sidebrain-custom.el"
;;;;;;  "sidebrain-debug.el" "sidebrain-effort.el" "sidebrain-macros.el"
;;;;;;  "sidebrain-menu.el" "sidebrain-projects.el" "sidebrain-vars.el"
;;;;;;  "sidebrain-voice.el" "sidebrain.el" "source-safe.el" "sr-speedbar.el"
;;;;;;  "subdirs.el" "sudo-ext.el" "tablegen-mode.el" "type-test.el"
;;;;;;  "vbnet-mode.el" "visual-basic-mode.el" "visws.el" "vvb-mode.el"
;;;;;;  "w32-browser.el" "w32-symlinks.el" "window-numbering.el"
;;;;;;  "workgroups.el" "yacc.el" "yahtml.el" "yasnippet.el" "yatex.el"
;;;;;;  "yatex19.el" "yatexadd.el" "yatexgen.el" "yatexhie.el" "yatexhks.el"
;;;;;;  "yatexm-o.el" "yatexpkg.el" "yatexprc.el") (21636 46785 106069
;;;;;;  150000))

;;;***

(provide 'loaddefs-local-site-lisp)
;; Local Variables:
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs-local-site-lisp.el ends here
