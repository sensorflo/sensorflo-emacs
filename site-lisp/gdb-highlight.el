;;; gdb-highlight.el --- make gdb buffers be mouse-sensitive.
;;; Copyright (C) 1997 Jamie Zawinski <jwz@netscape.com>

;; Author: Jamie Zawinski <jwz@netscape.com>
;; Created: 16-Apr-1997
;; Version: 1.2  (17-May-97)
;; Keywords: extensions, c, unix, tools, debugging
;;
;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:
;;
;;    This package makes most objects printed in a *gdb* buffer be
;;    mouse-sensitive: as text shows up in the buffer, it is parsed,
;;    and objects which are recognized have context-sensitive commands
;;    attached to them.  Generally, the types that are noticed are:
;;
;;      = function and method names;
;;      = variable and parameter names;
;;      = structure and object slots;
;;      = source file names;
;;      = type names;
;;      = breakpoint numbers;
;;      = stack frame numbers.
;;
;;    Any time one of those objects is presented in the *gdb* buffer,
;;    it will be mousable.  Clicking middle mouse button (button2) on
;;    it will take some default action -- edit the function, select
;;    the stack frame, disable the breakpoint, etc.  Clicking the right
;;    mouse button (button3) will bring up a menu of commands, including
;;    commands specific to the object under the mouse, or other objects
;;    on the same line.
;;
;;    In addition to these context-sensitive commands are more general
;;    gdb commands which were previously inaccessible via the mouse
;;    (listing breakpoints, returning values, etc); and the general
;;    comint/shell-buffer commands which had been present before.
;;
;;    If you notice an object being presented which could (usefully)
;;    be made mouse sensitive, but which currently is not, please let 
;;    me know.

;;; Installation:
;;
;;    To install, add this to your .emacs file:
;;        (add-hook 'gdb-mode-hook '(lambda () (require 'gdb-highlight)))

;;; TODO:
;;
;;    = It doesn't really work very well unless you've done `set width 0'
;;      in your .gdbinit.  It would be nice if this were fixed.
;;	(And with `set width 0', `set print pretty on' is the way to go.)
;;
;;    = In some contexts, the toggle-breakpoint command doesn't work,
;;      because this code doesn't know whether it's enabled.  It should
;;      remember, or figure it out, or something.
;;
;;    = Make it possible to edit the `keep' state of breakpoints.
;;
;;    = Is it useful to make addresses clickable?  If an address is
;;      always acompanied by a variable, then no.
;;
;;    = There has got to be a better way to implement `gdb-guess-file-name'.
;;
;;    = Make some new toolbar icons and put the most common commands on it.
;;
;;    = Maybe make gdb-toolbar-clear work more reliably by consulting a
;;      breakpoint-number extent?
;;
;;    = I want breakpoint icons in my source files, just like in Energize.
;;
;;    = Add a command to quit-and-restart the debugger, with the same
;;      breakpoints and program-arguments.  (This wouldn't be interesting
;;      if gdb didn't leak like a sieve...)
;;
;;    = Figure out some way to realize when extents are no longer interesting
;;      (stack frames and local variables that are no longer on the stack)
;;      and make them no longer be mousable.  This is tricky...  Nuke them
;;      whenever a "run" command is seen?
;;
;;    = Make C-x SPC in a source buffer use gdb-menu-command so that it will
;;      interrupt-and-continue the debugged program as necessary.
;;
;;    = Do stuff for watchpoints (but I never use them, myself.)

;;; WISHLIST:
;;
;;         (extracted from my 13-May-1997 message to comp.emacs and
;;         comp.emacs.xemacs, news:33785828.5A524730@netscape.com)
;;
;;    6.1. Make gdbsrc-mode not suck.
;;
;;         The idea behind gdbsrc-mode is on the side of the angels: one
;;         should be able to focus on the source code and not on the
;;         debugger buffer, absolutely.  But the implementation is just
;;         awful.
;;
;;         First and foremost, it should not change "modes" (in the more
;;         general sense).  Any commands that it defines should be on
;;         keys which are exclusively used for that purpose, not keys
;;         which are normally self-inserting.  I can't be the only person
;;         who usually has occasion to actually *edit* the sources which
;;         the debugger has chosen to display!  Switching into and out of
;;         gdbsrc-mode is prohibitive.
;;
;;         I want to be looking at my sources at all times, yet I don't
;;         want to have to give up my source-editing gestures.  I think
;;         the right way to accomplish this is to put the gdbsrc commands
;;         on the toolbar and on popup menus; or to let the user define
;;         their own keys (I could see devoting my kp_enter key to
;;         "step", or something common like that.)
;;
;;         Also it's extremely frustrating that one can't turn off gdbsrc
;;         mode once it has been loaded, without exiting and restarting
;;         emacs; that alone means that I'd probably never take the time
;;         to learn how to use it, without first having taken the time to
;;         repair it...
;;
;;    6.2. Make it easier access to variable values.
;;
;;         I want to be able to double-click on a variable name to
;;         highlight it, and then drag it to the debugger window to have
;;         its value printed.
;;
;;         I want gestures that let me write as well as read: for
;;         example, to store value A into slot B.
;;
;;    6.3. Make all breakpoints visible.
;;
;;         Any time there is a running gdb which has breakpoints, the
;;         buffers holding the lines on which those breakpoints are set
;;         should have icons in them.  These icons should be context-
;;         sensitive: I should be able to pop up a menu to enable or
;;         disable them, to delete them, to change their commands or
;;         conditions.
;;
;;         I should also be able to MOVE them.  It's annoying when you
;;         have a breakpoint with a complex condition or command on it,
;;         and then you realize that you really want it to be at a
;;         different location.  I want to be able to drag-and-drop the
;;         icon to its new home.
;;         
;;    6.4. Make a debugger status display window.
;;
;;         o  I want a window off to the side that shows persistent
;;            information -- it should have a pane which is a
;;            drag-editable, drag-reorderable representation of the
;;            elements on gdb's "display" list; they should be displayed
;;            here instead of being just dumped in with the rest of the
;;            output in the *gdb* buffer.
;;
;;         o  I want a pane that displays the current call-stack and
;;            nothing else.  I want a pane that displays the arguments
;;            and locals of the currently-selected frame and nothing
;;            else.  I want these both to update as I move around on the
;;            stack.
;;
;;            Since the unfortunate reality is that excavating this
;;            information from gdb can be slow, it would be a good idea
;;            for these panes to have a toggle button on them which meant
;;            "stop updating", so that when I want to move fast, I can,
;;            but I can easily get the display back when I need it again.
;;
;;         The reason for all of this is that I spend entirely too much
;;         time scrolling around in the *gdb* buffer; with gdb-highlight,
;;         I can just click on a line in the backtrace output to go to
;;         that frame, but I find that I spend a lot of time *looking*
;;         for that backtrace: since it's mixed in with all the other
;;         random output, I waste time looking around for things (and
;;         usually just give up and type "bt" again, then thrash around
;;         as the buffer scrolls, and I try to find the lower frames that
;;         I'm interested in, as they have invariably scrolled off the
;;         window already...
;;
;;    6.5. Save and restore breakpoints across emacs/debugger sessions.
;;
;;         This would be especially handy given that gdb leaks like a
;;         sieve, and with a big program, I only get a few dozen
;;         relink-and-rerun attempts before gdb has blown my swap space.
;;
;;    6.6. Keep breakpoints in sync with source lines.
;;
;;         When a program is recompiled and then reloaded into gdb, the
;;         breakpoints often end up in less-than-useful places.  For
;;         example, when I edit text which occurs in a file anywhere
;;         before a breakpoint, emacs is aware that the line of the bp
;;         hasn't changed, but just that it is in a different place
;;         relative to the top of the file.  Gdb doesn't know this, so
;;         your breakpoints end up getting set in the wrong places
;;         (usually the maximally inconvenient places, like *after* a
;;         loop instead of *inside* it).  But emacs knows, so emacs
;;         should inform the debugger, and move the breakpoints back to
;;         the places they were intended to be.
;;
;;     (Possibly the OOBR stuff does some of this, but can't tell,
;;     because I've never been able to get it to do anything but beep at
;;     me and mumble about environments.  I find it pretty funny that the
;;     manual keeps explaining to me how intuitive it is, without
;;     actually giving me a clue how to launch it...)


;;; Code:
;;
;; This code should be considered an example of how over-use of regular
;; expressions leads to code that is an unreadable, unmaintainable mess,
;; and why it's unfortunate that so much of emacs's speed depends on
;; their use, rather than on the use of more traditional parsers.

;(require 'gdb)

(define-key gdb-mode-map 'button3 'gdb-popup-menu)
(defvar gdb-popup-menu
  '("GDB Commands"
    ["Up Stack"			(gdb-menu-command "up" t)		t]
    ["Down Stack"		(gdb-menu-command "down" t)		t]
    ["Next Line"		(gdb-menu-command "next" t)		t]
    ["Next Line (Step In)"	(gdb-menu-command "step" t)		t]
    ["Continue"			(gdb-menu-command "continue" t)		t]
    ["Continue Until Return"	(gdb-menu-command "finish" t)		t]
    ("Return..."
     ["Return"			(gdb-menu-command "return" t)		t]
     ["Return 0"		(gdb-menu-command "return 0" t)		t]
     ["Return 1"		(gdb-menu-command "return 1" t)		t]
     ["Return -1"		(gdb-menu-command "return -1" t)	t]
     ["Return $"		(gdb-menu-command "return $" t)		t]
     )
    "---"
    ["Backtrace"		(gdb-menu-command "backtrace" t)	t]
    ["List Breakpoints"		(gdb-menu-command "info breakpoints" t)	t]
    ["List Local Variables"	(gdb-menu-command "info locals" t)	t]
    )
  "Commands for the popup menu in gdb-mode.
The comint-popup-menu is appended to this, and certain context-sensitive
commands may be prepended to it, depending on the location of the mouse
when the `gdb-popup-menu' command is invoked.")


;;; Faces and keymaps used for mousable tokens in the *gdb* buffer.

(defvar gdb-highlight-face		'gdb-highlight-face)  ; the base face
(defvar gdb-breakpoint-number-face	'gdb-breakpoint-number-face)
;(defvar gdb-breakpoint-keep-face	'gdb-breakpoint-keep-face)
(defvar gdb-breakpoint-enabled-face	'gdb-breakpoint-enabled-face)
(defvar gdb-function-name-face		'gdb-function-name-face)
(defvar gdb-function-location-face	'gdb-function-location-face)
(defvar gdb-variable-name-face		'gdb-variable-name-face)
(defvar gdb-type-name-face		'gdb-type-name-face)

(make-face 'gdb-highlight-face)
(or (face-differs-from-default-p 'gdb-highlight-face)
    (make-face-italic 'gdb-highlight-face))

(let ((faces '(gdb-breakpoint-number-face
	       gdb-breakpoint-enabled-face
	       ;gdb-breakpoint-keep-face
	       gdb-function-name-face
	       gdb-function-location-face
	       gdb-variable-name-face
	       gdb-type-name-face)))
  (while faces
    (make-face (car faces))
    (or (face-differs-from-default-p (car faces))
	(if (fboundp 'set-face-parent)
	    (set-face-parent (car faces) 'gdb-highlight-face)
	  (copy-face 'gdb-highlight-face (car faces))))
    (setq faces (cdr faces))))


(defvar gdb-token-map			; the base map, inherited by all.
  (let ((m (make-sparse-keymap)))
    (set-keymap-name m 'gdb-token-map)
    (define-key m 'button2 'undefined)
    ;;(define-key m 'button3 'gdb-token-popup)
    m))

(defvar gdb-breakpoint-number-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-name m 'gdb-breakpoint-number-map)
    (set-keymap-parent m gdb-token-map)
    ;; not sure if this is the most useful binding... maybe "delete" is better?
    (define-key m 'button2 'gdb-mouse-disable-breakpoint)
    m))

(defvar gdb-info-breakpoint-number-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-name m 'gdb-breakpoint-number-map)
    (set-keymap-parent m gdb-token-map)
    ;; not sure if this is the most useful binding... maybe "delete" is better?
    (define-key m 'button2 'gdb-mouse-toggle-breakpoint-enabled)
    m))

;(defvar gdb-breakpoint-keep-map
;  (let ((m (make-sparse-keymap)))
;    (set-keymap-name m 'gdb-breakpoint-keep-map)
;    (set-keymap-parent m gdb-token-map)
;    (define-key m 'button2 'gdb-token-mouse-toggle-keep)
;    m))

(defvar gdb-breakpoint-enabled-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-name m 'gdb-breakpoint-enabled-map)
    (set-keymap-parent m gdb-token-map)
    (define-key m 'button2 'gdb-mouse-toggle-breakpoint-enabled)
    m))

(defvar gdb-function-name-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-name m 'gdb-function-name-map)
    (set-keymap-parent m gdb-token-map)
    (define-key m 'button2 'gdb-mouse-edit-function)
    m))

(defvar gdb-function-location-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-name m 'gdb-function-location-map)
    (set-keymap-parent m gdb-token-map)
    (define-key m 'button2 'gdb-mouse-edit-function-location)
    m))

(defvar gdb-frame-number-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-name m 'gdb-frame-number-map)
    (set-keymap-parent m gdb-token-map)
    (define-key m 'button2 'gdb-mouse-goto-frame)
    m))

(defvar gdb-variable-name-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-name m 'gdb-variable-name-map)
    (set-keymap-parent m gdb-token-map)
    (define-key m 'button2 'gdb-mouse-print-variable)
    m))

(defvar gdb-type-name-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-name m 'gdb-type-name-map)
    (set-keymap-parent m gdb-token-map)
    (define-key m 'button2 'gdb-mouse-print-type)
    m))


;;; Token definitions.

;; These properties enumerate the faces and keymaps that will be put over
;; the tokens.

(put 'gdb-frame-number-token      'gdb-token-face   gdb-breakpoint-number-face)
(put 'gdb-frame-number-token      'gdb-token-keymap gdb-frame-number-map)

;(put 'gdb-breakpoint-keep-token  'gdb-token-face   gdb-breakpoint-keep-face)
;(put 'gdb-breakpoint-keep-token  'gdb-token-keymap gdb-breakpoint-keep-map)

(put 'gdb-enabled-token           'gdb-token-face  gdb-breakpoint-enabled-face)
(put 'gdb-enabled-token           'gdb-token-keymap gdb-breakpoint-enabled-map)

(put 'gdb-function-name-token     'gdb-token-face   gdb-function-name-face)
(put 'gdb-function-name-token     'gdb-token-keymap gdb-function-name-map)

(put 'gdb-function-location-token 'gdb-token-face   gdb-function-location-face)
(put 'gdb-function-location-token 'gdb-token-keymap gdb-function-location-map)

(put 'gdb-breakpoint-number-token 'gdb-token-face   gdb-breakpoint-number-face)
(put 'gdb-breakpoint-number-token 'gdb-token-keymap gdb-breakpoint-number-map)
(put 'gdb-info-breakpoint-number-token 'gdb-token-face
						    gdb-breakpoint-number-face)
(put 'gdb-info-breakpoint-number-token 'gdb-token-keymap
					        gdb-info-breakpoint-number-map)

(put 'gdb-frame-number-token      'gdb-token-face   gdb-breakpoint-number-face)
(put 'gdb-frame-number-token      'gdb-token-keymap gdb-frame-number-map)

(put 'gdb-variable-name-token     'gdb-token-face   gdb-variable-name-face)
(put 'gdb-variable-name-token     'gdb-token-keymap gdb-variable-name-map)

(put 'gdb-type-name-token         'gdb-token-face   gdb-type-name-face)
(put 'gdb-type-name-token         'gdb-token-keymap gdb-type-name-map)


;;; These regular expressions control what text corresponds to which tokens.

(defconst gdb-highlight-token-patterns
  ;; "May god forgive me for what I have unleashed." -- Evil Dead II.
  (purecopy
   (list
    ;; Breakpoints output:
    ;;
    ;; Breakpoint 5, XCreateWindow () at Window.c:136
    ;; Breakpoint 6, foobar (x=0x7fff3000 "baz") at blorp.c:5382
    ;;
    (list (concat "\\(Breakpoint "				; 1
		    "\\([0-9]+\\)"				; .2
		  "\\), "					; 1
		  "\\(0x[0-9a-fA-F]+ in \\)?"			; 3
		  "\\("						; 4
		    "\\([a-zA-Z0-9_]+\\):[a-zA-Z0-9_:~]+"	; .5
		    "\\|"					; .
		    "[a-zA-Z0-9_]+"				; .
		  "\\)"						; 4
		  "\\("						; 6
		    " *\\((.*)\\)"				; .7
		    " at \\("					; .8
		      "\\([^ \t\n:]+\\):"			; ..9
		      "\\([0-9]+\\)"				; ..10
		    "\\)"					; .8
		  "\\)?"					; 6
		  )
	  '(gdb-breakpoint-number-token				; 1
	    nil							; 2
	    nil							; 3
	    gdb-function-name-token				; 4 (+5)
	    gdb-type-name-token					; 5
	    nil							; 6
	    gdb-arglist-token					; 7
	    gdb-function-location-token				; 8 (9+10)
	    ))

    ;; Output of the "Break" command:
    ;;
    ;; Breakpoint 1 at 0x4881d4
    ;; Breakpoint 6 at 0xfa50f68: file cuexit.c, line 58.
    ;;
    (list (concat "\\(Breakpoint "				; 1
		    "\\([0-9]+\\)"				; .2
		  "\\) at "					; 1
		  "\\(0x[0-9A-Fa-f]+\\)"			; 3
		  "\\(: file "					; 4
		    "\\("					; .5
		      "\\([^ \t\n:]+\\)"			; ..6
		      ", line \\([0-9]+\\)"			; ..7
		    "\\)"					; .5
		  "\\)?"					; 4
		  )
	  '(gdb-breakpoint-number-token				; 1
	    nil							; 2
	    nil ;gdb-address-token				; 3
	    nil							; 4
	    gdb-function-location-token				; 5 (6+7)
	    ))

    ;; Note: breakpoint 5 (disabled) also set at pc 0x40b420.
    ;; Note: breakpoint 5 also set at pc 0x40b420.
    ;;
    (list (concat "Note: "					; 
		  "\\(breakpoint "				; 1
		    "\\([0-9]+\\)"				; .2
		  "\\)"						; 1
		  )
	  '(gdb-breakpoint-number-token				; 1
	    nil							; 2
	    ))

    ;; Stack Frames:
    ;;
    ;; 0xe1b8e0 in _OS_SELECT () at os_IRIX.s:50
    ;; XCreateWindow () at Window.c:136
    ;; #0  0x8e0db0 in _OS_SELECT () at os_IRIX.s:50
    ;; #0  XCreateWindow () at Window.c:136
    ;; Run till exit from #0  __ll_mul () at llmul.s:51
    ;;
    (list (concat "\\(Run till exit from \\)?"			; 1
		  "\\("						; 2
		    "#\\([0-9]+ *\\)"				; .3
		  "\\)?"					; 2
		  "\\("						; 4
		    "\\(0x[0-9A-Fa-f]+\\)"			; .5
		  " in +\\)?"					; 4
		  "\\("						; 6
		    "\\([a-zA-Z0-9_]+\\):[a-zA-Z0-9_:~]+"	; .7
		    "\\|"					; 6
		    "[a-zA-Z0-9_]+"				; 
		  "\\) ("					; 6
		  "\\("						; 8
		    "\\(.*\\)"					; .9
		      "\\bat \\("				; .10
		        "\\([^ \t\n:]+\\):"			; ..11
			"\\([0-9]+\\)"				; ..12
		      "\\)"					; .10
		    "\\)?"					; 8
		  )
	  '(nil							; 1
	    gdb-frame-number-token				; 2
	    nil							; 3
	    nil							; 4
	    nil ;gdb-address-token				; 5
	    gdb-function-name-token				; 6 (+7)
	    gdb-type-name-token					; 7
	    nil							; 8
	    gdb-arglist-token					; 9
	    gdb-function-location-token				; 10 (11+12)
	    ))

    ;; Info Breakpoints output:
    ;;
    ;; 1   breakpoint     keep y   0x0fa50f68 in exit at exit.c:58
    ;; 1   breakpoint     keep y   0x000a1b00  <exit+4>
    ;; 1   breakpoint     keep y   0x0fa429ac  <_write>
    ;; 6   breakpoint     keep y   0x00789490 in foo::bar(bad *) at x.cpp:99
    ;; 7   breakpoint     keep y   0x00789490  <foo::bar(bad *)+128>
    ;;
    (list (concat "\\([0-9]+ *\\) "				; 1
		  "\\(breakpoint *\\|watchpoint *\\) "		; 2
		  "\\(keep *\\|del *\\|dis *\\) "		; 3
		  "\\([yn] *\\) "				; 4
		  "\\(0x[0-9A-Fa-f]+\\) *"			; 5
		  "\\(in "					; 6
		    "\\("					; .7
		      "[a-zA-Z0-9_]+"				; ..
		      "\\|"					; .7
		      "\\([a-zA-Z0-9_]+\\):[a-zA-Z0-9_:~]+"	; ..8
		    "\\)"					; .7
		    "\\((.*)\\)?"				; 9
		    " at "					; .
		    "\\("					; .10
		      "\\([^ \t\n:]+\\):"			; ..11
		      "\\([0-9]+\\)"				; ..12
		    "\\)"					; .10
		  "\\|"						; 6
		    "<"						; .
		      "\\("					; .13
		        "\\([a-zA-Z0-9_]+\\):[a-zA-Z0-9_:~]+"	; ..14
		      "\\|"					; .13
		        "[a-zA-Z0-9_]+"				; ..
		      "\\)"					; .13
		    "\\((.*)\\)?"				; .15
		    "[^>\n]*>"					; .
		  "\\)?"					; 6
		  )
	  '(gdb-info-breakpoint-number-token			; 1
	    nil							; 2
	    nil ;gdb-breakpoint-keep-token			; 3
	    gdb-enabled-token					; 4
	    nil ;gdb-address-token				; 5
	    nil							; 6
	    gdb-function-name-token				; 7 (+8)
	    gdb-type-name-token					; 8
	    gdb-arglist-types-token				; 9
	    gdb-function-location-token				; 10 (11+12)
	    nil							; 11
	    nil							; 12
	    gdb-function-name-token				; 13
	    gdb-type-name-token					; 14
	    gdb-arglist-types-token				; 15
	    ))

    ;; Whatis and Ptype output:
    ;; type = struct _WidgetRec *
    ;; type = struct _WidgetRec {
    ;; type = int ()
    ;; type = struct <undefined> *(struct <undefined> *, void *, void (*)())
    ;; type = struct foo *(struct foo *, unsigned char, int)
    ;; type = unsigned int [4]
    ;;
    (list (concat "type = "
		  "\\("						; 1
		    "\\(signed \\|unsigned \\)?"		; .2
		    "\\(struct \\|class \\|union \\|enum \\)?"	; .3
		    "\\(<?[a-zA-Z_][a-zA-Z0-9_:]*>?\\)"		; .4
		  "\\)"						; 1
		  "[ *]*"					;
		  "\\("						; 5
		    "{?$\\|"					; .
		    "\\[[0-9]*\\]$\\|"				; .
		    "\\((.*)\\)"				; .6
		  "\\)"						; 5
		  )
	  '(gdb-type-name-token					; 1 (2+3+4)
	    nil							; 2
	    nil							; 3
	    nil							; 4
	    nil							; 5
	    gdb-arglist-types-token				; 6
	    ))

    ;; Ptype output:
    ;;     CorePart core;
    ;;     void *constraints;
    ;;     short x;
    ;;     unsigned short width;
    ;;     struct <undefined> *event_table;
    ;;     XtTMRec tm;
    ;;     void (*class_initialize)();
    ;;     unsigned char (*set_values)();
    ;;     unsigned char st_fstype[16];
    ;;     type = enum {XtGeometryYes, XtGeometryNo, XtGeometryAlmost}
    ;;
    (list (concat " *"
		  "\\("						; 1
		    "\\(signed \\|unsigned \\)?"		; .2
		    "\\(struct \\|class \\|union \\|enum \\)?"	; .3
		    "\\([a-zA-Z_][a-zA-Z0-9_:]*\\)"		; .4
		  "\\)"						; 1
		  "[ *]*"
		  "\\((\\**\\)?"				; 5
		  "\\([a-zA-Z_][a-zA-Z0-9_:]*\\)"		; 6
		  "\\()()\\)?"					; 7
		  "\\( *\\[[0-9]*\\]\\)?"			; 8
		  "; *$"
		  )
	  '(gdb-type-name-token					; 1 (2+3+4)
	    ))

    ;; Ptype output on C++ classes:
    ;;
    ;;     virtual foo (int);
    ;;     unsigned int foo(void);
    ;;     static long unsigned int * foo(bar *, baz *, unsigned int);
    ;;
    ;;   not handled:
    ;;     foo(bar *, _WidgetRec *, char const *, int);
    ;;     foo (foo &);
    ;;     foo & operator=(foo const &);
    ;;
    (list (concat " *"
		  "\\(static \\)?"				; 1
		  "\\("						; 2
		    "\\(signed \\|unsigned "			; .3
		       ;; #### not so sure about this:
		       "\\|long unsigned \\|short unsigned "	; .3
		    "\\)?"					; .3
		    "\\(struct \\|class \\|union \\|enum \\)?"	; .4
		    "\\([a-zA-Z_][a-zA-Z0-9_:]*\\)"		; .5
		  "\\)"						; 1
		  "[ *&]+"					; 
		  " *\\([a-zA-Z_][a-zA-Z0-9_:]*\\)"		; 6
		  " *\\((.*)\\)"				; 7
		  "; *$"					;
		  )
	  '(nil							; 1
	    gdb-type-name-token					; 2 (3+4+5)
	    nil							; 3
	    nil							; 4
	    nil							; 5
	    gdb-function-name-token				; 6
	    gdb-arglist-types-token				; 7
	    ))

    ;; Pointers to functions:
    ;;
    ;; $1 = {void ()} 0x4a1334 <fe_pulldown_cb>
    ;; $2 = (void (*)()) 0x4a1334 <fe_pulldown_cb>
    ;;
    (list (concat ".* = "
		  "[({]"
		  "\\("						; 1
		    "\\(signed \\|unsigned \\)?"		; .2
		    "\\(struct \\|class \\|union \\|enum \\)?"	; .3
		    "\\([a-zA-Z_][a-zA-Z0-9_:]*\\)"		; .4
		  "\\)"						; 1
		  " \\((\\*) ?\\)?"				; 5
		  "\\((.*)\\)"					; 6
		  "[)}] +"					;
		  "\\(0x[0-9A-Fa-f]+\\) +"			; 7
		  "<\\([a-zA-Z_~][a-zA-Z0-9_:]*\\)"		; 8
		  "\\+?[0-9]+?>"				; 
		  )
	  '(gdb-type-name-token					; 1 (2+3+4)
	    nil							; 2
	    nil							; 3
	    nil							; 4
	    nil							; 5
	    gdb-arglist-types-token				; 6
	    nil ;gdb-address-token				; 7
	    gdb-function-name-token				; 8
	    ))

    ;; Local variables and structures:
    ;;
    ;; shell = (struct _WidgetRec *) 0x10267350
    ;; delete_response = 270955344
    ;; allow_resize = 200 'È'
    ;; is_modal = 47 '/'
    ;; class_name = 0xf661d40 "TopLevelShell", 
    ;; static foo = 0x10791ec0, 
    ;; initialize = 0xf684770 <TopLevelInitialize>, 
    ;; av = {{
    ;;     name = "foo", 
    ;;     value = 270349836
    ;;   }, {
    ;;     name = 0x12 <Address 0x12 out of bounds>, 
    ;;     value = 0
    ;;   }, {
    ;;     name = 0x0, 
    ;;     value = 0
    ;;   }}
    ;;
    (list (concat " *"
		  "\\(static \\)?"				; 1
		  "\\([$a-zA-Z_][a-zA-Z0-9_:]*\\) = "		; 2
		  "\\(("					; 3
		    "\\("					; .4
		      "\\(signed \\|unsigned \\)?"		; ..5
		      "\\(struct \\|class \\|union \\|enum \\)?"; ..6
		      "\\([a-zA-Z_][a-zA-Z0-9_:]*\\)"		; ..7
		    "\\)"					; .4
		    "[ *]*)"					;
		  "\\)?"					; 3
		  "\\("						; 8
		    ".*"
		    " <\\([a-zA-Z_~][a-zA-Z0-9_:]*\\)"		; .9
		    "\\+?[0-9]+?>"				; .
		  "\\)?"					; 8
		  )
	  '(nil							; 1
	    gdb-variable-name-token				; 2
	    nil							; 3
	    gdb-type-name-token					; 4
	    nil							; 5
	    nil							; 6
	    nil							; 7
	    nil							; 8
	    gdb-function-name-token				; 9
	    ))

    ;; Purify output:
    ;;     UMR: Uninitialized memory read:
    ;;       * This is occurring while in:
    ;;          SHA1_Update    [algsha.c:137]
    ;;   * Reading 1 byte from 0xefffdb34 on the stack.
    (list (concat "[ \t]+"
		  "\\([a-zA-Z_~][a-zA-Z0-9_:]*\\)[ \t]*"	; 1
		  "\\[\\("					; 2
		      "\\([^ \t\n:]+\\):"			; .3
		      "\\([0-9]+\\)"				; .4
		    "\\)\\]"					; 2
		  )
	  '(gdb-function-name-token				; 1
	    gdb-function-location-token				; 2 (3+4)
	    ))

    ;; Purify output:
    ;;   * Address 0xefffdb34 is 36 bytes past start of local variable \
    ;;       "data" in function fe_EventForRNG.
    (list (concat ".*\\bAddress "
		  "\\(0x[0-9A-Fa-f]+\\) +"			; 1
		  ".*\\bvariable \""				;
		  "\\([a-zA-Z_][a-zA-Z0-9_:]*\\)\""		; 2
		  "\\("						; 3
		    ".*\\bfunction "				; .
		    "\\([a-zA-Z_~][a-zA-Z0-9_:]*\\)"		; .4
		  "\\)?"					; 3
		  )
	  '(nil ;gdb-address-token				; 1
	    gdb-variable-name-token				; 2
	    nil							; 3
	    gdb-function-name-token				; 4
	    ))
    ))
  "Patterns to highlight in gdb buffers.
Each element of this list takes the form
  ( \"regexp\" ( token-1 token-2 ... ))
where token-N is the token to place on the text matched
 by sub-pattern N in the match data.

The patterns should not begin with \"^\".")


(defun gdb-highlight-line ()
  "Highlight any tokens on the line which match gdb-highlight-token-patterns."
  (map-extents #'(lambda (e ignore) (delete-extent e))
	       nil
	       (point) (save-excursion (forward-line 1) (point))
	       nil nil 'gdb-token)
  (while (looking-at comint-prompt-regexp)
    (goto-char (match-end 0)))
  (if (eobp)
      nil
    (let ((tokens gdb-highlight-token-patterns)
	  (do-magic-variable-hack nil))
      (while tokens
	(if (not (looking-at (car (car tokens))))
	    (setq tokens (cdr tokens))
	  (let ((i 1)
		(types (nth 1 (car tokens))))
	    (if (eq (car types) 'gdb-variable-name-token)
		(setq do-magic-variable-hack t))
	    (while types
	      (cond ((not (and (car types)
			       (match-beginning i)))
		     nil)
		    ((memq (car types) '(gdb-arglist-token
					 gdb-arglist-types-token))
		     (gdb-highlight-arglist (car types)
					    (match-beginning i)
					    (match-end i)))
		    ((/= ?$ (char-after (match-beginning i)))
		     (gdb-highlight-token (car types)
					  (match-beginning i)
					  (match-end i))))
	      (setq i (1+ i)
		    types (cdr types)))

	    (if (not do-magic-variable-hack)
		;; we're done.
		(setq tokens nil)
	      ;; else, do a grody hack to cope with multiple variables
	      ;; on the same line.
	      (save-restriction
		(let ((p (point))
		      (ok nil))
		  (end-of-line)
		  (narrow-to-region p (point))
		  (goto-char (match-end 0))
		  (if (= (following-char) ?\{)
		      (progn
			(forward-char 1)
			(setq ok t))
		    (setq p (scan-sexps (point) 1 nil t))
		    (setq ok (if (null p)
				 nil
			       (goto-char p)
			       (if (or (= (following-char) ?\,)
				       (= (following-char) ?\}))
				   t
				 (setq p (scan-sexps (point) 1 nil t))
				 (if (null p)
				     nil
				   (goto-char p)
				   t)))))
		  (if ok
		      ;; skip over the comma and go around again.
		      (and (looking-at "}?[ \t]*,[ \t]*")
			   (goto-char (match-end 0)))
		    ;; saw something unexpected; give up on this line.
		    (setq tokens nil)))))
	    )))))
  nil)

(defun gdb-highlight-token (type start end)
  "Helper for gdb-highlight-line -- makes an extent for one matched token."
  (let ((e (make-extent start end)))
    (set-extent-property e 'gdb-token type)
    (set-extent-property e 'highlight 't)
    (set-extent-property e 'help-echo 'gdb-token-help-echo)
    (set-extent-property e 'face      (get type 'gdb-token-face))
    (set-extent-property e 'keymap    (get type 'gdb-token-keymap))
    e))

(defun gdb-highlight-arglist (type start end)
  "Helper for gdb-highlight-line. 
Makes extents for variables or types in an arglist."
  (save-match-data
    (save-excursion
      (goto-char end)
      (if (eq (preceding-char) ?\))
	  (setq end (1- end)))
      (goto-char start)
      (if (eq (following-char) ?\()
	  (forward-char 1))
      (set-extent-property (make-extent start end) 'gdb-token type)

      (cond
       ((eq type 'gdb-arglist-token)
	(let* ((pat1   "\\([a-zA-Z_][a-zA-Z0-9_:]*\\)=")
	       (pat2 ", \\([a-zA-Z_][a-zA-Z0-9_:]*\\)=")
	       (pat pat1))
	  (while(re-search-forward pat end t)
	    (gdb-highlight-token 'gdb-variable-name-token
				 (match-beginning 1) (match-end 1))
	    (cond ((looking-at
		    "0?x?[0-9A-Fa-f]+ <\\([a-zA-Z_~][a-zA-Z0-9_:]*\\)>")
		   (goto-char (match-end 0))
		   (gdb-highlight-token 'gdb-function-name-token
					(match-beginning 1) (match-end 1))))
	    (setq pat pat2))))

       ((eq type 'gdb-arglist-types-token)
	(let ((pat (eval-when-compile
		     (concat
		      "\\("						; 1
		        "\\(signed \\|unsigned \\)?"			; .2
			"\\(struct \\|class \\|union \\|enum \\)?"	; .3
			"\\(<?[a-zA-Z_~][a-zA-Z0-9_:]*>?\\)"		; .4
		      "\\)"						; 1
		      "[ *]*"
		      "\\((\\*) *(.*)\\)?"				; 5
		      ))))
	  (while (< (point) end)
	    (cond ((looking-at pat)
		   (goto-char (match-end 0))
		   (gdb-highlight-token 'gdb-type-name-token
					(match-beginning 1) (match-end 1))
		   (if (looking-at " *, *")
		       (goto-char (match-end 0))))
		  (t
		   ;; error -- try to cope...
		   (search-forward "," (1+ end) t))))))
       (t
	(error "unknown arglist type %s" type)))))
  nil)

(defun gdb-token-help-echo (extent)
  "Used as the 'mouse-help property of gdb-token extents,
to describe the binding on button2."
  (let* ((map (extent-property extent 'keymap))
	 (key 'button2)
	 (fn (and map (lookup-key map key)))
	 (doc (and fn (symbolp fn)
		   (if (fboundp fn)
		       (format "%s: %s" key (documentation fn))
		     (format "Error: %s is undefined" fn)))))
    (if doc
	(save-match-data
	  (if (string-match "\n" doc)
	      (setq doc (substring doc 0 (match-beginning 0))))))
    (or doc
	(concat "Error: no doc for "
		(symbol-name (extent-property extent 'gdb-token))))))

(defun gdb-get-line-token-extents (tokens)
  "Given a list of gdb-tokens, returns this line's extents of those types.
The returned value is a list of the same length as the `tokens' list, with
the corresponding extents in the corresponding positions.  If an extent
isn't found, nil is placed in the result-list instead."
  (setq tokens (append tokens nil))
  (let* ((result (make-list (length tokens) nil)))
    (save-excursion
      (beginning-of-line)
      (map-extents #'(lambda (e ignore)
		       (let ((type (extent-property e 'gdb-token))
			     (r1 tokens)
			     (r2 result))
			 (while r1
			   (cond ((and (car r1) (eq type (car r1)))
				  (setcar r1 nil)
				  (setcar r2 e)
				  (setq r1 nil)))
			   (setq r1 (cdr r1)
				 r2 (cdr r2))))
		       nil)
		   nil
		   (point)
		   (progn (forward-line 1) (point))
		   nil nil
		   'gdb-token)
      result)))


;;; Remembering directory names.
;;; gdb and gdb-mode conspire to hide from us the full file names of things
;;; that are presented into the buffer; this is an attempt to circumvent that.

(defvar gdb-highlight-last-directory nil)
(defvar gdb-highlight-last-directory-table nil)

(defun gdb-highlight-remember-directory ()
  ;; When gdb deigns to give us a full pathname, and it's in a different
  ;; directory than last time, cache it away on one of the nearby gdb-token
  ;; extents.  (We intern it to avoid hanging on to a lot of strings.)
  (cond ((and (boundp 'gdb-last-frame)
	      (car gdb-last-frame))
	 (cond ((not gdb-highlight-last-directory-table)
		(set (make-local-variable 'gdb-highlight-last-directory) nil)
		(set (make-local-variable 'gdb-highlight-last-directory-table)
		     (make-vector 211 0))))
	 (let ((dir (file-name-directory (car gdb-last-frame))))
	   (setq dir (intern dir gdb-highlight-last-directory-table))
	   (cond ((not (eq dir gdb-highlight-last-directory))
		  (let ((extent (previous-extent (current-buffer))))
		    (setq gdb-highlight-last-directory dir)
		    (while extent
		      (cond ((extent-property extent 'gdb-token)
			     (set-extent-property extent 'gdb-directory dir)
			     (setq extent nil))
			    (t
			     (setq extent (previous-extent extent))))))))))))

(defun gdb-guess-directory ()
  "Guess what directory gdb was talking about when it wrote the current line."
  (let ((extent (or (map-extents #'(lambda (e ignore) e)
				 (current-buffer) (point) (point-max))
		    (previous-extent (current-buffer))
		    (error "no extents")))
	(dir nil))
    (while extent
      (setq dir (extent-property extent 'gdb-directory))
      (if dir
	  (setq extent nil)
	(setq extent (previous-extent extent))))
    (if dir
	(symbol-name dir)
      default-directory)))

(defun gdb-guess-file-name (file)
  "Given a directoryless file name printed by gdb, find the file.
First it tries to expand the file relative to `gdb-guess-directory',
and if the resultant file doesn't exist, it tries every other directory
gdb has ever told us about, in no particular order."
  (abbreviate-file-name
   (if (file-name-absolute-p file)
       file
     (let ((file2 (expand-file-name file (gdb-guess-directory))))
       (if (file-exists-p file2)
	   file2
	 ;; Oh boy, gdb didn't tell us what directory it's in.
	 ;; A-hunting we will go.
	 (if (catch 'done
	       (mapatoms #'(lambda (dir)
			     (setq file2 (expand-file-name file
							   (symbol-name dir)))
			     (if (file-exists-p file2)
				 (throw 'done t)))
			 gdb-highlight-last-directory-table)
	       nil)
	     file2
	   (expand-file-name file)))))))


;;; Commands which are invoked from bindings in the keymaps of the tokens.

(defun gdb-mouse-toggle-breakpoint-enabled (event &optional what)
  "Toggle whether the breakpoint is enabled.
Looks for a gdb-breakpoint extent on the line under the mouse,
and executes an `enable' or `disable' command as appropriate.
Optional arg `what' may be 'enable, 'disable, or 'toggle (default.)"
  (interactive "@*e")
  (let (number target enabled-p)
    (save-excursion
      (mouse-set-point event)
      (let* ((extents (gdb-get-line-token-extents
		       '(gdb-breakpoint-number-token
			 gdb-info-breakpoint-number-token
			 gdb-enabled-token)))
	     (be (or (nth 0 extents) (nth 1 extents)))
	     (ee (nth 2 extents)))

	(or be
	    (error "no breakpoint-number extent on this line"))
	(setq number
	      (buffer-substring (extent-start-position be)
				(extent-end-position be)))
	(if (string-match " [0-9]+\\'" number)
	    (setq number (substring number (1+ (match-beginning 0)))))
	(setq number (string-to-int number))
	(or (> number 0)
	    (error "couldn't find breakpoint number"))
	(if (null ee)
	    (setq enabled-p 'unknown)
	  (setq target (extent-start-position ee))
	  (goto-char target)
	  (setq enabled-p
		(cond ((looking-at "[yY]\\b") t)
		      ((looking-at "[nN]\\b") nil)
		      (t (error "enabled is not y or n?")))))

	(cond ((eq what 'enable)
	       (setq enabled-p nil))
	      ((eq what 'disable)
	       (setq enabled-p t))
	      ((or (eq what 'toggle) (null what))
	       (if (eq enabled-p 'unknown)
		   (error
		    "can't toggle breakpoint: don't know current state")))
	      (t
	       (error "what must be enable, disable, toggle, or nil.")))
	))

    (gdb-menu-command (format "%s %d"
			      (if enabled-p "disable" "enable")
			      number)
		      nil)
    (message "%s breakpoint %d."
	     (if enabled-p "Disabled" "Enabled")
	     number)
    (cond (target
	   (save-excursion
	     (goto-char target)
	     (insert (if enabled-p "n" "y"))
	     (delete-char 1)
	     ;; don't let shell-fonts or font-lock second-guess us.
	     (remove-text-properties (1- (point)) (point) '(face))))))
  nil)

(defun gdb-mouse-enable-breakpoint (event)
  "Enable the breakpoint.
Looks for a gdb-breakpoint extent on the line under the mouse,
and executes an `enable' command"
  (interactive "@*e")
  (gdb-mouse-toggle-breakpoint-enabled event 'enable))

(defun gdb-mouse-disable-breakpoint (event)
  "Disable the breakpoint.
Looks for a gdb-breakpoint extent on the line under the mouse,
and executes a `disable' command"
  (interactive "@*e")
  (gdb-mouse-toggle-breakpoint-enabled event 'disable))


;; compatibility hack...
(or (fboundp 'extent-object) (fset 'extent-object 'extent-buffer))

(defun gdb-mouse-edit-function (event)
  "Edit the definition of this function (as with \\[find-tag])
Looks for a gdb-function-name extent on the line under the mouse,
and runs find-tag on the text under that extent."
  (interactive "@*e")
  (let (extent)
    (save-excursion
      (mouse-set-point event)
      (setq extent (or (car (gdb-get-line-token-extents
			     '(gdb-function-name-token)))
		       (error "no function-name extent on this line"))))
    (find-tag
     (buffer-substring (extent-start-position extent)
		       (extent-end-position extent)
		       (extent-object extent)))))


(defun gdb-mouse-edit-function-location (event)
  "Edit the source file of this function.
Looks for a gdb-function-location extent on line of the mouse,
and parses the text under it."
  (interactive "@*e")
  (let (file line)
    (save-excursion
      (mouse-set-point event)
      (let ((extent (or (car (gdb-get-line-token-extents
			      '(gdb-function-location-token)))
			(error "no function-location extent on this line"))))
	(goto-char (extent-start-position extent))
	(or (looking-at "\\([^ \t\n:,]+\\):\\([0-9]+\\)")
	    (looking-at "\\([^ \t\n:,]+\\),? line \\([0-9]+\\)")
	    (error "no file position on this line"))
	(setq file (buffer-substring (match-beginning 1) (match-end 1))
	      line (buffer-substring (match-beginning 2) (match-end 2)))
	(setq file (gdb-guess-file-name file)
	      line (string-to-int line))
	))
    (if (file-exists-p file)
	(find-file-other-window file)
      (signal 'file-error (list "File not found" file)))
    (goto-line line)))


(defun gdb-mouse-goto-frame (event)
  "Select this stack frame.
Looks for a gdb-frame-number extent on the line of the mouse,
and executes a `frame' command to select that frame."
  (interactive "@*e")
  (let (number)
    (save-excursion
      (mouse-set-point event)
      (let ((extent (or (car (gdb-get-line-token-extents
			      '(gdb-frame-number-token)))
			(error "no frame-number extent on this line"))))
	(goto-char (extent-start-position extent))
	(if (eq (following-char) ?#)
	    (forward-char 1))
	(setq number (string-to-int
		      (buffer-substring (point)
					(extent-end-position extent))))))
    (gdb-menu-command (format "frame %d" number) t))
  nil)


(defun gdb-mouse-get-variable-reference (event)
  "Returns a string which references the variable under the mouse.
This works even if the variable is deep inside nested arrays or structures.
If the variable seems to hold a pointer, then a \"*\" will be prepended."
  (save-excursion
    (let* ((extent (if (extentp event)
		       event
		     (progn
		       (mouse-set-point event)
		       (extent-at (point) nil 'gdb-token))))
	   dereference-p
	   name)
      (or (and extent
	       (eq (extent-property extent 'gdb-token)
		   'gdb-variable-name-token))
	  (error "not over a variable name"))
      (setq name (buffer-substring (extent-start-position extent)
				   (extent-end-position extent)))
      (save-excursion
	(goto-char (extent-end-position extent))
	(if (and (looking-at " *= *\\(([^)]+)\\)? *0x[0-9a-fA-F]+")   ; pointer
		 (progn
		   (goto-char (match-end 0))
		   (not (looking-at " +\""))))		       ; but not string
	    (setq dereference-p t))

	;; Now, if this variable is buried in a structure, compose a complete
	;; reference-chain to it.
	(goto-char (extent-start-position extent))

	(let ((done nil))
	  (while (not done)
	    (skip-chars-backward " \t")
	    (if (or (and (/= (preceding-char) ?\n)
			 (/= (preceding-char) ?\,)
			 (/= (preceding-char) ?\{))
		    (<= (buffer-syntactic-context-depth) 0))
		(setq done t)
	      (let ((p (scan-lists (point) -1 1)))
		(if (null p)
		    (setq done t)
		  (goto-char (setq p (- p 3)))
		  (cond
		   ((looking-at " = {")
		    (skip-chars-backward "a-zA-Z0-9_")
		    (if (= (preceding-char) ?\$)
			(forward-char -1))
		    (setq name (concat (buffer-substring (point) p) "." name)))

		   ((looking-at "}, +{")
		    (forward-char 1)
		    (let ((parse-sexp-ignore-comments nil)
			  (count 0))
		      (while (setq p (scan-sexps (point) -1 nil t))
			(goto-char p)
			(setq count (1+ count)))

		      (setq name (format "[%d].%s" count name))

		      ;; up out of the list
		      (skip-chars-backward " \t\n")
		      (if (= (preceding-char) ?\{)
			  (forward-char -1))

		      ;; we might be tightly nested in slot 0...
		      (while (= (preceding-char) ?\{)
			(forward-char -1)
			(setq name (concat "[0]" name)))

		      (skip-chars-backward " \t")
		      (if (= (preceding-char) ?=) (forward-char -1))
		      (skip-chars-backward " \t")
		      (setq p (point))
		      (skip-chars-backward "a-zA-Z0-9_")
		      (if (= (preceding-char) ?\$)
			  (forward-char -1))

		      (setq name (concat (buffer-substring (point) p) name))
		      ))
		   (t
		    (setq done t)))))))))

      (if dereference-p
	  (setq name (concat "*" name)))
      name)))

(defun gdb-mouse-print-variable (event)
  "Print the value of this variable.
Finds a variable under the mouse, and figures out whether it is inside of
a structure, and composes and executes a `print' command.  If the variable
seems to hold a pointer, prints the object pointed to."
  (interactive "@*e")
  (gdb-menu-command (concat "print "
			    (gdb-mouse-get-variable-reference event))
		    t))

(defun gdb-mouse-print-variable-type (event)
  "Describe the type of this variable.
Finds a variable under the mouse, and figures out whether it is inside of
a structure, and composes and executes a `whatis' command.  If the variable
seems to hold a pointer, describes the type of the object pointed to."
  (interactive "@*e")
  (gdb-menu-command (concat "whatis "
			    (gdb-mouse-get-variable-reference event))
		    t))

(defun gdb-mouse-print-type (event)
  "Describe this type.
Finds a type description under the mouse, and executes a `ptype' command."
  (interactive "@*e")
  (let* ((extent (save-excursion
		     (mouse-set-point event)
		     (extent-at (point) nil 'gdb-token)))
	 name)
    (or (and extent
	     (eq (extent-property extent 'gdb-token) 'gdb-type-name-token))
	(error "not over a type name"))
    (setq name (buffer-substring (extent-start-position extent)
				 (extent-end-position extent)))
    (gdb-menu-command (format "ptype %s" name)
		      t))
  nil)


;;; Popup menus

(defun gdb-menu-command (command &optional scroll-to-bottom)
  "Sends the command to gdb.
If gdb is not sitting at a prompt, interrupts it first
\(as if with \\[gdb-control-c-subjob]), executes the command, and then lets
the debugged program continue.

If scroll-to-bottom is true, then point will be moved to after the new
output.  Otherwise, an effort is made to avoid scrolling the window and 
to keep point where it was."

  ;; this is kinda like gdb-call except for the interrupt-first behavior,
  ;; but also it leaves the commands in the buffer instead of trying to
  ;; hide them.

  (let* ((proc (or (get-buffer-process (current-buffer))
		   (error "no process in %s" (buffer-name (current-buffer)))))
	 (window (selected-window))
	 wstart
	 (opoint (point))
	 was-at-bottom
	 running-p)

    (if (not (eq (current-buffer) (window-buffer window)))
	(setq window (get-buffer-window (current-buffer))))
    (setq wstart (window-start window))

    (let ((pmark (process-mark proc)))
      (setq was-at-bottom (>= (point) pmark))
      (goto-char pmark)
      (delete-region (point) (point-max)))

    (setq running-p (bolp))   ; maybe not the best way to tell...

    (cond (running-p
	   (message "Program is running -- interrupting first...")
	   (gdb-control-c-subjob)
	   (while (accept-process-output proc 1)
	     ;; continue accepting output as long as it's arriving
	     )))

    (message "%s" command)
    (goto-char (process-mark proc))
    (insert command)
    (comint-send-input)

    ;; wait for the command to be accepted
    (accept-process-output proc)
    (goto-char (process-mark proc))

    ;; continue, if we had interrupted
    (cond (running-p
	   (insert "continue")
	   (comint-send-input)))

    (if scroll-to-bottom
	(goto-char (process-mark proc))

      (set-window-start window wstart)
      (goto-char opoint)
      (if was-at-bottom
	  (if (pos-visible-in-window-p (process-mark proc) window)
	      (goto-char (process-mark proc))
	    (goto-char (window-end window))
	    (forward-line -2))))
    )
  nil)


(defun gdb-make-context-menu (event)
  "Returns a menu-desc corresponding to the stack-frame line under the mouse.
Returns nil if not over a stack-frame."
  (save-excursion
    (mouse-set-point event)
    (let* ((extents (gdb-get-line-token-extents
		     '(gdb-breakpoint-number-token
		       gdb-info-breakpoint-number-token
		       gdb-enabled-token
		       gdb-frame-number-token
		       gdb-function-name-token
		       gdb-function-location-token
		       gdb-arglist-token
		       gdb-arglist-types-token
		       gdb-variable-name-token
		       gdb-type-name-token
		       )))
	   (bnumber (or (nth 0 extents)
			(nth 1 extents)))
	   (enabled-p (nth 2 extents))
	   (fnumber (nth 3 extents))
	   (name (nth 4 extents))
	   (loc (nth 5 extents))
	   (al (nth 6 extents))
	   (alt (nth 7 extents))
	   (var (nth 8 extents))
	   (type (nth 9 extents))
	   (var-e var))

      ;; If this line has an arglist, only document variables and types
      ;; if the mouse is directly over them.
      (if (or al alt)
	  (setq var nil
		type nil))

      ;; Always prefer the object under the mouse to one elsewhere on the line.
      (let* ((e (extent-at (point) nil 'gdb-token))
	     (p (and e (extent-property e 'gdb-token))))
	(cond ((eq p 'gdb-function-name-token) (setq name e))
	      ((eq p 'gdb-variable-name-token) (setq var e var-e e))
	      ((eq p 'gdb-type-name-token) (setq type e))
	      ))

      ;; Extract the frame number (it may begin with "#".)
      (cond (fnumber
	     (goto-char (extent-start-position fnumber))
	     (if (eq (following-char) ?#)
		 (forward-char 1))
	     (setq fnumber
		   (string-to-int
		    (buffer-substring (point)
				      (extent-end-position fnumber))))))

      ;; Extract the breakpoint number (it may begin with "Breakpoint ".)
      (cond (bnumber
	     (setq bnumber
		   (buffer-substring (extent-start-position bnumber)
				     (extent-end-position bnumber)))
	     (if (string-match " [0-9]+\\'" bnumber)
		 (setq bnumber (substring bnumber (1+ (match-beginning 0)))))
	     (setq bnumber (string-to-int bnumber))
	     (or (> bnumber 0)
		 (error "couldn't parse breakpoint number"))))

      (cond ((null enabled-p)
	     (setq enabled-p 'unknown))
	    ((memq (char-after (extent-start-position enabled-p)) '(?y ?Y))
	     (setq enabled-p 't))
	    ((memq (char-after (extent-start-position enabled-p)) '(?n ?N))
	     (setq enabled-p 'nil))
	    (t
	     (setq enabled-p 'unknown)))

      ;; Convert the extents to strings.
      ;;
      (if name
	  (setq name (buffer-substring (extent-start-position name)
				       (extent-end-position name))))
      (if loc
	  (setq loc (buffer-substring (extent-start-position loc)
				      (extent-end-position loc))))
      (if var
	  (setq var (buffer-substring (extent-start-position var)
				      (extent-end-position var))))
      (if type
	  (setq type (buffer-substring (extent-start-position type)
				       (extent-end-position type))))

      ;; Return a menu description list.
      ;;
      (nconc
       (if (and bnumber (not (eq enabled-p 'nil)))
	   (list (vector (format "Disable Breakpoint %d"
				 bnumber)
			 (list 'gdb-mouse-disable-breakpoint event)
			 t)))
       (if (and bnumber (not (eq enabled-p 't)))
	   (list (vector (format "Enable Breakpoint %d"
				 bnumber)
			 (list 'gdb-mouse-enable-breakpoint event)
			 t)))
       (if bnumber
	   (list (vector (format "Delete Breakpoint %d" bnumber)
			 (list 'gdb-menu-command (format "delete %d" bnumber)
			       nil)
			 t)))
       (if var
	   (list (vector (format "Print Value of `%s'" var)
			 (list 'gdb-mouse-print-variable var-e)
			 t)
		 (vector (format "Print Type of `%s'" var)
			 (list 'gdb-mouse-print-variable-type var-e)
			 t)))
       (if name
	   (list (vector (format "Edit Definition of `%s'" name)
			 (list 'gdb-mouse-edit-function event)
			 t)
		 (vector (format "Set Breakpoint on `%s'" name)
			 (list 'gdb-menu-command (format "break %s" name) nil)
			 t)))
       (if loc
	   (list (vector (format "Visit Source Line (%s)" loc)
			 (list 'gdb-mouse-edit-function-location event)
			 t)))
       (if type
	   (list (vector (format "Describe Type `%s'" type)
			 (list 'gdb-menu-command (format "ptype %s" type) t)
			 t)))
       (if fnumber
	   (list (vector (format "Select Stack Frame %d" fnumber)
			 (list 'gdb-menu-command (format "frame %d" fnumber) t)
			 t)))
       ))))


(defun gdb-popup-menu (event)
  "Pop up a context-sensitive menu of gdb-mode commands."
  (interactive "_@e")
  (select-window (event-window event))
  (let (menu)
    (save-excursion
      (setq menu (append (if (boundp 'gdb-popup-menu)
			     (append (cdr gdb-popup-menu)
				     '("---")))
			 (if (boundp 'comint-popup-menu)
			     (cdr comint-popup-menu))))
      (let ((history (if (fboundp 'comint-make-history-menu)
			 (comint-make-history-menu)))
	    (context (gdb-make-context-menu event)))
	(if history
	    (setq menu
		  (append menu (list "---" (cons "Command History" history)))))
	(if context
	    (setq menu (append context (cons "---" menu))))
	)
      (setq menu (cons (if (boundp 'gdb-popup-menu)
			   (car gdb-popup-menu)
			 "GDB Commands")
		       menu)))
    (popup-menu menu event)))


;;; Patch it in...

(or (fboundp 'gdb-highlight-orig-filter)
    (fset 'gdb-highlight-orig-filter (symbol-function 'gdb-filter)))

(defun gdb-highlight-filter (proc string)
  (let ((p (marker-position (process-mark proc))))
    (prog1
	(gdb-highlight-orig-filter proc string)
      
      (save-match-data
	;;
	;; If there are no newlines in this string at all, then don't
	;; bother processing it -- we will pick up these characters on
	;; the next time around, when the line's newline gets inserted.
	;;
	(cond
	 ((string-match "\n" string)
	  (save-excursion
	    (set-buffer (process-buffer proc))
	    (goto-char p)
	    (let ((p2 (marker-position (process-mark proc)))
		  p3)
	      ;;
	      ;; If gdb has given us a full pathname, remember it.  (Do this
	      ;; before emitting any gdb-token extents, so that we attach it
	      ;; to the buffer *before* any of the extents to which it is
	      ;; known to correspond.
	      ;;
	      (gdb-highlight-remember-directory)
	      ;;
	      ;; Now highlight each line that has been written.  If we wrote
	      ;; the last half of a line, re-highlight that whole line.  (We
	      ;; need to do that so that the regexps will match properly;
	      ;; the "\n" test above also depends on this behavior.)
	      ;;
	      ;; But don't highlight lines longer than 5000 characters -- that
	      ;; probably means something is spewing, and we'll just get stuck
	      ;; hard in the regexp matcher.
	      ;;
	      (beginning-of-line)
	      (while (< (point) p2)
		(goto-char (prog1
			       (point)
			     (forward-line 1)
			     (setq p3 (point))))
		(if (< (- p3 (point)) 5000)
		    (gdb-highlight-line))
		(goto-char p3))))))))))

(fset 'gdb-filter 'gdb-highlight-filter)


(provide 'gdb-highlight)

;;; gdb-highlight.el ends here
