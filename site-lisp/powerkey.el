;;; powerkey.el --- Power keys in menubar
;; Copyright (C) 1993 Lars Lindberg <lli@sypro.cap.se>
;;
;; Author: Lars Lindberg <lli@sypro.cap.se>
;; Created: 15 Sep 1993
;; Version 1.5
;; Keywords: menubar mouse power-keys shortcut
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; Purpose of this package:
;;   To add short-cuts to the menubar.
;;   Example: The "Edit" item in the menubar has "Paste" in the
;;   menu. It is in fact (yank) that is called. And it has the
;;   keyboard short-cut "C-y". This package makes sure
;;   that instead of "Paste", the menu says something like "Paste :C-y"
;;
;; How it works:
;;   You might say that I redefine (define-key) to check for
;;   shortcuts in the event of a menubar definition.
;;   What I really do is that I use the excellent package advice.el,
;;   which makes things like this easy to do. Check it out!
;;   
;; Installation:
;;   (load "powerkey") or (require 'powerkey)
;;   If this is done at dump-time, all menus will be taken care of.
;;   If you have this line in your .emacs-file instead, then
;;   the menus "File", "Edit" etc, will not be taken care of.
;;   If you want power-keys for them too, then call (update-power-keys t)
;;   in your .emacs!
;;   Note1! You should do this before any other (load ...) and (require ...)
;;   to be sure to affect their key bindings.
;;   Note2! advice.el in emacs 19.19 cannot be dumped. There is
;;   a new version that can (I use it), it will be part of emacs 19.20.
;;
;;   This functionality is of course only for window-systems, so
;;   in your .emacs, you could have something like
;;   (if window-system
;;     (progn
;;      (require 'powerkey)
;;      (update-power-keys t)
;;      <other window stuff>
;;     ))
;;        
;;
;; Trouble shooting:
;;   If you notice that a package doesn't get any powerkeys in
;;   the menubar, then I've noticed different reasons for that:
;;   1. The package defines its keys *after* its menubar items.
;;      Seldom true, luckily. Anyway, please don't mail me about this,
;;      try that package writer.
;;   2. The package makes a special menubar-map *and* doesn't do
;;      (define-key <some-map> [menubar <menu-name>] ...) until
;;      *after* the definition of the menu items.
;;      "bookmark.el" is an example of this. I've notified Karl about
;;      this fact, but if you find any other packages with this
;;      behaviour, please mail that package maintainer.
;;   3. There is a special function for the menu-bar action, similar
;;      to the powerkey function, but not exactly the same. I think this
;;      is a design bug for that action. Please mail the maintainer
;;      of that package and if he/she doesn't agree, mail me and
;;      I'll try to convince him/her.
;;   4. Error in powerkey.el. Can that happen? Well, if it does,
;;      please mail me. I try to answer fast.
;;
;;   Point 1 and 2 could actually be solved by my package, if I
;;   postpone my definitions till *after* the complete loading
;;   of a file. In future enhancements. Anyone out there to do this?
;;
;;   Uwe Bonnes (bon@LTE.E_TECHNIK.uni-erlangen.de) suggests a workaround
;;   for 1 and 2, but you have to now which map they use:
;;   Example for shell-mode:
;;   (add-hook 'shell-load-hook
;;             (function (lambda () (update-power-keys shell-mode-map)))
;;             t)
;;   Example for bookmarks.el:
;;   (load "bookmarks")
;;   (update-power-keys menu-bar-bookmark-map)

;;; Change Log
;;   1.5 1993-09-30
;;       (update-power-keys) is now much (?) faster. It now also
;;       recognizes t as argument - update all *known* keymaps. (It
;;       doesn't find various major keymaps for updating.)
;;       The powerkey indication in the menu now is without a
;;       ":" in front of it. For backward compatibility, the
;;       ":" is still recognized for already defined items.
;;       Presentation manager understands TAB as shortcut distinguisher,
;;       that is now supported -- Eberhard Mattes.
;;       (mattes@azu.informatik.uni-stuttgart.de).
;;       Increased power-keys-field-width to 11 and now removes trailing
;;       blanks from descriptions. -- Maechler (also correction of typos).
;;       Better heuristic for not doubling powerkey indicators. -- Maechler 
;;       Now made the define-key advice independent of the
;;       actual argument names in (define-key). Using the actual
;;       arguments made the byte-compiler issue warnings about
;;       non declared variables (keymap, key and def) at *one*
;;       site. Redefinition of (define-key (a b c)) there?
;;       -- anonymous (ada@unison.com).
;;       Better (?) and more comments.
;;   1.4 1993-09-28
;;       Added workaround in "Trouble Shooting". -- Uwe Bonnes.
;;       Found bug in the regexp that recognizes menubar definitions.
;;       -- Martin Maechler (maechler@stat.math.ethz.ch).
;;   1.3 1993-09-27
;;       Now adheres to at least some rules of emacs code writing standard.
;;       That is, uses (require) and (provide), new DOS-compatible file
;;       name.
;;       Also did some work on the file header and the installation
;;       notes. -- Andy Scott (ascott@pcocd2.intel.com) and Per Abrahamsen
;;       (abraham@research.att.com)
;;   1.2 1993-09-24
;;       Old persistent bug.
;;       Better format for power-keys in menus. -- Thanks again, Uwe.
;;   1.1 1993-09-23
;;       Took care of a number of bugs. -- Uwe Bonnes <bon@LTE.E-TECHNIK.uni-erlangen.de>
;; Known bugs:
;;   Descriptions in menu bar items with double spaces will be chopped
;;   off at the double space. (Double spaces is how powerkey recognizes
;;   existing powerkeys in the menubar item).
;;   
;; Future enhancements:
;;   This should really be a basic built in feature of emacs,
;;   rather than a package.  So, the ultimate goal for this package
;;   is to become obsolete!
;;   Until then, these are things that have been suggested:
;;   Please feel free to send me code that implements this!
;;   1. If (define-key) is run during loading, then postpone the power
;;      key definition until loading has been done.
;;   2. Profile this package with profile.el to make it faster.

;;; Code:

(require 'advice)

(defvar power-keys-use-tab-p (eq window-system 'pm))
(defvar power-keys-field-width 11
  "*The width of the power-key field in the menubar.")
(defvar power-keys-width (+ power-keys-field-width 34)
  "*The width of the power-key field in the menubar.")
(defvar power-keys-mode-map nil
  "The mode map to look for keys. For internal use only.")

;;; Advice to define-key to enable power keys in menu-bar.  This
;;; advice is done *after* the definition has been done.  The idea is
;;; to try to recognize this definition as a menubar definition. Then
;;; make sure that there exists a keyboard shortcut, that there exists
;;; a menubar item definition for this command and then redefine this
;;; definition, now with a keyboard shortcut - a powerkey - added.
(defadvice define-key (after power-key preact act)
  "Marks menu-bar items with their 'power-key'."
  ;; First extract the arguments from define-key.
  (let ((keymap (ad-get-arg 0))
	(key (ad-get-arg 1))
	(def (ad-get-arg 2)))
    ;; Make sure this is a definition of type
    ;; '(STRING . SYMBOL)
    (and def
	 (consp def)
	 (stringp (car def))
	 (cdr def)
	 (symbolp (cdr def))
	 (let* ((description (car def))
		(command (cdr def))
		key-desc-list spaces
		power-key-name
		menubar-key-name
		(word-with-symbol-regexp "\\(\\w\\|\\s_\\)+")
		(menubar-item-regexp (concat "^menu-bar "
					     word-with-symbol-regexp
					     " "
					     word-with-symbol-regexp))
		;; Now get the correct keyboard shortcut.
		(where-is-key (or (where-is-internal command
						     keymap
						     power-keys-mode-map
						     t t)
				  (where-is-internal command
						     (current-local-map)
						     nil
						     t t))))
	   (and where-is-key
		;; Get all key definitions for this command in global,
		;; local and current keymap.
		(setq key-desc-list
		      (append
		       (where-is-internal command keymap power-keys-mode-map)
		       (where-is-internal command (current-local-map))))
		;; Now make sure that there exists a definition for
		;; a menubar item.
		(progn
		  (while
		      (and key-desc-list
			   (setq menubar-key-name
				 (key-description (car key-desc-list)))
			   (not (string-match menubar-item-regexp
					      menubar-key-name)))
		    (setq menubar-key-name nil)
		    (setq key-desc-list (cdr key-desc-list)))
		  menubar-key-name)
		(progn
		  (setq power-key-name (key-description where-is-key))
		  ;; Remove starting blanks in the description.
		  (setq description
			(substring description
				   (progn (string-match "^ *" description)
					  (match-end 0))))
		  ;; Remove trailing blanks
		  (and (string-match " +$" description)
		       (setq description
			     (substring description 0 (match-beginning 0))))
		  ;; Note1. Remove old power-key. It has at least two
		  ;; spaces before it. See Note2 below.
		  ;; For backward compatibility (powerkey.el 1.4), also
		  ;; test for spaces followed by ":".
		  (and
		   ;;(get command 'power-keys-marked-p)
		   (or (string-match (if power-keys-use-tab-p "\t" "  +")
				     description)
		       (string-match " +:" description))
		   (setq description
			 (substring description 0 (match-beginning 0))))
		  ;; Try to make this item have total length =
		  ;; power-keys-width.
		  (setq spaces
			(if power-keys-use-tab-p
			    "\t"
			  (make-string (max
					;; Note2. At least two, to be
					;; recognized at Note1 above.
					2
					(- power-keys-width
					   1
					   (length description)
					   power-keys-field-width))
				       ?\ )))
		  ;; Now make the new description with power-key
		  (setq description (concat description
					    spaces
					    power-key-name))
		  ;; Finally redefine.
		  (unwind-protect
		      (progn
			(ad-deactivate 'define-key)
			(define-key keymap key 
			  (cons description command))
			;(put command 'power-keys-marked-p t)
			)
		    (ad-activate 'define-key))))))))

;;;----------------------------------------
;;; Function for updating menubar to power keys. 
;;;----------------------------------------
(defun update-power-keys-1 (keymap)
  ;;Support function for (update-power-keys). Don't call this separately."
  (mapcar
   (function
    (lambda (item)
      (let (maybe-command
	    new-key-map)
	(and (listp item)
	     (listp (cdr item))
	     (setq maybe-command (cdr (cdr item)))
	     (cond
	      ((and (listp maybe-command)
		    (setq new-key-map (memq 'keymap item)))
	       (update-power-keys-1 new-key-map))
	      ((and (nlistp maybe-command)
		    (commandp maybe-command))
	       (define-key keymap (vector (car item)) (cdr item))))))))
   (cdr keymap))
  nil)

(defun update-power-keys (&optional keymap silent)
  "Updates the menubar.
Different scope depending on optional KEYMAP:
  nil updates (current global-map)
  t updates all *known* maps - (current global-map), (current-local-map),
    all keymaps in minor-mode-map-alist.
  keymap updates this keymap.
Non-nil optional second argument, SILENT, means no messages.

You shouldn't have to use this function if you dump this package
together with emacs."
  (cond
   ((null keymap)
    (or silent (message "Updating power keys for global map..."))
    (update-power-keys (current-global-map) t))
   ((eq keymap 't)
    (or (memq (current-global-map) (current-local-map))
	(update-power-keys nil t))
    (or silent (message "Updating power keys for local map..."))
    (update-power-keys (current-local-map) t)
    ;; All minor mode maps.
    (mapcar (function (lambda (cons-cell)
			(let* ((minor-mode-var (car cons-cell))
			       (value (eval minor-mode-var)))
			  (unwind-protect
			      (progn
				(set minor-mode-var t)
				(or silent
				    (message "Updating power keys for minor mode %s..."
					     (symbol-name minor-mode-var)))
				(update-power-keys (cdr cons-cell) t))
			    (set minor-mode-var value)))))
	    minor-mode-map-alist)
    nil)
   ((keymapp keymap)
    (or silent (message "Making power keys for the menubar..."))
    (let ((alist keymap)
	  found-keymap)
      (while (setq found-keymap (assq 'menu-bar alist))
	(unwind-protect
	    (progn
	      (setq power-keys-mode-map keymap)
	      (update-power-keys-1 (memq 'keymap found-keymap)))
	  (setq power-keys-mode-map nil))
	(setq alist (cdr (memq found-keymap alist))))))
   (t
    (signal 'wrong-type-argument (list keymap))))
  (or silent (message nil)))
    

(provide 'powerkey)

;;; powerkey.el ends here
