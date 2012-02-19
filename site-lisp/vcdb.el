;;; gud.el --- Grand Unified Debugger mode for running GDB and other debuggers

;; Authors: Nick Thompson (nix at nixfiles.com)
;;          Eric S. Raymond <esr@snark.thyrsus.com>

;; Copyright (C) 1992, 93, 94, 95, 96, 1998 Free Software Foundation, Inc.
;; Copyright (C) 2001 Nicholas Thompson

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; this is an emacs debugger mode for controlling a visual c++
;;;   debugginer in another window.  it requires vcdb.pl for
;;;   the command-line interface to the various COM objects
;;;

;;; mostly cloned from the gdb section of gud.el by nix

(require 'gud)

;;; History of argument lists passed to vcdb.
(defvar gud-vcdb-history nil)

(defun gud-vcdb-massage-args (file args) args)

(defvar gud-vcdb-marker-regexp
  (concat "\\[\\(.:?[^:\n]*\\)" ":" "\\([0-9]*\\)" "\\]\n"))

;(setq gud-vcdb-marker-regexp  (concat "\\[\\(.:?[^:\n]*\\)" ":" "\\([0-9]*\\)" "\\]\n"))
;(setq gud-vcdb-marker-regexp "xyzzy")


;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
;(defvar gud-marker-acc "")
;(make-variable-buffer-local 'gud-marker-acc)

(defun gud-vcdb-marker-filter (string)
  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output ""))

    ;; Process all the complete markers in this chunk.
    (while (string-match gud-vcdb-marker-regexp gud-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
       gud-last-frame
       (cons (substring gud-marker-acc (match-beginning 1) (match-end 1))
	     (string-to-int (substring gud-marker-acc
				       (match-beginning 2)
				       (match-end 2))))

       ;; (message (car gud-last-frame))

       ;; Append any text before the marker to the output we're going
       ;; to return - we don't include the marker in this text.
       output (concat output
		      (substring gud-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       gud-marker-acc (substring gud-marker-acc (match-end 0))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\\[" gud-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output (concat output (substring gud-marker-acc
						 0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (setq gud-marker-acc
		(substring gud-marker-acc (match-beginning 0))))

      (setq output (concat output gud-marker-acc)
	    gud-marker-acc ""))

    output))

(defun gud-vcdb-find-file (f)
  (save-excursion
    (let ((buf (find-file-noselect f)))
      (set-buffer buf)
      (gud-make-debug-menu)
;      (local-set-key [menu-bar debug tbreak]
;		     '("Temporary Breakpoint" . gud-tbreak))
;      (local-set-key [menu-bar debug finish] '("Finish Function" . gud-finish))
;      (local-set-key [menu-bar debug up] '("Up Stack" . gud-up))
;      (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
      buf)))

(defvar vcdb-minibuffer-local-map nil
  "Keymap for minibuffer prompting of vcdb startup command.")
(if vcdb-minibuffer-local-map
    ()
  (setq vcdb-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key
    vcdb-minibuffer-local-map "\C-i" 'comint-dynamic-complete-filename))

;;;###autoload
(defun vcdb (command-line)
  "Run vcdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (read-from-minibuffer "Run vcdb (like this): "
			       (if (consp gud-vcdb-history)
				   (car gud-vcdb-history)
				 "vcdb.pl ")
			       vcdb-minibuffer-local-map nil
			       '(gud-vcdb-history . 1))))

  (gud-common-init command-line 'gud-vcdb-massage-args
		   'gud-vcdb-marker-filter 'gud-vcdb-find-file)

  (gud-def gud-break  "break %f:%l"  "\C-b" "Set breakpoint at current line.")
  ; (gud-def gud-tbreak "tbreak %f:%l" "\C-t" "Set temporary breakpoint at current line.")
  (gud-def gud-remove "delete %f:%l"  "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "step"      "\C-s" "Step one source line with display.")
  ;; (gud-def gud-stepi  "si %p"     "\C-i" "Step one instruction with display.")
  (gud-def gud-next   "next"      "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "go"         "\C-r" "Continue with display.")
  (gud-def gud-finish "finish"       "\C-f" "Finish executing current function.")
;  (gud-def gud-up     "up %p"        "<" "Up N stack frames (numeric arg).")
;  (gud-def gud-down   "down %p"      ">" "Down N stack frames (numeric arg).")
  (gud-def gud-print  "print %e"     "\C-p" "Evaluate C expression at point.")

;  (local-set-key [menu-bar debug tbreak] '("Temporary Breakpoint" . gud-tbreak))
;  (local-set-key [menu-bar debug finish] '("Finish Function" . gud-finish))
;  (local-set-key [menu-bar debug up] '("Up Stack" . gud-up))
;  (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
  (setq comint-prompt-regexp "^(.*vcdb[+]?) *")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'vcdb-mode-hook)
  )
