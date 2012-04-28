;;; filealias.el --- find files by aliases
;;
;; Copyright 2009-2011 Florian Kaufmann <sensorflo@gmail.com>
;; 
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; 
;; This file is not part of GNU Emacs.
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;; Commentary: 
;;
;; TODO
;; - incorporate into file name / extension aliases
;; - all minibuffers should recocnige the different file name completions (emacs's , mines)
;; - after opening a file, message not only the old file aliases, but also the new dir aliases
;; 
;;; Variables:
(defvar filealias-alist nil)
(defvar filealias-default-root-dir "~/")

;;; Code: 
;; 
  
;;   "The keys/cars are aliases to the associated values/cdrs. A key is a string. A
;;   value is a string giving a file-/directoryname. ")
(defun abs-file-name (file-name)
  ""
  (if (not (equal "~~" (substring file-name 0 2)))
      file-name
    (let (root-dir
          middle-dir
          real-dir-start
          real-dir)
      (string-match "/" file-name)
      (setq real-dir-start (if (match-end 0) (match-end 0) (length file-name)))
      (setq real-dir (substring file-name real-dir-start (length file-name)))     
      (setq middle-dir (substring file-name 2 (- real-dir-start 1)))     
      (setq root-dir
            (if (string-match (regexp-quote (concat "/" middle-dir "/")) default-directory)
                (substring default-directory 0 (match-beginning 0))
              filealias-default-root-dir))
      (concat root-dir "/" middle-dir "/" real-dir))))

(defun filealias-minibuffer-complete ()
  "Replaces the alias in the minibuffer using the alist filealias-alist."
  (interactive)
  (let* ((alias-str (file-name-nondirectory (minibuffer-contents)))
         (alist-elt (assoc-string alias-str filealias-alist t)))
    (unless alist-elt (error "no such alias"))
    (delete-minibuffer-contents)
    (insert (abs-file-name (cdr alist-elt)))))

(define-key minibuffer-local-filename-completion-map [(backtab)] 'filealias-minibuffer-complete)
(define-key minibuffer-local-filename-completion-map [(backtab)] 'filealias-minibuffer-complete)

;; maybe put on c-return (or even just return??) in filename minibuffers
(defun view-main-file-in-dir ()
  "If the file name is a directory, and that directory is a ???, then view the
  main file of that directory instead of dired on that directory." )

;; find-file-ext : binds aliases to files within current directory. Aliases are global.
;; filealias     : binds aliases to (rel files/paths within current tree / to abs paths). Aliases are local to current directory.
;;
;; separating the two helps to have shorter (because the current directory is
;; the 'namespace' ) find-file-ext aliases, which are used more often

;;; filealias.el ends here

