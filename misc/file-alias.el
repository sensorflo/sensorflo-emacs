;;; filealias.el --- find files by aliases
;;
;; Copyright 2009-2012 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Filename: sensorflo.el
;; Description: find files by aliases
;; Created: 2009
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
;;; Installation:
;;  
;;  Optional:
;;  (add-hook 'find-file-hook 'file-alias-show-aliases)
;;  (define-key minibuffer-local-filename-must-match-map [(backtab)] 'file-alias-minibuffer-complete)
;;  (define-key minibuffer-local-completion-map          [(backtab)] 'file-alias-minibuffer-complete)
;;  
;;; Todo:
;; - customization more conventient & outside this file - here only the code should be
;; - file-alias-add-abbrev: add an abbrev for the current file/dir
;; find-file-ext : binds aliases to files within current directory. Aliases are global.
;; file-alias     : binds aliases to (rel files/paths within current tree / to abs paths). Aliases are local to current directory.
;;
;; separating the two helps to have shorter (because the current directory is
;; the 'namespace' ) find-file-ext aliases, which are used more often

(defvar file-alias-default-ext nil)
  
(defvar file-alias-root-alist)

(defvar file-alias-dir-alist nil
  "The keys/cars are aliases to the associated values/cdrs. A key is a string. A
  value is a string giving a file-/directoryname. ")

(defvar file-alias-map-map)

(defvar file-alias-default-root-dir nil)

(defvar file-alias-rel2-prefix "~~")

(defvar file-alias-separator ";"
  "Regex which separates the root-alias / dir-alias / file-alias
  within a full-alias. However, to seperate the file-base-alias from the file-ext-alias
  within the file-alias, still '.' is used.

  Choosing slash is not wise. Say you have this in your
  minibuffer

  WorkingDir/MyDirAlias/MyFileAlias

  Then the full-alias is MyFileAlias instead MyDirAlias/MyFileAlias.
  ")

(defvar file-alias-single-alias-is-file-alias t
  "If the full-alias contains no file-alias-separator, shoult it be
  a dir-alias or a file-alias ?") 

(defun file-alias-find-file()
  "As find-file, however the file to be opened is given as an full alias.

  FullAlias = RootAlias ';' DirAlias ';' FileAlias;
  FileAlias = FileBaseAlias '.' FileExtAlias;

  DirAlias: If given, it is looked up in file-alias-dir-alist and
  replaced by the found entry

  FileBaseAlias: If given, t"
  
  (interactive)
  (error "to be implemented"))

(defun file-alias-minibuffer-complete ()
  "Similar to file-cache-minibuffer-complete, however the full
  alias mechanism is used. See also file-alias-find-file."
  (interactive)
  (error "to be implemented"))

(defun file-alias-expand-full-alias (full-alias &optional leading-path)
  "Returns the expanded full alias"

  ;; definitions:
  ;;                              path
  ;;          dir-path                                  file-name
  ;; root-dir-path  rel-dir-path        file-name-sans-ext   file-name-ext    
  
  (let* ((tmp (file-alias-split-full-alias full-alias))
         (root-alias (nth 0 tmp))
         (dir-alias (nth 1 tmp))
         (file-base-alias (nth 2 tmp))
         (file-ext-alias (nth 3 tmp))
         real-root-path
         real-dir-path 
         real-file-name 
         file-alias-map ;; actual used map (filesys-filename to abbreviationlist)
         )
    
    ;; what if root-alias is ""
    ;; 1) also "" can be a valid root-alias which is looked up
    ;; 2) use root part of current working directory
    ;; 3) use path given by minibuffer as root
    (setq root-path (file-alias-expand-root-alias root-alias))
    
    (setq real-dir-path (file-alias-expand-dir-alias dir-alias root-path))

    ;; note that file-alias can be "", in which case the default, i.e. the
    ;; first file in the map will be opened
    
    ;; find file-alias-map to be used to find real-file-name
    (let ((iter file-alias-map-map))
      (while iter
        (when (string-match (nth 0 (car iter)) real-dir-path)
          (setq file-alias-map (eval (nth 1 (car iter))))
          (setq iter nil))
        (setq iter (cdr iter))))
    (unless file-alias-map (error))
    
    ;; determine real-file-name
    (let* ((alias-file-name-sans-ext (file-name-sans-extension file-alias))
           (file-alias-struct (file-alias-struct-by-alias alias-file-name-sans-ext file-alias-map))           
           (real-file-name-sans-ext (file-alias-struct-real-name file-alias-struct))
           (alias-file-name-ext (file-name-extension file-alias))
           (real-ext (or
                      (and alias-file-name-ext
                           (file-alias-struct-real-name (file-alias-struct-by-alias alias-file-name-ext file-alias-ext-map)))
                      (file-alias-struct-default-ext file-alias-struct)
                      file-alias-default-ext)))
      (setq real-file-name (concat real-file-name-sans-ext "." real-ext)))

    ;; actually open file
    (find-file-existing (concat real-dir-path "/" real-file-name))))

(defun file-alias-split-full-alias (full-alias)
  (let* (tmp
         (root-alias "")
         (dir-alias "")
         (file-alias "")
         (file-base-alias "")
         (file-ext-alias ""))
    
    (setq tmp (split-string full-alias (regexp-quote file-alias-separator)))
    (cond
     ((equal (length tmp) 3)
      (setq root-alias (nth 0 tmp)
            dir-alias (nth 1 tmp)
            file-alias (nth 2 tmp)))
     ((equal (length tmp) 2)
      (setq dir-alias (nth 0 tmp))
      (setq file-alias (nth 1 tmp))
     ((equal (length tmp) 1)
      (setq file-alias (nth 0 tmp)))
     (t (error "Either only 1 or then more than 2 separators")))
      
    (setq tmp (split-string file-alias "\\."))
    (file-base-alias (nth 0 tmp))
    (cond
     ((equal (length tmp) 1)) ; nop
     ((equal (length tmp) 2)
      (setq file-ext-alias (nth 1 tmp)))
     (t (error "More than 1 file ext seperators")))
    
    (list root-alias dir-alias file-base-alias file-ext-alias))))

(defun file-alias-expand-root-alias (root-alias)
  (let (root-path (assoc-string root-alias file-alias-root-alist t))
    (unless root-path (error "no such root alias"))
    root-path))

(defun file-alias-expand-rel2-path (rel2-path)
  "Expands the given relative2 path to an absolute path."
  (if (not (string-match (concat "^" (regexp-quote (concat file-alias-rel2-prefix "/")) rel2-path)))
      rel2-path
    (let (trailing-dir (substring (match-end 0))
          root-dir)
      (string-match (concat (regexp-quote (concat "/" trailing-dir)) "/?$") default-directory)
      (setq root-dir
            (if (match-beginning 0)
              (substring default-directory 0 (match-beginning 0))
              file-alias-default-root-dir))
      (concat root-dir "/" trailing-dir))))

(defun file-alias-expand-dir-alias(dir-alias root-path)
  "Returns the expanded dir alias as an absolute path"
  (let* ((alist-elt (assoc-string dir-alias file-alias-dir-alist t))
         (dir-rel2-path (cdr alist-elt)))
    (unless alist-elt (error "no such dir alias"))
    (if root-path
        (progn
          (string-match (concat "^" (regexp-quote (concat file-alias-rel2-prefix "/")) dir-rel2-path))
          (when (match-end 0)
            (setq dir-rel2-path (substring dir-rel2-path (match-end 0))))
          (concat root-path "/" dir-rel2-path))
      (file-alias-expand-rel2-path dir-rel2-path))))

(defun file-alias-minibuffer-complete ()
  "Replaces the alias in the minibuffer using the alist file-alias-dir-alist."
  (interactive)
  (delete-minibuffer-contents)
  (insert (file-alias-expand-alias (file-name-nondirectory (minibuffer-contents)))))

(defun file-alias-show-aliases ()
  "Shows in the echo area the list of aliass for the current
  file. If there exists no map for the current directory, do
  nothing."

  (interactive)
  (let (file-alias-map
        name-only
        alias-list
        alias-list-str
        (wd (file-name-directory (buffer-file-name))))
    
    ;; find map to be used using current working directory
    (let ((iter file-alias-map-map))
      (while iter
        (when (string-match (nth 0 (car iter)) wd)
          (setq file-alias-map (eval (nth 1 (car iter))))
          (setq iter nil))
        (setq iter (cdr iter))))

    ;; only output a message if there is a map for this directory
    (when file-alias-map-map
      ;; name only = directories and extension striped: /foo/bar.cpp -> bar
      (setq name-only (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
      
      
      (setq alias-list (file-alias-struct-alias-list (file-alias-struct-by-name name-only file-alias-map)))
      (setq alias-list-str (mapconcat (lambda(x) x) alias-list ", "))
       
      (message (concat "Alias are: " alias-list-str)))))

(defun file-alias-struct-by-alias (alias map)
  "Returns file-alias-struct element with given alias."
  (let* ((iter map)
         (found-elt (when iter (car iter))))
    (unless (equal alias "")
      (while iter
        (if (member alias (file-alias-struct-alias-list (car iter)))
            (setq found-elt (car iter)
                  iter nil)
          (setq iter (cdr iter)))))
    found-elt))

(defun file-alias-struct-by-name (name map)
  "Returns file-alias-struct element with given file name"
  (let ((iter map);; iterator over map
        found-elt)
    (while iter
      (if (equal name (file-alias-struct-real-name (car iter)))
          (setq found-elt (car iter)
                iter nil)
        (setq iter (cdr iter))))
    found-elt))

(defun file-alias-get-dir-map (real-rel-dir-path)
  "Returns dir map corresponding to real-rel-dir-path"
  (let ((iter file-alias-dir-map-map)
        found-map
        (case-fold-search t))
    (while iter
      (if (string-match (regexp-quote real-rel-dir-path) (caar iter))
          (setq found-map (nth 1 (car iter))
                iter nil))        
      (setq iter (cdr iter)))
    found-map))

(defun file-alias-struct-real-name (mapelt)
  "real file name without extension, e.g. PPCalibMod"
  (nth 0 mapelt))

(defun file-alias-struct-alias-list (mapelt)
  "list of aliasaliases for that file, e.g. (\"mod\" \"m\")"
  (nth 1 mapelt))

(defun file-alias-struct-default-ext (mapelt)
  "default extension for that file, e.g. h. If none is supplied,
   the global default is taken."
  (nth 2 mapelt))

;;; file-alias.el ends here

