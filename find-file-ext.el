;;; find-file-ext.el --- find file extension, notably assigning aliases to files
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

;;; Variables:

(defgroup ffe nil
  "Fine file extensions, notably assigning aliases to files"
  :group 'find-file)

;; BUG: Changing ffe-show-abbrev-upon-find has no imediate effect, ffe needs to
;; be reloaded afterwards.
(defcustom ffe-show-abbrev-upon-find nil
  "It non-nil, after finding a file, its aliases are shown in echo area.

That shall help to remember the aliases, since you get told every
know an then what they are."
  :type 'boolean
  :group 'ffe)

(defvar ffe-map-map nil
  "A list of PAIR. Each PAIR is a list containing two elements. 1st is
  a path, the 2nd is a MAP. A MAP is a list of ALIASDEF.")

(defvar ffe-ext-map nil)

(defvar ffe-dir-map-map nil)

;;; Code:

(defun ffe-find-file(abbrev-path)
  "As find-file, however the file to be opened is given as an
  abbreviation."

  ;; definitions:
  ;;                             path
  ;;          dir-path                                  file-name
  ;; root-dir-path  rel-dir-path        file-name-sans-ext   file-name-ext    
  
  ;; todo: completion
  (interactive "sAbbrev path : ")

  (let (abbrev-file-name 
        real-dir-path 
        real-file-name 
        ffe-map) ;; actual used map (filesys-filename to abbreviationlist)

    ;; find real-dir-path and extract abbrev-file-name from abbrev-path
    (let ((iter (split-string abbrev-path "/"))
          dir-map)
      (setq real-dir-path 
            (if (equal (car iter) "")
                (progn
                  (setq iter (cdr iter))
                  "W:/DieBonder")
              default-directory))
      (while (and iter (cdr iter))
        (if (equal (car iter) "..") 
            (setq real-dir-path (replace-regexp-in-string "/[^/]*?/?$" "" real-dir-path))
          (progn
            (setq dir-map (ffe-get-dir-map real-dir-path))
            (unless dir-map (error "Shit"))
            (setq real-dir-path
                  (concat real-dir-path "/"
                          (ffe-struct-real-name (ffe-struct-by-abbrev (car iter) dir-map))))))
        (setq iter (cdr iter)))
      (setq abbrev-file-name (car (or (cdr iter) iter))))
      
    ;; note that abbrev-file-name can be "", in which case the default, i.e. the
    ;; first file in the map will be opened
    
    ;; find ffe-map to be used to find real-file-name
    (let ((iter ffe-map-map))
      (while iter
        (when (string-match (regexp-quote (nth 0 (car iter))) real-dir-path) 
          (setq ffe-map (eval (nth 1 (car iter))))
          (setq iter nil))
        (setq iter (cdr iter))))
    (unless ffe-map (error "Directory '%s' has entry in ffe-map-map" real-dir-path))
    
    ;; determine real-file-name
    (let* ((abbrev-file-name-sans-ext (file-name-sans-extension abbrev-file-name))
           (ffe-struct (ffe-struct-by-abbrev abbrev-file-name-sans-ext ffe-map))           
           (real-file-name-sans-ext (ffe-struct-real-name ffe-struct))
           (abbrev-file-name-ext (file-name-extension abbrev-file-name))
           (real-ext (or
                      (and abbrev-file-name-ext
                           (ffe-struct-real-name (ffe-struct-by-abbrev abbrev-file-name-ext ffe-ext-map)))
                      (ffe-struct-default-ext ffe-struct)
                      "cpp")))
      (setq real-file-name (concat real-file-name-sans-ext "." real-ext)))

    ;; actually open file
    (unless (equal (substring real-dir-path -1 nil) "/")
        (setq real-dir-path (concat real-dir-path "/")))    
    (find-file-existing (concat real-dir-path real-file-name))))

(defun ffe-show-abbrevs ()
  "Shows in the echo area the list of abbrevs for the current
file. If there exists no map for the current directory, do
nothing."
  (interactive)
  (let (ffe-map)
    
    ;; find map to be used using current working directory
    (dolist (i ffe-map-map)
      (when (string-match (nth 0 i) default-directory)
    	(setq ffe-map (eval (nth 1 i)))))

    (if (null ffe-map)
	(message "No ffe-map defined for this file")
      ;; name only = directories and extension striped: /foo/bar.cpp -> bar
      (let* ((name-only (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
	     (abbrev-list (ffe-struct-abbrev-list (ffe-struct-by-name name-only ffe-map))))
	(message (if abbrev-list
		     (concat "ffe abbrevs are: " (mapconcat 'identity abbrev-list ", "))
		 "No abbrevs defined for this file"))))))

(defun ffe-struct-by-abbrev (abbrev map)
  "Returns ffe-struct element with given abbrev."
  (let* ((iter map)
         (found-elt (when iter (car iter))))
    (unless (equal abbrev "")
      (while iter
        (if (member abbrev (ffe-struct-abbrev-list (car iter)))
            (setq found-elt (car iter)
                  iter nil)
          (setq iter (cdr iter)))))
    found-elt))

(defun ffe-struct-by-name (name map)
  "Returns ffe-struct element with given file name"
  (let ((iter map);; iterator over map
        found-elt)
    (while iter
      (if (equal name (ffe-struct-real-name (car iter)))
          (setq found-elt (car iter)
                iter nil)
        (setq iter (cdr iter))))
    found-elt))

(defun ffe-get-dir-map (real-rel-dir-path)
  "Returns dir map corresponding to real-rel-dir-path"
  (let ((iter ffe-dir-map-map)
        found-map
        (case-fold-search t))
    (while iter
      (if (string-match (regexp-quote real-rel-dir-path) (caar iter))
          (setq found-map (nth 1 (car iter))
                iter nil))        
      (setq iter (cdr iter)))
    found-map))

(defun ffe-struct-real-name (mapelt)
  "real file name without extension, e.g. PPCalibMod"
  (nth 0 mapelt))

(defun ffe-struct-abbrev-list (mapelt)
  "list of abbreviations for that file, e.g. (\"mod\" \"m\")"
  (nth 1 mapelt))

(defun ffe-struct-default-ext (mapelt)
  "default extension for that file, e.g. h. If none is supplied,
   the global default is taken."
  (nth 2 mapelt))

(if ffe-show-abbrev-upon-find
    (add-hook 'find-file-hook 'ffe-show-abbrevs)
  (remove-hook 'find-file-hook 'ffe-show-abbrevs))

(provide 'find-file-ext)

;; what was that? To be removed 
;; (directory-files-and-attributes "." nil )

;;; find-file-ext.el ends here