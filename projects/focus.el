;;; focus.el --- customizations specific to the focus project
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://github.com/sensorflo/sensorflo-emacs/, then
;;      projects/focus.el
;;
;;; Commentary:
;;
;;; Code:
(require 'project)         ; https://github.com/sensorflo/sensorflo-emacs/
(require 're)              ; https://github.com/sensorflo/sensorflo-emacs/
(require 'tempo-ext)       ; https://gitorious.org/tempo-ext
(require 'tempo-snippets)  ; http://nschum.de/src/emacs/tempo-snippets/
(require 'font-lock-ext)   ; https://github.com/sensorflo/font-lock-ext/
(require 'find-file-ext)   ; https://github.com/sensorflo/find-file-ext/
(require 'cc-mode-ext)     ; https://github.com/sensorflo/sensorflo-emacs/
(require 'compile)

;;; misc settings

;;;###autoload
(defvar focus-file-name-regex "^\\(.*?/[^/]*futura_focus[^/]*\\)\\(?:/\\|$\\)"
  "Files matching this regexp belong to focus project.

Match group 1 contains name of project root directory.")

(defvar focus-verify-coding-system t)

;; TODO: Actually I don't want to _define_ the coding system to be used, I
;; want to prefer it over other possible alternatives.
;;;###autoload
(add-to-list 'file-coding-system-alist (cons focus-file-name-regex 'utf-8-unix))

;;;###autoload
(add-hook 'change-major-mode-after-body-hook 'focus-hook)

;;;###autoload
(add-hook 'c-mode-common-hook 'focus-c-mode-common-hook)

;;;###autoload
(add-hook 'compilation-mode-hook 'focus-compilation-mode-hook)

;;;###autoload
(add-hook 'hack-local-variables-hook 'focus-hack-local-variables-hook)

;;;###autoload
(defun focus-hook()
  (when (and
         (not (is-a-minibufer-mode))
         (eq (project-root-type) 'project-focus))
    (mode-message-start "focus-hook")

    ;; some of these variables might only make sense in within an is-edit-mode
    ;; buffer, but it's more simple to do all in one place and it doesn't
    ;; hurt.

    (set (make-local-variable 'require-final-newline) t)
    (set (make-local-variable 'fill-column) 80)
    (set (make-local-variable 'tab-width) 2)
    (set (make-local-variable 'indent-tabs-mode) nil)

    (set (make-local-variable 'ediff-default-filtering-regexp) "\\.\\(cpp\\|h\\|hpp\\|qml\\)")
    (make-local-variable 'grep-find-command)
    (grep-apply-setting 'grep-find-command (focus-grep-find-command))
    (set (make-local-variable 'compile-command)
         (concat "cd " (focus-root-dir) "build && ./build"))
    (set (make-local-variable 'grep-find-ext-command-function) 'focus-grep-find-command)
    (set (make-local-variable 'grep-find-ext-regexp-function) 'focus-grep-find-regexp)

    (make-local-variable 'cc-search-directories)
    (add-to-list 'cc-search-directories (concat (focus-root-dir) "/Qt_GUI"))

    (dolist (x (list
                "."
                (concat (focus-root-dir) "/*")
                (concat (focus-nearest-cmake-dir) "src")
                (concat (focus-nearest-cmake-dir) "src/*")
                (concat (focus-nearest-cmake-dir) "include/*")
                (concat (focus-nearest-cmake-dir) "include/*/*")))
      (add-to-list 'cc-search-directories x))

    (mode-message-end "focus-hook")))

;;;###autoload
(defun focus-c-mode-common-hook()
  (when (eq (project-root-type) 'project-focus)
    (mode-message-start "focus-c-mode-common-hook")
    (set (make-local-variable 'tempos-c++-open-brace-style) 'behind-conditional)

    (set (make-local-variable 'c-doc-comment-char) ?\*)
    ;;(c-add-style "user" (list '(c-block-comment-prefix . "* ")))
    (set (make-local-variable 'c-block-comment-prefix) "* ")
    (c-set-offset 'c 1)
    (c-set-offset 'access-label '-)
    (c-set-offset 'innamespace 0)
    (c-set-offset 'inclass '++)
    (c-set-offset 'inextern-lang 0)
    (c-set-offset 'member-init-intro '+)
    (c-set-offset 'statement-cont '+)
    (c-set-offset 'brace-list-open 0) ; brace of enum class
    (c-set-offset 'topmost-intro-cont '++) ; note that elements in ctor initializer list after an element initialized with {...} have that
    (c-set-offset 'topmost-intro-cont 0) ; also the actual definition after template<...>

    (mode-message-end "focus-c-mode-common-hook")))

;; (add-to-list
;;  'compilation-error-regexp-alist-alist
;;  `(focus-logfile
;;    ,(concat
;;      "^\\(?:[^:=\n]*[:=][ \t]+\\)?"                     ; - intro
;;      "\\([^:=\n]*?\\):"                                 ; 1 file name
;;      "\\([0-9]+\\):"                                    ; 2 line no
;;      "\\(?:\\([0-9]+\\):\\)?"                           ; 3 column no
;;      "\\(?:"                                            ; - message
;;      "[ \t]*error\\|"                                 ;   - error
;;      "\\([ \t]*W:\\| *[a-zA-Z]*[wW]arn\\)\\|"         ;   4 warn
;;      "\\([ \t]*required from\\|[ \t]*note\\)\\|"      ;   5 info
;;      "[^0-9\n]"                                       ;   - ??
;;      "\\)")
;;    1 2 3 (4 . 5)))

;;;###autoload
(defun focus-compilation-mode-hook()
  (when (eq (project-root-type) 'project-focus)
    (set (make-local-variable 'compilation-error-regexp-alist)
         '(gnu-sensorflo gcc-include-sensorflo cmake-sensorflo cppunit )) ;; add focus-logfile
    ;; Since unfortunatly we have so many warnings
    (set (make-local-variable 'compilation-skip-threshold) 2)
    (font-lock-add-keywords nil (list
      ;; Actually regexes for log. Here because of ctest output
      (list (concat
             "^\\(ERROR\\b\\)\\(?:\\(?:[ \t]*\\[.*?\\][ \t]*\\)?[ \t]*"
             ":[ \t]*\\([^:]*?\\)[ \t]*"
             ":[ \t]*\\([0-9]+\\)\\)?")
            '(1 compilation-error-face)
            '(2 font-lock-semi-unimportant nil t) ; file
            '(3 compilation-line-face nil t)) ; line
      (list (concat
             "^\\(WARNING\\b\\)\\(?:\\(?:[ \t]*\\[.*?\\][ \t]*\\)?[ \t]*"
             ":[ \t]*\\([^:]*?\\)[ \t]*"
             ":[ \t]*\\([0-9]+\\)\\)?")
            '(1 compilation-warning-face)
            '(2 font-lock-semi-unimportant nil t) ; file
            '(3 compilation-line-face nil t)) ; line
      (list (concat
             "^\\(TRACE\\b\\)\\(?:\\(?:[ \t]*\\[.*?\\][ \t]*\\)?[ \t]*"
             ":[ \t]*\\([^:]*?\\)[ \t]*"
             ":[ \t]*\\([0-9]+\\)\\)?")
            '(1 compilation-info-face)
            '(2 font-lock-semi-unimportant nil t) ; file
            '(3 compilation-line-face nil t))
      (list "^\\(log4cxx\\b\\)"
            '(1 compilation-info-face))
      ;; QTest
      (list "^\\(PASS\\)   : "
            '(1 'success))
      (list "^\\(FAIL!\\)  : "
            '(1 compilation-error-face))
      ;; ctest
      (list "^\\(Errors while running CTest\\)"
            '(1 compilation-error-face))
      (list "^\\(100% tests passed\\)"
            '(1 'success))
      ))))

;;;###autoload
(defun focus-hack-local-variables-hook()
  (when (and
         (not (is-a-minibufer-mode))
         (eq (project-root-type) 'project-focus)
         (is-edit-mode)
         focus-verify-coding-system
         (not (focus-coding-system-p)))
    (message (concat "%s: encoding system is %S which is not one of Focus' "
                     "coding systems, see focus-coding-system-p")
             (buffer-name) buffer-file-coding-system)
    (shell-command (concat "notify-send -t 1000 "
                           "'" (buffer-name) " has invalid encoding system!'"))))

(defun fucker()
  (or
   (when (stringp buffer-file-name) buffer-file-name)
   (when (stringp default-directory) default-directory)
   (error "buffer is not associated with anything on file system")))

(defun focus-root-dir ()
  (let ((fucker (fucker)))
    (string-match focus-file-name-regex fucker)
    (match-string 0 fucker)))

(defun focus-nearest-cmake-dir ()
  (locate-dominating-file (fucker) "CMakeLists.txt"))

;;;###autoload
(defun focus-before-save-hook ()
  (when (and
         focus-verify-coding-system
         (eq (project-root-type) 'project-focus)
         (not (focus-coding-system-p)))
    (if (y-or-n-p (format "%s is encoded in %S. Change coding to Focus' requirement being utf-8-unix?"
                          (buffer-name) buffer-file-coding-system))
        (setq buffer-file-coding-system 'utf-8-unix)
      (if (y-or-n-p "abort saving? ")
          (error "user aborted abort aving")))))

;;;###autoload
(add-hook 'before-save-hook 'focus-before-save-hook t)

;;; functions

(defun focus-coding-system-p ()
  "t if the buffer's coding system is allowed in Focus."
  ;; Note (sumary from list-coding-systems):
  ;; - iso-8859-1 and latin-1 are aliases to iso-latin-1
  ;; - Difference windows-1252 to iso-8859-1 (from wikipedia on Windows-1252):
  ;;   ... This encoding is a superset of ISO 8859-1, but differs from the IANA's
  ;;   ISO-8859-1 by using displayable characters rather than control characters
  ;;   in the 80 to 9F (hex) range...
  (member buffer-file-coding-system
          '(utf-8-unix us-ascii-Unix no-conversion)))

(defun focus-grep-find-command(&optional regexp)
  (concat
   "find " (focus-root-dir) " \\\n"
   "-regextype posix-egrep \\\n"
   "\\( -type d \\( -name .git -o -name 'build-*' \\) -prune \\) -o \\\n"
   "-type f \\\n"
   "-iregex '.*\\.(c|h|hpp|cpp|qml|txt|xml|json)' \\\n"
   "-print0 | xargs -0 grep --color=always -nIP \\\n"
   "-ie '" regexp "'"))

(defun focus-grep-find-regexp()
  (or
   (when
       (save-excursion
         (beginning-of-line)
         (or (looking-at "\\s-*\\(?:class\\|struct\\)\\s-+\\(.*?\\)\\_>")
             (looking-at "\\s-*static\\s-+const\\s-+\\(?:tDiagCondId\\|tItemId\\)\\s-+\\(.*?\\)\\_>")
             (looking-at "\\s-*SetBaseItemID\\s-*([^,\n]*,\\s-*\\(.*?\\)\\_>\\s-*)")
             (looking-at ".*PRS_INIT\\s-*([^,\n]*,\\s-*\\(.*?\\)\\_>")
             (looking-at ".*ACTUALS_INIT\\s-*(\\s-*\\(.*?\\)\\_>")
             (looking-at "\\s-*REGISTER_KEY_COMMAND\\s-*([^,\n]*,[^,\n]*,\\s-*&\\w+::\\(.*?\\)\\_>")
             (looking-at "\\s-*DECLARE_\\(?:READ\\|WRITE\\)_METHOD\\s-*([^,\n]*,\\s-*\\(.*?\\)\\_>")
             (and (looking-at "\\s-*\\(?:virtual\\s-+\\)?\\(?:EHRESULT\\|int32\\|void\\|double\\)\\(?:\\s-+\\|\\s-*&\\s-*\\)\\(\\(?:\\w\\|_\\)+\\)\\(.\\)")
                  (not (string= ":" (match-string 2)))
                  (not (string= "ehr" (match-string 1))))))
     (concat "\\b" (match-string-no-properties 1) "\\b"))
   (when (and (save-excursion (beginning-of-line) (looking-at "\\s-*ehr\\s-*\\+?=\\s-\\(\\(?:\\w\\|_\\)+\\)"))
              (<= (point) (match-end 1)))
     (concat "\\b" (match-string-no-properties 1) "\\b"))))

;;; focus.el ends here
