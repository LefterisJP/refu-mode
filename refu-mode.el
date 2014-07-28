;;; refumode.el --- Emacs mode for refu lang
;;; Commentary:
;;  Followed the Emacs mode tutorial to make it
;;  http://www.emacswiki.org/emacs/ModeTutorial
;;; Code:

(defvar refu-mode-hook nil)

(defvar refu-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for refu major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rsc\\'" . refu-mode))
(add-to-list 'auto-mode-alist '("\\.rsg\\'" . refu-mode))

;; refu keywords
(defconst refu-keywords
  '("implof" "defimplof" "fn" "module" "signature" "import" "for" "in"
    "if" "else" "elif" "return")
  "Keywords of the refu language except the special cases.

Special cases are: data.")

(defconst refu-constants
  '("true" "false" "nil")
  "Constants in the refu language.")

(defconst refu-builtin-types
  '("i8" "u8"
    "i16" "u16"
    "i32" "u32"
    "i64" "u64"
    "f32" "f64"
    "string" "string8"
    "bool")
  "Built in data types of the refu language.")

(defvar refu-identifier-regexp
  "\\([a-zA-z0-9]\\|_\\)+")

(defvar refu-variable-attributes
  "\\(&\\|*\\|~\\)"
  "Variable attributes like references '&' e.t.c.")

(defvar refu-variable-decl-nogen-regexp
  (concat "\\(" refu-identifier-regexp "\\):"
          refu-variable-attributes
          "*\\(" refu-identifier-regexp "\\)")
  "Regular expression for refu variable declaration not using generics.")

(defvar refu-variable-decl-gen-regexp
  (concat refu-variable-decl-nogen-regexp
          "<\\(" refu-identifier-regexp "\\)>")
  "Regular expression for refu variable declaration using generics.")

;; Set font lock options.
;; For information on the various faces check here:
;; http://www.gnu.org/software/emacs/manual/html_node/ccmode/Faces.html
;; For examples on how to make advanced fontification based on the
;; language rules check C mode here:
;; http://cc-mode.sourceforge.net/src/cc-fonts.el
;;
;; Guide for Searh based fontification:
;; http://ergoemacs.org/emacs_manual/elisp/Search_002dbased-Fontification.html
;; General colouring guide:
;; http://ergoemacs.org/emacs/elisp_syntax_coloring.html
(defconst refu-font-lock-keywords
  (list
   '(refu-match-data-decl (1 font-lock-keyword-face)
                          (2 font-lock-variable-name-face))
   `(,(regexp-opt refu-constants 'words) . font-lock-constant-face)
   `(,(regexp-opt refu-builtin-types 'words) . font-lock-builtin-face)
   '(refu-match-types-after-functions 2 font-lock-type-face)
   '(refu-match-functions 1 font-lock-function-name-face)
   '(refu-match-module-impl-name 2 font-lock-constant-face)
   '(refu-match-module-signature-name 1 font-lock-constant-face)
   '(refu-match-variable-decl-gen (1 font-lock-variable-name-face)
                                  (3 font-lock-type-face)
                                  (4 font-lock-type-face))
   '(refu-match-variable-decl (1 font-lock-variable-name-face)
                              (2 font-lock-type-face))
   `(,(regexp-opt refu-keywords 'words) . font-lock-keyword-face)
   )
  "The font lock options for refu.")

(defun refu-match-regexp (re limit)
  "Generic regular expression matching wrapper for RE with a given LIMIT."
  (re-search-forward re
                     limit ; search bound
                     t     ; no error, return nil
                     nil   ; do not repeat
                     ))

(defun refu-match-data-decl (limit)
  "Search the buffer forward until LIMIT matching data declarations.

First match should be a keyword and second an identifier."
  (refu-match-regexp
   (concat
      " *\\(data\\) *\\(" refu-identifier-regexp "\\)")
   limit))


(defun refu-match-functions (limit)
  "Search the buffer forward until LIMIT matching function names.

Highlight the 1st result."
  (refu-match-regexp
   (concat
      "fn *\\(" refu-identifier-regexp "\\)")
   limit))

(defun refu-match-types-after-functions (limit)
  "Search the buffer forward until LIMIT matching types after functions."
  (refu-match-regexp
   (concat
    "fn *" refu-identifier-regexp "(.*) *-> *\\(" refu-identifier-regexp "\\)")
   limit))

(defun refu-match-module-impl-name (limit)
  "Search the buffer forward until LIMIT matching modules implementations."
  (refu-match-regexp
   (concat refu-identifier-regexp " *implof *"
           "\\(" refu-identifier-regexp "\\)")
   limit))

(defun refu-match-module-signature-name (limit)
  "Search the buffer forward until LIMIT matching modules signatures."
  (refu-match-regexp
   (concat "signature *\\(" refu-identifier-regexp "\\)")
   limit))

(defun refu-match-variable-decl (limit)
  "Search the buffer forward until LIMIT matching variable declarations.

First match should be an identifier and second a typename."
  (refu-match-regexp
   refu-variable-decl-nogen-regexp
   limit))

(defun refu-match-variable-decl-gen (limit)
  "Search the buffer forward until LIMIT matching generic variable declarations.

First match should be an identifier and second and third a typename."
  (refu-match-regexp
   refu-variable-decl-gen-regexp
   limit))

;; refu syntax table
(defvar refu-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; '_' underscore is a valid part of a word
    (modify-syntax-entry ?_ "w" st)
    ;; c++ style comments in the syntax table
    ;; more info on the syntax flags here:
    ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Flags.html
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for the refu language.")

(defun refu-indent-line ()
  "Indent a line as refu code."
  (interactive)
  (beginning-of-line) ()
  (if (bobp)            ; if at beginning of buffer
      (indent-line-to 0)
    ;else
    (progn)))

(defun refu-mode ()
  "Major mode for editing refu lang buffers."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table refu-mode-syntax-table)
  (use-local-map refu-mode-map)
  ;; specify syntax highlighting
  (set (make-local-variable 'font-lock-defaults) '(refu-font-lock-keywords))
  ;; register indentation functions
  (set (make-local-variable 'indent-line-function) 'refu-indent-line)
  ;; set major mode name and run hooks
  (setq major-mode 'refu-mode)
  (setq mode-name "refu")
  (run-hooks 'refu-mode-hook))

(provide 'refu-mode)
;;; refu-mode ends here
