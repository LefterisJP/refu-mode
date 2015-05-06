;;; refu-mode.el --- Major mode for refulang
;; Copyright (C) 2015  Lefteris Karapetsas

;; Author: Lefteris Karapetsas  <lefteris@refu.co>
;; Keywords: languages
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Solidity is a high-level language whose syntax is similar to that
;; of Javascript and it is designed to compile to code for the
;; Ethereum Virtual Machine.

;; This package provides a major mode for writing Solidity code.

;;; Code:
(require 'cc-mode)
(defgroup refu nil
  "Major mode for refu language"
  :group 'languages ;; Emacs -> Programming -> Languages
  :prefix "refu-"
  :link '(url-link :tag "Github" "https://github.com/LefterisJP/refu-mode"))

(defcustom refu-mode-hook nil
  "Callback hook to execute whenever a refu file is loaded."
  :group 'refu)

(defcustom refu-exec-path "/usr/bin/refu"
  "Path to the compiler binary."
  :group 'solidity
  :type 'string
  :package-version '(refu . "0.1.0"))

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
  '("class"
    "instance"
    "fn"
    "module"
    "signature"
    "import"
    "for"
    "in"
    "if"
    "else"
    "elif"
    "return")
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
  "\\(\\(?:[a-zA-z_]\\(?:[a-zA-z0-9_]\\)*\\)+\\)")

(defvar refu-variable-attributes
  "\\(&\\|*\\|\\|~\\)"
  "Variable attributes like references '&' e.t.c.")

(defvar refu-variable-decl-nogen-regexp
  (concat "\\(" refu-identifier-regexp "\\):"
          ;; refu-variable-attributes
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
   '(refu-match-functions (1 font-lock-keyword-face)
                          (2 font-lock-function-name-face))
   '(refu-match-type-decl (1 font-lock-type-face)
                          (2 font-lock-variable-name-face))
   '(refu-match-variable-decl (1 font-lock-variable-name-face)
                              (2 font-lock-type-face))
   ;; '(refu-match-variable-decl-gen (1 font-lock-variable-name-face)
   ;;                                (3 font-lock-type-face)
   ;;                                (4 font-lock-type-face))
   `(,(regexp-opt refu-constants 'words) . font-lock-constant-face)
   `(,(regexp-opt refu-builtin-types 'words) . font-lock-type-face)
   `(,(regexp-opt refu-keywords 'words) . font-lock-keyword-face))
  "The font lock options for refu.")

(defun refu-match-regexp (re limit)
  "Generic regular expression matching wrapper for RE with a given LIMIT."
  (re-search-forward re
                     limit ; search bound
                     t     ; no error, return nil
                     nil   ; do not repeat
                     ))

(defun refu-match-type-decl (limit)
  "Search the buffer forward until LIMIT matching data declarations.

First match should be a keyword and second an identifier."
  (refu-match-regexp
   (concat
      " *\\(type\\) *" refu-identifier-regexp)
   limit))


(defun refu-match-functions (limit)
  "Search the buffer forward until LIMIT matching function names.

Highlight the 1st result."
  (refu-match-regexp
   (concat
    " *\\(fn\\) *" refu-identifier-regexp)
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
   (concat
    " *" refu-identifier-regexp " *: *" refu-identifier-regexp)
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

(define-derived-mode refu-mode c-mode "refu"
  "Major mode for editing refulang buffers."
  (set-syntax-table refu-mode-syntax-table)
  ;; specify syntax highlighting
  (setq font-lock-defaults '(refu-font-lock-keywords))
  ;; register indentation functions, basically the c-mode ones
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)

  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'adaptive-fill-mode)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'fill-paragraph-handle-comment)

  ;; now set their values
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'indent-line-function) 'c-indent-line)
  (set (make-local-variable 'indent-region-function) 'c-indent-region)
  (set (make-local-variable 'normal-auto-fill-function) 'c-do-auto-fill)
  (set (make-local-variable 'comment-multi-line) t)
  (set (make-local-variable 'comment-line-break-function)
       'c-indent-new-comment-line)
  (run-hooks 'solidity-mode-hook))

(provide 'refu-mode)
;;; refu-mode ends here
