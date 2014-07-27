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
(add-to-list 'auto-mode-alist '("\\.rf\\'" . refu-mode))
(add-to-list 'auto-mode-alist '("\\.rfs\\'" . refu-mode))

;; refu keywords
(defconst refu-keywords
  '("data" "implof" "fn" "module" "signature")
  "Keywords of the refu language.")

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
  "\<\([:alnum:]\|_\>\)?")

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
   `(,(regexp-opt refu-keywords 'words) . font-lock-keyword-face)
   `(,(regexp-opt refu-constants 'words) . font-lock-constant-face)
   `(,(regexp-opt refu-builtin-types 'words) . font-lock-type-face)
   ;; '(refu-match-identifiers . font-lock-variable-name-face))
   '(refu-match-functions . font-lock-function-name-face))
   ;; `(,(concat
   ;;                        "fn *"
   ;;                        "\(" refu-identifier-regexp "\)"
   ;;                        " *(") . font-lock-function-name-face))
  "The font lock options for refu.")

(defun refu-match-regexp (re limit)
  (re-search-forward re
                     limit ; search bound
                     t     ; no error, return nil
                     nil   ; do not repeat
                     ))
;; (defun refu-match-identifiers (limit)
;;   (if (refu-match-regexp "\<fn \>" limit)
;;       t
;;       ;;else

(defun refu-match-functions (limit)
  (let ((match
         (refu-match-regexp 
          "fn *\\(\\([a-zA-z0-9]\\|_\\)+\\)"
          limit)))
    (message "LDEL match is %s" match)
    (message "LDEL (match-string 1) is %s" (match-string 1))
  (if match
      (match-string 1)
      ;;else
      nil)))
      
    

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
