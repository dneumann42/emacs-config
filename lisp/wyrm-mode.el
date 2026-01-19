;;; wyrm-mode.el --- Major mode for Wyrm scripting language -*- lexical-binding: t -*-

;;; Commentary:
;; A major mode for editing Wyrm files (.wyrm).
;; Wyrm is a Tcl-like scripting language with:
;; - Commands separated by newlines or semicolons
;; - Braces {} for literal quoting (no substitution)
;; - $var and ${var} for variable substitution
;; - [cmd] for command substitution
;; - # for comments

;;; Code:

(defgroup wyrm nil
  "Major mode for Wyrm scripting language."
  :group 'languages
  :prefix "wyrm-")

(defcustom wyrm-indent-offset 2
  "Number of spaces for each indentation level in Wyrm."
  :type 'integer
  :group 'wyrm)

(defvar wyrm-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments start with #
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    ;; Braces are paired delimiters
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    ;; Brackets for command substitution
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    ;; Parentheses for indexing
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    ;; $ is a symbol constituent
    (modify-syntax-entry ?$ "'" table)
    ;; Word constituents
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?* "w" table)
    (modify-syntax-entry ?: "w" table)
    table)
  "Syntax table for `wyrm-mode'.")

(defconst wyrm-keywords
  '("if" "elif" "else" "while" "fun" "return" "break" "continue"
    "set" "unset" "puts" "eval" "info" "eq" "nth" "length" "inc")
  "Wyrm built-in commands and keywords.")

(defconst wyrm-constants
  '("t" "true" "false" "nil")
  "Wyrm constants.")

(defvar wyrm-font-lock-keywords
  `(;; Comments (handled by syntax table, but ensure visibility)
    ("^\\s-*#.*$" . font-lock-comment-face)

    ;; Variables: $var, ${var}, $var(index)
    ("\\$\\(?:{[^}]+}\\|[a-zA-Z_][a-zA-Z0-9_]*\\)\\(?:([^)]+)\\)?"
     . font-lock-variable-name-face)

    ;; Command substitution brackets
    ("\\[" . font-lock-keyword-face)
    ("\\]" . font-lock-keyword-face)

    ;; Keywords at start of command
    (,(concat "\\_<" (regexp-opt wyrm-keywords 'symbols) "\\_>")
     . font-lock-keyword-face)

    ;; Function definitions: fun name
    ("\\<fun\\>\\s-+\\([a-zA-Z_][a-zA-Z0-9_-]*\\)"
     (1 font-lock-function-name-face))

    ;; Constants
    (,(concat "\\_<" (regexp-opt wyrm-constants 'symbols) "\\_>")
     . font-lock-constant-face)

    ;; Numbers
    ("\\_<-?[0-9]+\\(?:\\.[0-9]+\\)?\\_>" . font-lock-constant-face)

    ;; Operators in @ expressions
    ("@\\s-*{[^}]*}" . font-lock-builtin-face)

    ;; Braced literals (dim them slightly)
    ("{[^{}]*}" . font-lock-string-face))
  "Font lock keywords for `wyrm-mode'.")

(defun wyrm--line-opens-block-p ()
  "Return non-nil if the current line opens a block."
  (save-excursion
    (beginning-of-line)
    (looking-at ".*{\\s-*\\(#.*\\)?$")))

(defun wyrm--line-closes-block-p ()
  "Return non-nil if the current line closes a block."
  (save-excursion
    (back-to-indentation)
    (looking-at "}")))

(defun wyrm--previous-line-indent ()
  "Return the indentation of the previous non-blank line."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp)) (looking-at "^\\s-*$"))
      (forward-line -1))
    (current-indentation)))

(defun wyrm--previous-line-opens-block-p ()
  "Return non-nil if the previous non-blank line opens a block."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp)) (looking-at "^\\s-*$"))
      (forward-line -1))
    (wyrm--line-opens-block-p)))

(defun wyrm-indent-line ()
  "Indent the current line according to Wyrm syntax."
  (interactive)
  (let ((indent 0)
        (prev-indent (wyrm--previous-line-indent))
        (opens-block (wyrm--previous-line-opens-block-p))
        (closes-block (wyrm--line-closes-block-p)))
    (setq indent prev-indent)
    (when opens-block
      (setq indent (+ indent wyrm-indent-offset)))
    (when closes-block
      (setq indent (max 0 (- indent wyrm-indent-offset))))
    (indent-line-to indent)))

(defvar wyrm-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `wyrm-mode'.")

;;;###autoload
(define-derived-mode wyrm-mode prog-mode "Wyrm"
  "Major mode for editing Wyrm scripts.

\\{wyrm-mode-map}"
  :syntax-table wyrm-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local indent-line-function #'wyrm-indent-line)
  (setq-local font-lock-defaults '(wyrm-font-lock-keywords))
  (setq-local tab-width wyrm-indent-offset)
  (setq-local indent-tabs-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wyrm\\'" . wyrm-mode))

(provide 'wyrm-mode)
;;; wyrm-mode.el ends here
