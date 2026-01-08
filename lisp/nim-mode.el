;;; nim-mode.el --- Basic Nim major mode -*- lexical-binding: t -*-

;; Minimal Nim mode to avoid dependency issues.

(defgroup nim nil
  "Basic Nim mode."
  :group 'languages)

(defconst nim--keywords
  '("as" "asm" "block" "break" "case" "cast" "concept" "const" "continue"
    "converter" "defer" "discard" "distinct" "do" "elif" "else" "end" "enum"
    "except" "export" "finally" "for" "from" "func" "if" "import" "include"
    "interface" "iterator" "let" "macro" "method" "mixin" "of" "out" "proc"
    "raise" "ref" "return" "template" "try" "tuple" "type" "using" "var"
    "when" "while" "with" "yield")
  "Nim keywords for basic font lock.")

(defconst nim--types
  '("int" "int8" "int16" "int32" "int64" "uint" "uint8" "uint16" "uint32"
    "uint64" "float" "float32" "float64" "char" "string" "bool" "seq" "array"
    "set" "table" "ref" "ptr" "object" "tuple" "void" "auto")
  "Nim built-in types for basic font lock.")

(defconst nim--keywords-regexp
  (concat "\\_<" (regexp-opt nim--keywords t) "\\_>")
  "Regexp matching Nim keywords.")

(defconst nim--types-regexp
  (concat "\\_<" (regexp-opt nim--types t) "\\_>")
  "Regexp matching Nim built-in types.")

(defconst nim-font-lock-keywords
  `((,nim--keywords-regexp . font-lock-keyword-face)
    (,nim--types-regexp . font-lock-type-face)
    ("\\<\\(true\\|false\\|nil\\)\\>" . font-lock-constant-face))
  "Basic Nim font lock rules.")

(defvar nim-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Line comments: # ...
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    st)
  "Syntax table for `nim-mode'.")

(defun nim--indent-line ()
  "Indent current line in a basic, safe way."
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case _err
                    (max 0 (save-excursion
                             (forward-line -1)
                             (current-indentation)))
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

;;;###autoload
(define-derived-mode nim-mode prog-mode "Nim"
  "Major mode for editing Nim files (basic)."
  :syntax-table nim-mode-syntax-table
  (setq-local font-lock-defaults '(nim-font-lock-keywords))
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local standard-indent 2)
  (setq-local indent-line-function #'nim--indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nimble\\'" . nim-mode))

(provide 'nim-mode)
;;; nim-mode.el ends here
