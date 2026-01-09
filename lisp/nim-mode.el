;;; nim-mode.el --- Basic Nim major mode -*- lexical-binding: t -*-

;; Minimal Nim mode to avoid dependency issues.

(defvar nim--log-buffer "*nim-log*")
;; Minimal stubs required by nim-suggest.
(defvar-local nim--inside-compiler-dir-p nil)
(defvar nim-eldoc--skip-regex "\\s-+")
(defvar nimsuggest-options nil)
(defvar nimsuggest-local-options nil)
(defvar nim-dotty-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "_" st)
    st)
  "Syntax table for dotted identifiers in Nim.")

(defun nim-current-symbol ()
  "Return the Nim symbol at point."
  (thing-at-point 'symbol t))

(defun nim-eldoc-inside-paren-p ()
  "Return non-nil if point is inside parentheses."
  (let ((open (nth 1 (syntax-ppss))))
    (and open
         (eq (char-after open) ?\())))

(defun nim-eldoc--try-p ()
  "Return non-nil when Eldoc should update."
  t)

(defun nim-eldoc-off ()
  "No-op stub for nim-suggest compatibility."
  nil)

(defun nim-log (fmt &rest args)
  "Log Nim-mode related messages without requiring full nim-mode."
  (with-current-buffer (get-buffer-create nim--log-buffer)
    (goto-char (point-max))
    (insert (apply #'format fmt args) "\n")))

(defgroup nim nil
  "Basic Nim mode."
  :group 'languages)

(defface nim-paren-face
  '((t :inherit font-lock-delimiter-face))
  "Face used for Nim parentheses."
  :group 'nim)

(defface nim-bracket-face
  '((t :inherit font-lock-delimiter-face))
  "Face used for Nim brackets."
  :group 'nim)

(defface nim-brace-face
  '((t :inherit font-lock-delimiter-face))
  "Face used for Nim braces."
  :group 'nim)

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

(defconst nim--paren-regexp
  "[()]"
  "Regexp matching Nim parentheses.")

(defconst nim--bracket-regexp
  "[\\[\\]]"
  "Regexp matching Nim brackets.")

(defconst nim--brace-regexp
  "[{}]"
  "Regexp matching Nim braces.")

(defconst nim--capitalized-ident-regexp
  "\\_<[A-Z][A-Za-z0-9_]*\\_>"
  "Regexp matching capitalized Nim identifiers (likely types).")

(defconst nim--type-def-regexp
  "^\\s-*\\([A-Z][A-Za-z0-9_]*\\)\\*?\\s-*="
  "Regexp matching simple type definitions.")

(defconst nim--var-decl-regexp
  "\\_<\\(var\\|let\\|const\\)\\_>\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\*?"
  "Regexp matching variable declarations.")

(defconst nim--proc-def-regexp
  "\\_<\\(proc\\|func\\|method\\|iterator\\|template\\|macro\\)\\_>\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\*?"
  "Regexp matching function-like declarations.")

(defconst nim--function-call-face
  (if (facep 'font-lock-function-call-face)
      'font-lock-function-call-face
    'font-lock-function-name-face)
  "Face to use for Nim function calls.")

(defun nim--match-function-call (limit)
  "Match Nim function calls up to LIMIT."
  (let ((regexp "\\(?:\\_<\\|\\.\\)\\([A-Za-z_][A-Za-z0-9_]*\\)\\_>\\s-*(")
        found)
    (while (and (not found)
                (re-search-forward regexp limit t))
      (let* ((match-beg (match-beginning 1))
             (match-end (match-end 1))
             (prev-word (save-excursion
                          (goto-char match-beg)
                          (skip-syntax-backward "w_")
                          (skip-chars-backward " \t")
                          (let ((end (point)))
                            (skip-syntax-backward "w_")
                            (buffer-substring-no-properties (point) end)))))
        (unless (member prev-word
                        '("proc" "func" "method" "iterator" "template" "macro"
                          "type" "import" "include" "var" "let" "const"))
          (setq found t)
          (set-match-data (list match-beg match-end)))))
    found))

(defun nim--match-for-var (limit)
  "Match loop variables in `for` clauses up to LIMIT."
  (let (found)
    (while (and (not found)
                (re-search-forward "\\_<for\\_>\\s-+" limit t))
      (let ((end (save-excursion
                   (if (re-search-forward "\\_<in\\_>\\|:" limit t)
                       (match-beginning 0)
                     limit))))
        (while (and (not found)
                    (re-search-forward "\\_<[A-Za-z_][A-Za-z0-9_]*\\_>" end t))
          (let ((name (match-string-no-properties 0)))
            (unless (member name '("for" "in"))
              (setq found t)
              (set-match-data (list (match-beginning 0) (match-end 0))))))))
    found))

(defconst nim-font-lock-keywords
  `((,nim--keywords-regexp . font-lock-keyword-face)
    (,nim--types-regexp . font-lock-type-face)
    (,nim--type-def-regexp 1 font-lock-type-face)
    (,nim--capitalized-ident-regexp . font-lock-type-face)
    (,nim--proc-def-regexp 2 font-lock-function-name-face)
    (,nim--var-decl-regexp 2 font-lock-variable-name-face)
    (nim--match-for-var . font-lock-variable-name-face)
    (nim--match-function-call . ,nim--function-call-face)
    ("\\<\\(true\\|false\\|nil\\)\\>" . font-lock-constant-face)
    (,nim--paren-regexp . nim-paren-face)
    (,nim--bracket-regexp . nim-bracket-face)
    (,nim--brace-regexp . nim-brace-face))
  "Basic Nim font lock rules.")

(defvar nim-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Line comments: # ...
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    ;; Delimiters
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
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
  (setq-local tags-add-tables nil)
  (setq-local tags-file-name nil)
  (setq-local tags-table-list nil)
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
