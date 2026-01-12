;;; nim-ts-mode.el --- Tree-sitter Nim major mode -*- lexical-binding: t -*-

(require 'treesit)
(require 'nim-mode nil t)

(defvar nim-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'nim
   :feature 'punctuation
   '([ "." ";" "," ":" ] @font-lock-delimiter-face
     [ "(" ")" "[" "]" "{" "}" "{." ".}" ] @font-lock-bracket-face)
   :language 'nim
   :feature 'operator
   '([ "=" ] @font-lock-operator-face
     (infix_expression operator: _ @font-lock-operator-face)
     (prefix_expression operator: _ @font-lock-operator-face))
   :language 'nim
   :feature 'builtin
   '((blank_identifier) @font-lock-builtin-face
     (nil_literal) @font-lock-constant-face)
   :language 'nim
   :feature 'comment
   '((comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face)
   :language 'nim
   :feature 'doc
   '((documentation_comment) @font-lock-doc-face
     (block_documentation_comment) @font-lock-doc-face)
   :language 'nim
   :feature 'string
   '((interpreted_string_literal) @font-lock-string-face
     (long_string_literal) @font-lock-string-face
     (raw_string_literal) @font-lock-string-face
     (generalized_string) @font-lock-string-face
     (char_literal) @font-lock-string-face
     (escape_sequence) @font-lock-escape-face)
   :language 'nim
   :feature 'number
   '((integer_literal) @font-lock-number-face
     (float_literal) @font-lock-number-face
     (custom_numeric_literal) @font-lock-number-face)
   :language 'nim
   :feature 'function
   '((call
      function: [
                 (identifier) @font-lock-function-call-face
                 (dot_expression
                  right: (identifier) @font-lock-function-call-face)])
      
     (generalized_string
      function: [
                 (identifier) @font-lock-function-call-face
                 (dot_expression
                  right: (identifier) @font-lock-function-call-face)])
      
     (proc_declaration name: (_) @font-lock-function-name-face)
     (func_declaration name: (_) @font-lock-function-name-face)
     (converter_declaration name: (_) @font-lock-function-name-face)
     (method_declaration name: (_) @font-lock-function-name-face)
     (template_declaration name: (_) @font-lock-preprocessor-face)
     (macro_declaration name: (_) @font-lock-preprocessor-face))
   :language 'nim
   :feature 'variable
   '((symbol_declaration name: (_) @font-lock-variable-name-face)
     (parameter_declaration
      (symbol_declaration_list
       (symbol_declaration name: (_) @font-lock-variable-name-face))))
   :language 'nim
   :feature 'type
   '((exported_symbol "*" @font-lock-type-face)
     (_
      [
       type: [
              (type_expression (identifier))
              (type_expression (accent_quoted (identifier)))]
        @font-lock-type-face
       return_type: [
                     (type_expression (identifier))
                     (type_expression (accent_quoted (identifier)))]
        @font-lock-type-face])
      
     (except_branch
      values: (expression_list
               [
                (identifier) @font-lock-type-face
                (infix_expression
                 left: (identifier) @font-lock-type-face
                 operator: "as"
                 right: (identifier) @font-lock-variable-name-face)])))
                
   :language 'nim
   :feature 'property
   '((dot_expression right: (identifier) @font-lock-property-name-face))
   :language 'nim
   :feature 'keyword
   '([ "if" "when" "case" "elif" "else" ] @font-lock-keyword-face
     (of_branch "of" @font-lock-keyword-face)
     [ "import" "include" "export" ] @font-lock-preprocessor-face
     (import_from_statement "from" @font-lock-preprocessor-face)
     (except_clause "except" @font-lock-preprocessor-face)
     [ "for" "while" "continue" "break" ] @font-lock-keyword-face
     (for "in" @font-lock-keyword-face)
     [ "macro" "template" "const" "let" "var" "asm" "bind" "block"
       "concept" "defer" "discard" "distinct" "do" "enum" "mixin"
       "nil" "object" "out" "ptr" "ref" "static" "tuple" "type"]
     @font-lock-keyword-face
     [ "proc" "func" "method" "converter" "iterator" ] @font-lock-keyword-face
     [ "and" "or" "xor" "not" "div" "mod" "shl" "shr" "from" "as"
       "of" "in" "notin" "is" "isnot" "cast" ] @font-lock-operator-face
     [ "return" "yield" "try" "except" "finally" "raise" ] @font-lock-keyword-face))
  "Tree-sitter font-lock settings for `nim-ts-mode'.")

(defvar nim-ts-mode--font-lock-feature-list
  '((comment doc)
    (keyword type)
    (function variable property builtin)
    (string number operator punctuation))
  "Tree-sitter font-lock feature list for `nim-ts-mode'.")

(define-derived-mode nim-ts-mode prog-mode "Nim[TS]"
  "Major mode for Nim using tree-sitter."
  (when (boundp 'nim-mode-syntax-table)
    (set-syntax-table nim-mode-syntax-table))
  (when (treesit-ready-p 'nim)
    (treesit-parser-create 'nim))
  (setq-local treesit-font-lock-settings nim-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list nim-ts-mode--font-lock-feature-list)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (treesit-major-mode-setup))

(provide 'nim-ts-mode)
;;; nim-ts-mode.el ends here
