;;; lang.el --- Language and LSP setup -*- lexical-binding: t -*-

(use-package eglot
  :ensure t
  :requires (eldoc-box)
  :hook
  (prog-mode . my/eglot-ensure-maybe)
  :config
  ;; Disable inlay hints globally unless explicitly re-enabled elsewhere.
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)
  (defun my/eglot-setup-keys ()
    "Keep Eglot bindings consistent across languages (clang, TS, etc.)."
    (local-set-key (kbd "M-<return>") #'eglot-code-actions)
    (local-set-key (kbd "C-c f") #'eglot-format)
    (local-set-key (kbd "<f2>") #'eglot-rename)
    (local-set-key (kbd "C-c d") #'xref-find-definitions)
    (local-set-key (kbd "C-c b") #'xref-pop-marker-stack)
    (local-set-key (kbd "C-c h") #'my/eldoc-show-help)
    (local-set-key (kbd "C-M-<return>") #'my/eldoc-show-help))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-setup-keys)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local eldoc-message-function #'my/eldoc-cleanup-markdown-code-fence)))
  (add-hook 'eglot-managed-mode-hook #'flymake-mode)
  (defun my/clangd-command ()
    ;; Prefer system clangd; keeps args centralized.
    (or (executable-find "clangd") "clangd"))
  (add-to-list 'eglot-server-programs
               `((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode objc-ts-mode)
                 . ,(list (my/clangd-command)
                          "--background-index"
                          "--clang-tidy"
                          "--completion-style=detailed"
                          "--header-insertion=never"
                          "--pch-storage=memory")))
  (add-to-list 'eglot-server-programs
               '((typescript-mode typescript-ts-mode tsx-ts-mode
                                  js-mode js-ts-mode)
                 . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((json-mode json-ts-mode)
                 . ("vscode-json-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((yaml-mode yaml-ts-mode)
                 . ("yaml-language-server" "--stdio")))
  (defun my/rust-analyzer-command ()
    ;; Prefer system rust-analyzer; keeps args centralized.
    (or (executable-find "rust-analyzer") "rust-analyzer"))
  (add-to-list 'eglot-server-programs
               `((rust-mode rust-ts-mode)
                 . ,(list (my/rust-analyzer-command)))))

(use-package scheme
  :ensure nil
  :init
  (add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
  (add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode)))

(defun my/guile-lsp-command ()
  "Return the Guile LSP command list when available."
  (cond
   ((executable-find "guile-lsp-server") (list "guile-lsp-server"))
   ((executable-find "guile-lsp") (list "guile-lsp"))
   (t nil)))

(with-eval-after-load 'eglot
  (when-let* ((cmd (my/guile-lsp-command)))
    (add-to-list 'eglot-server-programs `((scheme-mode) . ,cmd))))

(defun my/c-style ()
  (setq-local c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil))

(add-hook 'c-mode-common-hook #'my/c-style)

(use-package typescript-mode
  :ensure t
  :init
  (setq typescript-indent-level 2)
  :hook
  ((typescript-mode typescript-ts-mode tsx-ts-mode js-mode js-ts-mode json-mode yaml-mode)
   . my/node-add-bin-to-path)
  ((typescript-mode typescript-ts-mode tsx-ts-mode js-mode js-ts-mode)
   . my/typescript-eldoc-box-prettify))

(use-package rust-mode
  :ensure t
  :init
  (setq rust-format-on-save t)
  :hook
  ((rust-mode rust-ts-mode) . my/eglot-ensure-maybe))

(use-package org
  :ensure t
  :init
  (setq org-hide-leading-stars t)
  :config
  (dolist (face-scale '((org-document-title . 3.0)
                        (org-level-1 . 2.5)
                        (org-level-2 . 2.0)
                        (org-level-3 . 1.5)
                        (org-level-4 . 1.0)
                        (org-level-5 . 1.0)
                        (org-level-6 . 1.0)
                        (org-level-7 . 1.0)
                        (org-level-8 . 1.0)))
    (set-face-attribute (car face-scale) nil :weight 'bold :height (cdr face-scale))))

(use-package org-bullets
  :ensure t
  :after org
  :hook
  (org-mode . org-bullets-mode))

(defun my/typescript-eldoc-box-prettify ()
  "Prettify TS/JS Eldoc popups by using eldoc-box' TS formatter."
  (when (require 'eldoc-box nil t)
    (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t)))

;; Python config - prefer uv-managed virtualenvs and local language servers.
(defun my/python-uv-venv-root ()
  "Return the root directory of a uv-managed venv when available."
  (when-let* ((root (locate-dominating-file default-directory ".venv"))
              (venv (expand-file-name ".venv" root)))
    (when (file-directory-p venv)
      venv)))

(defun my/python-add-uv-venv-to-path ()
  "Add a uv venv's bin directory to PATH/exec-path and set python."
  (let ((uv-shebang (save-excursion
                      (goto-char (point-min))
                      (looking-at
                       "^#!.*/env -S uv run --quiet\\b"))))
    (when uv-shebang
      (setq-local python-shell-interpreter "uv")
      (setq-local python-shell-interpreter-args "run --quiet"))
    (when-let* ((venv (my/python-uv-venv-root))
                (bin (expand-file-name "bin" venv)))
      (when (file-directory-p bin)
        (setq-local exec-path (cons bin (seq-remove (lambda (p) (string= p bin)) exec-path)))
        (let* ((path-var (concat "PATH=" bin path-separator (getenv "PATH")))
               (existing (seq-remove (lambda (env) (string-prefix-p "PATH=" env))
                                     process-environment)))
          (setq-local process-environment (cons path-var existing)))
        (let ((py (expand-file-name "python" bin)))
          (when (and (file-executable-p py)
                     (not uv-shebang))
            (setq-local python-shell-interpreter py)))))))

(defun my/python-venv-bin-exe (exe)
  "Return EXE from a uv venv bin dir when available."
  (when-let* ((venv (my/python-uv-venv-root))
              (bin (expand-file-name "bin" venv))
              (path (expand-file-name exe bin)))
    (when (file-executable-p path)
      path)))

(defun my/python-eglot-server (&rest _ignored)
  "Return the Python LSP server command list when available."
  (cond
   ((or (my/python-venv-bin-exe "basedpyright-langserver")
        (executable-find "basedpyright-langserver"))
    (list (or (my/python-venv-bin-exe "basedpyright-langserver")
              "basedpyright-langserver")
          "--stdio"))
   ((or (my/python-venv-bin-exe "pyright-langserver")
        (executable-find "pyright-langserver"))
    (list (or (my/python-venv-bin-exe "pyright-langserver")
              "pyright-langserver")
          "--stdio"))
   ((or (my/python-venv-bin-exe "pylsp")
        (executable-find "pylsp"))
    (list (or (my/python-venv-bin-exe "pylsp")
              "pylsp")))
   (t nil)))

;; Prefer a local Python LSP when available.
(with-eval-after-load 'eglot
  ;; Resolve the Python LSP command at connection time so uv venvs are respected.
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) . my/python-eglot-server)))

;; Activate uv venvs for Python buffers.
(defun my/python-setup-eglot ()
  "Configure PATH for uv venvs and start Eglot for Python."
  (my/python-add-uv-venv-to-path)
  (require 'eglot)
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) . my/python-eglot-server))
  (when (my/eglot-server-available-p)
    (eglot-ensure)))

(add-hook 'python-mode-hook #'my/python-setup-eglot)
(add-hook 'python-ts-mode-hook #'my/python-setup-eglot)

;; Nim config - nimsuggest for docs, nim check for diagnostics.

(use-package nim-mode
  :ensure nil
  :mode (("\\.nim\\'" . nim-mode)
         ("\\.nimble\\'" . nim-mode))
  :init
  (require 'nim-mode nil t))

(with-eval-after-load 'nim-mode
  (condition-case err
      (require 'nim-suggest)
    (error
     (message "Nim suggest skipped: %s" (error-message-string err))))
  (let ((nimble-nimsuggest (expand-file-name "~/.nimble/bin/nimsuggest")))
    (setq nimsuggest-path
          (or (executable-find "nimsuggest")
              (when (file-executable-p nimble-nimsuggest) nimble-nimsuggest)))))
(use-package flycheck
  :ensure t)

(defun my/nim-safe-indent ()
  "Indent Nim safely; fall back instead of throwing SMIE errors."
  (interactive)
  (condition-case err
      (nim-indent-line)
    (error
     (message "Nim indent fallback: %s" (error-message-string err))
     (indent-relative))))

(defun my/nim-safe-forward-sexp (arg)
  "Move forward across Nim sexps safely, falling back on errors.
ARG is forwarded to `smie-forward-sexp' or `forward-sexp'."
  (interactive "p")
  (condition-case err
      ;; Bind `forward-sexp-function' to nil so any nested `forward-sexp' calls
      ;; use the built-in implementation and avoid recursive loops.
      (let ((forward-sexp-function nil))
        (if (fboundp 'smie-forward-sexp)
            (smie-forward-sexp arg)
          (forward-sexp arg)))
    (error
     (message "Nim sexp fallback: %s" (error-message-string err))
     (let ((forward-sexp-function nil))
       (forward-sexp arg)))))

(defun my/nim-setup-nimsuggest ()
  "Configure Nim docs via nimsuggest and diagnostics via nim check."
  (when (fboundp 'nimsuggest-mode)
    (nimsuggest-mode 1))
  ;; Avoid SMIE crashes during indentation by wrapping nim-indent-line.
  (setq-local indent-line-function #'my/nim-safe-indent)
  ;; Avoid SMIE crashes during sexp movement (C-M-SPC/mark-sexp).
  (setq-local forward-sexp-function #'my/nim-safe-forward-sexp)
  (flycheck-mode 1)
  (setq-local flycheck-checker 'nim))
(add-hook 'nim-mode-hook #'my/nim-setup-nimsuggest)
(add-hook 'nimscript-mode-hook #'my/nim-setup-nimsuggest)

;; TAGS generation via ntagger on first xref jump.
(defvar my/ntagger-roots (make-hash-table :test 'equal))
(defvar my/ntagger-installing nil)

(defun my/ntagger-project-root ()
  "Return the current project root for TAGS generation."
  (or (when (fboundp 'projectile-project-root)
        (ignore-errors (projectile-project-root)))
      (when-let* ((proj (project-current nil)))
        (car (project-roots proj)))
      (locate-dominating-file default-directory ".git")
      (locate-dominating-file default-directory "nimble.lock")
      default-directory))

(defun my/ntagger-installed-p ()
  "Return non-nil when ntagger is available."
  (executable-find "ntagger"))

(defun my/ntagger-install ()
  "Install ntagger using nimble when missing."
  (when (and (not (my/ntagger-installed-p))
             (executable-find "nimble")
             (not my/ntagger-installing))
    (setq my/ntagger-installing t)
    (let ((buf (get-buffer-create "*ntagger-install*")))
      (with-current-buffer buf
        (erase-buffer))
      (message "Installing ntagger via nimble...")
      (let ((exit-code (call-process "nimble" nil buf t
                                     "install" "https://github.com/elcritch/ntagger")))
        (setq my/ntagger-installing nil)
        (if (and (numberp exit-code) (zerop exit-code))
            (message "ntagger installed.")
          (message "ntagger install failed; see *ntagger-install*"))))))

(defun my/ntagger-ensure-gitignore (root tags-file)
  "Add TAGS to .gitignore in ROOT when TAGS-FILE exists."
  (let ((gitignore (expand-file-name ".gitignore" root)))
    (when (and (file-exists-p gitignore) (file-exists-p tags-file))
      (with-temp-buffer
        (insert-file-contents gitignore)
        (unless (re-search-forward "^/?TAGS$" nil t)
          (goto-char (point-max))
          (unless (or (bobp) (eq (char-before) ?\n))
            (insert "\n"))
          (insert "TAGS\n")
          (write-region (point-min) (point-max) gitignore nil 'quiet))))))

(defvar my/ntagger-include-nimble-deps t
  "When non-nil, include Nimble deps listed in *.nimble.")

(defvar my/ntagger-exclude-patterns
  '("deps" "gen_qcborcommon.nim" "/tests" "tests/")
  "Substring patterns to exclude from ntagger scans.")

(defun my/ntagger-find-nimble-file (root)
  "Return the first *.nimble file in ROOT."
  (car (directory-files root t "\\.nimble\\'")))

(defun my/ntagger-parse-nimble-deps (nimble-file)
  "Parse NIMBLE-FILE and return a list of dependency names."
  (let (deps)
    (with-temp-buffer
      (insert-file-contents nimble-file)
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*requires\\b" nil t)
        (let ((line-end (line-end-position)))
          (while (re-search-forward "\"\\([^\"]+\\)\"" line-end t)
            (let* ((spec (match-string 1))
                   (name (car (split-string spec "[[:space:]]+" t))))
              (when (and name (not (string-empty-p name)))
                (push name deps)))))))
    (delete-dups (nreverse deps))))

(defun my/ntagger-nimble-pkg-dir (name)
  "Return the newest pkgs2 directory for NAME, if any."
  (let* ((base (expand-file-name "~/.nimble/pkgs2"))
         (pattern (concat "^" (regexp-quote name) "-"))
         (candidates (when (file-directory-p base)
                       (directory-files base t pattern))))
    (when candidates
      (car (sort candidates
                 (lambda (a b)
                   (time-less-p
                    (file-attribute-modification-time (file-attributes b))
                    (file-attribute-modification-time (file-attributes a)))))))))

(defun my/ntagger-nimble-dep-dirs (root)
  "Return Nimble dependency directories for ROOT."
  (let ((nimble (my/ntagger-find-nimble-file root)))
    (when nimble
      (let* ((deps (my/ntagger-parse-nimble-deps nimble))
             (dirs (delq nil (mapcar #'my/ntagger-nimble-pkg-dir deps))))
        (delete-dups dirs)))))

(defun my/ntagger-generate-tags (root)
  "Generate TAGS in ROOT using ntagger."
  (let ((default-directory root)
        (buf (get-buffer-create "*ntagger*"))
        (args (list "-f" "TAGS" "--private" "--system"))
        (roots (list ".")))
    (when my/ntagger-include-nimble-deps
      (setq roots (append roots (my/ntagger-nimble-dep-dirs root))))
    (dolist (pat my/ntagger-exclude-patterns)
      (setq args (append args (list "--exclude" pat))))
    (setq args (append args roots))
    (with-current-buffer buf
      (erase-buffer))
    (let ((exit-code (apply #'call-process "ntagger" nil buf t args)))
      (if (and (numberp exit-code) (zerop exit-code))
          t
        (message "ntagger failed; see *ntagger*")
        nil))))

(defun my/ntagger-tags-file (root)
  "Return the TAGS file path for ROOT."
  (when root
    (expand-file-name "TAGS" root)))

(defun my/ntagger-ensure-tags ()
  "Ensure TAGS exist for the current Nim project."
  (when (derived-mode-p 'nim-mode)
    (let* ((root (my/ntagger-project-root))
           (tags-file (my/ntagger-tags-file root)))
      (when (and root tags-file)
        (unless (my/ntagger-installed-p)
          (my/ntagger-install))
        (when (my/ntagger-installed-p)
          (my/ntagger-generate-tags root))
        (when (file-exists-p tags-file)
          (my/ntagger-ensure-gitignore root tags-file))))))

(defun my/ntagger-regenerate ()
  "Regenerate TAGS for the current Nim project."
  (interactive)
  (let* ((root (my/ntagger-project-root))
         (tags-file (my/ntagger-tags-file root)))
    (unless (and root tags-file)
      (user-error "No project root found for TAGS generation"))
    (unless (my/ntagger-installed-p)
      (my/ntagger-install))
    (when (my/ntagger-installed-p)
      (if (my/ntagger-generate-tags root)
          (progn
            (my/ntagger-ensure-gitignore root tags-file)
            (message "ntagger: TAGS regenerated"))
        (message "ntagger: TAGS generation failed")))))

(defun my/ntagger-xref-advice (orig-fn &rest args)
  "Ensure ntagger TAGS exist before xref jumps."
  (my/ntagger-ensure-tags)
  (apply orig-fn args))

(dolist (fn '(xref-find-definitions
              xref-find-definitions-other-window
              xref-find-definitions-other-frame
              xref-find-references))
  (when (fboundp fn)
    (advice-add fn :around #'my/ntagger-xref-advice)))

(defun my/ntagger--parse-tags (tags-file symbol)
  "Return xref matches for SYMBOL in TAGS-FILE."
  (let (matches)
    (with-temp-buffer
      (insert-file-contents tags-file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (unless (or (string-prefix-p "!_" line) (string-empty-p line))
            (when (string-match "^\\([^\t]+\\)\t\\([^\t]+\\)\t[^;]*;\"\\(.*\\)$" line)
              (let* ((name (match-string 1 line))
                     (file (match-string 2 line))
                     (rest (match-string 3 line)))
                (when (string= name symbol)
                  (when (string-match "line:\\([0-9]+\\)" rest)
                    (let* ((line-num (string-to-number (match-string 1 rest)))
                           (path (if (file-name-absolute-p file)
                                     file
                                   (expand-file-name file (file-name-directory tags-file)))))
                      (push (xref-make name
                                       (xref-make-file-location path line-num 1))
                            matches))))))))
        (forward-line 1)))
    (nreverse matches)))

(defun my/nim-xref-backend ()
  "Use ntagger TAGS for Nim xref."
  (when (derived-mode-p 'nim-mode)
    'nim-ntagger))

(with-eval-after-load 'xref
  (add-hook 'xref-backend-functions #'my/nim-xref-backend t)
  (defun my/nim-xref-setup ()
    "Prefer ntagger tags in Nim buffers."
    (setq-local tags-add-tables nil)
    (setq-local xref-backend-functions '(my/nim-xref-backend))
    (setq-local tags-table-list nil))
  (add-hook 'nim-mode-hook #'my/nim-xref-setup)
  (cl-defmethod xref-backend-identifier-at-point ((_backend (eql nim-ntagger)))
    (thing-at-point 'symbol t))
  (cl-defmethod xref-backend-definitions ((_backend (eql nim-ntagger)) symbol)
    (my/ntagger-ensure-tags)
    (let* ((root (my/ntagger-project-root))
           (tags-file (my/ntagger-tags-file root)))
      (if (and tags-file (file-exists-p tags-file))
          (my/ntagger--parse-tags tags-file symbol)
        (user-error "No TAGS file found in project root"))))
  (cl-defmethod xref-backend-references ((_backend (eql nim-ntagger)) _symbol)
    nil))

(use-package yaml-mode
  :ensure t
  :hook
  (yaml-mode . eglot-ensure))

(use-package json-mode
  :ensure t
  :hook
  (json-mode . eglot-ensure))

(provide 'lang)
