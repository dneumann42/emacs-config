;;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'subr-x)
(require 'seq)
(require 'cl-lib)

;; Disable backup (~), auto-save (#), and lock (.#) files.
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Load custom settings from a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Ensure nimble-installed tools like nimlangserver are on PATH for Emacs.
(let ((nimble-bin (expand-file-name "~/.nimble/bin")))
  (add-to-list 'exec-path nimble-bin)
  (setenv "PATH" (concat nimble-bin path-separator (getenv "PATH"))))

;; Prefer project-local Node tools for language servers (tsserver, eslint, etc.).
(defun my/node-add-bin-to-path ()
  "Add node_modules/.bin from the current project to PATH/exec-path."
  (when-let* ((root (locate-dominating-file default-directory "package.json"))
              (bin (expand-file-name "node_modules/.bin" root)))
    (when (file-directory-p bin)
      (setq-local exec-path (cons bin (seq-remove (lambda (p) (string= p bin)) exec-path)))
      (let* ((path-var (concat "PATH=" bin path-separator (getenv "PATH")))
             (existing (seq-remove (lambda (env) (string-prefix-p "PATH=" env))
                                   process-environment)))
        (setq-local process-environment (cons path-var existing))))))

(add-to-list 'default-frame-alist '(alpha-background . 90))
(set-frame-parameter nil 'alpha-background 90)
(let ((wallust-theme (expand-file-name "wallust-theme.el" user-emacs-directory)))
  (when (file-exists-p wallust-theme)
    (load-file wallust-theme)
    (load-theme 'wallust t)))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(line-number-mode 1)
(global-auto-revert-mode 1)
(pixel-scroll-precision-mode 1)

;; Dashboard
(setq inhibit-startup-screen t)

(defun my/dashboard ()
  (interactive)
  (let ((buffer-calendar "*Calendar*"))
    (calendar)
    (delete-other-windows)
    (switch-to-buffer (get-buffer buffer-calendar))))

(add-hook 'emacs-startup-hook #'my/dashboard)

(defun my/format-buffer ()
  "Indent the entire buffer using the active major mode."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(global-display-line-numbers-mode 1)
(prettify-symbols-mode 1)

(global-set-key (kbd "C-c t n") #'tab-line-switch-to-next-tab)
(global-set-key (kbd "C-c t p") #'tab-line-switch-to-prev-tab)
(global-set-key (kbd "C-M-<return>") #'my/eldoc-show-help)
(global-set-key (kbd "C-x /") #'my/consult-ripgrep-project)

(defun my/consult-ripgrep-project ()
  "Live grep the current projectile/project root with Consult + ripgrep."
  (interactive)
  (let ((dir (or (when (fboundp 'projectile-project-root)
                   (ignore-errors (projectile-project-root)))
                 (when-let* ((proj (project-current)))
                   (car (project-roots proj)))
                 default-directory)))
    (consult-ripgrep dir)))

(defun my/eldoc-show-help ()
  "Show docs in a popup when possible, otherwise fall back to the echo area."
  (interactive)
  (require 'eldoc)
  (require 'eldoc-box)
  ;; Populate the doc buffer without showing it.
  (let ((display-buffer-overriding-action '((display-buffer-no-window))))
    (condition-case err
        (eldoc-print-current-symbol-info t)
      (error
       (message "Eldoc error: %s" (error-message-string err)))))
  (let ((eldoc--doc-buffer (eldoc-doc-buffer)))
    (when (and eldoc--doc-buffer
               (buffer-live-p eldoc--doc-buffer)
               (not (equal "" (with-current-buffer eldoc--doc-buffer (buffer-string)))))
      (eldoc-box-help-at-point))))

(defun my/elisp-eval-sexp-at-point ()
  "Evaluate the s-expression at point and echo the result."
  (interactive)
  (let ((sexp (thing-at-point 'sexp t)))
    (unless sexp
      (user-error "No s-expression at point"))
    (let* ((form (car (read-from-string sexp)))
           (result (condition-case err
                       (eval form)
                     (error
                      (message "Eval error: %s" (error-message-string err))
                      (signal (car err) (cdr err))))))
      (message "%s" (prin1-to-string result))
      result)))

(defun my/elisp-setup-keys ()
  (define-key emacs-lisp-mode-map (kbd "M-<return>") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c f") #'my/format-buffer)
  (define-key emacs-lisp-mode-map (kbd "C-c d") #'xref-find-definitions)
  (define-key emacs-lisp-mode-map (kbd "C-c b") #'xref-pop-marker-stack)
  (define-key emacs-lisp-mode-map (kbd "C-c h") #'my/eldoc-show-help)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'my/elisp-eval-sexp-at-point)
  (define-key emacs-lisp-mode-map (kbd "C-M-<return>") #'my/eldoc-show-help))

(add-hook 'emacs-lisp-mode-hook #'my/elisp-setup-keys)
(add-hook 'lisp-interaction-mode-hook #'my/elisp-setup-keys)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Ensure elisp/builtin docstrings are available via Eldoc.
            (setq-local eldoc-documentation-function
                        #'elisp-eldoc-documentation-function)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Ensure elisp/builtin docstrings are available.
            (setq-local eldoc-documentation-function #'elisp-eldoc-documentation-function)))

(setq eldoc-echo-area-use-multiline-p t
      eldoc-idle-delay 0.2
      eldoc-documentation-strategy #'eldoc-documentation-compose
      eldoc-ellipsis "..."
      eldoc-echo-area-prefer-doc-buffer nil
      eldoc-doc-buffer-separator "\n\n")

(add-to-list 'display-buffer-alist
             '("\\*eldoc\\*" display-buffer-no-window))

(defun my/eldoc-doc-buffer-no-window (orig &optional interactive)
  "Call ORIG without ever displaying the *eldoc* buffer.
INTERACTIVE is ignored; always fetches the buffer silently."
  (let ((display-buffer-overriding-action '((display-buffer-no-window))))
    (funcall orig nil)))
(advice-add 'eldoc-doc-buffer :around #'my/eldoc-doc-buffer-no-window)

(global-eldoc-mode 1)

(defun my/eldoc-cleanup-markdown-code-fence (format-string &rest args)
  "Strip Markdown code fences and flatten newlines so Eldoc stays on one line."
  (when format-string
    (let* ((raw (apply #'format format-string args))
           (clean raw))
      ;; Drop an opening ```lang line and the closing ``` line.
      (setq clean (replace-regexp-in-string "\\`[ \t]*```[[:alnum:]-_]+[ \t]*\n" "" clean))
      (setq clean (replace-regexp-in-string "\n```[ \t]*\\'" "" clean))
      ;; Flatten remaining newlines to keep the echo area stable.
      (setq clean (string-trim (replace-regexp-in-string "[ \t]*\n[ \t]*" " " clean)))
      (eldoc-minibuffer-message "%s" clean))))

(defun my/nim-eldoc-cleanup-effects (format-string &rest args)
  "Drop Nim LSP effect markers like \"try !IOError\" from Eldoc strings."
  (when format-string
    (let* ((raw (apply #'format format-string args))
           (clean raw))
      ;; Remove Nim effect annotations/pragmas anywhere in the string.
      (setq clean (replace-regexp-in-string "\\s-+try\\b" "" clean))
      (setq clean (replace-regexp-in-string "\\s-+![^ \t\n(){}\\[\\],;]+" "" clean))
      (setq clean (replace-regexp-in-string "{\\.[^}]*\\}" "" clean))
      ;; Collapse any leftover repeated whitespace.
      (setq clean (replace-regexp-in-string "[ \t]+" " " clean))
      ;; Reuse the markdown/newline cleanup to keep output tidy.
      (my/eldoc-cleanup-markdown-code-fence "%s" clean))))

(defun my/nim-eglot-eldoc-filter (callback &rest _ignored)
  "Request Eldoc from Eglot and strip Nim effect noise before CALLBACK."
  (when (fboundp 'eglot-eldoc-function)
    (eglot-eldoc-function
     (lambda (&rest docs)
       (let* ((cleaned (mapcar (lambda (doc)
                                 (my/nim-eldoc-cleanup-effects "%s" doc))
                               docs))
              (non-empty (seq-remove #'string-empty-p cleaned)))
         (when non-empty
           (apply callback non-empty)))))))

(defun my/eglot-server-program-for-mode ()
  "Return the Eglot server program for the current buffer, if configured."
  (when (boundp 'eglot-server-programs)
    (seq-some
     (lambda (entry)
       (let ((modes (car entry))
             (program (cdr entry)))
         (cond
          ((symbolp modes)
           (when (derived-mode-p modes) program))
          ((and (listp modes) (seq-every-p #'symbolp modes))
           (when (apply #'derived-mode-p modes) program))
          ((functionp modes)
           (when (funcall modes major-mode) program))
          (t nil))))
     eglot-server-programs)))

(defun my/eglot-server-available-p ()
  "Return non-nil when a configured Eglot server executable is available."
  (when-let* ((program (my/eglot-server-program-for-mode)))
    (let ((cmd (if (functionp program) (funcall program) program)))
      (when (listp cmd)
        (let ((exe (car cmd)))
          (and (stringp exe) (executable-find exe)))))))

(defun my/eglot-ensure-maybe ()
  "Start Eglot only when a server is configured and available."
  (when (require 'eglot nil t)
    (cond
     ((derived-mode-p 'scheme-mode)
      (when (and (not (my/guile-lsp-command))
                 (bound-and-true-p eglot-managed-mode))
        (eglot-managed-mode -1)))
     ((derived-mode-p 'python-mode 'python-ts-mode)
      nil)
     ((my/eglot-server-available-p)
      (eglot-ensure)))))

(use-package line-reminder
  :ensure t
  :config
  (global-line-reminder-mode t))
  
(use-package eldoc-box
  :ensure t
  :config
  (setq eldoc-box-frame-parameters
        '((internal-border-width . 2)
          (left-fringe . 0)
          (right-fringe . 0)
          (undecorated . t)))

  (defun my/eldoc-box-border-style (_parent-frame)
    "Style the eldoc-box child frame with a dark pastel border."
    (let ((frame (selected-frame)))
      (when (frame-live-p frame)
        (set-frame-parameter frame 'internal-border-width 6)
        (set-face-attribute 'eldoc-box-border frame
                            :background "#2f3e46")
        (when (facep 'child-frame-border)
          (set-face-background 'child-frame-border "#2f3e46" frame))
        (set-face-attribute 'eldoc-box-body frame
                            :background "#0f1218"
                            :foreground (face-foreground 'default nil t)
                            :box nil))))

  (add-hook 'eldoc-box-frame-hook #'my/eldoc-box-border-style)
  (eldoc-box-hover-at-point-mode 1))

(defun my/eldoc-box-fontify-markdown (&rest _)
  "Use Markdown/GFM font-lock (including fenced code) in eldoc popovers."
  (when (require 'markdown-mode nil t)
    (cl-pushnew '("nim" . nim-mode) markdown-code-lang-modes :test #'equal)
    (gfm-view-mode)
    (font-lock-ensure)))
(add-hook 'eldoc-box-buffer-hook #'my/eldoc-box-fontify-markdown)

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
(load "nim-mode.el")
(require 'nim-suggest)
(let ((nimble-nimsuggest (expand-file-name "~/.nimble/bin/nimsuggest")))
  (setq nimsuggest-path
        (or (executable-find "nimsuggest")
            (when (file-executable-p nimble-nimsuggest) nimble-nimsuggest))))
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
  (nimsuggest-mode 1)
  ;; Avoid SMIE crashes during indentation by wrapping nim-indent-line.
  (setq-local indent-line-function #'my/nim-safe-indent)
  ;; Avoid SMIE crashes during sexp movement (C-M-SPC/mark-sexp).
  (setq-local forward-sexp-function #'my/nim-safe-forward-sexp)
  (flycheck-mode 1)
  (setq-local flycheck-checker 'nim))
(add-hook 'nim-mode-hook #'my/nim-setup-nimsuggest)
(add-hook 'nimscript-mode-hook #'my/nim-setup-nimsuggest)

(use-package yaml-mode
  :ensure t
  :hook
  (yaml-mode . eglot-ensure))

(use-package json-mode
  :ensure t
  :hook
  (json-mode . eglot-ensure))

(use-package parinfer-rust-mode
  :ensure t
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl"
        parinfer-rust-auto-download t)
  :config
  (defun my/parinfer-rust-ignore-quit (orig &rest args)
    "Ignore `minibuffer-quit' errors from Parinfer checks."
    (condition-case err
        (apply orig args)
      (quit nil)))
  (advice-add 'parinfer-rust--check-for-issues :around #'my/parinfer-rust-ignore-quit)
  (defun my/parinfer-rust-auto-accept-indentation (orig &rest args)
    "Automatically accept Parinfer indentation fixes without prompting."
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _args) t)))
      (apply orig args)))
  (advice-add 'parinfer-rust--check-for-indentation :around
              #'my/parinfer-rust-auto-accept-indentation)
  :hook
  ((emacs-lisp-mode
    lisp-mode
    lisp-interaction-mode
    scheme-mode
    clojure-mode
    fennel-mode
    common-lisp-mode)
   . (lambda ()
       (setq indent-tabs-mode nil)
       (parinfer-rust-mode 1))))

(use-package eros
  :ensure t
  :config
  (eros-mode))

(use-package geiser
  :ensure t
  :init
  (setq geiser-default-implementation 'guile
        geiser-repl-per-project-p nil
        geiser-repl-use-other-window t
        geiser-repl-history-filename (expand-file-name "geiser-history"
                                                       user-emacs-directory))
  :config
  (defun my/guile-repl-buffer ()
    "Return the current Guile REPL buffer, if any."
    (when (require 'geiser-repl nil t)
      (geiser-repl--repl/impl 'guile)))

  (defun my/guile-repl-visible-p ()
    "Return non-nil when the Guile REPL buffer is visible."
    (when-let* ((buf (my/guile-repl-buffer)))
      (get-buffer-window buf 'visible)))

  (defun my/guile-repl-live-p ()
    "Return non-nil when a Guile Geiser REPL is available."
    (when-let* ((buf (my/guile-repl-buffer)))
      (when (buffer-live-p buf)
        (let ((proc (get-buffer-process buf)))
          (and proc (process-live-p proc))))))

  (defun my/guile-attach-repl ()
    "Associate the current Scheme buffer with the shared Guile REPL."
    (when (require 'geiser-repl nil t)
      (when-let* ((buf (my/guile-repl-buffer)))
        (geiser-repl--set-this-buffer-repl buf))))

  (defun my/guile-repl ()
    "Start or switch to a Guile Geiser REPL."
    (interactive)
    (require 'geiser-repl)
    (let ((geiser-default-implementation 'guile))
      (geiser 'guile)
      (when (fboundp 'geiser-repl)
        (geiser-repl))))

  (defun my/guile-show-repl ()
    "Show the Guile REPL in a split if it isn't visible."
    (when-let* ((buf (my/guile-repl-buffer)))
      (unless (my/guile-repl-visible-p)
        (display-buffer
         buf
         '((display-buffer-reuse-window
            display-buffer-below-selected)
           (window-height . 0.3))))))

  (defun my/guile-auto-repl ()
    "Start a Guile REPL without stealing focus."
    (when (require 'geiser-repl nil t)
      (unless (my/guile-repl-live-p)
        (let ((geiser-default-implementation 'guile))
          (save-window-excursion
            (geiser 'guile)
            (when (fboundp 'geiser-repl)
              (geiser-repl)))))))

  (defun my/guile-ensure-repl ()
    "Ensure a Guile REPL exists, starting one if needed."
    (require 'geiser-repl nil t)
    (require 'geiser-guile nil t)
    (let* ((binary (or (and (boundp 'geiser-guile-binary) geiser-guile-binary)
                       "guile"))
           (exe (and (stringp binary) (executable-find binary))))
      (if (not exe)
          (message "Geiser: %s not found on PATH; cannot start Guile REPL" binary)
        (unless (my/guile-repl-live-p)
          (my/guile-auto-repl))
        (if (my/guile-repl-live-p)
            (my/guile-show-repl)
          (message "Geiser: REPL failed to start; check *Geiser Log*")))))

  (defun my/guile-eval-last-sexp-at-point ()
    "Evaluate the last sexp and show the result at point."
    (interactive)
    (my/guile-ensure-repl)
    (let* ((bounds (or (bounds-of-thing-at-point 'sexp)
                       (save-excursion
                         (ignore-errors
                           (let ((end (point)))
                             (backward-sexp)
                             (cons (point) end))))))
           (beg (car-safe bounds))
           (end (cdr-safe bounds)))
      (unless (and beg end)
        (user-error "No s-expression at point"))
      (my/guile-eval-region-with-overlay beg end)))

  (defun my/guile-eval-region-with-overlay (beg end)
    "Evaluate the region and show the result at point."
    (interactive "r")
    (my/guile-ensure-repl)
    (condition-case err
        (let* ((ret (geiser-eval-region/wait beg end 3))
               (result
                (cond
                 ((not ret) "Geiser: no response (timeout)")
                 ((geiser-eval--retort-error ret)
                  (geiser-eval--error-str (geiser-eval--retort-error ret)))
                 ((geiser-eval--retort-p ret)
                  (geiser-eval--retort-result-str ret nil))
                 (t (format "Geiser: unexpected response %S" ret)))))
          (cond
           ((fboundp 'eros--eval-overlay)
            (eros--eval-overlay result end))
           ((fboundp 'eros-eval-overlay)
            (eros-eval-overlay result end))
           (t
            (message "%s" result))))
      (error
       (message "Geiser eval error: %s" (error-message-string err)))))

  (defun my/guile-eval-definition-with-overlay ()
    "Evaluate the defun at point and show the result at point."
    (interactive)
    (my/guile-ensure-repl)
    (save-excursion
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (my/guile-eval-region-with-overlay (point) end))))

  (defun my/geiser-capf-for-symbol (&optional predicate)
    "Guard Geiser symbol completion until a REPL exists."
    (when (my/guile-repl-live-p)
      (geiser-capf--for-symbol predicate)))

  (defun my/geiser-capf-for-module (&optional predicate)
    "Guard Geiser module completion until a REPL exists."
    (when (my/guile-repl-live-p)
      (geiser-capf--for-module predicate)))

  (defun my/guile-find-definition ()
    "Jump to the symbol at point using Geiser when available."
    (interactive)
    (if (fboundp 'geiser-edit-symbol-at-point)
        (geiser-edit-symbol-at-point)
      (call-interactively #'xref-find-definitions)))

  (defun my/guile-setup-keys ()
    (setq-local indent-tabs-mode nil)
    (local-set-key (kbd "M-<return>") #'my/guile-eval-definition-with-overlay)
    (local-set-key (kbd "C-c f") #'my/format-buffer)
    (local-set-key (kbd "C-c d") #'my/guile-find-definition)
    (local-set-key (kbd "C-c b") #'xref-pop-marker-stack)
    (local-set-key (kbd "C-c h") #'geiser-doc-symbol)
    (local-set-key (kbd "C-c e") #'my/guile-eval-last-sexp-at-point)
    (local-set-key (kbd "C-c C-c") #'my/guile-eval-last-sexp-at-point)
    (local-set-key (kbd "C-c r") #'my/guile-eval-region-with-overlay)
    (local-set-key (kbd "C-c B") #'geiser-eval-buffer)
    (local-set-key (kbd "C-c z") #'my/guile-repl)
    (local-set-key (kbd "C-M-<return>") #'my/eldoc-show-help))

  (defun my/guile-setup-geiser-keys ()
    "Bind overlay eval keys in `geiser-mode-map' when available."
    (when (boundp 'geiser-mode-map)
      (define-key geiser-mode-map (kbd "M-<return>") #'my/guile-eval-definition-with-overlay)
      (define-key geiser-mode-map (kbd "C-c e") #'my/guile-eval-last-sexp-at-point)
      (define-key geiser-mode-map (kbd "C-c C-c") #'my/guile-eval-last-sexp-at-point)
      (define-key geiser-mode-map (kbd "C-c r") #'my/guile-eval-region-with-overlay)))

  (defvar-local my/guile--geiser-retry-count 0
    "Retries left for enabling Geiser in the current Scheme buffer.")

  (defun my/guile-enable-geiser-mode ()
    "Enable Geiser once the REPL is available."
    (when (> my/guile--geiser-retry-count 0)
      (if (my/guile-repl-live-p)
          (progn
            (my/guile-attach-repl)
            (geiser-mode 1)
            (my/guile-show-repl))
        (setq my/guile--geiser-retry-count (1- my/guile--geiser-retry-count))
        (run-at-time 0.2 nil #'my/guile-enable-geiser-mode))))

  (defun my/guile-scheme-setup ()
    "Initialize Geiser, completion, and REPL for Scheme buffers."
    (my/guile-ensure-repl)
    (setq my/guile--geiser-retry-count 15)
    (my/guile-enable-geiser-mode)
    (remove-hook 'completion-at-point-functions
                 #'geiser-capf--for-symbol t)
    (remove-hook 'completion-at-point-functions
                 #'geiser-capf--for-module t)
    (add-hook 'completion-at-point-functions
              #'my/geiser-capf-for-module nil t)
    (add-hook 'completion-at-point-functions
              #'my/geiser-capf-for-symbol nil t))

  (add-hook 'scheme-mode-hook #'my/guile-scheme-setup -10)
  (add-hook 'scheme-mode-hook #'my/guile-setup-keys)
  (with-eval-after-load 'geiser-mode
    (my/guile-setup-geiser-keys)))

(use-package geiser-guile
  :ensure t
  :after geiser
  :config
  (setq geiser-guile-binary (or (executable-find "guile") "guile")))

(use-package sly
  :ensure t
  :config
  (add-hook 'lisp-mode-hook #'sly-editing-mode)
  (with-eval-after-load 'sly
    (defun my/sly-jump-to-repl ()
      "Return an SLY REPL buffer, starting one if needed."
      (unless (sly-current-connection)
        (call-interactively #'sly))
      (or (sly-current-mrepl)
          (progn
            (sly-mrepl)
            (sly-current-mrepl))))

    (defun my/sly-quickload-and-in-package (system)
      "In the SLY REPL, quickload SYSTEM and switch into its package.
SYSTEM is prompted as a symbol name without the leading colon."
      (interactive "sQuickload system: ")
      (let* ((name (string-trim system))
             (repl (my/sly-jump-to-repl)))
        (when repl
          (pop-to-buffer repl)
          (goto-char (point-max))
          (sly-mrepl--insert-input (format "(ql:quickload :%s)" name))
          (sly-mrepl-return)
          (goto-char (point-max))
          (sly-mrepl--insert-input (format "(in-package :%s)" name))
          (sly-mrepl-return))))

    (define-key sly-editing-mode-map (kbd "M-<return>") #'sly-compile-defun)
    (define-key sly-editing-mode-map (kbd "C-c f") #'my/format-buffer)
    (define-key sly-editing-mode-map (kbd "C-c d") #'sly-edit-definition)
    (define-key sly-editing-mode-map (kbd "C-c b") #'sly-pop-find-definition-stack)
    (define-key sly-editing-mode-map (kbd "C-c h") #'sly-describe-symbol)
    (define-key sly-editing-mode-map (kbd "C-c e") #'sly-eval-last-expression)
    (define-key sly-editing-mode-map (kbd "C-c ~") #'my/sly-quickload-and-in-package)))

(use-package yascroll
  :ensure t
  :config
  (global-yascroll-bar-mode 1))

(use-package magit
  :ensure t)

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

(use-package shell-pop
  :ensure t
  :bind
  (("C-`" . shell-pop)
   ("C-M-`" . my/shell-pop-new-tab))
  :init
  (require 'seq)
  (setq shell-pop-window-size 30
        shell-pop-window-position "bottom"
        shell-pop-full-span nil
        shell-pop-shell-type '("eshell" "*shell-pop-eshell*" (lambda () (eshell)))
        shell-pop-universal-key nil)
  :config
  (defun my/shell-pop-tab-buffers ()
    (seq-filter
     (lambda (buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'eshell-mode)
              (string-prefix-p "*shell-pop-eshell" (buffer-name buffer)))))
     (buffer-list (selected-frame))))

  (defun my/shell-pop-setup-tabs ()
    (setq-local tab-line-tabs-function #'my/shell-pop-tab-buffers)
    (tab-line-mode 1))

  (defun my/shell-pop-new-tab ()
    "Create a new shell-pop eshell tab and show it."
    (interactive)
    (let ((eshell-buffer-name (generate-new-buffer-name "*shell-pop-eshell*")))
      (save-window-excursion
        (eshell))
      (setq shell-pop-last-shell-buffer-name eshell-buffer-name)
      (shell-pop 1)))

  (add-hook 'eshell-mode-hook #'my/shell-pop-setup-tabs)
  (add-to-list 'display-buffer-alist
               '("^\\*shell-pop-eshell"
                 (display-buffer-reuse-window display-buffer-below-selected)
                 (window-height . 0.3)
                 (inhibit-same-window . t)
                 (dedicated . t))))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode 1))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package zoom
  :ensure t
  :config
  (zoom-mode t))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winup-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-buffer-name-function            #'treemacs-default-buffer-name
          treemacs-buffer-name-prefix              " *Treemacs-Buffer-"
          treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        nil
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(treemacs-start-on-boot)

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (defun my/consult-projectile-find-file ()
    "Find a project file with Consult, showing an initial list and preview."
    (interactive)
    (require 'consult)
    (let* ((root (or (when (fboundp 'projectile-project-root)
                       (ignore-errors (projectile-project-root)))
                     default-directory))
           (default-directory root)
           (files (if (fboundp 'projectile-current-project-files)
                      (projectile-current-project-files)
                    (directory-files-recursively root ".*" t)))
           (state (consult--file-preview)))
      (when-let* ((file (consult--read
                         files
                         :prompt "Project file: "
                         :category 'file
                         :require-match t
                         :sort nil
                         :state state)))
        (find-file (expand-file-name file root)))))
  (define-key projectile-mode-map (kbd "C-c p") #'my/consult-projectile-find-file)
  (define-key projectile-mode-map (kbd "C-x p p") #'projectile-switch-project)
  (define-key projectile-mode-map (kbd "C-x p a") #'projectile-add-known-project)
  (define-key projectile-mode-map (kbd "C-x p t") #'treemacs-add-project-to-workspace))

;; package management (straight.el or use-package)
(use-package vertico
  :ensure t
  :config (vertico-mode))

(use-package consult
  :ensure t
  :config
  ;; Live preview while moving through Consult candidates (hit M-. to force when debounced).
  (setq consult-preview-key '(:debounce 0.2 any)))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)       
   ("C-;" . embark-dwim)      
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-fontify-code-blocks-natively t
        markdown-enable-highlighting-syntax t
        markdown-hide-markup t))

(use-package marginalia
  :ensure t
  :config (marginalia-mode))

(use-package corfu
  :ensure t
  :init
  (setq corfu-auto t
        corfu-auto-prefix 2
        corfu-cycle t)
  (global-corfu-mode)
  :config
  (defun my/corfu-disable-in-minibuffer ()
    (when (minibufferp)
      (corfu-mode -1)))
  (add-hook 'minibuffer-setup-hook #'my/corfu-disable-in-minibuffer)

  ;; Nim completions can be heavy; keep Corfu but disable auto popup there.
  (add-hook 'nim-mode-hook
            (lambda ()
              (setq-local corfu-auto nil))))

(use-package dockerfile-mode
  :ensure
  :config)

;; Reload theme when wallust-theme.el changes
(let ((wallust-theme (expand-file-name "wallust-theme.el" user-emacs-directory))
      (wallust-theme-name 'wallust))
  (defun my/wallust-force-black-background ()
    (let ((black "#000000"))
      (set-face-attribute 'default nil :background black)
      (set-face-attribute 'fringe nil :background black)
      (set-face-attribute 'line-number nil :background black)
      (set-face-attribute 'line-number-current-line nil :background black)
      (set-face-attribute 'mode-line nil :background black)
      (set-face-attribute 'mode-line-inactive nil :background black)
      (set-face-attribute 'header-line nil :background black)))
  (defun my/wallust-load-theme ()
    (when (file-exists-p wallust-theme)
      (load-file wallust-theme)
      (load-theme wallust-theme-name t)
      (my/wallust-force-black-background)))
  (my/wallust-load-theme)
  (when (fboundp 'file-notify-add-watch)
    (file-notify-add-watch
     wallust-theme
     '(change)
     (lambda (_event) (my/wallust-load-theme)))))
