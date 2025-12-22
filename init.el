;;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'subr-x)
(require 'seq)
(require 'cl-lib)

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

;; Make Catppuccin background transparent so the frame inherits the underlying
;; terminal/compositor background.
(setq catppuccin-transparent-background t)

(add-to-list 'default-frame-alist '(alpha-background . 90))
(set-frame-parameter nil 'alpha-background 90)
(load-theme 'catppuccin :no-confirm)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(line-number-mode 1)
(global-auto-revert-mode 1)

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

(defun my/eldoc-show-help ()
  "Show docs in a popup when possible, otherwise fall back to the echo area."
  (interactive)
  (require 'eldoc)
  (require 'eldoc-box)
  (let* ((eldoc-echo-area-prefer-doc-buffer nil)
         (eldoc-display-functions (list #'eldoc-display-in-echo-area)))
    ;; Populate the doc buffer without showing it.
    (let ((display-buffer-overriding-action '((display-buffer-no-window))))
      (eldoc-print-current-symbol-info nil))
    (let ((eldoc--doc-buffer (eldoc-doc-buffer)))
      (when (and eldoc--doc-buffer
                 (buffer-live-p eldoc--doc-buffer)
                 (not (equal "" (with-current-buffer eldoc--doc-buffer (buffer-string)))))
        (eldoc-box-help-at-point)))))

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

(setq eldoc-echo-area-use-multiline-p t
      eldoc-idle-delay 0.2
      eldoc-documentation-strategy #'eldoc-documentation-compose
      eldoc-ellipsis "..."
      eldoc-echo-area-prefer-doc-buffer nil)

(add-to-list 'display-buffer-alist
             '("\\*eldoc\\*" display-buffer-no-window))

(defun my/eldoc-doc-buffer-no-window (orig &optional interactive)
  "Call ORIG without ever displaying the *eldoc* buffer.
INTERACTIVE is ignored; always fetches the buffer silently."
  (let ((display-buffer-overriding-action '((display-buffer-no-window))))
    (funcall orig nil)))
(advice-add 'eldoc-doc-buffer :around #'my/eldoc-doc-buffer-no-window)

(global-eldoc-mode 1)

(use-package line-reminder
  :ensure t
  :config
  (global-line-reminder-mode t)
  (setq line-reminder-show-option 'linum))
  
(use-package eldoc-box
  :ensure t
  :config
  (eldoc-box-hover-at-point-mode 1))

(use-package eglot
  :ensure t
  :requires (eldoc-box)
  :hook
  (prog-mode . eglot-ensure)
  :config
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
                 . ("yaml-language-server" "--stdio"))))



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
   . my/node-add-bin-to-path))

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
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; package management (straight.el or use-package)
(use-package vertico
  :ensure t
  :config (vertico-mode))

(use-package consult
  :ensure t)

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
  (add-hook 'minibuffer-setup-hook #'my/corfu-disable-in-minibuffer))

(use-package nim-mode
  :ensure t
  :init
  (defun my/nim-eglot-server ()
    ;; Prefer nimlangserver (matches lsp-nim-langserver config) and
    ;; fall back to nimlsp if that's what's available.
    (or (executable-find "nimlangserver")
        (executable-find "nimlsp")
        "nimlangserver"))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(nim-mode . ,(list (my/nim-eglot-server))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f9d423fcd4581f368b08c720f04d206ee80b37bfb314fa37e279f554b6f415e9"
     default))
 '(package-selected-packages
   '(catppuccin-theme corfu doom-modeline eldoc-box embark-consult
                      json-mode line-reminder marginalia
                      multiple-cursors nim-mode parinfer-rust-mode
                      shell-pop sly-quicklisp treemacs-icons-dired
                      treemacs-magit treemacs-projectile
                      typescript-mode vertico volatile-highlights
                      yaml-mode yascroll zoom)))
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
