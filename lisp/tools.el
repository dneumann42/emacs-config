;;; tools.el --- Dev tools -*- lexical-binding: t -*-

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
       (when (boundp 'electric-pair-mode)
         (electric-pair-local-mode -1))
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
(provide 'tools)
