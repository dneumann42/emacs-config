;;; completion.el --- Completion and search -*- lexical-binding: t -*-

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


(provide 'completion)
