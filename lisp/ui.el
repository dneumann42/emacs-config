;;; ui.el --- UI helpers -*- lexical-binding: t -*-

(use-package line-reminder
  :ensure t
  :config
  ;; Avoid linum (deprecated in Emacs 30); use fringe indicators instead.
  (setq line-reminder-show-option 'indicators)
  (global-line-reminder-mode t))
  
(use-package eldoc-box
  :ensure t
  :config
  (setq eldoc-box-frame-parameters
        '((internal-border-width . 0)
          (left-fringe . 0)
          (right-fringe . 0)
          (undecorated . t)
          (minibuffer . nil)))

  (defun my/eldoc-box-border-style (_parent-frame)
    "Style the eldoc-box child frame without a border."
    (let ((frame (selected-frame)))
      (when (frame-live-p frame)
        (set-frame-parameter frame 'internal-border-width 0)
        (set-face-attribute 'eldoc-box-border frame :background "unspecified")
        (when (facep 'child-frame-border)
          (set-face-background 'child-frame-border "unspecified" frame))
        (set-face-attribute 'eldoc-box-body frame
                            :background "#0f1218"
                            :foreground (face-foreground 'default nil t)
                            :box nil))))

  (add-hook 'eldoc-box-frame-hook #'my/eldoc-box-border-style)
  (add-hook 'eldoc-box-buffer-hook
            (lambda ()
              (setq-local mode-line-format nil)
              (setq-local header-line-format nil)
              (when (boundp 'eldoc-box-buffer-map)
                (define-key eldoc-box-buffer-map (kbd "q")
                  (lambda ()
                    (interactive)
                    (eldoc-box-quit-frame)
                    (my/eldoc-restore-focus))))))
  (when (display-graphic-p)
    (eldoc-box-hover-at-point-mode 1)))

(defun my/eldoc-box-fontify-markdown (&rest _)
  "Use Markdown/GFM font-lock (including fenced code) in eldoc popovers."
  (when (require 'markdown-mode nil t)
    (cl-pushnew '("nim" . nim-mode) markdown-code-lang-modes :test #'equal)
    (gfm-view-mode)
    (font-lock-ensure)))
(add-hook 'eldoc-box-buffer-hook #'my/eldoc-box-fontify-markdown)

(provide 'ui)
