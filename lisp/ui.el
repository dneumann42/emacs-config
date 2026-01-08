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

(provide 'ui)
