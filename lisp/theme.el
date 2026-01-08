;;; theme.el --- Themes -*- lexical-binding: t -*-

(use-package catppuccin-theme
  :ensure t)

(let ((wallust-theme (expand-file-name "wallust-theme.el" user-emacs-directory)))
  (when (file-exists-p wallust-theme)
    (load-file wallust-theme)
    (load-theme 'wallust t))
  (unless (custom-theme-enabled-p 'wallust)
    (load-theme 'catppuccin t)))

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
    (if (file-exists-p wallust-theme)
        (progn
          (load-file wallust-theme)
          (load-theme wallust-theme-name t)
          (my/wallust-force-black-background))
      (load-theme 'catppuccin t)))
  (my/wallust-load-theme)
  (when (and (file-exists-p wallust-theme)
             (fboundp 'file-notify-add-watch))
    (file-notify-add-watch
     wallust-theme
     '(change)
     (lambda (_event) (my/wallust-load-theme)))))

(provide 'theme)
