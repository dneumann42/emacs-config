;; -*- lexical-binding: t; -*-

(require 'org-datetree)

(defvar org-link-preview-overlays nil
  "Compatibility shim for older Org versions.")

(setq my/org-journal-file "journal.org")

(defun my/get-org-link (filename)
  (expand-file-name filename "~/org"))

(defun my/open-journal ()
  (interactive)
  (find-file-other-window (my/get-org-link my/org-journal-file)))

(defun my/open-journal-at-today ()
  (interactive)
  (find-file (my/get-org-link my/org-journal-file))
  (org-datetree-find-date-create (calendar-current-date))
  (org-fold-show-entry)
  (org-fold-show-subtree)
  (goto-char (point-max)))

(defun my/open-journal-at-date (date)
  (interactive)
  (find-file (my/get-org-link my/org-journal-file))
  (org-datetree-find-date-create date)
  (org-show-entry)
  (org-show-subtree)
  (goto-char (point-max)))

(defun my/org-journal-open-from-calendar ()
  (interactive)
  (my/open-journal-at-date (calendar-cursor-to-date)))

(defun my/org-redisplay-inline-images ()
  (when (derived-mode-p 'org-mode)
    (run-with-idle-timer 0.15 nil #'org-redisplay-inline-images)))

(defun my/org-setup-pretty ()
  (setq-local org-link-descriptive t)
  (visual-line-mode 1)
  (my/org-redisplay-inline-images)
  (add-hook 'after-save-hook #'org-redisplay-inline-images nil t))

(use-package org
  :ensure t
  :init
  (setq
   org-hide-leading-stars t
   org-agenda-include-diary t
   org-link-descriptive t
   org-startup-with-inline-images t
   org-image-actual-width 64
   org-display-remote-inline-images 'cache
   url-automatic-caching t
   org-startup-folded 'content)

  (setq org-capture-templates
        `(("j" "Journal Entry" entry
           (file+datetree (my/get-org-link my/org-journal-file))
           "* %?"
           :empty-lines 1)))

  :hook
  (org-mode . my/org-setup-pretty)

  :config
  (dolist (face-scale '((org-document-title . 2.0)
                        (org-level-1 . 1.75)
                        (org-level-2 . 1.5)
                        (org-level-3 . 1.25)
                        (org-date . 1.25)
                        (org-level-4 . 1.0)
                        (org-level-5 . 1.0)
                        (org-level-6 . 1.0)
                        (org-level-7 . 1.0)
                        (org-level-8 . 1.0)))
    (set-face-attribute (car face-scale) nil :weight 'bold :height (cdr face-scale)))

  (define-key calendar-mode-map (kbd "j") #'my/org-journal-open-from-calendar)
  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c C") #'calendar)
  (global-set-key (kbd "C-c j") #'my/open-journal-at-today)

  (global-set-key
   (kbd "C-c g")
   (lambda ()
     (interactive)
     (let ((default-directory "~/org/"))
       (magit-stage-modified)
       (magit-commit-create '("-m" "sync"))
       (magit-push-current-to-pushremote nil))))
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell      . t)
       (python     . t)
       (ruby       . t)
       (js         . t)
       (sql        . t)
       (sqlite     . t)
       (perl       . t)
       (java       . t)
       (C          . t)
       (dot        . t)
       (plantuml   . t)
       (latex      . t)))
   (setq org-confirm-babel-evaluate nil)))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-remoteimg
  :vc (:url "https://github.com/gaoDean/org-remoteimg"
       :rev :newest)
  :after org)

(use-package valign
  :ensure t
  :init
  (setq valign-fancy-bar t)
  :hook (org-mode . valign-mode))

(use-package org-modern
  :ensure t
  :after org
  :init
  (setq
   org-modern-table nil
   org-modern-variable-pitch nil   ;; keep monospaced so tables stay sane
   org-modern-star 'replace)
  :hook (org-mode . org-modern-mode))
