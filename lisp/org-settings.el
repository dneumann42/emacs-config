;; -*- lexical-binding: t; -*-

(require 'org-datetree)

(setq
   my/org-journal-file "journal.org")

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

(use-package org
  :ensure t
  :init
  (setq
   org-hide-leading-stars t
   org-agenda-include-diary t
   org-startup-with-inline-images t
   org-image-actual-width 64
   org-display-remote-inline-images 'cache
   url-automatic-caching t)
  (setq org-capture-templates
        `(("j" "Journal Entry" entry (file+datetree (my/get-org-link my/org-journal-file))
           "* %?"
           :empty-lines 1)))
  :hook
  (org-mode . visual-line-mode)
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
  ;; Make calendar mode work with journal
  (define-key calendar-mode-map (kbd "j") #'my/org-journal-open-from-calendar)
  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c C") #'calendar)
  (global-set-key (kbd "C-c j") #'my/open-journal-at-today)
  (global-set-key (kbd "C-c g") (lambda () (interactive)
                                 (let ((default-directory "~/org/"))
                                   (magit-stage-modified)
                                   (magit-commit-create '("-m" "sync"))
                                   (magit-push-current-to-pushremote nil)))))
    
(use-package org-bullets
  :ensure t
  :after org
  :hook
  (org-mode . org-bullets-mode))

(use-package org-remoteimg
  :vc (:url "https://github.com/gaoDean/org-remoteimg"
       :rev :newest)
  :after org
  :config)
  
