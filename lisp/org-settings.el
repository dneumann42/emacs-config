;; -*- lexical-binding: t; -*-

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
    (set-face-attribute (car face-scale) nil :weight 'bold :height (cdr face-scale))))
    
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
  
