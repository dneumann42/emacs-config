;; -*- lexical-binding: t; -*-

(require 'org-datetree)
(require 'org)
(require 'org-element)
(require 'ob)
(require 'ob-tcl)

;;; Journal helpers

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

;;; Display helpers

(defun my/org-redisplay-inline-images ()
  (when (derived-mode-p 'org-mode)
    (run-with-idle-timer 0.15 nil #'org-redisplay-inline-images)))

(defun my/org-setup-pretty ()
  (setq-local org-link-descriptive t)
  (visual-line-mode 1)
  (my/org-redisplay-inline-images)
  (add-hook 'after-save-hook #'org-redisplay-inline-images nil t))

;;; Main org configuration

(use-package org
  :ensure t
  :init
  (setq org-hide-leading-stars t
        org-agenda-include-diary t
        org-link-descriptive t
        org-startup-with-inline-images t
        org-image-actual-width 64
        org-display-remote-inline-images 'cache
        url-automatic-caching t
        org-startup-folded nil
        org-confirm-babel-evaluate nil
        org-babel-default-header-args
        (cons '(:results . "output drawer replace")
              (assq-delete-all :results org-babel-default-header-args)))

  (setq org-capture-templates
        `(("j" "Journal Entry" entry
           (file+datetree ,(my/get-org-link my/org-journal-file))
           "* %?"
           :empty-lines 1)))

  :hook
  (org-mode . my/org-setup-pretty)

  :bind
  (("C-c c" . org-capture)
   ("C-c C" . calendar)
   ("C-c j" . my/open-journal-at-today)
   ("C-c g" . my/org-sync))

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
  (set-face-attribute 'org-level-1 nil :underline t)

  (with-eval-after-load 'calendar
    (define-key calendar-mode-map (kbd "j") #'my/org-journal-open-from-calendar))

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
     (latex      . t)
     (tcl        . t))))

(defun my/org-sync ()
  (interactive)
  (let ((default-directory "~/org/"))
    (magit-stage-modified)
    (magit-commit-create '("-m" "sync"))
    (magit-push-current-to-pushremote nil)))

;;; Org packages

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
  (setq org-modern-table nil
        org-modern-variable-pitch nil
        org-modern-star 'replace
        org-auto-align-tags t
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-agenda-tags-column 0
        org-ellipsis "…")

  :hook (org-mode . org-modern-mode)

  :config
  (set-face-attribute 'org-code nil
                      :inherit 'fixed-pitch
                      :background "#1e1e1e"
                      :box '(:line-width 1 :color "#333333" :style released-button))
  (set-face-attribute 'org-verbatim nil
                      :inherit 'fixed-pitch
                      :background "#1e1e1e"
                      :box '(:line-width 1 :color "#333333" :style released-button))
  (set-face-attribute 'org-block nil
                      :background "#1e1e1e"
                      :extend t)
  (set-face-attribute 'region nil
                      :background "#3e4a5b"
                      :extend t))

;;; Inline buttons

(defface my/org-button-face
  '((t :box (:line-width 1 :style released-button)
       :foreground "#88c0d0"
       :background "#2e3440"
       :weight bold))
  "Face for inline org buttons.")

(defvar my/org-inline-button-actions
  '(("Run"    . my/org-inline-button-run-next-src)
    ("Export" . my/org-inline-button-export-dispatch)
    ("Export PDF" . my/org-export-pdf))
  "Alist mapping button labels to functions.")

(defun my/org-inline-button--action (label)
  "Return function for LABEL, defaulting to a message."
  (or (cdr (assoc label my/org-inline-button-actions))
      (lambda ()
        (interactive)
        (message "Button pressed: %s" label))))

(defun my/org-inline-button-run-prev-src ()
  "Execute the previous Org babel source block."
  (interactive)
  (save-excursion
    (condition-case _err
        (progn
          (org-babel-previous-src-block 1)
          (org-babel-execute-src-block))
      (error (message "No previous source block found.")))))

(defun my/org-inline-button-run-next-src ()
  "Execute the next Org babel source block."
  (interactive)
  (save-excursion
    (condition-case _err
        (progn
          (org-babel-next-src-block 1)
          (org-babel-execute-src-block))
      (error (message "No next source block found.")))))

(defun my/org-inline-button-export-dispatch ()
  "Open Org export dispatch."
  (interactive)
  (call-interactively #'org-export-dispatch))

(defun my/org-export-pdf ()
  "Export pdf of org document"
  (interactive)
  (call-interactively #'org-latex-export-to-pdf))

(defun my/org-inline-buttons-clear ()
  "Remove all inline button overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'my-org-button t))

(defun my/org-activate-inline-buttons ()
  "Turn [button:LABEL] into clickable overlays without modifying text."
  (interactive)
  (my/org-inline-buttons-clear)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[button:\\([^]\n]+\\)\\]" nil t)
      (let* ((label (string-trim (match-string 1)))
             (start (match-beginning 0))
             (end   (match-end 0))
             (ov (make-overlay start end nil t t))
             (cmd (my/org-inline-button--action label))
             (map (make-sparse-keymap))
             (spc-cmd (lambda ()
                        (interactive)
                        (if (and (> (point) start) (< (point) end))
                            (funcall cmd)
                          (insert " ")))))
        (overlay-put ov 'priority 1000)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'display label)
        (define-key map [mouse-1] cmd)
        (define-key map (kbd "RET") cmd)
        (define-key map (kbd "SPC") spc-cmd)
        (overlay-put ov 'keymap map)
        (overlay-put ov 'mouse-face 'highlight)
        (overlay-put ov 'help-echo (format "%s" label))
        (overlay-put ov 'face 'my/org-button-face)
        (overlay-put ov 'my-org-button t)))))

(defun my/org-inline-buttons--after-change (_beg _end _len)
  "Re-activate inline buttons after edits."
  (when (derived-mode-p 'org-mode)
    (my/org-activate-inline-buttons)))

(define-minor-mode my/org-inline-buttons-mode
  "Minor mode to render [button:LABEL] markers as clickable UI buttons."
  :init-value nil
  :lighter " Btn"
  (if my/org-inline-buttons-mode
      (progn
        (my/org-activate-inline-buttons)
        (add-hook 'after-change-functions #'my/org-inline-buttons--after-change nil t)
        (add-hook 'after-save-hook #'my/org-activate-inline-buttons nil t))
    (remove-hook 'after-change-functions #'my/org-inline-buttons--after-change t)
    (remove-hook 'after-save-hook #'my/org-activate-inline-buttons t)
    (my/org-inline-buttons-clear)))

(defun my/org-insert-button-marker (label)
  "Insert an inline button marker [button:LABEL] at point."
  (interactive (list (read-string "Button label: " "Run")))
  (insert (format "[button:%s]" label))
  (when (bound-and-true-p my/org-inline-buttons-mode)
    (my/org-activate-inline-buttons)))

(defun my/org-insert-src-block-with-buttons (lang)
  "Insert a #+begin_src LANG ... #+end_src"
  (interactive
   (list (completing-read "Src language: "
                          (mapcar #'symbol-name (mapcar #'car org-babel-load-languages))
                          nil nil)))
  (let ((start (point)))
    (insert (format "#+begin_src %s\n" lang))
    (save-excursion
      (insert "\n#+end_src\n"))
    (goto-char (+ start (length (format "#+begin_src %s\n" lang))))
    (when (bound-and-true-p my/org-inline-buttons-mode)
      (my/org-activate-inline-buttons))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c s") #'my/org-insert-src-block-with-buttons))

(add-hook 'org-mode-hook #'my/org-inline-buttons-mode)

;;; Hide src block markers

(defvar-local my/org-hide-src-markers-overlays nil)

(defface my/org-src-lang-tag-face
  '((t :inherit shadow :weight bold :height 0.85))
  "Face used for the src language tag.")

(defun my/org-hide-src-markers-clear ()
  (when my/org-hide-src-markers-overlays
    (mapc #'delete-overlay my/org-hide-src-markers-overlays)
    (setq my/org-hide-src-markers-overlays nil)))

(defun my/org-hide-src-markers ()
  "Hide #+end_src, and replace #+begin_src LANG with a small LANG tag."
  (interactive)
  (my/org-hide-src-markers-clear)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+begin_src\\s-+\\([^ \t\n]+\\).*\\(?:\n\\|\\'\\)" nil t)
      (let* ((lang (match-string 1))
             (ov (make-overlay (match-beginning 0) (match-end 0) nil t t)))
        (overlay-put ov 'display
                     (concat (propertize lang 'face 'my/org-src-lang-tag-face) "\n"))
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'priority 1000)
        (push ov my/org-hide-src-markers-overlays)))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+end_src.*\\(?:\n\\|\\'\\)" nil t)
      (let ((ov (make-overlay (match-beginning 0) (match-end 0) nil t t)))
        (overlay-put ov 'display "")
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'priority 1000)
        (push ov my/org-hide-src-markers-overlays)))))

(defun my/org-hide-src-markers--after-change (&rest _)
  (when (derived-mode-p 'org-mode)
    (my/org-hide-src-markers)))

(define-minor-mode my/org-hide-src-markers-mode
  "Visually hide Org src block markers; show language tag at top-left."
  :init-value nil
  :lighter " Ⓢ"
  (if my/org-hide-src-markers-mode
      (progn
        (my/org-hide-src-markers)
        (add-hook 'after-change-functions #'my/org-hide-src-markers--after-change nil t)
        (add-hook 'after-save-hook #'my/org-hide-src-markers nil t))
    (remove-hook 'after-change-functions #'my/org-hide-src-markers--after-change t)
    (remove-hook 'after-save-hook #'my/org-hide-src-markers t)
    (my/org-hide-src-markers-clear)))

(add-hook 'org-mode-hook #'my/org-hide-src-markers-mode)

;;; Pretty results display

(defun my/org-pretty-results ()
  "Overlay RESULT drawers so you mostly see just the value."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^:results:\\(\n\\(?:.*\n\\)*?\\):end:\n?" nil t)
      (let* ((start (match-beginning 0))
             (end   (match-end 0))
             (body  (match-string 1))
             (value (string-trim body))
             (ov (make-overlay start end nil t t)))
        (overlay-put ov 'my-pretty-results t)
        (overlay-put ov 'priority 1100)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'display
                     (concat
                      (propertize "Results: " 'face 'shadow)
                      (propertize value 'face 'org-code)
                      "\n"))))))

(defun my/org-pretty-results-clear ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'my-pretty-results t))

(defun my/org-pretty-results--after-change (&rest _)
  (my/org-pretty-results))

(define-minor-mode my/org-pretty-results-mode
  "Pretty display for Org :results: drawers."
  :init-value nil
  :lighter " Res"
  (if my/org-pretty-results-mode
      (progn
        (my/org-pretty-results)
        (add-hook 'after-save-hook #'my/org-pretty-results nil t)
        (add-hook 'after-change-functions #'my/org-pretty-results--after-change nil t))
    (remove-hook 'after-save-hook #'my/org-pretty-results t)
    (remove-hook 'after-change-functions #'my/org-pretty-results--after-change t)
    (my/org-pretty-results-clear)))

(add-hook 'org-mode-hook #'my/org-pretty-results-mode)

;;; Hide RESULTS labels

(defvar-local my/org-hide-results-label-overlays nil)

(defun my/org-hide-results-label-clear ()
  (when my/org-hide-results-label-overlays
    (mapc #'delete-overlay my/org-hide-results-label-overlays)
    (setq my/org-hide-results-label-overlays nil)))

(defun my/org-hide-results-label ()
  "Hide Org RESULTS: keyword lines."
  (interactive)
  (my/org-hide-results-label-clear)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+RESULTS:.*\n" nil t)
      (let ((ov (make-overlay (match-beginning 0) (match-end 0) nil t t)))
        (overlay-put ov 'display "")
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'priority 1000)
        (push ov my/org-hide-results-label-overlays)))))

(defun my/org-hide-results-label--after-change (&rest _)
  (my/org-hide-results-label))

(define-minor-mode my/org-hide-results-label-mode
  "Visually hide Org RESULTS: labels."
  :init-value nil
  :lighter " Ⓡ"
  (if my/org-hide-results-label-mode
      (progn
        (my/org-hide-results-label)
        (add-hook 'after-change-functions #'my/org-hide-results-label--after-change nil t)
        (add-hook 'after-save-hook #'my/org-hide-results-label nil t))
    (remove-hook 'after-change-functions #'my/org-hide-results-label--after-change t)
    (remove-hook 'after-save-hook #'my/org-hide-results-label t)
    (my/org-hide-results-label-clear)))

(add-hook 'org-mode-hook #'my/org-hide-results-label-mode)

(provide 'org-settings)
