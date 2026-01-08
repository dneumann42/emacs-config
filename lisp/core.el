;;; core.el --- Core settings -*- lexical-binding: t -*-


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'subr-x)
(require 'seq)
(require 'cl-lib)

;; Avoid auto-loading TAGS files (ntagger emits ctags format).
(setq tags-add-tables nil
      tags-file-name nil
      tags-table-list nil)

(defun my/ctags-tags-file-p (file)
  "Return non-nil when FILE looks like a ctags tags file."
  (when (and (stringp file) (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file nil 0 256)
      (goto-char (point-min))
      (re-search-forward "^!_TAG_FILE_FORMAT" nil t))))

(defun my/etags-skip-ctags (orig &rest args)
  "Skip ctags TAGS files when etags tries to load them."
  (let* ((arg (car args))
         (file (cond
                ((stringp arg) arg)
                ((and (boundp 'tags-file-name) tags-file-name))
                ((and (boundp 'tags-table-list) (car tags-table-list))))))
    (if (my/ctags-tags-file-p file)
        nil
      (apply orig args))))

(with-eval-after-load 'etags
  (advice-add 'visit-tags-table-buffer :around #'my/etags-skip-ctags))


;; Disable backup (~), auto-save (#), and lock (.#) files.
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Silence the audible bell; use a visual bell instead.
(setq ring-bell-function 'ignore
      visible-bell t)

;; Load custom settings from a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

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

(add-to-list 'default-frame-alist '(alpha-background . 90))
(set-frame-parameter nil 'alpha-background 90)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(line-number-mode 1)
(global-auto-revert-mode 1)
(pixel-scroll-precision-mode 1)

;; Dashboard
(setq inhibit-startup-screen t)

(defun my/dashboard ()
  (interactive)
  (let ((buffer-calendar "*Calendar*"))
    (calendar)
    (delete-other-windows)
    (switch-to-buffer (get-buffer buffer-calendar))))

(add-hook 'emacs-startup-hook #'my/dashboard)

(defun my/format-buffer ()
  "Indent the entire buffer using the active major mode."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(global-display-line-numbers-mode 1)
(prettify-symbols-mode 1)

(global-set-key (kbd "C-c t n") #'tab-line-switch-to-next-tab)
(global-set-key (kbd "C-c t p") #'tab-line-switch-to-prev-tab)
(global-set-key (kbd "C-M-<return>") #'my/eldoc-show-help-smart)
(global-set-key (kbd "C-x /") #'my/consult-ripgrep-project)

(defun my/consult-ripgrep-project ()
  "Live grep the current projectile/project root with Consult + ripgrep."
  (interactive)
  (let ((dir (or (when (fboundp 'projectile-project-root)
                   (ignore-errors (projectile-project-root)))
                 (when-let* ((proj (project-current)))
                   (car (project-roots proj)))
                 default-directory)))
    (consult-ripgrep dir)))

(defun my/eldoc-show-help ()
  "Show docs in a popup when possible, otherwise fall back to the echo area."
  (interactive)
  (require 'eldoc)
  (require 'eldoc-box)
  ;; Populate the doc buffer without showing it.
  (let ((display-buffer-overriding-action '((display-buffer-no-window))))
    (condition-case err
        (eldoc-print-current-symbol-info t)
      (error
       (message "Eldoc error: %s" (error-message-string err)))))
  (let ((eldoc--doc-buffer (eldoc-doc-buffer)))
    (when (and eldoc--doc-buffer
               (buffer-live-p eldoc--doc-buffer)
               (not (equal "" (with-current-buffer eldoc--doc-buffer (buffer-string)))))
      (if (display-graphic-p)
          (eldoc-box-help-at-point)
        (display-buffer eldoc--doc-buffer)))))

(defvar my/eldoc-focus-window nil)
(defvar my/eldoc-focus-frame nil)

(defun my/eldoc-show-help-window ()
  "Show eldoc in a regular window and focus it."
  (my/eldoc-show-help)
  (let ((buf (eldoc-doc-buffer)))
    (when (buffer-live-p buf)
      (setq my/eldoc-focus-window (selected-window)
            my/eldoc-focus-frame (selected-frame))
      (when (and (boundp 'eldoc-box--frame)
                 (frame-live-p eldoc-box--frame)
                 (frame-visible-p eldoc-box--frame))
        (eldoc-box-quit-frame))
      (let ((win (display-buffer
                  buf
                  '((display-buffer-reuse-window
                     display-buffer-below-selected)
                    (window-height . 0.3)))))
        (when (window-live-p win)
          (select-window win)
          (with-current-buffer buf
            (setq-local cursor-type t)
            (use-local-map (copy-keymap (current-local-map)))
            (local-set-key (kbd "q")
                           (lambda ()
                             (interactive)
                             (quit-window t)
                             (my/eldoc-restore-focus)))))))))

(defun my/eldoc-show-help-smart ()
  "Show eldoc; press twice to open a focused doc window."
  (interactive)
  (if (eq last-command 'my/eldoc-show-help-smart)
      (my/eldoc-show-help-window)
    (my/eldoc-show-help)))

(defun my/eldoc-restore-focus (&rest _)
  "Restore focus to the last window after eldoc-box hides."
  (when (frame-live-p my/eldoc-focus-frame)
    (select-frame-set-input-focus my/eldoc-focus-frame))
  (when (window-live-p my/eldoc-focus-window)
    (set-frame-selected-window (window-frame my/eldoc-focus-window) my/eldoc-focus-window)
    (select-window my/eldoc-focus-window))
  (setq my/eldoc-focus-window nil
        my/eldoc-focus-frame nil))

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
(add-hook 'lisp-interaction-mode-hook #'my/elisp-setup-keys)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Ensure elisp/builtin docstrings are available via Eldoc.
            (setq-local eldoc-documentation-function
                        #'elisp-eldoc-documentation-function)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Ensure elisp/builtin docstrings are available.
            (setq-local eldoc-documentation-function #'elisp-eldoc-documentation-function)))

(setq eldoc-echo-area-use-multiline-p t
      eldoc-idle-delay 0.2
      eldoc-documentation-strategy #'eldoc-documentation-compose
      eldoc-ellipsis "..."
      eldoc-echo-area-prefer-doc-buffer nil
      eldoc-doc-buffer-separator "\n\n")

(add-to-list 'display-buffer-alist
             '("\\*eldoc\\*" display-buffer-no-window))

(defun my/eldoc-doc-buffer-no-window (orig &optional interactive)
  "Call ORIG without ever displaying the *eldoc* buffer.
INTERACTIVE is ignored; always fetches the buffer silently."
  (let ((display-buffer-overriding-action '((display-buffer-no-window))))
    (funcall orig nil)))
(advice-add 'eldoc-doc-buffer :around #'my/eldoc-doc-buffer-no-window)

(global-eldoc-mode 1)

(defun my/eldoc-cleanup-markdown-code-fence (format-string &rest args)
  "Strip Markdown code fences and flatten newlines so Eldoc stays on one line."
  (when format-string
    (let* ((raw (apply #'format format-string args))
           (clean raw))
      ;; Drop an opening ```lang line and the closing ``` line.
      (setq clean (replace-regexp-in-string "\\`[ \t]*```[[:alnum:]-_]+[ \t]*\n" "" clean))
      (setq clean (replace-regexp-in-string "\n```[ \t]*\\'" "" clean))
      ;; Flatten remaining newlines to keep the echo area stable.
      (setq clean (string-trim (replace-regexp-in-string "[ \t]*\n[ \t]*" " " clean)))
      (eldoc-minibuffer-message "%s" clean))))

(defun my/nim-eldoc-cleanup-effects (format-string &rest args)
  "Drop Nim LSP effect markers like \"try !IOError\" from Eldoc strings."
  (when format-string
    (let* ((raw (apply #'format format-string args))
           (clean raw))
      ;; Remove Nim effect annotations/pragmas anywhere in the string.
      (setq clean (replace-regexp-in-string "\\s-+try\\b" "" clean))
      (setq clean (replace-regexp-in-string "\\s-+![^ \t\n(){}\\[\\],;]+" "" clean))
      (setq clean (replace-regexp-in-string "{\\.[^}]*\\}" "" clean))
      ;; Collapse any leftover repeated whitespace.
      (setq clean (replace-regexp-in-string "[ \t]+" " " clean))
      ;; Reuse the markdown/newline cleanup to keep output tidy.
      (my/eldoc-cleanup-markdown-code-fence "%s" clean))))

(defun my/nim-eglot-eldoc-filter (callback &rest _ignored)
  "Request Eldoc from Eglot and strip Nim effect noise before CALLBACK."
  (when (fboundp 'eglot-eldoc-function)
    (eglot-eldoc-function
     (lambda (&rest docs)
       (let* ((cleaned (mapcar (lambda (doc)
                                 (my/nim-eldoc-cleanup-effects "%s" doc))
                               docs))
              (non-empty (seq-remove #'string-empty-p cleaned)))
         (when non-empty
           (apply callback non-empty)))))))

(defun my/eglot-server-program-for-mode ()
  "Return the Eglot server program for the current buffer, if configured."
  (when (boundp 'eglot-server-programs)
    (seq-some
     (lambda (entry)
       (let ((modes (car entry))
             (program (cdr entry)))
         (cond
          ((symbolp modes)
           (when (derived-mode-p modes) program))
          ((and (listp modes) (seq-every-p #'symbolp modes))
           (when (apply #'derived-mode-p modes) program))
          ((functionp modes)
           (when (funcall modes major-mode) program))
          (t nil))))
     eglot-server-programs)))

(defun my/eglot-server-available-p ()
  "Return non-nil when a configured Eglot server executable is available."
  (when-let* ((program (my/eglot-server-program-for-mode)))
    (let ((cmd (if (functionp program) (funcall program) program)))
      (when (listp cmd)
        (let ((exe (car cmd)))
          (and (stringp exe) (executable-find exe)))))))

(defun my/eglot-ensure-maybe ()
  "Start Eglot only when a server is configured and available."
  (when (require 'eglot nil t)
    (cond
     ((derived-mode-p 'scheme-mode)
      (when (and (not (my/guile-lsp-command))
                 (bound-and-true-p eglot-managed-mode))
        (eglot-managed-mode -1)))
     ((derived-mode-p 'python-mode 'python-ts-mode)
      nil)
     ((my/eglot-server-available-p)
      (eglot-ensure)))))

(provide 'core)
