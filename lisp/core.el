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

(defvar my/dashboard-buffer-name "*Dashboard*")

(defvar-local my/dashboard-projects nil)

(defface my/dashboard-title-face
  '((t :weight bold :height 1.3))
  "Face for the dashboard title.")

(defface my/dashboard-section-face
  '((t :weight bold :foreground "#6fa8dc"))
  "Face for dashboard section headers.")

(defface my/dashboard-help-face
  '((t :foreground "#93c47d"))
  "Face for dashboard help text.")

(defface my/dashboard-project-face
  '((t :foreground "#ffd966"))
  "Face for dashboard project entries.")

(defface my/dashboard-muted-face
  '((t :foreground "#999999"))
  "Face for muted dashboard text.")

(defun my/dashboard-open-project-1 () (interactive) (my/dashboard-open-project-index 1))
(defun my/dashboard-open-project-2 () (interactive) (my/dashboard-open-project-index 2))
(defun my/dashboard-open-project-3 () (interactive) (my/dashboard-open-project-index 3))
(defun my/dashboard-open-project-4 () (interactive) (my/dashboard-open-project-index 4))
(defun my/dashboard-open-project-5 () (interactive) (my/dashboard-open-project-index 5))
(defun my/dashboard-open-project-6 () (interactive) (my/dashboard-open-project-index 6))
(defun my/dashboard-open-project-7 () (interactive) (my/dashboard-open-project-index 7))
(defun my/dashboard-open-project-8 () (interactive) (my/dashboard-open-project-index 8))
(defun my/dashboard-open-project-9 () (interactive) (my/dashboard-open-project-index 9))

(defvar my/dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'my/dashboard-refresh)
    (define-key map (kbd "1") #'my/dashboard-open-project-1)
    (define-key map (kbd "2") #'my/dashboard-open-project-2)
    (define-key map (kbd "3") #'my/dashboard-open-project-3)
    (define-key map (kbd "4") #'my/dashboard-open-project-4)
    (define-key map (kbd "5") #'my/dashboard-open-project-5)
    (define-key map (kbd "6") #'my/dashboard-open-project-6)
    (define-key map (kbd "7") #'my/dashboard-open-project-7)
    (define-key map (kbd "8") #'my/dashboard-open-project-8)
    (define-key map (kbd "9") #'my/dashboard-open-project-9)
    map)
  "Keymap for `my/dashboard-mode'.")

(define-derived-mode my/dashboard-mode special-mode "Dashboard"
  "Major mode for the Emacs start screen.")
(set-keymap-parent my/dashboard-mode-map special-mode-map)

(defun my/dashboard-projects ()
  "Return a list of recent project roots."
  (cond
   ((fboundp 'projectile-load-known-projects)
    (projectile-load-known-projects)
    (or (and (boundp 'projectile-known-projects)
             projectile-known-projects)
        (and (fboundp 'projectile-relevant-known-projects)
             (projectile-relevant-known-projects))))
   ((fboundp 'project-known-project-roots)
    (project-known-project-roots))
   (t nil)))

(defun my/dashboard-open-project-index (index)
  "Open the project at INDEX in the dashboard list."
  (interactive)
  (let ((project (nth (1- index) my/dashboard-projects)))
    (unless project
      (user-error "No project at %d" index))
    (if (fboundp 'projectile-switch-project-by-name)
        (projectile-switch-project-by-name project)
      (dired project))))

(defun my/dashboard--calendar-string ()
  "Return a string for the current month calendar."
  (let* ((date (calendar-current-date))
         (month (nth 0 date))
         (year (nth 2 date)))
    (with-temp-buffer
      (calendar-generate-month month year 1)
      (buffer-string))))

(defun my/dashboard--insert-agenda ()
  "Insert today's agenda into the dashboard buffer."
  (save-window-excursion
    (require 'org-agenda)
    (let ((org-agenda-span 1)
          (org-agenda-start-on-weekday nil)
          (org-agenda-window-setup 'current-window))
      (org-agenda-list 1)
      (when-let ((buf (get-buffer "*Org Agenda*")))
        (with-current-buffer buf
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defun my/dashboard-refresh ()
  "Refresh the dashboard contents."
  (interactive)
  (let ((buf (get-buffer-create my/dashboard-buffer-name)))
    (with-current-buffer buf
      (my/dashboard-mode)
      (use-local-map my/dashboard-mode-map)
      (setq-local overriding-local-map my/dashboard-mode-map)
      (setq my/dashboard-projects (my/dashboard-projects))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Start" 'face 'my/dashboard-title-face) "\n\n")
        (insert (propertize "Calendar" 'face 'my/dashboard-section-face) "\n")
        (insert (propertize "────────" 'face 'my/dashboard-muted-face) "\n")
        (insert (my/dashboard--calendar-string))
        (insert "\n")
        (insert "\n" (propertize "Agenda" 'face 'my/dashboard-section-face) "\n")
        (insert (propertize "──────" 'face 'my/dashboard-muted-face) "\n")
        (let ((agenda (my/dashboard--insert-agenda)))
          (if (and agenda (stringp agenda))
              (insert agenda)
            (insert "No agenda items.\n")))
        (insert "\n" (propertize "Recent Projects" 'face 'my/dashboard-section-face) "\n")
        (insert (propertize "──────────────" 'face 'my/dashboard-muted-face) "\n")
        (insert (propertize "Keys: 1-9 open project, g refresh"
                            'face 'my/dashboard-help-face)
                "\n\n")
        (if my/dashboard-projects
            (cl-loop for proj in (seq-take my/dashboard-projects 9)
                     for idx from 1 do
                     (insert (propertize (format "%d. %s\n" idx proj)
                                         'face 'my/dashboard-project-face)))
          (insert (propertize "No projects found.\n"
                              'face 'my/dashboard-muted-face)))))))

(defun my/dashboard-setup-windows ()
  "Show the start screen in a single window."
  (delete-other-windows)
  (my/dashboard-refresh)
  (switch-to-buffer my/dashboard-buffer-name))

(defun my/dashboard ()
  "Open the start screen."
  (interactive)
  (my/dashboard-setup-windows))

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
(global-set-key (kbd "C-c |") #'my/toggle-window-split)
(global-set-key (kbd "C-M-<return>") #'my/eldoc-show-help-smart)
(global-set-key (kbd "C-x /") #'my/consult-ripgrep-project)
(global-set-key (kbd "C-x f") #'my/consult-projectile-find-file)

(defun my/consult-ripgrep-project ()
  "Live grep the current projectile/project root with Consult + ripgrep."
  (interactive)
  (let ((dir (or (when (fboundp 'projectile-project-root)
                   (ignore-errors (projectile-project-root)))
                 (when-let* ((proj (project-current)))
                   (car (project-roots proj)))
                 default-directory)))
    (consult-ripgrep dir)))

(defun my/window-state-toggle-split (state)
  "Return window STATE with split directions toggled."
  (cond
   ((eq state 'vc) 'hc)
   ((eq state 'hc) 'vc)
   ((consp state)
    (if (and (fboundp 'proper-list-p) (proper-list-p state))
        (mapcar #'my/window-state-toggle-split state)
      (cons (my/window-state-toggle-split (car state))
            (my/window-state-toggle-split (cdr state)))))
   (t state)))

(defun my/window-main-root ()
  "Return the root window of the main area, excluding side windows."
  (let ((root (frame-root-window)))
    (if-let ((child (window-child root)))
        (let ((node child)
              found)
          (while node
            (if (window-parameter node 'window-side)
                (setq node (window-next-sibling node))
              (setq found node
                    node nil)))
          (or found (window-main-window)))
      (window-main-window))))

(defun my/side-window-list ()
  "Return a list of live side windows on the current frame."
  (seq-filter (lambda (win)
                (window-parameter win 'window-side))
              (window-list nil 'no-minibuf)))

(defun my/main-window-list ()
  "Return a list of non-side windows on the current frame."
  (seq-filter (lambda (win)
                (and (not (window-parameter win 'window-side))
                     (with-current-buffer (window-buffer win)
                       (not (derived-mode-p 'treemacs-mode)))))
              (window-list nil 'no-minibuf)))

(defun my/side-window-snapshots ()
  "Capture buffers and geometry for current side windows."
  (let ((snaps
         (mapcar (lambda (win)
                   (list :buffer (window-buffer win)
                         :side (window-parameter win 'window-side)
                         :slot (window-parameter win 'window-slot)
                         :width (window-total-width win)
                         :height (window-total-height win)
                         :dedicated (window-dedicated-p win)))
                 (my/side-window-list))))
    (when-let* ((treemacs-win (when (fboundp 'treemacs-get-local-window)
                                (treemacs-get-local-window)))
                (buf (and (window-live-p treemacs-win)
                          (window-buffer treemacs-win))))
      (push (list :buffer buf
                  :side 'left
                  :slot 0
                  :width (window-total-width treemacs-win)
                  :height (window-total-height treemacs-win)
                  :dedicated (window-dedicated-p treemacs-win)
                  :force-side t)
            snaps))
    snaps))

(defun my/restore-side-windows (snapshots)
  "Restore side windows from SNAPSHOTS."
  (dolist (snap snapshots)
    (let* ((buf (plist-get snap :buffer))
           (force-side (plist-get snap :force-side))
           (side (or (plist-get snap :side) 'left))
           (slot (plist-get snap :slot))
           (width (plist-get snap :width))
           (height (plist-get snap :height))
           (params `((side . ,side)
                     (slot . ,slot)))
           (win (if force-side
                    (display-buffer-in-side-window buf params)
                  (display-buffer-in-side-window buf params))))
      (when (window-live-p win)
        (set-window-dedicated-p win (plist-get snap :dedicated))
        (when (memq side '(left right))
          (ignore-errors
            (window-resize win (- width (window-total-width win)) t)))
        (when (memq side '(top bottom))
          (ignore-errors
            (window-resize win (- height (window-total-height win)) nil)))))))

(defun my/toggle-two-window-split ()
  "Toggle split direction when exactly two main windows are present."
  (let* ((wins (my/main-window-list)))
    (when (= (length wins) 2)
      (let* ((w1 (nth 0 wins))
             (w2 (nth 1 wins))
             (buf1 (window-buffer w1))
             (buf2 (window-buffer w2))
             (start1 (window-start w1))
             (start2 (window-start w2))
             (point1 (window-point w1))
             (point2 (window-point w2))
             (sel1 (eq (selected-window) w1))
             (split-horiz (window-combined-p w1 'horizontal)))
        (select-window w1)
        (delete-other-windows w1)
        (if split-horiz
            (split-window-vertically)
          (split-window-horizontally))
        (let ((new1 (selected-window))
              (new2 (next-window)))
          (set-window-buffer new1 buf1)
          (set-window-buffer new2 buf2)
          (set-window-start new1 start1)
          (set-window-start new2 start2)
          (set-window-point new1 point1)
          (set-window-point new2 point2)
          (select-window (if sel1 new1 new2))))
      t)))

(defun my/rebuild-splits-opposite ()
  "Rebuild main splits with opposite orientation, preserving buffers."
  (let* ((wins (my/main-window-list))
         (count (length wins)))
    (when (> count 1)
      (let* ((bufs (mapcar #'window-buffer wins))
             (sel (selected-window))
             (side-by-side (window-combined-p (car wins) 'horizontal))
             (split-fn (if side-by-side
                           #'split-window-vertically
                         #'split-window-horizontally)))
        (delete-other-windows (car wins))
        (dotimes (_ (1- count))
          (funcall split-fn)
          (select-window (next-window)))
        (let ((new-wins (my/main-window-list)))
          (cl-mapc (lambda (w b) (set-window-buffer w b))
                   new-wins bufs)
          (when (window-live-p sel)
            (select-window sel))))
      t)))

(defun my/toggle-window-split ()
  "Toggle all window splits between horizontal and vertical."
  (interactive)
  (let* ((cur (selected-window))
         (cur-frame (selected-frame))
         (side-snapshots (my/side-window-snapshots))
         (_state (window-state-get (frame-root-window) t)))
    (dolist (win (my/side-window-list))
      (delete-window win))
    (when-let* ((treemacs-win (when (fboundp 'treemacs-get-local-window)
                                (treemacs-get-local-window))))
      (when (window-live-p treemacs-win)
        (delete-window treemacs-win)))
    (let ((ok (my/rebuild-splits-opposite)))
      (unless ok
        (message "Toggle split: no change (layout too constrained).")))
    (my/restore-side-windows side-snapshots)
    (when (window-live-p cur)
      (select-frame-set-input-focus cur-frame)
      (select-window cur))))



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
