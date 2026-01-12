;;; lang.el --- Language and LSP setup -*- lexical-binding: t -*-

(use-package eglot
  :ensure t
  :requires (eldoc-box)
  :hook
  (prog-mode . my/eglot-ensure-maybe)
  :config
  ;; Disable inlay hints globally unless explicitly re-enabled elsewhere.
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)
  (defun my/eglot-setup-keys ()
    "Keep Eglot bindings consistent across languages (clang, TS, etc.)."
    (local-set-key (kbd "M-<return>") #'eglot-code-actions)
    (local-set-key (kbd "C-c f") #'eglot-format)
    (local-set-key (kbd "<f2>") #'eglot-rename)
    (local-set-key (kbd "C-c d") #'xref-find-definitions)
    (local-set-key (kbd "C-c b") #'xref-pop-marker-stack)
    (local-set-key (kbd "C-c h") #'my/eldoc-show-help)
    (local-set-key (kbd "C-M-<return>") #'my/eldoc-show-help-smart))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-setup-keys)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local eldoc-message-function #'my/eldoc-cleanup-markdown-code-fence)))
  (add-hook 'eglot-managed-mode-hook #'flymake-mode)
  (defun my/clangd-command ()
    ;; Prefer system clangd; keeps args centralized.
    (or (executable-find "clangd") "clangd"))
  (add-to-list 'eglot-server-programs
               `((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode objc-ts-mode)
                 . ,(list (my/clangd-command)
                          "--background-index"
                          "--clang-tidy"
                          "--completion-style=detailed"
                          "--header-insertion=never"
                          "--pch-storage=memory")))
  (add-to-list 'eglot-server-programs
               '((typescript-mode typescript-ts-mode tsx-ts-mode
                                  js-mode js-ts-mode)
                 . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((json-mode json-ts-mode)
                 . ("vscode-json-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((yaml-mode yaml-ts-mode)
                 . ("yaml-language-server" "--stdio")))
  (defun my/rust-analyzer-command ()
    ;; Prefer system rust-analyzer; keeps args centralized.
    (or (executable-find "rust-analyzer") "rust-analyzer"))
  (add-to-list 'eglot-server-programs
               `((rust-mode rust-ts-mode)
                 . ,(list (my/rust-analyzer-command)))))

(use-package scheme
  :ensure nil
  :init
  (add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
  (add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode)))

(defun my/guile-lsp-command ()
  "Return the Guile LSP command list when available."
  (cond
   ((executable-find "guile-lsp-server") (list "guile-lsp-server"))
   ((executable-find "guile-lsp") (list "guile-lsp"))
   (t nil)))

(with-eval-after-load 'eglot
  (when-let* ((cmd (my/guile-lsp-command)))
    (add-to-list 'eglot-server-programs `((scheme-mode) . ,cmd))))

(defun my/c-style ()
  (setq-local c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil))

(add-hook 'c-mode-common-hook #'my/c-style)

(use-package typescript-mode
  :ensure t
  :init
  (setq typescript-indent-level 2)
  :hook
  ((typescript-mode typescript-ts-mode tsx-ts-mode js-mode js-ts-mode json-mode yaml-mode)
   . my/node-add-bin-to-path)
  ((typescript-mode typescript-ts-mode tsx-ts-mode js-mode js-ts-mode)
   . my/typescript-eldoc-box-prettify))

(use-package rust-mode
  :ensure t
  :init
  (setq rust-format-on-save t)
  :hook
  ((rust-mode rust-ts-mode) . my/eglot-ensure-maybe))

(defun my/typescript-eldoc-box-prettify ()
  "Prettify TS/JS Eldoc popups by using eldoc-box' TS formatter."
  (when (require 'eldoc-box nil t)
    (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t)))

;; Python config - prefer uv-managed virtualenvs and local language servers.
(defun my/python-uv-venv-root ()
  "Return the root directory of a uv-managed venv when available."
  (when-let* ((root (locate-dominating-file default-directory ".venv"))
              (venv (expand-file-name ".venv" root)))
    (when (file-directory-p venv)
      venv)))

(defun my/python-add-uv-venv-to-path ()
  "Add a uv venv's bin directory to PATH/exec-path and set python."
  (let ((uv-shebang (save-excursion
                      (goto-char (point-min))
                      (looking-at
                       "^#!.*/env -S uv run --quiet\\b"))))
    (when uv-shebang
      (setq-local python-shell-interpreter "uv")
      (setq-local python-shell-interpreter-args "run --quiet"))
    (when-let* ((venv (my/python-uv-venv-root))
                (bin (expand-file-name "bin" venv)))
      (when (file-directory-p bin)
        (setq-local exec-path (cons bin (seq-remove (lambda (p) (string= p bin)) exec-path)))
        (let* ((path-var (concat "PATH=" bin path-separator (getenv "PATH")))
               (existing (seq-remove (lambda (env) (string-prefix-p "PATH=" env))
                                     process-environment)))
          (setq-local process-environment (cons path-var existing)))
        (let ((py (expand-file-name "python" bin)))
          (when (and (file-executable-p py)
                     (not uv-shebang))
            (setq-local python-shell-interpreter py)))))))

(defun my/python-venv-bin-exe (exe)
  "Return EXE from a uv venv bin dir when available."
  (when-let* ((venv (my/python-uv-venv-root))
              (bin (expand-file-name "bin" venv))
              (path (expand-file-name exe bin)))
    (when (file-executable-p path)
      path)))

(defun my/python-eglot-server (&rest _ignored)
  "Return the Python LSP server command list when available."
  (cond
   ((or (my/python-venv-bin-exe "basedpyright-langserver")
        (executable-find "basedpyright-langserver"))
    (list (or (my/python-venv-bin-exe "basedpyright-langserver")
              "basedpyright-langserver")
          "--stdio"))
   ((or (my/python-venv-bin-exe "pyright-langserver")
        (executable-find "pyright-langserver"))
    (list (or (my/python-venv-bin-exe "pyright-langserver")
              "pyright-langserver")
          "--stdio"))
   ((or (my/python-venv-bin-exe "pylsp")
        (executable-find "pylsp"))
    (list (or (my/python-venv-bin-exe "pylsp")
              "pylsp")))
   (t nil)))

;; Prefer a local Python LSP when available.
(with-eval-after-load 'eglot
  ;; Resolve the Python LSP command at connection time so uv venvs are respected.
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) . my/python-eglot-server)))

;; Activate uv venvs for Python buffers.
(defun my/python-setup-eglot ()
  "Configure PATH for uv venvs and start Eglot for Python."
  (my/python-add-uv-venv-to-path)
  (require 'eglot)
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) . my/python-eglot-server))
  (when (my/eglot-server-available-p)
    (eglot-ensure)))

(add-hook 'python-mode-hook #'my/python-setup-eglot)
(add-hook 'python-ts-mode-hook #'my/python-setup-eglot)

;; Nim config - nimsuggest for docs, nim check for diagnostics.

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (boundp 'treesit-language-source-alist))
  (add-to-list 'treesit-language-source-alist
               '(nim "https://github.com/alaviss/tree-sitter-nim")))

(use-package epc
  :ensure t)

(use-package deferred
  :ensure t)

(use-package nim-mode
  :ensure nil
  :init
  (setq tags-add-tables nil)
  (setq tags-file-name nil)
  (setq tags-table-list nil)
  (require 'nim-mode nil t))

(defun my/nim-mode-dispatch ()
  "Use tree-sitter Nim mode when available."
  (interactive)
  (require 'nim-ts-mode nil t)
  (if (and (fboundp 'nim-ts-mode)
           (fboundp 'treesit-available-p)
           (treesit-available-p)
           (treesit-language-available-p 'nim))
      (nim-ts-mode)
    (nim-mode)))

(add-to-list 'auto-mode-alist '("\\.nim\\'" . my/nim-mode-dispatch))
(add-to-list 'auto-mode-alist '("\\.nimble\\'" . my/nim-mode-dispatch))
(add-to-list 'auto-mode-alist '("\\.nims\\'" . nimscript-mode))

(defvar nimsuggest-options nil)
(defvar nimsuggest-local-options nil)
(defvar nimsuggest-accept-process-delay 0.1)
(defvar nimsuggest-accept-process-timeout-count 300)
(defvar nimsuggest-dirty-directory
  (expand-file-name "nimsuggest/" temporary-file-directory)
  "Directory for nimsuggest temp files.")

(with-eval-after-load 'nim-mode
  (require 'deferred nil t)
  (condition-case err
      (require 'nim-suggest)
    (error
     (message "Nim suggest skipped: %s" (error-message-string err))))
  (with-eval-after-load 'nim-suggest
    (setq nimsuggest-accept-process-delay 0.2
          nimsuggest-accept-process-timeout-count 1200)
    (defun my/nimsuggest-start-with-pipe (orig &rest args)
      "Start nimsuggest using a pipe instead of a PTY."
      (let ((process-connection-type nil))
        (apply orig args)))
    (advice-add 'nimsuggest--start-server-deferred :around #'my/nimsuggest-start-with-pipe))
  (let ((nimble-nimsuggest (expand-file-name "~/.nimble/bin/nimsuggest")))
    (setq nimsuggest-path
          (or (executable-find "nimsuggest")
              (when (file-executable-p nimble-nimsuggest) nimble-nimsuggest)))))
(use-package flycheck
  :ensure t)

(defvar-local my/nim-flymake--process nil)
(defconst my/nim-flymake--diagnostic-regexp
  "^\\(.*\\)(\\([0-9]+\\), \\([0-9]+\\)) \\(Error\\|Warning\\|Hint\\): \\(.*\\)$")

(defun my/nim-flymake--severity (kind)
  "Map Nim diagnostic KIND to Flymake severity."
  (pcase kind
    ("Error" :error)
    ("Warning" :warning)
    (_ :note)))

(defun my/nim-flymake-backend (report-fn &rest _args)
  "Run `nim check` and report diagnostics for the current buffer."
  (when (process-live-p my/nim-flymake--process)
    (kill-process my/nim-flymake--process))
  (let ((source (buffer-file-name)))
    (if (or (not source) (not (executable-find "nim")))
        (funcall report-fn nil)
      (let* ((buf (current-buffer))
             (default-directory (file-name-directory source))
             (output (generate-new-buffer " *nim-check*"))
             (cmd (list "nim" "check" "--listFullPaths" "--hints:on"
                        "--warnings:on" source)))
        (setq my/nim-flymake--process
              (make-process
               :name "nim-flymake"
               :buffer output
               :stderr output
               :command cmd
               :noquery t
               :sentinel
               (lambda (proc _event)
                 (when (and (memq (process-status proc) '(exit signal))
                            (eq proc my/nim-flymake--process))
                   (unwind-protect
                       (when (buffer-live-p buf)
                         (with-current-buffer buf
                           (let (diags)
                             (when (buffer-live-p output)
                               (with-current-buffer output
                                 (goto-char (point-min))
                                 (while (re-search-forward my/nim-flymake--diagnostic-regexp nil t)
                                   (let* ((file (match-string 1))
                                          (line (string-to-number (match-string 2)))
                                          (col (string-to-number (match-string 3)))
                                          (kind (match-string 4))
                                          (msg (match-string 5))
                                          (abs (expand-file-name file)))
                                     (when (file-equal-p abs source)
                                       (let* ((region (flymake-diag-region buf line (max 1 col)))
                                              (beg (car region))
                                              (end (cdr region))
                                              (severity (my/nim-flymake--severity kind)))
                                         (push (flymake-make-diagnostic buf beg end severity msg)
                                               diags)))))))
                             (funcall report-fn diags))))
                     (when (buffer-live-p output)
                       (kill-buffer output)))
                   (setq my/nim-flymake--process nil)))))))))

(defun my/nim-flymake-setup ()
  "Enable `nim check` diagnostics for the current buffer."
  (require 'flymake)
  (setq-local flymake-no-changes-timeout nil)
  (add-hook 'flymake-diagnostic-functions #'my/nim-flymake-backend nil t)
  (flymake-mode 1)
  (flymake-start)
  (add-hook 'after-save-hook #'flymake-start nil t))

(defun my/nim--line-increases-indent-p (line)
  "Return non-nil when LINE should increase indentation."
  (or (string-match-p ":[[:space:]]*\\(#.*\\)?$" line)
      (string-match-p "=[[:space:]]*\\(#.*\\)?$" line)
      (string-match-p "^[[:space:]]*\\(let\\|var\\|const\\|type\\|case\\|proc\\|func\\|method\\|iterator\\|template\\|macro\\|converter\\|block\\|if\\|for\\|while\\|try\\|when\\)\\_>.*=[[:space:]]*\\(#.*\\)?$" line)
      (string-match-p "^[[:space:]]*\\w+[[:space:]]*=[[:space:]]*\\(object\\|tuple\\|enum\\)\\_>[[:space:]]*\\(#.*\\)?$" line)))

(defun my/nim--line-outdents-p ()
  "Return non-nil when the current line should outdent."
  (save-excursion
    (back-to-indentation)
    (looking-at-p "\\(else\\|elif\\|of\\|except\\|finally\\)\\_>")))

(defun my/nim--current-line-top-level-p ()
  "Return non-nil when the current line starts a top-level block."
  (save-excursion
    (back-to-indentation)
    (looking-at-p
     "\\(proc\\|func\\|method\\|iterator\\|template\\|macro\\|converter\\|type\\|var\\|let\\|const\\|when\\|if\\|for\\|while\\|try\\|block\\)\\_>")))

(defun my/nim--prev-line-blank-p ()
  "Return non-nil when the previous line is blank."
  (save-excursion
    (forward-line -1)
    (looking-at "^[[:space:]]*$")))

(defun my/nim--parent-line-info ()
  "Return a plist with the parent line's :indent and :line."
  (save-excursion
    (let ((base-indent nil)
          (parent-indent nil)
          (parent-line nil))
      (while (and (not (bobp)) (null base-indent))
        (forward-line -1)
        (unless (looking-at "^[[:space:]]*$")
          (setq base-indent (current-indentation))))
      (when base-indent
        (while (and (not (bobp)) (null parent-indent))
          (forward-line -1)
          (unless (looking-at "^[[:space:]]*$")
            (when (< (current-indentation) base-indent)
              (setq parent-indent (current-indentation))
              (setq parent-line
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))))))
      (when parent-indent
        (list :indent parent-indent :line parent-line)))))

(defun my/nim-indent-line ()
  "Indent Nim line using simple block heuristics."
  (let* ((offset (if (boundp 'nim-indent-offset) nim-indent-offset 2))
         (savep (> (current-column) (current-indentation)))
         (parent (my/nim--parent-line-info))
         (indent (condition-case _err
                     (save-excursion
                       (let ((found nil)
                             (base 0)
                             (increase nil))
                         (while (and (not found) (not (bobp)))
                           (forward-line -1)
                           (unless (looking-at "^[[:space:]]*$")
                             (setq found t)
                             (setq base (current-indentation))
                             (setq increase
                                   (my/nim--line-increases-indent-p
                                    (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))))
                         (max 0 (+ base (if increase offset 0)))))
                   (error 0))))
    (when (my/nim--line-outdents-p)
      (setq indent (max 0 (- indent offset))))
    (when (and (my/nim--current-line-top-level-p)
               (plist-get parent :line)
               (string-match-p "^[[:space:]]*type\\_>" (plist-get parent :line)))
      ;; When leaving a type block, align with the `type` line.
      (setq indent (plist-get parent :indent)))
    (when (and (my/nim--current-line-top-level-p)
               (my/nim--prev-line-blank-p)
               (plist-get parent :indent))
      ;; After a blank line, reset top-level forms to their parent indent.
      (setq indent (plist-get parent :indent)))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun my/nim-safe-indent ()
  "Indent Nim safely; fall back instead of throwing SMIE errors."
  (interactive)
  (condition-case err
      (cond
       ((fboundp 'my/nim-indent-line) (my/nim-indent-line))
       ((fboundp 'nim--indent-line) (nim--indent-line))
       ((fboundp 'nim-indent-line) (nim-indent-line))
       (t (indent-relative)))
    (error
     (message "Nim indent fallback: %s" (error-message-string err))
     (cond
      ((fboundp 'my/nim-indent-line) (my/nim-indent-line))
      ((fboundp 'nim--indent-line) (nim--indent-line))
      (t (indent-relative))))))

(defun my/nim-safe-forward-sexp (arg)
  "Move forward across Nim sexps safely, falling back on errors.
ARG is forwarded to `smie-forward-sexp' or `forward-sexp'."
  (interactive "p")
  (condition-case err
      ;; Bind `forward-sexp-function' to nil so nested calls use built-in behavior.
      (let ((forward-sexp-function nil))
        (forward-sexp arg))
    (error
     (message "Nim sexp fallback: %s" (error-message-string err))))) 

(defvar my/nimsuggest-completion-timeout 0.35
  "Maximum time in seconds to wait for nimsuggest completions.")

(defun my/nimsuggest--candidate-name (item)
  "Return the best display name for ITEM."
  (let ((qpath (nim--epc-qpath item)))
    (cond
     ((stringp qpath) qpath)
     ((and (listp qpath) (car (last qpath))) (car (last qpath)))
     (t nil))))

(defun my/nimsuggest--call-sync (method callback)
  "Call nimsuggest with METHOD and return CALLBACK result or nil on timeout."
  (let* ((buf (current-buffer))
         (start (time-to-seconds))
         (res :pending))
    (nimsuggest--call-epc
     method
     (lambda (candidates)
       (when (eq (current-buffer) buf)
         (setq res (funcall callback candidates)))))
    (while (and (eq res :pending)
                (eq (current-buffer) buf)
                (< (- (time-to-seconds) start) my/nimsuggest-completion-timeout))
      (accept-process-output nil 0.02))
    (unless (eq res :pending)
      res)))

(defun my/nimsuggest--format-candidates (items)
  "Format nimsuggest ITEMS into completion candidates."
  (let (out)
    (dolist (item items)
      (let ((name (my/nimsuggest--candidate-name item)))
        (when (and name (> (length name) 0))
          (let ((cand (copy-sequence name)))
            (put-text-property 0 (length cand) 'my/nimsuggest-epc item cand)
            (push cand out)))))
    (delete-dups (nreverse out))))

(defun my/nimsuggest-capf ()
  "Completion-at-point backend for Nim using nimsuggest."
  (when (and (fboundp 'nimsuggest-available-p)
             (nimsuggest-available-p)
             (buffer-file-name))
    (let ((end (point))
          (start (save-excursion
                   (skip-syntax-backward "w_")
                   (point))))
      (when (< start end)
        (let ((candidates
               (my/nimsuggest--call-sync
                'sug
                (lambda (items)
                  (my/nimsuggest--format-candidates items)))))
          (when (and candidates (listp candidates))
            (list start end candidates
                  :annotation-function
                  (lambda (cand)
                    (let ((item (get-text-property 0 'my/nimsuggest-epc cand)))
                      (when item
                        (format " %s" (nim--epc-symkind item)))))
                  :exclusive 'no)))))))

(defun my/nim--diagnostic-at-point ()
  "Return the first Flymake diagnostic on the current line."
  (when (and (bound-and-true-p flymake-mode)
             (fboundp 'flymake-diagnostics))
    (car (flymake-diagnostics (line-beginning-position)
                              (line-end-position)))))

(defun my/nim--format-diagnostic (diag)
  "Format DIAG as a readable string."
  (let* ((kind (flymake-diagnostic-type diag))
         (label (pcase kind
                  ('error "Error")
                  ('warning "Warning")
                  (_ "Hint"))))
    (format "%s: %s" label (flymake-diagnostic-text diag))))

(defface my/nim-diagnostic-label-error
  '((t :inherit error :weight bold))
  "Face for Nim error labels.")

(defface my/nim-diagnostic-label-warning
  '((t :inherit warning :weight bold))
  "Face for Nim warning labels.")

(defface my/nim-diagnostic-label-hint
  '((t :inherit shadow :weight bold))
  "Face for Nim hint labels.")

(defface my/nim-diagnostic-message
  '((t :inherit default))
  "Face for Nim diagnostic messages.")

(defun my/nim--diagnostic-buffer (diag)
  "Populate and return a buffer that displays DIAG."
  (let ((buf (get-buffer-create "*nim-diagnostics*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (let* ((kind (flymake-diagnostic-type diag))
             (label-face (pcase kind
                           ('error 'my/nim-diagnostic-label-error)
                           ('warning 'my/nim-diagnostic-label-warning)
                           (_ 'my/nim-diagnostic-label-hint)))
             (label (pcase kind
                      ('error "Error")
                      ('warning "Warning")
                      (_ "Hint")))
             (msg (flymake-diagnostic-text diag)))
        (insert (propertize (concat label ": ") 'face label-face))
        (insert (propertize msg 'face 'my/nim-diagnostic-message)))
      (special-mode)
      (setq mode-line-format nil)
      (setq-local header-line-format nil)
      (setq-local tab-line-format nil)
      (setq-local truncate-lines t)
      (setq-local display-line-numbers nil)
      (setq buffer-read-only t))
    buf))

(defvar my/nim--diagnostic-child-frame nil)
(defvar my/nim--diagnostic-source-buffer nil)
(defvar my/nim--diagnostic-source-line nil)

(defun my/nim--diagnostic-max-lines ()
  "Return the maximum lines for the diagnostic popup."
  3)

(defun my/nim--diagnostic-child-params ()
  "Return child frame parameters near point for diagnostics."
  (when (display-graphic-p)
    (let* ((pos (posn-at-point))
           (xy (and pos (posn-x-y pos)))
           (edges (window-pixel-edges))
           (left (nth 0 edges))
           (top (nth 1 edges))
           (x (and (numberp left) (consp xy) (numberp (car xy))
                   (+ left (car xy))))
           (y (and (numberp top) (consp xy) (numberp (cdr xy))
                   (+ top (cdr xy) (frame-char-height)))))
      (when (and (numberp x) (numberp y))
        `((parent-frame . ,(selected-frame))
          (left . ,x)
          (top . ,y)
          (minibuffer . nil)
          (no-accept-focus . t)
          (border-width . 0)
          (internal-border-width . 0)
          (vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)
          (undecorated . t)
          (no-other-frame . t)
          (width . 80)
          (height . ,(my/nim--diagnostic-max-lines))
          (cursor-type . nil))))))

(defun my/nim--hide-diagnostic-child-frame ()
  "Hide the diagnostic child frame when present."
  (when (frame-live-p my/nim--diagnostic-child-frame)
    (delete-frame my/nim--diagnostic-child-frame))
  (setq my/nim--diagnostic-child-frame nil))

(defun my/nim--diagnostic-still-valid-p ()
  "Return non-nil if point is still on the source diagnostic line."
  (and (buffer-live-p my/nim--diagnostic-source-buffer)
       (eq (current-buffer) my/nim--diagnostic-source-buffer)
       (numberp my/nim--diagnostic-source-line)
       (= (line-number-at-pos) my/nim--diagnostic-source-line)))

(defun my/nim--maybe-hide-diagnostic-on-move ()
  "Hide the diagnostic popup when point moves off the source line."
  (unless (my/nim--diagnostic-still-valid-p)
    (remove-hook 'post-command-hook #'my/nim--maybe-hide-diagnostic-on-move)
    (setq my/nim--diagnostic-source-buffer nil
          my/nim--diagnostic-source-line nil)
    (my/nim--hide-diagnostic-child-frame)))



(defun my/nim--show-diagnostic (diag)
  "Display DIAG in a floating child frame near point."
  (require 'eldoc-box)
  (let* ((buf (my/nim--diagnostic-buffer diag))
         (child-params (append eldoc-box-frame-parameters
                               (my/nim--diagnostic-child-params)))
         (win (display-buffer-in-child-frame
               buf
               `((child-frame-parameters . ,child-params)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))))
    (my/nim--hide-diagnostic-child-frame)
    (when (window-live-p win)
      (setq my/eldoc-focus-window (selected-window)
            my/eldoc-focus-frame (selected-frame)
            my/nim--diagnostic-child-frame (window-frame win)
            my/nim--diagnostic-source-buffer (current-buffer)
            my/nim--diagnostic-source-line (line-number-at-pos))
      (with-selected-window win
        (set-window-parameter win 'mode-line-format nil)
        (set-window-parameter win 'header-line-format nil)
        (set-window-margins win 0 0)
        (with-current-buffer buf
          (use-local-map (copy-keymap (current-local-map)))
          (local-set-key
           (kbd "q")
           (lambda ()
             (interactive)
             (remove-hook 'post-command-hook #'my/nim--maybe-hide-diagnostic-on-move)
             (setq my/nim--diagnostic-source-buffer nil
                   my/nim--diagnostic-source-line nil)
             (my/nim--hide-diagnostic-child-frame)
             (my/eldoc-restore-focus)))))))
  (add-hook 'post-command-hook #'my/nim--maybe-hide-diagnostic-on-move))


(defun my/nim--show-diagnostic-window (diag)
  "Display DIAG in a focused window."
  (my/nim--hide-diagnostic-child-frame)
  (let ((buf (my/nim--diagnostic-buffer diag)))
    (setq my/eldoc-focus-window (selected-window)
          my/eldoc-focus-frame (selected-frame))
    (let ((win (display-buffer
                buf
                '((display-buffer-reuse-window display-buffer-below-selected)
                  (window-height . 0.3)))))
      (when (window-live-p win)
        (select-window win)
        (with-current-buffer buf
          (use-local-map (copy-keymap (current-local-map)))
          (local-set-key
           (kbd "q")
           (lambda ()
             (interactive)
             (quit-window t)
             (my/eldoc-restore-focus))))))))

(defun my/nim-show-doc-or-diagnostic ()
  "Show Nim diagnostics at point, otherwise show docs."
  (interactive)
  (let ((diag (my/nim--diagnostic-at-point)))
    (if diag
        (if (eq last-command 'my/nim-show-doc-or-diagnostic)
            (my/nim--show-diagnostic-window diag)
          (my/nim--show-diagnostic diag))
      (if (eq last-command 'my/nim-show-doc-or-diagnostic)
          (my/eldoc-show-help-window)
        (my/eldoc-show-help)))))

(defun my/nim-setup-nimsuggest ()
  "Configure Nim docs via nimsuggest and diagnostics via nim check."
  (if (derived-mode-p 'nim-ts-mode)
      (when (fboundp 'nimsuggest-ensure)
        (condition-case err
            (nimsuggest-ensure)
          (error
           (message "Nim suggest skipped: %s" (error-message-string err)))))
    (when (fboundp 'nimsuggest-mode)
      (nimsuggest-mode 1)))
  (when (fboundp 'nimsuggest-eldoc--nimsuggest)
    (setq-local eldoc-documentation-function
                (lambda (&rest _args)
                  (nimsuggest-eldoc--nimsuggest))))
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)
  (setq-local nim-indent-offset 2)
  (local-set-key (kbd "C-M-<return>") #'my/nim-show-doc-or-diagnostic)
  (setq-local tags-file-name nil)
  (unless (derived-mode-p 'nim-ts-mode)
    ;; Avoid SMIE crashes during indentation by wrapping nim-indent-line.
    (setq-local indent-line-function #'my/nim-safe-indent)
    ;; Avoid SMIE crashes during sexp movement (C-M-SPC/mark-sexp).
    (setq-local forward-sexp-function #'my/nim-safe-forward-sexp))
  (add-hook 'completion-at-point-functions #'my/nimsuggest-capf nil t)
  (when (fboundp 'flycheck-mode)
    (flycheck-mode -1))
  (unless (derived-mode-p 'nimscript-mode)
    (my/nim-flymake-setup)))
(add-hook 'nim-mode-hook #'my/nim-setup-nimsuggest)
(add-hook 'nim-ts-mode-hook #'my/nim-setup-nimsuggest)
(add-hook 'nimscript-mode-hook #'my/nim-setup-nimsuggest)

;; TAGS generation via ntagger on first xref jump.
(defvar my/ntagger-roots (make-hash-table :test 'equal))
(defvar my/ntagger-installing nil)

(defun my/ntagger-project-root ()
  "Return the current project root for TAGS generation."
  (or (when (fboundp 'projectile-project-root)
        (ignore-errors (projectile-project-root)))
      (when-let* ((proj (project-current nil)))
        (car (project-roots proj)))
      (locate-dominating-file default-directory ".git")
      (locate-dominating-file default-directory "nimble.lock")
      default-directory))

(defun my/ntagger-installed-p ()
  "Return non-nil when ntagger is available."
  (executable-find "ntagger"))

(defun my/ntagger-install ()
  "Install ntagger using nimble when missing."
  (when (and (not (my/ntagger-installed-p))
             (executable-find "nimble")
             (not my/ntagger-installing))
    (setq my/ntagger-installing t)
    (let ((buf (get-buffer-create "*ntagger-install*")))
      (with-current-buffer buf
        (erase-buffer))
      (message "Installing ntagger via nimble...")
      (let ((exit-code (call-process "nimble" nil buf t
                                     "install" "https://github.com/elcritch/ntagger")))
        (setq my/ntagger-installing nil)
        (if (and (numberp exit-code) (zerop exit-code))
            (message "ntagger installed.")
          (message "ntagger install failed; see *ntagger-install*"))))))

(defun my/ntagger-ensure-gitignore (root tags-file)
  "Add TAGS to .gitignore in ROOT when TAGS-FILE exists."
  (let ((gitignore (expand-file-name ".gitignore" root)))
    (when (and (file-exists-p gitignore) (file-exists-p tags-file))
      (with-temp-buffer
        (insert-file-contents gitignore)
        (unless (re-search-forward "^/?TAGS$" nil t)
          (goto-char (point-max))
          (unless (or (bobp) (eq (char-before) ?\n))
            (insert "\n"))
          (insert "TAGS\n")
          (write-region (point-min) (point-max) gitignore nil 'quiet))))))

(defvar my/ntagger-include-nimble-deps t
  "When non-nil, include Nimble deps listed in *.nimble.")

(defvar my/ntagger-exclude-patterns
  '("deps" "gen_qcborcommon.nim" "/tests" "tests/")
  "Substring patterns to exclude from ntagger scans.")

(defun my/ntagger-find-nimble-file (root)
  "Return the first *.nimble file in ROOT."
  (car (directory-files root t "\\.nimble\\'")))

(defun my/ntagger-parse-nimble-deps (nimble-file)
  "Parse NIMBLE-FILE and return a list of dependency names."
  (let (deps)
    (with-temp-buffer
      (insert-file-contents nimble-file)
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*requires\\b" nil t)
        (let ((line-end (line-end-position)))
          (while (re-search-forward "\"\\([^\"]+\\)\"" line-end t)
            (let* ((spec (match-string 1))
                   (name (car (split-string spec "[[:space:]]+" t))))
              (when (and name (not (string-empty-p name)))
                (push name deps)))))))
    (delete-dups (nreverse deps))))

(defun my/ntagger-nimble-pkg-dir (name)
  "Return the newest pkgs2 directory for NAME, if any."
  (let* ((base (expand-file-name "~/.nimble/pkgs2"))
         (pattern (concat "^" (regexp-quote name) "-"))
         (candidates (when (file-directory-p base)
                       (directory-files base t pattern))))
    (when candidates
      (car (sort candidates
                 (lambda (a b)
                   (time-less-p
                    (file-attribute-modification-time (file-attributes b))
                    (file-attribute-modification-time (file-attributes a)))))))))

(defun my/ntagger-nimble-dep-dirs (root)
  "Return Nimble dependency directories for ROOT."
  (let ((nimble (my/ntagger-find-nimble-file root)))
    (when nimble
      (let* ((deps (my/ntagger-parse-nimble-deps nimble))
             (dirs (delq nil (mapcar #'my/ntagger-nimble-pkg-dir deps))))
        (delete-dups dirs)))))

(defun my/ntagger-generate-tags (root)
  "Generate TAGS in ROOT using ntagger."
  (let ((default-directory root)
        (buf (get-buffer-create "*ntagger*"))
        (args (list "-f" "TAGS" "--private" "--system"))
        (roots (list ".")))
    (when my/ntagger-include-nimble-deps
      (setq roots (append roots (my/ntagger-nimble-dep-dirs root))))
    (dolist (pat my/ntagger-exclude-patterns)
      (setq args (append args (list "--exclude" pat))))
    (setq args (append args roots))
    (with-current-buffer buf
      (erase-buffer))
    (let ((exit-code (apply #'call-process "ntagger" nil buf t args)))
      (if (and (numberp exit-code) (zerop exit-code))
          t
        (message "ntagger failed; see *ntagger*")
        nil))))

(defun my/ntagger-tags-file (root)
  "Return the TAGS file path for ROOT."
  (when root
    (expand-file-name "TAGS" root)))

(defun my/ntagger-ensure-tags ()
  "Ensure TAGS exist for the current Nim project."
  (when (derived-mode-p 'nim-mode)
    (let* ((root (my/ntagger-project-root))
           (tags-file (my/ntagger-tags-file root)))
      (when (and root tags-file)
        (unless (my/ntagger-installed-p)
          (my/ntagger-install))
        (when (my/ntagger-installed-p)
          (my/ntagger-generate-tags root))
        (when (file-exists-p tags-file)
          (my/ntagger-ensure-gitignore root tags-file))))))

(defun my/ntagger-regenerate ()
  "Regenerate TAGS for the current Nim project."
  (interactive)
  (let* ((root (my/ntagger-project-root))
         (tags-file (my/ntagger-tags-file root)))
    (unless (and root tags-file)
      (user-error "No project root found for TAGS generation"))
    (unless (my/ntagger-installed-p)
      (my/ntagger-install))
    (when (my/ntagger-installed-p)
      (if (my/ntagger-generate-tags root)
          (progn
            (my/ntagger-ensure-gitignore root tags-file)
            (message "ntagger: TAGS regenerated"))
        (message "ntagger: TAGS generation failed")))))

(defun my/ntagger-xref-advice (orig-fn &rest args)
  "Ensure ntagger TAGS exist before xref jumps."
  (my/ntagger-ensure-tags)
  (apply orig-fn args))

(dolist (fn '(xref-find-definitions
              xref-find-definitions-other-window
              xref-find-definitions-other-frame
              xref-find-references))
  (when (fboundp fn)
    (advice-add fn :around #'my/ntagger-xref-advice)))

(defun my/ntagger--parse-tags (tags-file symbol)
  "Return xref matches for SYMBOL in TAGS-FILE."
  (let (matches)
    (with-temp-buffer
      (insert-file-contents tags-file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (unless (or (string-prefix-p "!_" line) (string-empty-p line))
            (when (string-match "^\\([^\t]+\\)\t\\([^\t]+\\)\t[^;]*;\"\\(.*\\)$" line)
              (let* ((name (match-string 1 line))
                     (file (match-string 2 line))
                     (rest (match-string 3 line)))
                (when (string= name symbol)
                  (when (string-match "line:\\([0-9]+\\)" rest)
                    (let* ((line-num (string-to-number (match-string 1 rest)))
                           (path (if (file-name-absolute-p file)
                                     file
                                   (expand-file-name file (file-name-directory tags-file)))))
                      (push (xref-make name
                                       (xref-make-file-location path line-num 1))
                            matches))))))))
        (forward-line 1)))
    (nreverse matches)))

(defun my/nim-xref-backend ()
  "Use ntagger TAGS for Nim xref."
  (when (derived-mode-p 'nim-mode 'nim-ts-mode)
    'nim-ntagger))

(with-eval-after-load 'xref
  (add-hook 'xref-backend-functions #'my/nim-xref-backend t)
  (defun my/nim-xref-setup ()
    "Prefer ntagger tags in Nim buffers."
    (setq-local tags-add-tables nil)
    (setq-local tags-file-name nil)
    (setq-local xref-backend-functions '(my/nim-xref-backend))
    (setq-local tags-table-list nil))
  (add-hook 'nim-mode-hook #'my/nim-xref-setup)
  (add-hook 'nim-ts-mode-hook #'my/nim-xref-setup)
  (cl-defmethod xref-backend-identifier-at-point ((_backend (eql nim-ntagger)))
    (thing-at-point 'symbol t))
  (cl-defmethod xref-backend-definitions ((_backend (eql nim-ntagger)) symbol)
    (my/ntagger-ensure-tags)
    (let* ((root (my/ntagger-project-root))
           (tags-file (my/ntagger-tags-file root)))
      (if (and tags-file (file-exists-p tags-file))
          (my/ntagger--parse-tags tags-file symbol)
        (user-error "No TAGS file found in project root"))))
  (cl-defmethod xref-backend-references ((_backend (eql nim-ntagger)) _symbol)
    nil))

(use-package yaml-mode
  :ensure t
  :hook
  (yaml-mode . eglot-ensure))

(use-package json-mode
  :ensure t
  :hook
  (json-mode . eglot-ensure))

(provide 'lang)
