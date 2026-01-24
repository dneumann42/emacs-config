;;; dashboard.el --- Custom dashboard -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'seq)
(require 'calendar)

(setq inhibit-startup-screen t)

(defvar my/dashboard-buffer-name "*Dashboard*")

(defvar-local my/dashboard-projects nil)

(defface my/dashboard-title-face
  '((t :weight bold :height 1.3 :foreground "#f1f5fb"))
  "Face for the dashboard title.")

(defface my/dashboard-section-face
  '((t :weight bold :foreground "#7fd6ff"))
  "Face for dashboard section headers.")

(defface my/dashboard-help-face
  '((t :foreground "#95e6cb"))
  "Face for dashboard help text.")

(defface my/dashboard-project-face
  '((t :foreground "#ffd580"))
  "Face for dashboard project entries.")

(defface my/dashboard-muted-face
  '((t :foreground "#9aa3af"))
  "Face for muted dashboard text.")

(defface my/dashboard-default-face
  '((t :inherit fixed-pitch :foreground "#d8dde5"))
  "Face for the dashboard buffer.")

(defface my/dashboard-card-border-face
  '((t :foreground "#7fd6ff"))
  "Face for dashboard card borders.")

(defface my/dashboard-calendar-header-face
  '((t :foreground "#7fd6ff" :weight bold))
  "Face for the calendar header.")

(defface my/dashboard-calendar-today-face
  '((t :foreground "#ffd580" :weight bold))
  "Face for today's date in the calendar.")

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
  (let* ((decoded-time (decode-time (current-time)))
         (year (nth 5 decoded-time))
         (month (nth 4 decoded-time))
         (day (nth 3 decoded-time)))
    (with-temp-buffer
     (calendar-generate-month month year 1)
     (goto-char (point-min))
     (add-face-text-property (line-beginning-position) (line-end-position)
                             'my/dashboard-calendar-header-face)
     (forward-line 1)
     (add-face-text-property (line-beginning-position) (line-end-position)
                             'my/dashboard-calendar-header-face)
     (goto-char (point-min))
     (when (re-search-forward
            (format "\\(?:^\\|\\s-\\)\\(%2d\\)\\(?:\\s-\\|$\\)" day)
            nil t)
       (add-face-text-property (match-beginning 1) (match-end 1)
                               'my/dashboard-calendar-today-face))
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
          (buffer-string))))))

(defun my/dashboard--section-width ()
  "Return a usable width for centered sections."
  (let ((win (get-buffer-window my/dashboard-buffer-name)))
    (min 88 (max 54 (or (and win (window-body-width win)) 80)))))

(defun my/dashboard--center-line (line width)
  "Center LINE within WIDTH."
  (let* ((trim (truncate-string-to-width line width 0 ?\s))
         (pad (max 0 (/ (- width (string-width trim)) 2))))
    (concat (make-string pad ?\s) trim)))

(defun my/dashboard--card-lines (text width)
  "Return a list of padded lines for TEXT within WIDTH."
  (let* ((lines (split-string (or text "") "\n"))
         (inner (max 10 width)))
    (mapcar (lambda (line)
              (let ((trim (truncate-string-to-width line inner 0 ?\s)))
                (concat trim (make-string (max 0 (- inner (string-width trim))) ?\s))))
            lines)))

(defun my/dashboard--insert-card (title body)
  "Insert a centered card with TITLE and BODY."
  (let* ((width (my/dashboard--section-width))
         (inner (max 10 (- width 4)))
         (lines (my/dashboard--card-lines body inner))
         (title-text (truncate-string-to-width title (- inner 4) 0 ?\s))
         (pad (max 0 (/ (- inner (string-width title-text)) 2)))
         (title-line (concat (make-string pad ?\s) title-text))
         (left (make-string (max 0 (/ (- (window-body-width) width) 2)) ?\s))
         (border-h (make-string inner ?─))
         (border-face 'my/dashboard-card-border-face))
    ;; Top border
    (insert left
            (propertize "╭" 'face border-face)
            (propertize border-h 'face border-face)
            (propertize "╮" 'face border-face)
            "\n")
    ;; Title line
    (insert left
            (propertize "│" 'face border-face)
            (my/dashboard--center-line title-line inner)
            (propertize "│" 'face border-face)
            "\n")
    ;; Separator
    (insert left
            (propertize "├" 'face border-face)
            (propertize border-h 'face border-face)
            (propertize "┤" 'face border-face)
            "\n")
    ;; Content lines
    (dolist (line lines)
      (insert left
              (propertize "│" 'face border-face)
              line
              (propertize "│" 'face border-face)
              "\n"))
    ;; Bottom border
    (insert left
            (propertize "╰" 'face border-face)
            (propertize border-h 'face border-face)
            (propertize "╯" 'face border-face)
            "\n\n")))

(defun my/dashboard-refresh ()
  "Refresh the dashboard contents."
  (interactive)
  (let ((buf (get-buffer-create my/dashboard-buffer-name)))
    (with-current-buffer buf
      (my/dashboard-mode)
      (use-local-map my/dashboard-mode-map)
      (setq-local truncate-lines t)
      (setq-local display-line-numbers nil)
      (setq-local cursor-type nil)
      (setq-local face-remapping-alist `((default . my/dashboard-default-face)))
      (when-let ((win (get-buffer-window buf)))
        (set-window-margins win 0 0))
      (setq my/dashboard-projects (my/dashboard-projects))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let* ((title "An editor for wizards")
               (win (get-buffer-window buf))
               (win-width (or (and win (window-body-width win)) (frame-width)))
               (section-width (my/dashboard--section-width))
               (left (make-string (max 0 (/ (- win-width section-width) 2)) ?\s)))
          (insert "\n"
                  left
                  (propertize (my/dashboard--center-line title section-width)
                              'face 'my/dashboard-title-face)
                  "\n\n"))
        (my/dashboard--insert-card
         (propertize "Calendar" 'face 'my/dashboard-section-face)
         (my/dashboard--calendar-string))
        (my/dashboard--insert-card
         (propertize "Agenda" 'face 'my/dashboard-section-face)
         (or (my/dashboard--insert-agenda) "No agenda items.\n"))
        (my/dashboard--insert-card
         (propertize "Recent Projects" 'face 'my/dashboard-section-face)
         (concat
          (propertize "Keys: 1-9 open project, g refresh\n"
                      'face 'my/dashboard-help-face)
          "\n"
          (if my/dashboard-projects
              (mapconcat (lambda (pair)
                           (propertize (format "%d. %s" (car pair) (cdr pair))
                                       'face 'my/dashboard-project-face))
                         (cl-loop for proj in (seq-take my/dashboard-projects 9)
                                  for idx from 1
                                  collect (cons idx proj))
                         "\n")
            (propertize "No projects found."
                        'face 'my/dashboard-muted-face))))))
    (with-current-buffer buf
      (goto-char (point-min)))))

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

(defun my/dashboard-resize-handler (&optional _frame)
  "Refresh dashboard when window is resized."
  (when-let ((buf (get-buffer my/dashboard-buffer-name)))
    (when (get-buffer-window buf)
      (my/dashboard-refresh))))

(add-hook 'window-size-change-functions #'my/dashboard-resize-handler)

(provide 'dashboard)
