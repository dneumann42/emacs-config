;;; project-commands.el --- Project commands -*- lexical-binding: t -*-

(require 'projectile)
(require 'shell-pop nil t)
(require 'eshell nil t)

(defun my/commands/get-commands-file-path ()
  (concat
   (projectile-project-root)  ".commands"))

(defun my/commands/tostring (cmds)
  (string-join cmds "\n"))

(defun my/commands/fromstring (str)
  (split-string str "\n" t))

(defun my/commands/read-commands-file ()
  (let ((path (my/commands/get-commands-file-path)))
    (if (file-exists-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
      "")))

(defun my/commands/new ()
  "Create a new command"
  (interactive)
  (let ((line (read-string "Enter command: "))
        (path (my/commands/get-commands-file-path)))
    (write-region (concat line "\n") nil path t 0)))

(defun my/commands/edit ()
  "Create a new command"
  (interactive))

(defun my/commands/run-last ()
  "Create a new command"
  (interactive))

(defun my/commands/run ()
  "Prompt for a saved command and run it inside the shell-pop eshell."
  (interactive)
  (let* ((root (or (projectile-project-root)
                   default-directory))
         (cmds (my/commands/fromstring (my/commands/read-commands-file))))
    (unless root
      (user-error "Not in a Projectile project"))
    (unless cmds
      (user-error "No project commands found"))
    (unless (featurep 'shell-pop)
      (user-error "shell-pop package is not available"))
    (let ((cmd (completing-read "Command: " cmds nil t nil nil (car cmds)))
          shell-buffer-name)
      (let ((shell-pop-default-directory root))
        (shell-pop 1)
        (setq shell-buffer-name shell-pop-last-shell-buffer-name))
      (let ((buf (and shell-buffer-name
                      (get-buffer shell-buffer-name))))
        (if (not buf)
            (user-error "shell-pop buffer is not available")
          (with-current-buffer buf
            (unless (featurep 'eshell)
              (user-error "Eshell is not available"))
            (when (fboundp 'eshell/cd)
              (eshell/cd root))
            (goto-char (point-max))
            (insert cmd)
            (eshell-send-input)
            (pop-to-buffer buf)))))))
