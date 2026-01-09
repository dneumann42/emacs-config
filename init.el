;;; -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defun my/load-config (file)
  "Load a config FILE from the user Emacs directory."
  (load-file (expand-file-name file user-emacs-directory)))

(my/load-config "lisp/core.el")
(my/load-config "lisp/ui.el")
(my/load-config "lisp/lang.el")
(my/load-config "lisp/tools.el")
(my/load-config "lisp/projects.el")
(my/load-config "lisp/completion.el")
(my/load-config "lisp/theme.el")
