;; -*- lexical-binding: t; -*-
;;; ob-tcl.el --- Org Babel functions for TCL evaluation

(require 'ob)
(require 'org-macs)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("tcl" . "tcl"))

(defvar org-babel-default-header-args:tcl '())

(defcustom org-babel-tcl-command "tclsh"
  "Command to execute TCL code."
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:tcl (body params)
  "Execute a block of TCL code with org-babel."
  (let* ((result-params (cdr (assq :result-params params)))
         (full-body (org-babel-expand-body:generic body params))
         (tmp-file (org-babel-temp-file "tcl-" ".tcl")))
    (with-temp-file tmp-file
      (insert full-body))
    (org-babel-eval (format "%s %s" org-babel-tcl-command tmp-file) "")))

(defun org-babel-prep-session:tcl (_session _params)
  "TCL does not support sessions."
  (error "TCL does not support sessions"))

(provide 'ob-tcl)
;;; ob-tcl.el ends here
