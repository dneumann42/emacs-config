#!/usr/bin/env bash
set -euo pipefail

ROOT="${HOME}/.emacs.d"
LOG_DIR="${ROOT}/debug-logs"
mkdir -p "${LOG_DIR}"
TS="$(date +%Y%m%d-%H%M%S)"

LOG_INIT="${LOG_DIR}/debug-init-${TS}.log"
LOG_BATCH="${LOG_DIR}/debug-batch-${TS}.log"
LOG_MSGS="${LOG_DIR}/debug-messages-${TS}.log"

cat > "${LOG_DIR}/debug-eval-${TS}.el" <<'ELISP'
(setq debug-on-error t)
(message "user-emacs-directory=%s" user-emacs-directory)
(message "user-init-file=%s" user-init-file)
(message "load-path=%s" load-path)
(message "tags-file-name=%s" tags-file-name)
(message "tags-table-list=%s" tags-table-list)
(message "tags-add-tables=%s" tags-add-tables)
(let ((buf (get-buffer "*Messages*")))
  (when buf
    (with-current-buffer buf
      (write-region (point-min) (point-max)
                    (expand-file-name "debug-messages.log" "~/.emacs.d/debug-logs")
                    nil 'quiet))))
ELISP

# 1) Batch load of init to catch read syntax errors.
emacs --batch --debug-init -l "${ROOT}/init.el" \
  --eval "(message \"Batch load ok\")" \
  > "${LOG_BATCH}" 2>&1 || true

# 2) Interactive debug init (use in a real terminal with -nw).
# This will drop you into Emacs; exit with C-x C-c when done.
# It logs debug output to a file.
if [ -t 0 ]; then
  emacs -nw --debug-init -l "${ROOT}/init.el" \
    --eval "(load \"${LOG_DIR}/debug-eval-${TS}.el\")" \
    2>&1 | tee "${LOG_INIT}"
else
  echo "No TTY detected; skipping -nw run." >> "${LOG_INIT}"
fi

# 3) Copy the *Messages* log if it exists.
if [ -f "${HOME}/.emacs.d/debug-logs/debug-messages.log" ]; then
  cp "${HOME}/.emacs.d/debug-logs/debug-messages.log" "${LOG_MSGS}"
fi

echo "Logs written to ${LOG_DIR}"
