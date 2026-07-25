;;; fe-terminal.el --- Terminal emulation via vterm  -*- lexical-binding: t -*-
;;; Code:

(use-package vterm
  :ensure t
  :bind ("C-c t" . vterm))

(unless (executable-find "k9s")
  (when (executable-find "brew")
    (message "Installing k9s...")
    (if (zerop (call-process "brew" nil "*k9s-install*" nil "install" "k9s"))
        (message "k9s installed successfully.")
      (message "k9s install failed; see *k9s-install* buffer."))))

(defvar vterm-shell)

(defun fe-terminal-k9s ()
  "Switch to the dedicated k9s vterm buffer, creating it if needed."
  (interactive)
  (if (get-buffer "*k9s*")
      (pop-to-buffer-same-window "*k9s*")
    (let ((vterm-shell "k9s"))
      (vterm "*k9s*"))))

(global-set-key (kbd "C-c k") #'fe-terminal-k9s)

(provide 'fe-terminal)
;;; fe-terminal.el ends here
