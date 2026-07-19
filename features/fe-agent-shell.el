;;; fe-agent-shell.el --- Claude Code via agent-shell (ACP)  -*- lexical-binding: t -*-
;;; Code:

;; Ensure the ACP bridge binary is installed, mirroring the gopls auto-install
;; pattern in fe-golang.el.
(unless (executable-find "claude-agent-acp")
  (when (executable-find "npm")
    (message "Installing claude-agent-acp...")
    (if (zerop (call-process "npm" nil "*claude-agent-acp-install*" nil
                              "install" "-g" "@agentclientprotocol/claude-agent-acp"))
        (message "claude-agent-acp installed successfully.")
      (message "claude-agent-acp install failed; see *claude-agent-acp-install* buffer."))))

(use-package agent-shell
  :ensure t
  :bind ("C-c c" . agent-shell)
  :config
  ;; Login-based auth: run `claude` outside Emacs at least once to log in.
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  ;; Default new sessions to Claude Code's "Auto" permission mode.
  (setq agent-shell-anthropic-default-session-mode-id "auto")
  ;; `agent-shell' (C-c c) already reuses an existing shell buffer for the
  ;; current project when one is open. When none exists, always start a
  ;; fresh session instead of prompting to resume/pick one.
  (setq agent-shell-session-strategy 'new)
  ;; Default `agent-shell' to Claude Code instead of prompting for an agent.
  (setq agent-shell-preferred-agent-config 'claude-code))

(provide 'fe-agent-shell)
;;; fe-agent-shell.el ends here
