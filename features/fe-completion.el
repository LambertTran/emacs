;;; fe-completion.el --- In-buffer completion and LSP  -*- lexical-binding: t -*-
;;; Code:

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package corfu
  :init
  :ensure t
  :config
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  :ensure t
  :config
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  (setq tab-always-indent 'complete))

;; Enable auto completion and configure quitting
(setq corfu-auto t
      corfu-auto-delay 0.3        ; default 0.2; slightly less eager to query capf
      corfu-quit-no-match 'separator) ;; or t

;; Eldoc (hover/signature help) polls the backend (eglot) on point movement;
;; give it enough breathing room that normal reading/navigation pauses don't
;; trigger it, since a still-indexing server (e.g. gopls on a fresh project)
;; can be slow to answer and makes cursor movement itself feel laggy.
(setq eldoc-idle-delay 1.5)

;; Eglot already turns on flymake-mode (its diagnostics backend) in any
;; buffer it manages, so this mainly covers non-eglot buffers, like
;; emacs-lisp-mode, that ship their own flymake backend.
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

(use-package eglot
  :defer t
  :config
  ;; Kill each server once its last managed buffer is closed, instead of
  ;; letting it linger for the rest of the session.
  (setq eglot-autoshutdown t)
  ;; Disable eglot/jsonrpc event logging for performance. This used to be
  ;; toggled as a side effect of opening a Terraform file (see
  ;; terraform-ts-mode.el); it's a global eglot/jsonrpc setting, not specific
  ;; to any one language server, so it lives here instead.
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-size 0)
  (setq eglot-sync-connect nil)
  ;; Batch more edits before sending didChange to the server, reducing
  ;; round-trips while typing (default 0.5).
  (setq eglot-send-changes-idle-time 0.7))

(provide 'fe-completion)
;;; fe-completion.el ends here
