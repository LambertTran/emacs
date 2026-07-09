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
      corfu-quit-no-match 'separator) ;; or t

(use-package eglot
  :defer t
  :config
  ;; Kill each server once its last managed buffer is closed, instead of
  ;; letting it linger for the rest of the session.
  (setq eglot-autoshutdown t)
  ;; Disable eglot/jsonrpc event logging for performance. This used to be
  ;; toggled as a side effect of opening a Terraform file (see
  ;; vendored/terraform-ts-mode.el); it's a global eglot/jsonrpc setting, not
  ;; specific to any one language server, so it lives here instead.
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-size 0)
  (setq eglot-sync-connect nil))

(provide 'fe-completion)
;;; fe-completion.el ends here
