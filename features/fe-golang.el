;;; fe-golang.el --- Go support  -*- lexical-binding: t -*-
;;; Code:

(use-package go-ts-mode
  :mode "\\.go\\'"
  :hook (go-ts-mode . eglot-ensure))

(use-package go-mod-ts-mode
  :mode "\\.mod\\'"
  :hook (go-mod-ts-mode . eglot-ensure))

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun fe-golang--eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-ts-mode-hook #'fe-golang--eglot-format-buffer-on-save)

;; Gopls settings https://github.com/golang/tools/blob/master/gopls/doc/settings.md
(with-eval-after-load 'eglot
  (setq-default eglot-workspace-configuration
                (append eglot-workspace-configuration
                        '((:gopls .
                           ((usePlaceholders . t)
                            (matcher . "CaseInsensitive")))))))

(provide 'fe-golang)
;;; fe-golang.el ends here
