;;; fe-core.el --- Performance, backup, and misc utilities  -*- lexical-binding: t -*-
;;; Code:

(setq native-comp-speed 3) ;; maximum native Elisp speed!

(setq create-lockfiles nil)
(make-directory "~/.saves" t)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-file-name-transforms `((".*" ,"~/.saves" t)))

;; Force emacs to use PATH
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

(provide 'fe-core)
;;; fe-core.el ends here
