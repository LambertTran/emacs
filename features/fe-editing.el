;;; fe-editing.el --- Evil mode and global keybindings  -*- lexical-binding: t -*-
;;; Code:

(setq evil-want-C-i-jump nil)
(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :bind
  ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; ESC cancels all
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(defun ls ()
  "Lists the contents of the current directory."
  (interactive)
  (shell-command "open -a iterm ."))

(global-set-key (kbd "C-x t") 'ls)

;; MacOS option key as meta
(setq mac-option-modifier 'meta)

;; find definition
(global-set-key (kbd "M-g j") 'xref-find-definitions)
;; go back
(global-set-key (kbd "M-g b") 'xref-pop-marker-stack)

(provide 'fe-editing)
;;; fe-editing.el ends here
