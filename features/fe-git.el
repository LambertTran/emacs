;;; fe-git.el --- Git integration  -*- lexical-binding: t -*-
;;; Code:

(use-package magit
  :ensure t
  :custom
  ;; open magit status in same buffer
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (global-set-key (kbd "C-x g") 'magit-status)

  ;; open file in git
  (use-package git-link
    :ensure t
    :config
    (setq git-link-open-in-browser t))

  (global-set-key (kbd "C-c l") 'git-link))

(provide 'fe-git)
;;; fe-git.el ends here
