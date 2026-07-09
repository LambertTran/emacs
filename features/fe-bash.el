;;; fe-bash.el --- Bash support  -*- lexical-binding: t -*-
;;; Code:

(use-package bash-ts-mode
  :mode "\\.sh\\'"
  :hook (bash-ts-mode . eglot-ensure))

(provide 'fe-bash)
;;; fe-bash.el ends here
