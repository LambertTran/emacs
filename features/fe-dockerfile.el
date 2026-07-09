;;; fe-dockerfile.el --- Dockerfile support  -*- lexical-binding: t -*-
;;; Code:

(use-package dockerfile-ts-mode
  :mode "\\Dockerfile\\'"
  :hook (dockerfile-ts-mode . eglot-ensure))

(provide 'fe-dockerfile)
;;; fe-dockerfile.el ends here
