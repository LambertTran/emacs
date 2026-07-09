;;; fe-python.el --- Python support  -*- lexical-binding: t -*-
;;; Code:

(use-package python-ts-mode
  :mode "\\.py\\'"
  :hook (python-ts-mode . eglot-ensure))

(provide 'fe-python)
;;; fe-python.el ends here
