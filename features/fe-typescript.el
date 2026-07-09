;;; fe-typescript.el --- TypeScript support  -*- lexical-binding: t -*-
;;; Code:

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :hook (typescript-ts-mode . eglot-ensure))

(provide 'fe-typescript)
;;; fe-typescript.el ends here
