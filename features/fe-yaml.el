;;; fe-yaml.el --- YAML support  -*- lexical-binding: t -*-
;;; Code:

(use-package yaml-ts-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :hook (yaml-ts-mode . eglot-ensure))

(provide 'fe-yaml)
;;; fe-yaml.el ends here
