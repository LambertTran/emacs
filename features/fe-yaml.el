;;; fe-yaml.el --- YAML support  -*- lexical-binding: t -*-
;;; Code:

(use-package yaml-ts-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :hook ((yaml-ts-mode . eglot-ensure)
         (yaml-ts-mode . (lambda () (setq tab-width 2)))))

(provide 'fe-yaml)
;;; fe-yaml.el ends here
