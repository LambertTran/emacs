;;; fe-javascript.el --- JavaScript support  -*- lexical-binding: t -*-
;;; Code:

(use-package js-ts-mode
  :mode "\\.js\\'"
  :hook (js-ts-mode . eglot-ensure))

(provide 'fe-javascript)
;;; fe-javascript.el ends here
