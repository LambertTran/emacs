;;; fe-terraform.el --- Terraform support  -*- lexical-binding: t -*-
;;; Code:

;; terraform-ts-mode isn't on MELPA, so fetch it straight from upstream via
;; package-vc instead of hand-vendoring a copy of the file in this repo.
(use-package terraform-ts-mode
  :vc (:url "https://github.com/kgrotel/terraform-ts-mode"))

(provide 'fe-terraform)
;;; fe-terraform.el ends here
