;;; fe-terraform.el --- Terraform support  -*- lexical-binding: t -*-
;;; Code:

;; terraform-ts-mode isn't on MELPA, so fetch it straight from upstream via
;; package-vc instead of hand-vendoring a copy of the file in this repo.
;; Pinned to a specific commit (the tip of upstream's main branch as of
;; 2026-10-17) rather than floating, so upgrades are an explicit edit here.
(use-package terraform-ts-mode
  :vc (:url "https://github.com/kgrotel/terraform-ts-mode"
       :rev "28bafd1c56cfeb94c5a3f2acedc3aba2c6a6bc24"))

(provide 'fe-terraform)
;;; fe-terraform.el ends here
