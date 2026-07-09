;;; fe-org.el --- Org mode  -*- lexical-binding: t -*-
;;; Code:

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun fe-org--visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . fe-org--visual-fill))

;; insert src org
(global-set-key (kbd "C-c a") 'org-agenda-list)
(global-set-key (kbd "C-c s") 'org-insert-structure-template)
(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (set-face-foreground 'org-block-begin-line "#4e4e4e")
  (set-face-background 'org-block-begin-line "#3a3a3a"))

;; extend block code
(custom-set-faces
 '(org-block ((t (:extend t)))))

;; set block code indentation
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0
      org-startup-truncated nil
      org-src-tab-acts-natively t
      org-startup-indented t)

;; disable image size
(setq org-image-actual-width nil)

(provide 'fe-org)
;;; fe-org.el ends here
