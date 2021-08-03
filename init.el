;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

(use-package org
  :ensure t
  :defer t
  :config
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  )

(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-mode t)
 '(package-selected-packages
   '(powerline-evil disable-mouse visual-line-mode company-tern terraform-mode company-anaconda use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(mode-line ((t (:background "dark slate gray" :foreground "green" :box nil :height 1.10))))
 '(mode-line-inactive ((t (:height 1.00))))

 '(org-block ((t (:extend t)))))
