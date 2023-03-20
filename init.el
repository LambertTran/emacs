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
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t))

(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-mode t)
 '(org-agenda-files
   '("/opt/lb/emacs/settings.org" "/Users/alirom/org-agenda/todos.org" "/Users/alirom/org-agenda/habits.org" "/Users/alirom/org-agenda/journal.org"))
 '(package-selected-packages
   '(prettier-js web-mode rego-mode terraform-doc gitlab-ci-mode gitlab-pipeline gitlab x509-mode eglot graphql-mode treemacs company-go lsp-pyright go-mode company-lsp company-yasnippet org-make-toc toc-org ws-butler kubernetes-evil kubernetes org-tempo org-temp ob-go protobuf-mode dap-go lsp-ui yasnippet lsp-mode go-autocomplete super-save powerline-evil disable-mouse visual-line-mode company-tern terraform-mode company-anaconda use-package))
 '(warning-suppress-log-types '((use-package) (use-package) (comp)))
 '(warning-suppress-types '((use-package) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-confirm-face ((t (:background "blue" :foreground "#008700" :weight bold))))
 '(ivy-current-match ((t (:inherit highlight :extend t :background "black" :foreground "green" :weight bold))))
 '(ivy-highlight-face ((t (:background "dark red"))))
 '(ivy-minibuffer-match-face-2 ((t (:background "dark red" :foreground "#eeeeee" :weight bold))))
 '(ivy-minibuffer-match-highlight ((t (:inherit highlight :foreground "green"))))
 '(ivy-prompt-match ((t (:inherit ivy-current-match :background "white"))))
 '(lsp-headerline-breadcrumb-path-face ((t (:foreground "light green"))))
 '(lsp-headerline-breadcrumb-project-prefix-face ((t (:foreground "light green" :weight bold))))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :underline (:color "Red1" :style wave :position wave)))))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:foreground "light green" :weight bold))))
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :underline (:color "Green" :style wave :position wave)))))
 '(mode-line ((t (:background "dark slate gray" :foreground "green" :box nil :height 1.1))))
 '(mode-line-inactive ((t (:height 1.0))))
 '(org-block ((t (:extend t))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "systemGreenColor" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "red" :weight bold)))))
(put 'upcase-region 'disabled nil)
