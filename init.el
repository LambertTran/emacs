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

;; Required before `package-initialize' activates installed packages: some
;; packages' autoloads (e.g. terraform-ts-mode's) call `treesit-ready-p' at
;; top level, which is void until `treesit' itself has been required.
(require 'treesit)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Refresh the package index on first run, and periodically afterward --
;; without this, a stale cached index can reference package versions that
;; have since been pruned from the archive, breaking new installs.
(let ((archive-file (expand-file-name "archives/melpa/archive-contents" package-user-dir)))
  (when (or (not package-archive-contents)
            (not (file-exists-p archive-file))
            (> (float-time
                (time-since
                 (file-attribute-modification-time (file-attributes archive-file))))
               (* 7 24 60 60)))
    (package-refresh-contents)))

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

(add-to-list 'load-path (expand-file-name "features" user-emacs-directory))

(dolist (file (sort (file-expand-wildcards
                     (expand-file-name "features/fe-*.el" user-emacs-directory))
                    #'string<))
  (load file nil t))

;; Machine-local configuration that shouldn't be committed to the repo
;; (e.g. secrets, per-machine paths). Loaded last so it can override
;; anything above.
(let ((local-files (file-expand-wildcards
                     (expand-file-name "local/*.el" user-emacs-directory))))
  (dolist (file (sort local-files #'string<))
    (load file nil t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(agent-shell corfu counsel-projectile doom-modeline doom-themes evil
                 exec-path-from-shell git-link kubed magit org-bullets
                 rainbow-delimiters smex super-save visual-fill-column
                 ws-butler yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:foreground "#98be65"))))
 '(font-lock-variable-name-face ((t (:foreground "#51afef"))))
 '(org-block ((t (:extend t)))))
