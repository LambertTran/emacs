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

;; Exact versions to install for every package pulled in via `:ensure t'
;; below. Bump a string here and restart Emacs to upgrade that package;
;; package.el will fetch exactly that version instead of whatever happens
;; to be newest in the archive at `package-refresh-contents' time.
(defvar my/package-versions
  '((agent-shell . "20261017.42")
    (corfu . "20261014.1751")
    (counsel . "20260913.715")
    (counsel-projectile . "20260918.709")
    (doom-modeline . "20261013.257")
    (doom-themes . "20260826.1413")
    (evil . "20260728.1438")
    (evil-collection . "20261014.225")
    (evil-nerd-commenter . "20260507.414")
    (evil-surround . "20240325.852")
    (exec-path-from-shell . "20260423.1833")
    (git-link . "20260723.2213")
    (golden-ratio . "20230912.1825")
    (ivy . "20260913.715")
    (ivy-rich . "20230425.1422")
    (magit . "20261016.1052")
    (markdown-mode . "20261009.18")
    (nerd-icons . "20261011.1410")
    (nerd-icons-dired . "20260415.1746")
    (nerd-icons-ivy-rich . "20250307.1005")
    (org-bullets . "20200317.1740")
    (projectile . "20260930.434")
    (rainbow-delimiters . "20210515.1254")
    (smex . "20151212.2209")
    (super-save . "20260318.1214")
    (visual-fill-column . "20251110.1039")
    (vterm . "20260914.1641")
    (ws-butler . "20250310.205")
    (yasnippet . "20250602.1342"))
  "Exact package versions pinned in this config, keyed by package name.
Every entry here corresponds to a `use-package' declaration with
`:ensure t' somewhere under features/. There's no entry for `org' because
this config relies on the version bundled with Emacs itself, and none for
`terraform-ts-mode' because it's installed via `package-vc' (see
features/fe-terraform.el), which pins a commit instead of an archive
version.")

(defun my/package-ensure-pinned (name &rest _)
  "Install the exact version of package NAME recorded in `my/package-versions'.
Used as `use-package-ensure-function' so every `:ensure t' in this config
installs a specific, reproducible version instead of just \"latest\"."
  (let* ((pkg (if (consp name) (car name) name))
         (version (alist-get pkg my/package-versions)))
    (unless (package-installed-p pkg (and version (version-to-list version)))
      (let ((desc (and version
                        (seq-find (lambda (d) (string= (package-version-join
                                                         (package-desc-version d))
                                                        version))
                                  (cdr (assq pkg package-archive-contents))))))
        (cond
         (desc (package-install desc))
         (version (warn "my/package-ensure-pinned: pinned version %s of %s not found in archive contents; installing latest instead"
                         version pkg)
                  (package-install pkg))
         (t (package-install pkg)))))))

(setq use-package-ensure-function #'my/package-ensure-pinned)

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
   '(agent-shell corfu counsel-projectile doom-modeline doom-themes
                 evil-collection evil-nerd-commenter evil-surround
                 exec-path-from-shell git-link golden-ratio
                 magit markdown-mode nerd-icons-dired
                 nerd-icons-ivy-rich org-bullets rainbow-delimiters
                 smex super-save terraform-ts-mode visual-fill-column
                 vterm ws-butler yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:foreground "#98be65"))))
 '(font-lock-variable-name-face ((t (:foreground "#51afef"))))
 '(org-block ((t (:extend t)))))
