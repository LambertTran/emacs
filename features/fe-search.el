;;; fe-search.el --- Search, completion UI, and project management  -*- lexical-binding: t -*-
;;; Code:

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper-isearch)
	     ("C-a" . swiper-all-thing-at-point)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; Adds M-x recent command sorting for counsel-M-x
(use-package smex
  :ensure t
  :defer 1
  :after counsel)

;; Adds file/buffer type icons to ivy candidates (M-x, switch-buffer, etc.)
(use-package nerd-icons-ivy-rich
  :ensure t
  :after ivy
  :init
  (nerd-icons-ivy-rich-mode 1))

;; ivy-rich must load after nerd-icons-ivy-rich configures its own
;; transformers, otherwise it overwrites them with its plain-text ones.
(use-package ivy-rich
  :ensure t
  :after nerd-icons-ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-q p") 'projectile-command-map)
  (setq projectile-use-git-grep t))

;; make sure projectile works with counsel
(use-package counsel-projectile
  :ensure t)

(defun projectile-discover-projects-in-directory (directory)
  "Discover any projects in DIRECTORY and add them to the projectile cache.
This function is not recursive and only adds projects with roots
at the top level of DIRECTORY."
  (interactive
   (list (read-directory-name "Starting directory: ")))
  (let ((subdirs (directory-files directory t)))
    (mapcar
     (lambda (dir)
       (when (and (file-directory-p dir)
                  (not (member (file-name-nondirectory dir) '(".." "."))))
         (let ((default-directory dir)
               (projectile-cached-project-root dir))
           (when (projectile-project-p)
             (projectile-add-known-project (projectile-project-root))))))
     subdirs)))

(provide 'fe-search)
;;; fe-search.el ends here
