;;; fe-git.el --- Git integration  -*- lexical-binding: t -*-
;;; Code:

(use-package magit
  :ensure t
  :custom
  ;; open magit status in same buffer
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (global-set-key (kbd "C-x g") 'magit-status)

  ;; open file in git
  (use-package git-link
    :ensure t
    :config
    (setq git-link-open-in-browser t))

  (global-set-key (kbd "C-c l") 'git-link))

(defun lbt-git-clone-from-clipboard ()
  "Clone the git repo URL on the clipboard, open it, and track it in Projectile.

Prompts for the parent directory to clone into, starting at the home
directory; the repo is cloned as a subdirectory of it, named after the
repository (mirroring plain `git clone' with no explicit destination)."
  (interactive)
  (let* ((url (string-trim (current-kill 0)))
         (name (or (magit-clone--url-to-name url)
                   (user-error "Clipboard doesn't look like a git URL: %s" url)))
         (parent (read-directory-name "Clone into: " "~/"))
         (dest (expand-file-name name parent)))
    (when (file-exists-p dest)
      (user-error "%s already exists" dest))
    (message "Cloning %s into %s..." url dest)
    (let ((exit-code (call-process "git" nil (get-buffer-create "*git-clone*") nil
                                    "clone" url dest)))
      (unless (zerop exit-code)
        (pop-to-buffer "*git-clone*")
        (error "Git clone failed (exit code %d); see *git-clone* buffer" exit-code)))
    (message "Cloned %s into %s" url dest)
    (projectile-add-known-project dest)
    (find-file dest)))

(provide 'fe-git)
;;; fe-git.el ends here
