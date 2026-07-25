;;; fe-custom.el --- Custom user-defined functions  -*- lexical-binding: t -*-
;;; Code:

(defun lbt-copy-file-path-to-clipboard ()
  "Copy the current buffer's file path to the clipboard."
  (interactive)
  (let ((path (buffer-file-name)))
    (if path
        (progn
          (kill-new path)
          (message "Copied path: %s" path))
      (message "Buffer is not visiting a file"))))

(global-set-key (kbd "C-c j f") 'lbt-copy-file-path-to-clipboard)

(defun lbt ()
  "Open the current directory in iTerm."
  (interactive)
  (shell-command "open -a iterm ."))

(global-set-key (kbd "C-c j t") 'lbt)

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

(global-set-key (kbd "C-c j c") 'lbt-git-clone-from-clipboard)


(provide 'fe-custom)
;;; fe-custom.el ends here
