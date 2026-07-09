;;; fe-yaml.el --- YAML support  -*- lexical-binding: t -*-
;;; Code:

(use-package yaml-ts-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :hook ((yaml-ts-mode . eglot-ensure)
         (yaml-ts-mode . (lambda () (setq tab-width 2)))
         (yaml-ts-mode . fe-yaml-breadcrumb-mode)))

;; yaml-ts-mode ships no imenu/which-function support, so there's nothing to
;; hang a breadcrumb off of. This walks upward from point by indentation to
;; find each shallower ancestor key and shows the path in the header-line --
;; handy for big/deeply-nested files where the parent context has scrolled
;; off screen.

(defun fe-yaml--key-at-line ()
  "If the current line defines a YAML mapping key, return (INDENT . KEY).
Handles list-item prefixes like \"- key:\"."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\( *\\)\\(?:- +\\)*\\([A-Za-z0-9_.\"'-]+\\):\\(?:[ \t]\\|$\\)")
      (cons (length (match-string 1)) (match-string 2)))))

(defun fe-yaml-breadcrumb ()
  "Return a breadcrumb string of ancestor YAML keys above point."
  (save-excursion
    (beginning-of-line)
    (let* ((here (fe-yaml--key-at-line))
           (ceiling (if here (car here) (current-indentation)))
           (path (if here (list (cdr here)) nil)))
      (while (and (> ceiling 0) (not (bobp)))
        (forward-line -1)
        (let ((k (fe-yaml--key-at-line)))
          (when (and k (< (car k) ceiling))
            (push (cdr k) path)
            (setq ceiling (car k)))))
      (mapconcat #'identity path " > "))))

(defvar-local fe-yaml--breadcrumb-line nil)

(defun fe-yaml--maybe-update-breadcrumb ()
  "Recompute the breadcrumb only when the current line has changed.
Keeps the header-line always present (even when blank) so the window
doesn't resize/flicker as point moves on and off key/value lines."
  (let ((line (line-number-at-pos)))
    (unless (eq line fe-yaml--breadcrumb-line)
      (setq fe-yaml--breadcrumb-line line)
      (setq header-line-format (concat " " (fe-yaml-breadcrumb))))))

(define-minor-mode fe-yaml-breadcrumb-mode
  "Show the ancestor YAML key path for the current line in the header-line."
  :lighter nil
  (if fe-yaml-breadcrumb-mode
      (progn
        (add-hook 'post-command-hook #'fe-yaml--maybe-update-breadcrumb nil t)
        (fe-yaml--maybe-update-breadcrumb))
    (remove-hook 'post-command-hook #'fe-yaml--maybe-update-breadcrumb t)
    (setq header-line-format nil)))

(provide 'fe-yaml)
;;; fe-yaml.el ends here
