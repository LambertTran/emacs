;;; fe-kubernetes.el --- Kubernetes support  -*- lexical-binding: t -*-
;;; Code:

(use-package kubed
  :ensure t
  :bind ("C-c k" . kubed-prefix-map)
  :config
  ;; kubed-list-mode (and modes derived from it, e.g. kubed-pods-mode) bind
  ;; single-letter commands like "d", "e", "x", "u", "w", "g", "o" directly in
  ;; their major-mode map. Evil's normal state shadows all of these with its
  ;; own bindings (evil-delete, evil-undo, ...), so put these buffers in
  ;; motion state instead, where kubed's own keymap takes over.
  (with-eval-after-load 'evil
    (evil-set-initial-state 'kubed-list-mode 'motion)
    ;; Evil's motion state itself binds several of the same single-key
    ;; letters kubed-list-mode-map uses for its own commands (g, e, w, !, ?,
    ;; /, +, |, RET, ...) for things like `gg', word motion, and search.
    ;; Evil's state keymaps take precedence over major-mode keymaps, so
    ;; without this those kubed commands (e.g. "g" to refresh) would never
    ;; fire. Re-bind them for motion state so kubed wins in this buffer.
    (evil-define-key 'motion kubed-list-mode-map
      (kbd "RET") #'kubed-list-select-resource
      "o"         #'kubed-list-select-resource-other-window
      (kbd "C-o") #'kubed-list-display-resource
      "D"         #'kubed-list-delete
      "P"         #'kubed-list-patch
      "x"         #'kubed-list-delete-marked
      "e"         #'kubed-list-edit
      "!"         #'kubed-list-kubectl-command
      "g"         #'kubed-list-update
      "/"         #'kubed-list-set-filter
      "|"         #'kubed-list-fit-column-width-to-content
      "d"         #'kubed-list-mark-for-deletion
      "u"         #'kubed-list-unmark
      "w"         #'kubed-list-copy-as-kill
      "+"         #'kubed-list-create
      "?"         #'kubed-list-transient))

  ;; kubed shells out to `kubectl' inheriting `process-environment' verbatim
  ;; (no dedicated hook for scrubbing it). If AWS_PROFILE / AWS_ACCESS_KEY_ID /
  ;; etc. are set in the environment (e.g. from an aws-vault or SSO shell),
  ;; the EKS auth plugin can pick them up and point `kubectl' at the wrong
  ;; account/cluster instead of using the kubeconfig's own credentials. Strip
  ;; AWS_* vars whenever the program being run is kubed's `kubectl'.
  (defun fe-kubernetes--kubectl-p (program)
    (equal program (kubed-kubectl-program)))

  (defun fe-kubernetes--without-aws-env (orig-fun program &rest args)
    (if (fe-kubernetes--kubectl-p program)
        (let ((process-environment
               (seq-remove (lambda (e) (string-prefix-p "AWS_" e))
                           process-environment)))
          (apply orig-fun program args))
      (apply orig-fun program args)))

  (advice-add 'process-file :around #'fe-kubernetes--without-aws-env)
  (advice-add 'call-process :around #'fe-kubernetes--without-aws-env)

  (defun fe-kubernetes--make-process-without-aws-env (orig-fun &rest args)
    (let ((command (plist-get args :command)))
      (if (and command (fe-kubernetes--kubectl-p (car command)))
          (let ((process-environment
                 (seq-remove (lambda (e) (string-prefix-p "AWS_" e))
                             process-environment)))
            (apply orig-fun args))
        (apply orig-fun args))))

  (advice-add 'make-process :around #'fe-kubernetes--make-process-without-aws-env))

(provide 'fe-kubernetes)
;;; fe-kubernetes.el ends here
