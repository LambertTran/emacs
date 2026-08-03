;;; fe-markdown.el --- Markdown support  -*- lexical-binding: t -*-
;;; Code:

(unless (executable-find "pandoc")
  (when (executable-find "brew")
    (message "Installing pandoc...")
    (if (zerop (call-process "brew" nil "*pandoc-install*" nil "install" "pandoc"))
        (message "pandoc installed successfully.")
      (message "pandoc install failed; see *pandoc-install* buffer."))))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :custom (markdown-command "pandoc --standalone")
  :bind (:map markdown-mode-map ("C-c C-c x" . fe-markdown-preview-xwidget)))

;; `markdown-preview' always hands off to the OS browser, and the built-in
;; in-buffer alternative (`markdown-live-preview-mode', C-c C-c l) renders
;; via eww's shr engine, which is fine for text but weak on CSS/tables.
;; xwidget-webkit gives a real browser engine inside an Emacs buffer, so
;; prefer it here when this build of Emacs has xwidget support.

(defvar-local fe-markdown--preview-session nil)
(defvar-local fe-markdown--preview-timer nil)

(defun fe-markdown--preview-refresh ()
  "Re-render the buffer to HTML and reload the associated xwidget preview."
  (when (and fe-markdown--preview-session (xwidget-live-p fe-markdown--preview-session))
    (markdown-export)
    (with-current-buffer (xwidget-buffer fe-markdown--preview-session)
      (xwidget-webkit-reload))))

(defun fe-markdown--preview-schedule-refresh (&rest _)
  "Debounce a preview refresh after the buffer changes."
  (let ((buf (current-buffer)))
    (when (timerp fe-markdown--preview-timer)
      (cancel-timer fe-markdown--preview-timer))
    (setq fe-markdown--preview-timer
          (run-with-idle-timer
           0.5 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (fe-markdown--preview-refresh))))))))

(defun fe-markdown-preview-xwidget ()
  "Render the current Markdown buffer to HTML and preview it via xwidget-webkit
in a window to the right, refreshing the preview automatically as the buffer
changes."
  (interactive)
  (unless (featurep 'xwidget-internal)
    (user-error "This Emacs build has no xwidget support"))
  (require 'xwidget)
  (let* ((source-window (selected-window))
         (url (concat "file://" (markdown-export)))
         (live-session (and fe-markdown--preview-session
                             (xwidget-live-p fe-markdown--preview-session)
                             fe-markdown--preview-session))
         (preview-window (and live-session
                               (get-buffer-window (xwidget-buffer live-session)))))
    (select-window (or preview-window (split-window-right)))
    (xwidget-webkit-browse-url url (not live-session))
    (select-window source-window)
    (setq fe-markdown--preview-session (xwidget-webkit-current-session)))
  (add-hook 'after-change-functions #'fe-markdown--preview-schedule-refresh nil t))

(provide 'fe-markdown)
;;; fe-markdown.el ends here
