;;; fe-ui.el --- UI, theme, and mode line  -*- lexical-binding: t -*-
;;; Code:

(tool-bar-mode -1)
(tooltip-mode -1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(column-number-mode t)
(global-auto-revert-mode t)
(toggle-scroll-bar -1)
(global-visual-line-mode 1)
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)
(setq ring-bell-function 'ignore)
;; disable tabbing
(setq-default indent-tabs-mode nil)

;; set warning to Error
(setq warning-minimum-level :emergency)

;; Register the font in default-frame-alist so every future frame (GUI or
;; emacsclient) picks it up, even when this file runs headless during
;; `emacs --daemon` startup -- set-frame-font alone doesn't persist to
;; frames created later in that case, since no graphical frame exists yet
;; for it to apply to or register from.
(add-to-list 'default-frame-alist '(font . "Monaco-17"))
(when (display-graphic-p)
  (set-frame-font "Monaco 17" nil t))

;; use with doom-mode-line
;; https://github.com/doomemacs/themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(custom-set-faces
 `(font-lock-keyword-face ((t (:foreground ,(doom-color 'green '257)))))
 `(font-lock-variable-name-face ((t (:foreground ,(doom-color 'blue '1000))))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; https://github.com/rainstormstudio/nerd-icons.el#installing-fonts
(use-package nerd-icons
  :ensure t
  :config
  ;; Auto-install the Nerd Font glyphs on first run; skip once present so
  ;; startup doesn't hit the network (or re-download) every time.
  (let ((font-file (expand-file-name "Library/Fonts/NFM.ttf" (getenv "HOME"))))
    (unless (file-exists-p font-file)
      (nerd-icons-install-fonts t))))

;; https://github.com/seagle0128/doom-modeline#install
;; use with nerd-icons
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; https://github.com/rainstormstudio/nerd-icons-dired
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; https://github.com/roman/golden-ratio.el
(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1))

(provide 'fe-ui)
;;; fe-ui.el ends here
