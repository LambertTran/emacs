;; Emacs normally activates installed packages (running their autoload
;; files) before loading init.el. Some packages' autoloads (e.g.
;; terraform-ts-mode's) call `treesit-ready-p' at top level, which is void
;; until `treesit' itself has been required. Defer package activation to
;; init.el, which requires `treesit' first.
(setq package-enable-at-startup nil)
