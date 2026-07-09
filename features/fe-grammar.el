;;; fe-grammar.el --- Tree-sitter grammar setup  -*- lexical-binding: t -*-
;;; Code:

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomode "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (hcl "https://github.com/MichaHoffmann/tree-sitter-hcl")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (jsonnet "https://github.com/sourcegraph/tree-sitter-jsonnet")
        (comment "https://github.com/stsewd/tree-sitter-comment")
        (sql "https://github.com/DerekStride/tree-sitter-sql")
        (org "https://github.com/milisims/tree-sitter-org")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun fe-treesit-install-all-grammars ()
  "Install every tree-sitter grammar listed in `treesit-language-source-alist'.
Run this manually (M-x fe-treesit-install-all-grammars) after adding a
new language, rather than on every startup."
  (interactive)
  (mapc #'treesit-install-language-grammar
        (mapcar #'car treesit-language-source-alist)))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)))

(provide 'fe-grammar)
;;; fe-grammar.el ends here
