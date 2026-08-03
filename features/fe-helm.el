;;; fe-helm.el --- Helm chart template support  -*- lexical-binding: t -*-
;;; Code:

;; Helm templates are YAML with Go-template `{{ }}` directives spliced in,
;; which isn't valid YAML on its own -- plain `yaml-ts-mode' chokes on the
;; very first `{{- if }}'/`{{- end }}' wrapper and produces a whole-buffer
;; ERROR node (no highlighting, no indentation).
;;
;; Fix: parse the buffer with the `helm' dialect of tree-sitter-go-template
;; as the primary/host grammar (it understands the full if/range/with/end
;; structure and treats everything else as opaque `text' nodes), then inject
;; a single combined `yaml' parser across all of those `text' node ranges.
;; Because tree-sitter's included-ranges mechanism parses non-contiguous
;; ranges as if concatenated, the gaps left by `{{ ... }}' interpolations
;; disappear entirely from the embedded parser's point of view -- and since
;; those interpolations almost always sit at the end of a `key: value' line,
;; the concatenated result is valid YAML. This mirrors the combined-injection
;; setup nvim-treesitter already ships for its `helm' filetype.

(require 'treesit)
(require 'yaml-ts-mode)

(defvar fe-helm--font-lock-settings
  (append
   (treesit-font-lock-rules
    :language 'helm
    :feature 'bracket
    '((["{{" "}}" "{{-" "-}}" "(" ")"]) @font-lock-bracket-face)

    :language 'helm
    :feature 'keyword
    '((["if" "else" "end" "range" "with" "define" "block" "template"
        "break" "continue"])
      @font-lock-keyword-face)

    :language 'helm
    :feature 'operator
    '((["|" ":=" "="]) @font-lock-operator-face)

    :language 'helm
    :feature 'variable
    '((variable) @font-lock-variable-name-face
      (dot) @font-lock-variable-name-face
      (field_identifier) @font-lock-property-use-face)

    :language 'helm
    :feature 'function
    '((function_call function: (identifier) @font-lock-function-call-face)
      (method_call
       method: (selector_expression
                field: (field_identifier) @font-lock-function-call-face)))

    :language 'helm
    :feature 'string
    '([(interpreted_string_literal) (raw_string_literal)]
      @font-lock-string-face)

    :language 'helm
    :feature 'number
    '([(int_literal) (float_literal) (imaginary_literal)]
      @font-lock-number-face)

    :language 'helm
    :feature 'constant
    '([(true) (false) (nil)] @font-lock-constant-face)

    :language 'helm
    :feature 'comment
    '((comment) @font-lock-comment-face)

    :language 'helm
    :feature 'error
    :override t
    '((ERROR) @font-lock-warning-face))
   yaml-ts-mode--font-lock-settings)
  "Combined helm+yaml font-lock settings for `helm-ts-mode'.")

(defvar fe-helm--range-settings
  (treesit-range-rules
   :embed 'yaml
   :host 'helm
   '((text) @capture))
  "Range rules injecting a combined `yaml' parser into helm `text' nodes.")

;;;###autoload
(define-derived-mode helm-ts-mode text-mode "Helm"
  "Major mode for Helm chart YAML templates, powered by tree-sitter.

Combines the `helm' dialect of tree-sitter-go-template (host grammar,
understands `{{ }}' control structures) with an injected `yaml' parser
covering the literal YAML text between template directives."
  :group 'helm
  :syntax-table yaml-ts-mode--syntax-table
  (when (and (treesit-ready-p 'helm) (treesit-ready-p 'yaml))
    (treesit-parser-create 'helm)

    (setq-local comment-start "# ")
    (setq-local comment-end "")
    (setq-local indent-tabs-mode nil)

    (setq-local treesit-range-settings fe-helm--range-settings)

    (setq-local treesit-font-lock-settings fe-helm--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (string type keyword)
                  (constant escape-sequence number property
                            variable function operator)
                  (bracket delimiter error misc-punctuation)))

    (treesit-major-mode-setup)))

;; Only Helm chart template files should get this mode -- everything else
;; under charts/ (Chart.yaml, values.yaml) is plain YAML. Registered on
;; `after-init-hook' rather than here so it's guaranteed to win over
;; `fe-yaml.el''s plain "\\.ya?ml\\'" pattern regardless of fe-*.el load
;; order (auto-mode-alist entries added later take precedence).
(when (and (treesit-ready-p 'helm) (treesit-ready-p 'yaml))
  (add-hook 'after-init-hook
            (lambda ()
              (add-to-list 'auto-mode-alist
                           '("/templates/.*\\.ya?ml\\'" . helm-ts-mode))
              (add-to-list 'auto-mode-alist
                           '("helmfile.*\\.ya?ml\\'" . helm-ts-mode)))))

(provide 'fe-helm)
;;; fe-helm.el ends here
