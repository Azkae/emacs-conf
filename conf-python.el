;;; -*- lexical-binding: t; -*-

(defvar conf--python-current-root nil)
(defun conf--python-track-venv()
  (interactive)
  (when (not (file-remote-p buffer-file-name))
    (let ((root (locate-dominating-file default-directory "pyproject.toml")))
      (when (and root (not (string= root conf--python-current-root)))
        (message "checking for root %s" root)
        (when-let* ((venv-path (expand-file-name ".venv" root))
                    ((file-directory-p venv-path)))
          (progn
            (message "Applying venv: %s (.venv)" venv-path)
            (setq conf--python-current-root root)
            (pyvenv-activate venv-path)))))))

(add-hook 'find-file-hook 'conf--python-track-venv)

(use-package python
  :straight (:type built-in)
  :bind
  (:map python-ts-mode-map
        ("C-c C-l" . nil))
  :config
  (defun my-remove-python-completion-at-point ()
    "Remove python-completion-at-point from completion-at-point-functions."
    (setq completion-at-point-functions
          (remove #'python-completion-at-point completion-at-point-functions)))

  (add-hook 'python-mode-hook #'my-remove-python-completion-at-point)
  (add-hook 'python-ts-mode-hook #'my-remove-python-completion-at-point))

(el-patch-feature python)

;; TODO: use treesit-font-lock-recompute-features instead
(el-patch-defvar python--treesit-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'python
   '((comment) @font-lock-comment-face)

   :feature 'string
   :language 'python
   '((string) @python--treesit-fontify-string
     (interpolation ["{" "}"] @font-lock-misc-punctuation-face))


   :feature 'keyword
   :language 'python
   `([,@python--treesit-keywords] @font-lock-keyword-face
     ((identifier) @font-lock-keyword-face
      (:match "\\`self\\'" @font-lock-keyword-face)))

   :feature 'definition
   :language 'python
   '((function_definition
      name: (identifier) @font-lock-function-name-face)
     (class_definition
      name: (identifier) @font-lock-type-face)
     (parameters (identifier) @font-lock-variable-name-face)
     (el-patch-remove
       (parameters (typed_parameter (identifier) @font-lock-variable-name-face)))

     (parameters (default_parameter name: (identifier) @font-lock-variable-name-face))
     (parameters (typed_default_parameter name: (identifier) @font-lock-variable-name-face))
     (lambda_parameters (identifier) @font-lock-variable-name-face)
     (for_in_clause
      left: (identifier) @font-lock-variable-name-face)
     ((import_from_statement
       name: ((dotted_name (identifier) @font-lock-type-face)))
      (:match "\\`[A-Z][A-Za-z0-9]+\\'" @font-lock-type-face))
     (import_from_statement
      name: ((dotted_name (identifier) @font-lock-variable-name-face))))

   :feature 'builtin
   :language 'python
   `((call function: (identifier) @font-lock-builtin-face
           (:match ,(rx-to-string
                     `(seq bol (or ,@python--treesit-builtins) eol))
                   @font-lock-builtin-face))
     (attribute attribute: (identifier) @font-lock-builtin-face
                (:match ,(rx-to-string
                          `(seq bol
                                (or ,@python--treesit-special-attributes) eol))
                        @font-lock-builtin-face)))

   :feature 'decorator
   :language 'python
   '((decorator "@" @font-lock-type-face)
     (decorator (call function: (identifier) @font-lock-type-face))
     (decorator (identifier) @font-lock-type-face)
     (decorator [(attribute) (call (attribute))] @python--treesit-fontify-dotted-decorator))

   :feature 'function
   :language 'python
   '(((call function: (identifier) @font-lock-type-face)
      (:match "\\`[A-Z][A-Za-z0-9]+\\'" @font-lock-type-face))
     (call function: (identifier) @font-lock-function-call-face)
     (call arguments: (argument_list (keyword_argument
                                      name: (identifier) @font-lock-property-name-face)))
     (call function: (attribute
                      attribute: (identifier) @font-lock-function-call-face)))

   :feature 'constant
   :language 'python
   '([(true) (false) (none)] @font-lock-constant-face
     ((assignment  (identifier) @font-lock-constant-face)
      (:match "\\`[A-Z][A-Z0-9_]+\\'" @font-lock-constant-face))
     ((call arguments: (argument_list (identifier) @font-lock-constant-face))
      (:match "\\`[A-Z][A-Z0-9_]+\\'" @font-lock-constant-face))
     ((attribute
       attribute: (identifier) @font-lock-constant-face)
      (:match "\\`[A-Z][A-Z0-9_]+\\'" @font-lock-constant-face)))

   :feature 'assignment
   :language 'python
   `(;; Variable names and LHS.
     (assignment left: (identifier)
                 @font-lock-variable-name-face)
     (assignment left: (attribute
                        attribute: (identifier)
                        @font-lock-variable-name-face))
     (augmented_assignment left: (identifier)
                           @font-lock-variable-name-face)
     (named_expression name: (identifier)
                       @font-lock-variable-name-face)
     (for_statement left: (identifier) @font-lock-variable-name-face)
     (pattern_list [(identifier)
                    (list_splat_pattern (identifier))]
                   @font-lock-variable-name-face)
     (tuple_pattern [(identifier)
                     (list_splat_pattern (identifier))]
                    @font-lock-variable-name-face)
     (list_pattern [(identifier)
                    (list_splat_pattern (identifier))]
                   @font-lock-variable-name-face))


   :feature 'type
   :language 'python
   `(((identifier) @font-lock-type-face
      (:match ,(rx-to-string
                `(seq bol (or ,@python--treesit-exceptions)
                  eol))
              @font-lock-type-face))
     (type [(identifier) (none)] @font-lock-type-face)
     (type (attribute attribute: (identifier) @font-lock-type-face))
     ;; We don't want to highlight a package of the type
     ;; (e.g. pack.ClassName).  So explicitly exclude patterns with
     ;; attribute, since we handle dotted type name in the previous
     ;; rule.  The following rule handle
     ;; generic_type/list/tuple/splat_type nodes.
     (type (_ !attribute [[(identifier) (none)] @font-lock-type-face
                          (attribute attribute: (identifier) @font-lock-type-face) ]))
     ;; collections.abc.Iterator[T] case.
     (type (subscript (attribute attribute: (identifier) @font-lock-type-face)))
     ;; Nested optional type hints, e.g. val: Lvl1 | Lvl2[Lvl3[Lvl4]].
     (type (binary_operator) @python--treesit-fontify-union-types)
     ;;class Type(Base1, Sequence[T]).
     (class_definition
      superclasses:
      (argument_list [(identifier) @font-lock-type-face
                      (attribute attribute: (identifier) @font-lock-type-face)
                      (subscript (identifier) @font-lock-type-face)
                      (subscript (attribute attribute: (identifier) @font-lock-type-face))]))

     ;; Pattern matching: case [str(), pack0.Type0()].  Take only the
     ;; last identifier.
     (class_pattern (dotted_name (identifier) @font-lock-type-face :anchor))

     ;; Highlight the second argument as a type in isinstance/issubclass.
     ((call function: (identifier) @func-name
            (argument_list :anchor (_)
                           [(identifier) @font-lock-type-face
                            (attribute attribute: (identifier) @font-lock-type-face)
                            (tuple (identifier) @font-lock-type-face)
                            (tuple (attribute attribute: (identifier) @font-lock-type-face))]
                           (:match ,python--treesit-type-regex @font-lock-type-face)))
      (:match "^is\\(?:instance\\|subclass\\)$" @func-name))

     ;; isinstance(t, int|float).
     ((call function: (identifier) @func-name
            (argument_list :anchor (_)
                           (binary_operator) @python--treesit-fontify-union-types-strict))
      (:match "^is\\(?:instance\\|subclass\\)$" @func-name))
     ((identifier) @font-lock-type-face
      (:match "\\`[A-Z][A-Za-z0-9]+\\'" @font-lock-type-face)))

   :feature 'escape-sequence
   :language 'python
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :feature 'number
   :language 'python
   '([(integer) (float)] @font-lock-number-face)

   :feature 'property
   :language 'python
   '((attribute
      attribute: (identifier) @font-lock-property-use-face)
     (class_definition
      body: (block
             (expression_statement
              (assignment left:
                          (identifier) @font-lock-property-use-face)))))

   :feature 'operator
   :language 'python
   `([,@python--treesit-operators] @font-lock-operator-face)

   :feature 'bracket
   :language 'python
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :feature 'delimiter
   :language 'python
   '(["," "." ":" ";" (ellipsis)] @font-lock-delimiter-face)

   :feature 'variable
   :language 'python
   '((identifier) @python--treesit-fontify-variable))
  "Tree-sitter font-lock settings.")

(use-package pyvenv)

(provide 'conf-python)
