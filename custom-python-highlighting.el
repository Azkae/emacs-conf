;;; -*- lexical-binding: t; -*-

(el-patch-feature python)

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
     (el-patch-remove
       (parameters (identifier) @font-lock-variable-name-face)
       (parameters (typed_parameter (identifier) @font-lock-variable-name-face))
       (parameters (default_parameter name: (identifier) @font-lock-variable-name-face)))
     )

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
   '((call function: (identifier) @font-lock-function-call-face)
     (call function: (attribute
                      attribute: (identifier) @font-lock-function-call-face)))

   :feature 'constant
   :language 'python
   '([(true) (false) (none)] @font-lock-constant-face)

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
   ;; Override built-in faces when dict/list are used for type hints.
   :override t
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
      (:match "^is\\(?:instance\\|subclass\\)$" @func-name)))

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

(provide 'custom-python-highlighting)
