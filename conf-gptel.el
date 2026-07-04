;; -*- lexical-binding: t; -*-

(use-package gptel
  :hook
  (gptel-mode . corfu-mode)
  (gptel-mode . (lambda () (setq-local completion-at-point-functions '(gptel-preset-capf))))
  :bind
  (:map gptel-mode-map
        ("C-c C-k" . gptel-abort)
        ("C-c C-<return>" . gptel-send))
  :config
  (global-set-key (kbd "C-c , g") 'gptel)
  (global-set-key (kbd "C-c , m") 'gptel-menu)
  (global-set-key (kbd "C-c , a") 'gptel-add)
  (global-set-key (kbd "C-c , ,") 'gptel-menu)
  (global-set-key (kbd "C-c , t") 'gptel-tools)
  (global-set-key (kbd "C-c , C") 'gptel--suffix-context-buffer)
  (when-let* ((anthropic-api-key (password-store-get "anthropic-api-key")))
    (setq
     gptel-model 'claude-sonnet-5
     gptel-backend (gptel-make-anthropic "Claude"
				     :stream t
				     :key anthropic-api-key))

    (gptel-make-anthropic "Claude-thinking"
      :key anthropic-api-key
      :stream t
      :models '(claude-sonnet-5 claude-opus-4-8)
      :request-params '(:thinking (:type "adaptive")))

    (gptel-make-openai "llama-server"
      :host "localhost:8123"
      :protocol "http"
      :stream t
      :key "ignored"
      :models '(gemma-4-E4B-it)))

  (defun conf--gptel-start-rewrite-session ()
    (interactive)
    (let ((buffer-name (generate-new-buffer-name "*gptel-rewrite*"))
          (gptel-default-mode 'markdown-mode))
      (gptel buffer-name nil "@rewrite ")
      (switch-to-buffer buffer-name)))

  (defun conf--gptel-demote-headings (start end)
    "Demote lines beginning with '* ' in the region."
    (interactive "r")
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (beginning-of-line)
          (when (looking-at "^\\* ")
            (org-demote-subtree))
          (forward-line 1)))))

  (defun conf--gptel-convert-to-headings (start end)
    "Replace */ by actual headings"
    (interactive "r")
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char end)
        (while (> (point) start)
          (beginning-of-line)
          (when (looking-at "^\\*/ ")
            (delete-char 2)
            (insert (make-string (1+ (org-current-level)) ?*)))
          (forward-line -1)))))

  (defun conf--gptel-fix-separators (start end)
    "Replace --- by org separators"
    (interactive "r")
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (beginning-of-line)
          (when (looking-at "^---$")
            (insert "--"))
          (forward-line 1)))))

  (defun conf--gptel-fix-indented-begin-tool (start end)
    "Add a newline before #+begin_tool when not at beginning of line in region."
    (interactive "r")
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char start)
        (while (re-search-forward "^\\(.+\\)\\(#\\+begin_tool\\)" end t)
          (replace-match "\\1\n\\2")))))

  (defun conf--align-tables-in-region (start end)
    (interactive "r")
    (when (eq major-mode 'org-mode)
      (save-excursion
        (let ((end-marker (copy-marker end)))  ; marker moves with insertions
          (goto-char start)
          (while (< (point) end-marker)
            (beginning-of-line)
            (when (looking-at "^|")
              (while (looking-at "^|")
                (forward-line 1))
              (forward-line -1)
              (org-table-align))
            (forward-line 1))
          (set-marker end-marker nil)))))

  (add-hook 'gptel-post-response-functions 'conf--gptel-demote-headings)
  (add-hook 'gptel-post-response-functions 'conf--gptel-convert-to-headings)
  (add-hook 'gptel-post-response-functions 'conf--gptel-fix-separators)
  (add-hook 'gptel-post-response-functions 'conf--gptel-fix-indented-begin-tool)
  (add-hook 'gptel-post-response-functions 'conf--align-tables-in-region)

  (global-set-key (kbd "C-c , r") 'gptel-rewrite)
  (global-set-key (kbd "C-c , R") 'conf--gptel-start-rewrite-session)

  (defun conf--gptel-start-quick-session ()
    (interactive)
    (let ((buffer-name (generate-new-buffer-name "*gptel-quick*"))
          (default-directory (or (and (fboundp 'project-root)
                                      (project-current)
                                      (project-root (project-current)))
                                 default-directory)))
      (gptel buffer-name nil "* ")
      (switch-to-buffer buffer-name)))

  (global-set-key (kbd "C-c , q") 'conf--gptel-start-quick-session)

  (setq gptel-default-mode 'org-mode)
  (setq gptel-include-tool-results t)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "* ")
  (setq gptel--set-buffer-locally t)
  (setq gptel-highlight-methods '(face))

  (add-hook 'gptel-mode-hook
            (lambda () (when (derived-mode-p 'org-mode)
                         (setq-local require-final-newline nil))))

  (add-to-list 'gptel-directives '(critical . "Prioritize substance, clarity, and depth. Challenge all my proposals, designs, and conclusions as hypotheses to be tested. Sharpen follow-up questions for precision, surfacing hidden assumptions, trade offs, and failure modes early. Default to terse, logically structured, information-dense responses unless detailed exploration is required. Skip unnecessary praise unless grounded in evidence. Explicitly acknowledge uncertainty when applicable. Always propose at least one alternative framing. Accept critical debate as normal and preferred. Treat all factual claims as provisional unless cited or clearly justified. Cite when appropriate. Acknowledge when claims rely on inference or incomplete information. Favor accuracy over sounding certain."))

  (add-to-list 'gptel-directives '(reword . "You are a text enhancement assistant. Your task is to rewrite the user's text with improved grammar, syntax, and expression while preserving their natural voice. Follow these guidelines:

1. *Fix errors*: Correct grammar, punctuation, spelling, and sentence structure
2. *Enhance clarity*: Improve word choice, sentence flow, and overall readability
3. *Preserve voice*: Maintain the user's original tone, style, and level of formality
4. *Keep meaning intact*: Ensure the core message and intent remain unchanged
5. *Subtle improvements*: Make enhancements feel natural, not forced or over-polished
6. *Formatting guideline*: Do not use em dashes (—) in your enhanced text. Use alternative punctuation such as commas, semicolons, parentheses, or periods instead.

Provide only the improved version unless the user requests explanations or has specific questions about the changes."))

  (gptel-make-preset 'rewrite
    :description "Text improvement and grammar correction"
    :system 'reword)

  (gptel-make-tool
   :name "elisp_eval"
   :function (lambda (code)
               (condition-case err
                   (let ((result (eval (read code))))
                     (format "Result: %S" result))
                 (error
                  (format "Error: %S" err))))
   :description "Evaluate elisp code and return the result"
   :args (list '(:name "code"
                       :type string
                       :description "The elisp code to evaluate"))
   :confirm t
   :include t
   :category "emacs-modify")

  ;; (gptel-make-tool
  ;;  :name "read_buffer"
  ;;  :function (lambda (buffer)
  ;;              (unless (buffer-live-p (get-buffer buffer))
  ;;                (error "error: buffer %s is not live." buffer))
  ;;              (with-current-buffer  buffer
  ;;                (buffer-substring-no-properties (point-min) (point-max))))
  ;;  :description "return the contents of an emacs buffer"
  ;;  :args (list '(:name "buffer"
  ;;                      :type string
  ;;                      :description "the name of the buffer whose contents are to be retrieved"))
  ;;  :confirm t
  ;;  :include t
  ;;  :category "emacs")

  (gptel-make-tool
   :name "elisp_describe_variable"
   :function (lambda (variable)
               (save-window-excursion (describe-variable (intern variable))))
   :description "describe variable of the current emacs session"
   :args (list '(:name "variable"
                       :type string
                       :description "variable name"))
   :include t
   :category "emacs")

  (gptel-make-tool
   :name "elisp_describe_face"
   :function (lambda (variable)
               (save-window-excursion (describe-face (intern variable))))
   :description "describe variable of the current emacs session"
   :args (list '(:name "variable"
                       :type string
                       :description "face name"))
   :include t
   :category "emacs")

  (gptel-make-tool
   :name "elisp_describe_function"
   :function (lambda (variable)
               (save-window-excursion (describe-function (intern variable))))
   :description "describe elisp function"
   :args (list '(:name "variable"
                       :type string
                       :description "function name"))
   :include t
   :category "emacs")

  (gptel-make-tool
   :name "elisp_prefix_completion"
   :function (lambda (prefix limit page type show-private)
               (let* ((completions '())
                      (case-fold-search nil)
                      (max-results (or limit 50))
                      (page-num (or page 1))
                      (skip-count (* (1- page-num) max-results)))
                 (mapatoms
                  (lambda (symbol)
                    (when (and (string-prefix-p prefix (symbol-name symbol))
                               (or (eq show-private t)
                                   (not (string-match-p "--" (symbol-name symbol))))
                               (cond
                                ((string= type "function") (fboundp symbol))
                                ((string= type "variable") (boundp symbol))
                                ((string= type "face") (facep symbol))
                                (t (or (fboundp symbol) (boundp symbol) (facep symbol)))))
                      (push (symbol-name symbol) completions))))
                 (let* ((sorted-completions (sort completions #'string<))
                        (total-count (length sorted-completions))
                        (total-pages (ceiling (/ (float total-count) max-results)))
                        (offset-completions (seq-drop sorted-completions skip-count))
                        (final-completions (seq-take offset-completions max-results)))
                   (if (and (> total-count skip-count)
                            (> (length offset-completions) max-results))
                       (append final-completions
                               (list (format "... page %d of %d (total: %d symbols)"
                                             page-num total-pages total-count)))
                     final-completions))))
   :description "Get completion candidates for elisp symbols matching a prefix"
   :args (list '(:name "prefix"
                       :type string
                       :description "The partial symbol name to complete")
               '(:name "limit"
                       :type integer
                       :description "Maximum number of results to return (default: 50)"
                       :optional t)
               '(:name "page"
                       :type integer
                       :description "Page number to display (default: 1, starts from 1)"
                       :optional t)
               '(:name "type"
                       :type string
                       :enum ["function" "variable" "face"]
                       :description "Filter by symbol type: function, variable, or face. If not specified, returns all types"
                       :optional t)
               '(:name "show-private"
                       :type boolean
                       :description "If t, show private symbols. If nil, hide private symbols (the default)"
                       :optional t))
   :confirm nil
   :include t
   :category "emacs")

  (gptel-make-tool
   :name "elisp_regexp_completion"
   :function (lambda (regexp limit page type show-private)
               (let* ((completions '())
                      (case-fold-search nil)
                      (max-results (or limit 50))
                      (page-num (or page 1))
                      (skip-count (* (1- page-num) max-results)))
                 (mapatoms
                  (lambda (symbol)
                    (when (and (string-match-p regexp (symbol-name symbol))
                               (or (eq show-private t)
                                   (not (string-match-p "--" (symbol-name symbol))))
                               (cond
                                ((string= type "function") (fboundp symbol))
                                ((string= type "variable") (boundp symbol))
                                ((string= type "face") (facep symbol))
                                (t (or (fboundp symbol) (boundp symbol) (facep symbol)))))
                      (push (symbol-name symbol) completions))))
                 (let* ((sorted-completions (sort completions #'string<))
                        (total-count (length sorted-completions))
                        (total-pages (ceiling (/ (float total-count) max-results)))
                        (offset-completions (seq-drop sorted-completions skip-count))
                        (final-completions (seq-take offset-completions max-results)))
                   (if (and (> total-count skip-count)
                            (> (length offset-completions) max-results))
                       (append final-completions
                               (list (format "... page %d of %d (total: %d symbols)"
                                             page-num total-pages total-count)))
                     final-completions))))
   :description "Get completion candidates for elisp symbols matching a regexp"
   :args (list '(:name "regexp"
                       :type string
                       :description "The partial symbol name to complete")
               '(:name "limit"
                       :type integer
                       :description "Maximum number of results to return (default: 50)"
                       :optional t)
               '(:name "page"
                       :type integer
                       :description "Page number to display (default: 1, starts from 1)"
                       :optional t)
               '(:name "type"
                       :type string
                       :enum ["function" "variable" "face"]
                       :description "Filter by symbol type: function, variable, or face. If not specified, returns all types"
                       :optional t)
               '(:name "show-private"
                       :type boolean
                       :description "If t, show private symbols. If nil, hide private symbols (the default)"
                       :optional t))
   :confirm nil
   :include t
   :category "emacs")

  (gptel-make-preset 'visible-buffers
    :description "Include the full text of all buffers visible in the frame."
    :context
    '(:eval (mapcar #'window-buffer (remove (selected-window)
                                            (window-list (selected-frame))))))

  (gptel-make-preset 'verb-generate
    :description "Generate a verb http client file"
    :system "You are an expert at creating Emacs Verb mode files for API testing and documentation. Given an API description, you will generate a well-structured org-mode file that uses Verb mode syntax.

# Your output should follow this structure:

1. *Header*: Start with an org-mode title using the API name and =:verb:= tag
2. *Template section*: Include the base URL and common headers
3. *Endpoints*: Create sections for each endpoint with:
   - Descriptive heading (use `**` for endpoints)
   - HTTP method and path
   - Request body (if applicable) in =#+BEGIN_SRC json= blocks
   - Template variables using `{{(verb-var variable-name)}}` for secrets

*Formatting rules:*
- Use proper org-mode hierarchy (`*`, `**`, `***`)
- Include =:verb:= tag in the main heading
- Use `template` directive for base URL and common headers
- Format JSON request bodies in code blocks with proper indentation
- Prefer simpler endpoint with maximum 2 parameters (using `read-string`), duplicate endpoint with multiple variation if that makes more sense.

*Example patterns to follow:*
- Template variables: `{{(verb-var token)}}`
- User input variables: `{{(read-string \"reportId: \")}}`
- Headers: `X-Api-Key:`, `Content-Type:`, `Authorization:`
- JSON formatting with 2-space indentation

*Complete example:*

```org-mode
* Weather API                                                                      :verb:
template https://api.weather.com/v1
Authorization: Bearer {{(verb-var api-key)}}
Content-Type: application/json

** Get current weather
GET /weather/current?city={{(read-string \"City: \")}}

** Create weather alert
POST /alerts

#+BEGIN_SRC json
{
  \"city\": \"Paris\",
  \"threshold\": {{(read-string \"Threshold: \")}}
}
#+END_SRC

** Get alert status
GET /alerts/{{(read-string \"Alert ID: \")}}/status
```

Respond with a a complete Verb file and nothing else.")

  (gptel-make-preset 'visible-text
    :description "Include visible text from all windows in the frame."
    :context
    '(:eval (mapcar (lambda (win) ;; Create (<buffer> :bounds ((start . end)))
                      `(,(window-buffer win)
                        :bounds ((,(window-start win) . ,(window-end win)))))
                    (remove (selected-window) (window-list (selected-frame))))))

  (defun gptel-review-code ()
    "Send current region or buffer for code review in a dedicated buffer."
    (interactive)
    (let* ((review-buffer (get-buffer-create "*Code Review*"))
           (code-text (if (use-region-p)
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (buffer-substring-no-properties (point-min) (point-max))))
           (source-info (format "Code review for %s:\n" (buffer-name)))
           (prompt (concat "Please review this code:\n\n=\n" code-text "\n=")))

      ;; Setup the review buffer
      (with-current-buffer review-buffer
        (erase-buffer)
        (markdown-mode)
        (insert source-info)
        (insert "=" (make-string 50 ?=) "=\n\n")
        (insert "Requesting code review...\n\n"))

      ;; Send request directly to the review buffer
      (gptel-request prompt
                     :buffer review-buffer
                     :position (with-current-buffer review-buffer (point-max))
                     :system "You are a code reviewer. Provide a concise review focusing on critical issues, bugs, and immediate improvements. Keep responses brief."
                     :stream t)

      ;; Show the buffer
      (pop-to-buffer review-buffer)))

  (defun gptel-collapse-tool-blocks ()
    "Collapse all #+begin_tool blocks in the current org buffer."
    (interactive)
    (org-block-map
     (lambda ()
       (when (save-excursion
               (beginning-of-line 1)
               (looking-at "^[ \t]*#\\+begin_tool\\b"))
         (org-fold-hide-block-toggle t)))))

  (defun setup-gptel-note ()
    (interactive)
    (gptel-mode 1)
    (org-show-all '(headings blocks))
    (gptel-collapse-tool-blocks))

  (defun conf--gptel-add-auto-local-var ()
    "Ensure that this file opens with `gptel-mode' enabled."
    (save-excursion
      (let ((enable-local-variables t))  ; Ensure we can modify local variables
        (if (and (save-excursion
                   (goto-char (point-min))
                   (looking-at ".*-\\*-")))  ; If there's a -*- line
            ;; First remove any existing eval, then add the new one
            (modify-file-local-variable-prop-line
             'eval nil 'delete))
        ;; Always add our eval
        (add-file-local-variable-prop-line
         'eval '(and (fboundp 'setup-gptel-note) (setup-gptel-note))))))

  (add-hook 'gptel-save-state-hook #'conf--gptel-add-auto-local-var)

  (defvar conf--gptel-save-directory "~/Dropbox/gptel-chats/"
  "Directory where gptel conversations are saved.")

  (defun conf--gptel-save-buffer ()
    "Save the current gptel buffer with proper integration.
If the buffer is not yet associated with a file, prompt for a filename
and prepend it with a timestamp. Otherwise, save normally."
    (interactive)
    (if (buffer-file-name)
        ;; Buffer already has a file, just save it normally
        (save-buffer)
      ;; Buffer doesn't have a file yet, create timestamped filename
      (unless (file-exists-p conf--gptel-save-directory)
        (make-directory conf--gptel-save-directory t))

      (let* ((timestamp (format-time-string "%Y%m%dT%H%M%S"))
             (user-filename (read-string "Filename: "))
             (full-filename (concat timestamp "--" (string-replace " " "-" user-filename) ".org"))
             (filepath (expand-file-name full-filename conf--gptel-save-directory)))

        ;; Set the buffer's file name and save
        (set-visited-file-name filepath)
        (save-buffer)
        (message "Saved to %s" filepath))))

  (defun conf--gptel-find-file ()
    "Run find-file in `conf--gptel-save-directory'."
    (interactive)
    (let ((default-directory conf--gptel-save-directory))
      (call-interactively #'find-file)))

  (global-set-key (kbd "C-c , f") 'conf--gptel-find-file)

;; Add the keybinding to gptel-mode-map
  (with-eval-after-load 'gptel
    (define-key gptel-mode-map (kbd "M-s") #'conf--gptel-save-buffer))

  (defun gptel-psql-execute (sql-command &optional database)
    "Execute a PostgreSQL command using psql -c.
SQL-COMMAND is the SQL query to execute.
DATABASE is optional and specifies which database to connect to."
    (let* ((db-arg (if (and database (not (string-empty-p database)))
                       (list "-d" database)
                     nil))
           (args (append '("-c") (list sql-command) db-arg))
           (output (with-output-to-string
                     (with-current-buffer standard-output
                       (apply #'call-process "psql" nil t nil args)))))
      output))

  (gptel-make-tool
   :name "execute_psql"
   :function #'gptel-psql-execute
   :description "Execute a PostgreSQL SQL command using psql -c and return the results. This can be used to query databases, check table schemas, or run any valid SQL command.
NEVER use this tool to modify the user database. Use it solely to explore database tables and their content. If the user requests a query, write it out as text rather than executing it with this tool."
   :args (list
          '(:name "sql_command"
            :type string
            :description "The SQL command to execute, e.g. 'SELECT * FROM users LIMIT 10;' or '\\dt' to list tables")
          '(:name "database"
            :type string
            :description "The database name to connect to. If not provided, uses the default database."
            :optional t))
   :category "database"
   :confirm t
   :include t)

  (gptel-make-preset 'psql
    :description "Access to postgres db"
    :tools '(:append ("execute_psql"))
    :system "Use the execute_psql tool to explore the database if necessary and then reply with requested query from the user. Do not execute the query that the user requested, simply reply with the requested query.")

  (defun conf--gptel-bash-execute (command)
    "Execute a bash COMMAND and return its output (stdout + stderr)."
    (with-output-to-string
      (with-current-buffer standard-output
        (call-process "bash" nil t nil "-c" command))))

  (gptel-make-tool
   :name "execute_bash"
   :function #'conf--gptel-bash-execute
   :description "Execute an arbitrary bash command and return its combined stdout and stderr output. Use this to run shell commands, scripts, inspect the filesystem, check system state, or perform any task expressible as a bash command."
   :args (list
          '(:name "command"
                  :type string
                  :description "The bash command to execute, e.g. 'ls -la ~/' or 'git log --oneline -10'"))
   :category "shell"
   :confirm t
   :include t)

  (gptel-make-preset 'sh
    :description "Access to sh commands"
    :tools '(:append ("execute_bash")))

  (gptel-make-preset 'emacs
    :description "Access to emacs tools"
    :tools '(:append ("elisp_describe_variable" "elisp_describe_face" "elisp_describe_function" "elisp_prefix_completion" "elisp_regexp_completion")))

  (gptel-make-preset 'json
    :description "JIT only: use json schema following @json cookie"
    :pre (lambda ()
           (setq-local gptel--schema
                       (buffer-substring-no-properties (point) (point-max)))
           (delete-region (point) (point-max)))
    :include-reasoning nil)

  (gptel-make-preset 'cli
    :description "Write cli commands"
    :system "The user will ask for a cli command. Respond with only the cli command, without code fences")

  (gptel-make-preset 'introspect
    :pre (lambda () (require 'gptel-agent-tools-introspection))
    :tools '("introspection")
    :description "TOOLS: Emacs introspection"
    :system
    "Your job is to dive into Elisp code and understand the APIs and
structure of elisp libraries and Emacs.  Use the provided tools to do
so, but do not make duplicate tool calls for information already
available in the chat.

<tone>
1. Be terse and to the point.  Speak directly.
2. Explain your reasoning.
3. Do NOT hedge or qualify.
4. If you don't know, say you don't know.
5. Do not offer unprompted advice or clarifications.
6. Never apologize.
7. Do NOT summarize your answers.
</tone>")

  (defun my/remove-text-properties-in-region (beg end)
    "Remove all text properties in the region from BEG to END."
    (interactive "r")
    (set-text-properties beg end nil)))

  (defun my/remove-overlay-in-region (beg end)
    "Remove all overlays in the region from BEG to END."
    (interactive "r")
    (remove-overlays beg end))

(use-package mcp
  :config
  (require 'gptel-integrations)
  (setq mcp-hub-servers nil)
  (add-to-list 'mcp-hub-servers '("deepwiki" :command "npx"
                                  :args ("mcp-remote" "https://mcp.deepwiki.com/mcp")))
  (add-to-list 'mcp-hub-servers '("posthog" :command "npx"
                                  :args ("mcp-remote" "https://mcp.posthog.com/mcp")))
  (add-to-list 'mcp-hub-servers `("webfetch"
                                  :command "docker"
                                  :args ("run" "-i" "--rm" "mcp/fetch")))
  (when-let* ((brave-api-key (password-store-get "brave-api-key")))
    (add-to-list 'mcp-hub-servers
               `("brave"
                 :command "docker"
                 :args ("run" "-i" "--rm" "-e" "BRAVE_API_KEY" "docker.io/mcp/brave-search")
                 :env (:BRAVE_API_KEY ,brave-api-key)))))


(use-package gptel-agent
  :config (gptel-agent-update)
  (require 'gptel-agent-tools-introspection))


(defun gptel-review-packages ()
  "Send current region or buffer for code review in a dedicated buffer."
  (interactive)
  (let* ((review-buffer (get-buffer-create "*Package review*"))
         (code-text (buffer-substring-no-properties (point-min) (point-max)))
         (prompt (concat "Please review this code:\n\n=\n" code-text "\n=")))

    (with-current-buffer review-buffer
      (erase-buffer)
      (markdown-mode)
      (insert "Requesting package review...\n\n"))

    (gptel-request prompt
      :buffer review-buffer
      :position (with-current-buffer review-buffer (point-max))
      :system "You are a code security reviewer, analyze the code to find malware or malicious changes. Respond concisely."
      :stream t)

    (pop-to-buffer review-buffer)))

(use-package macher
  :straight (:host github :repo "kmontag/macher")

  :custom
  ;; The org UI has structured navigation and nice content folding.
  (macher-action-buffer-ui 'org)

  :config
  (with-eval-after-load 'gptel (macher-install)))


(use-package gptel-quick
  :straight (gptel-quick :type git :host github :repo "karthink/gptel-quick")
  :config
  (keymap-set embark-general-map "q" #'gptel-quick)
  (add-to-list 'embark-post-action-hooks '(gptel-quick embark--unmark-target)))


(provide 'conf-gptel)
