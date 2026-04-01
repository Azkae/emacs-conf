;;; package-review.el --- Review straight.el package updates -*- lexical-binding: t -*-

;; Author: Romain
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience
;; URL:

;;; Commentary:

;; After pulling package updates with `straight-pull-package-and-deps',
;; invoke `package-review' to see what changed.
;;
;; For each package whose current HEAD differs from the pinned lockfile
;; commit, a diff is displayed in a dedicated review buffer.
;;
;; Usage:
;;   M-x package-review        — review all changed packages
;;   M-x package-review-package — review a single package (with completion)
;;
;; Key bindings in the review buffer:
;;   n / p     — next / previous hunk
;;   N / P     — next / previous file (package)
;;   q         — bury buffer
;;   TAB       — toggle section fold
;;   RET       — open the file at point (if in a diff context)

;;; Code:

(require 'cl-lib)
(require 'diff-mode)

;; Declare straight.el symbols used at runtime to silence byte-compiler
;; warnings when the package is compiled before straight.el is loaded.
(defvar straight--recipe-cache)
(declare-function straight--lockfile-read-all "straight" ())
(declare-function straight--map-repos         "straight" (func))
(declare-function straight--repos-dir         "straight" (&rest segments))

(defgroup package-review nil
  "Review straight.el package updates."
  :group 'tools
  :prefix "package-review-")

(defcustom package-review-buffer-name "*Package Review*"
  "Name of the buffer used to display package diffs."
  :type 'string
  :group 'package-review)

(defcustom package-review-git-log-format "%h %s (%an, %cr)"
  "Format string passed to git log --format for the commit summary header."
  :type 'string
  :group 'package-review)

(defcustom package-review-show-diff t
  "Whether to include the full unified diff in the review buffer."
  :type 'boolean
  :group 'package-review)

(defcustom package-review-diff-context-lines 3
  "Number of context lines to include in the unified diff."
  :type 'natnum
  :group 'package-review)

;;; ── Internal helpers ──────────────────────────────────────────────────────

(defun package-review--lockfile-pins ()
  "Return an alist of (LOCAL-REPO . PINNED-COMMIT) from all straight profiles."
  (straight--lockfile-read-all))

(defun package-review--head-commit (local-repo)
  "Return the current HEAD commit hash for LOCAL-REPO, or nil on error."
  (let ((default-directory (straight--repos-dir local-repo)))
    (when (file-directory-p default-directory)
      (condition-case nil
          (string-trim
           (shell-command-to-string "git rev-parse HEAD 2>/dev/null"))
        (error nil)))))

(defun package-review--changed-packages ()
  "Return a list of plists for packages that have changed since pinned commit.

Each plist has:
  :package    — package name string
  :local-repo — local-repo directory name string
  :pinned     — the pinned commit SHA string
  :head       — the current HEAD commit SHA string
  :repo-dir   — absolute path to the repo directory"
  (let ((pins (package-review--lockfile-pins))
        changed)
    (straight--map-repos
     (lambda (recipe)
       (let* ((local-repo (plist-get recipe :local-repo))
              (package    (plist-get recipe :package))
              (pinned     (cdr (assoc local-repo pins)))
              (head       (package-review--head-commit local-repo))
              (repo-dir   (straight--repos-dir local-repo)))
         (when (and pinned head
                    (not (string= pinned head))
                    (not (string-empty-p pinned))
                    (not (string-empty-p head)))
           (push (list :package    package
                       :local-repo local-repo
                       :pinned     pinned
                       :head       head
                       :repo-dir   repo-dir)
                 changed)))))
    (nreverse changed)))

(defun package-review--git-log (repo-dir from to)
  "Return git log lines for commits between FROM and TO in REPO-DIR."
  (let ((default-directory repo-dir))
    (string-trim
     (shell-command-to-string
      (format "git log --format=%s %s..%s 2>/dev/null"
              (shell-quote-argument package-review-git-log-format)
              from to)))))

(defun package-review--git-diff (repo-dir from to)
  "Return unified diff between FROM and TO in REPO-DIR."
  (let ((default-directory repo-dir))
    (shell-command-to-string
     (format "git diff -U%d %s..%s -- '*.el' 2>/dev/null"
             package-review-diff-context-lines
             from to))))

;;; ── Major mode ────────────────────────────────────────────────────────────

(defvar package-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "g")   #'package-review)
    (define-key map (kbd "TAB") #'outline-cycle)
    (define-key map (kbd "<backtab>") #'outline-cycle-buffer)
    map)
  "Key map for `package-review-mode'.
Inherits all of `diff-mode' bindings (n/p hunks, N/P files, RET jump).")

(define-derived-mode package-review-mode diff-mode "PkgReview"
  "Major mode for reviewing straight.el package updates.

Derives from `diff-mode', so all diff navigation is available:
  n / p     — next / previous hunk
  N / P     — next / previous file (package)
  RET       — visit file at point
  q         — quit window
  g         — refresh (re-run `package-review')

`outline-minor-mode' is enabled so each package is a collapsible section:
  TAB       — cycle visibility of section at point
  S-TAB     — cycle global visibility"
  :group 'package-review
  (setq-local outline-regexp "^\\*+ ")
  (outline-minor-mode 1)
  ;; diff-mode enables read-only via view-mode; keep it but allow our keys
  (setq buffer-read-only t))

;;; ── Buffer rendering ──────────────────────────────────────────────────────

(defun package-review--insert-heading (level label)
  "Insert an outline heading at LEVEL (number of stars) with LABEL."
  (insert (concat (make-string level ?*) " " label "\n")))

(defun package-review--render-entry (entry)
  "Insert the review section for one changed package ENTRY."
  (let* ((package    (plist-get entry :package))
         (pinned     (plist-get entry :pinned))
         (head       (plist-get entry :head))
         (repo-dir   (plist-get entry :repo-dir))
         (short-pin  (substring pinned 0 (min 12 (length pinned))))
         (short-head (substring head   0 (min 12 (length head))))
         (log        (package-review--git-log repo-dir pinned head))
         (diff       (when package-review-show-diff
                       (package-review--git-diff repo-dir pinned head))))
    (package-review--insert-heading
     1 (format "%s  (%s → %s)" package short-pin short-head))
    ;; Commit log
    (package-review--insert-heading 2 "Commits")
    (if (string-empty-p log)
        (insert "  (no commits found)\n")
      (dolist (line (split-string log "\n" t))
        (insert "  " line "\n")))
    ;; Unified diff
    (when package-review-show-diff
      (package-review--insert-heading 2 "Diff")
      (if (or (null diff) (string-empty-p (string-trim diff)))
          (insert "  (no diff in .el files)\n")
        ;; Insert raw diff and let diff-mode handle the rest
        (let ((diff-start (point)))
          (insert diff)
          ;; Ensure it ends with a newline
          (unless (eq (char-before) ?\n) (insert "\n"))
          (ignore diff-start))))))

(defun package-review--build-buffer (entries)
  "Populate a fresh review buffer with all ENTRIES."
  (with-current-buffer (get-buffer-create package-review-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Header
      (insert (format "Package Review — %s — %d package(s) changed\n"
                      (format-time-string "%F %T") (length entries)))
      ;; Each changed package
      (dolist (entry entries)
        (package-review--render-entry entry)))
    (package-review-mode)
    (goto-char (point-min))
    (current-buffer)))

;;; ── Public commands ───────────────────────────────────────────────────────

;;;###autoload
(defun package-review ()
  "Review all straight.el packages that have changed since the lockfile.

Compares the current HEAD of every managed git repository against the
commit pinned in the straight versions lockfile, and displays a unified
diff in a dedicated buffer for every package that differs."
  (interactive)
  (message "Scanning straight repos for changes…")
  (let ((entries (package-review--changed-packages)))
    (if (null entries)
        (message "No packages have changed since the lockfile — nothing to review.")
      (let ((buf (package-review--build-buffer entries)))
        (pop-to-buffer buf)
        (message "Reviewing %d changed package(s)." (length entries))))))

;;;###autoload
(defun package-review-package (package)
  "Review a single straight.el PACKAGE interactively.

Prompts with completion for a package whose HEAD differs from the lockfile."
  (interactive
   (let* ((pins (package-review--lockfile-pins))
          (candidates
           (let (acc)
             (straight--map-repos
              (lambda (recipe)
                (let* ((local-repo (plist-get recipe :local-repo))
                       (pkg        (plist-get recipe :package))
                       (pinned     (cdr (assoc local-repo pins)))
                       (head       (package-review--head-commit local-repo)))
                  (when (and pinned head
                             (not (string= pinned head))
                             (not (string-empty-p pinned))
                             (not (string-empty-p head)))
                    (push pkg acc)))))
             (nreverse acc))))
     (if (null candidates)
         (user-error "No changed packages found — nothing to review")
       (list (completing-read "Review package: " candidates nil t)))))
  (let* ((recipe     (gethash package straight--recipe-cache))
         (local-repo (plist-get recipe :local-repo))
         (pins       (package-review--lockfile-pins))
         (pinned     (cdr (assoc local-repo pins)))
         (head       (package-review--head-commit local-repo)))
    (cond
     ((null recipe)
      (user-error "Package %s is not known to straight.el" package))
     ((null pinned)
      (user-error "Package %s has no pinned commit in the lockfile" package))
     ((string= pinned head)
      (message "Package %s is already at the pinned commit (%s) — nothing to review."
               package (substring pinned 0 12)))
     (t
      (let ((buf (package-review--build-buffer
                  (list (list :package    package
                              :local-repo local-repo
                              :pinned     pinned
                              :head       head
                              :repo-dir   (straight--repos-dir local-repo))))))
        (pop-to-buffer buf)
        (message "Reviewing %s." package))))))

(provide 'package-review)
;;; package-review.el ends here
