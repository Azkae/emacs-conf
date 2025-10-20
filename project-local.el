(defvar conf--project-local-identifier ".project"
  "Filename(s) that identifies a directory as a project.
You can specify a single filename or a list of names.")

(defun conf--project-try-local (dir)
  "Determine if DIR is a non-VC project.
DIR must include a .project file to be considered a project."
  (if-let ((root (if (listp conf--project-local-identifier)
                     (seq-some (lambda (n)
                                 (locate-dominating-file dir n))
                               conf--project-local-identifier)
                   (locate-dominating-file dir conf--project-local-identifier))))
      (cons 'local root)))

(cl-defmethod project-root ((project (head local)))
  "Return root directory of current PROJECT."
  (cdr project))

(cl-defmethod project-ignores ((project (head local)) dir)
  "Return list of ignore patterns for the local PROJECT in DIR."
  (let ((default-ignores (cl-call-next-method project dir)))
    (append default-ignores
            (if (listp conf--project-local-identifier)
                conf--project-local-identifier
              (list conf--project-local-identifier)))))

(provide 'project-local)
