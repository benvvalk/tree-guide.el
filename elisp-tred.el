(require 'treesit)

(defvar elisp-tred-mode)

(defvar elisp-tred-indent-size 2
  "Number of space characters to indent, for each successive level of the tree.")

(defvar-local elisp-tred--tree-buffer nil)

(define-derived-mode elisp-tred--tree-mode special-mode
  "TM"
  "Mode for displaying lisp code as a tree."
  nil)

(defun elisp-tred--kill-tree-buffer ()
  (when (buffer-live-p elisp-tred--tree-buffer)
    (kill-buffer elisp-tred--tree-buffer)))

(defun elisp-tred--leaf-node-p (treesit-node)
  (equal (treesit-node-type treesit-node) "symbol"))

(defun elisp-tred--insert-treesit-node (treesit-node indent-by)
  (when (<= indent-by 3) ;; dev plan: test/debug a single level, then two levels, then all levels
    (let* ((indent-size (* indent-by elisp-tred-indent-size))
           (indent-string (make-string indent-size ?\s)))
      (insert indent-string)
      (if (elisp-tred--leaf-node-p treesit-node)
          (insert (concat (treesit-node-text treesit-node) "\n"))
        (let* ((children (treesit-node-children treesit-node t))
               (first-child (car children))
               (tail-children (cdr children)))
          (insert "(")
          (if first-child
              (if (elisp-tred--leaf-node-p first-child)
                  (elisp-tred--insert-treesit-node first-child 0)
                (insert "\n")
                (elisp-tred--insert-treesit-node first-child (1+ indent-by)))
            (insert ")\n"))
          (dolist (child tail-children)
            (elisp-tred--insert-treesit-node child (1+ indent-by))))))))

(defun elisp-tred-refresh-tree-buffer ()
  (interactive)
  (let ((root-node (treesit-buffer-root-node 'elisptred)))
    (with-current-buffer elisp-tred--tree-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (toplevel-list-node (treesit-node-children root-node))
          (elisp-tred--insert-treesit-node toplevel-list-node 0))))))

(defun elisp-tred--enable ()
  (message "enabling elisp-tred")
  (unless (treesit-ready-p 'elisptred)
	(if (not (treesit-language-available-p 'elisptred))
        (user-error "Cannot find treesit grammar for elisptred")
	  (treesit-create-parser 'elisptred)))
  (unless (buffer-live-p elisp-tred--tree-buffer)
    (setq-local elisp-tred--tree-buffer
                (get-buffer-create
                 (format "*elisp-tred buffer for %s"
                         (buffer-name))))
    (with-current-buffer elisp-tred--tree-buffer
      (elisp-tred--tree-mode)))
  (display-buffer elisp-tred--tree-buffer)
  (elisp-tred-refresh-tree-buffer))

(define-minor-mode elisp-tred-mode
  "Minor mode for Elisp tree editing."
  :lighter "ET"
  (if elisp-tred-mode
      (elisp-tred--enable)
    (elisp-tred--kill-tree-buffer)))
