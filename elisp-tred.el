(require 'treesit)

(defvar elisp-tred-mode)

(defvar elisp-tred-indent-size 2
  "Number of space characters to indent, for each successive level of the tree.")

(defvar-local elisp-tred--tree-buffer nil)

(define-minor-mode elisp-tred-mode
  "Minor mode for Elisp tree editing."
  :lighter "ET"
  (if elisp-tred-mode
      (elisp-tred--enable)
    (elisp-tred--kill-tree-buffer)))

(define-derived-mode elisp-tred--tree-mode special-mode
  "TM"
  "Mode for displaying lisp code as a tree."
  nil)

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

(defun elisp-tred--kill-tree-buffer ()
  (when (buffer-live-p elisp-tred--tree-buffer)
    (kill-buffer elisp-tred--tree-buffer)))

(defun elisp-tred--get-label (node)
  (let ((node-type (treesit-node-type node)))
    (concat (pcase node-type
              ("list" "(")
              ("symbol" (treesit-node-text node))
              (_ ""))
            (format " [%s]" node-type))))

(defun elisp-tred--get-label (node)
  (let ((node-type (treesit-node-type node)))
    (concat (pcase node-type
              ("list" "(")
              ("symbol" (treesit-node-text node))
              (")" ")"))
            (format " [%s]" node-type))))

(defun elisp-tred--get-tree-widget (node)
  `(tree-widget
    :node (push-button
           :tag ,(elisp-tred--get-label node)
           :button-face 'default
           :format "%[%t%]\n")
    :treesit-node ,node
    :expander elisp-tred--expander))

(defun elisp-tred--expander (widget)
  (let ((node (widget-get widget :treesit-node)))
    (mapcar 'elisp-tred--get-tree-widget
            (elisp-tred--get-children node))))

(defun elisp-tred-refresh-tree-buffer ()
  (interactive)
  (let ((root-node (treesit-buffer-root-node 'elisptred)))
    (with-current-buffer elisp-tred--tree-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
		(widget-create (elisp-tred--get-tree-widget root-node))))))

(defun elisp-tred--leaf-node-p (treesit-node)
  (equal (treesit-node-type treesit-node) "symbol"))

(defun elisp-tred--get-children (node)
  "Get children of NODE, filtering out parentheses and other structural elements."
  (seq-filter (lambda (child)
                (let ((type (treesit-node-type child)))
                  (not (member type '("(")))))
              (treesit-node-children node)))
