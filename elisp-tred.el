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
      (elisp-tred--tree-mode)
      ;; Register a callback to update the labels of certain tree
      ;; nodes, when they are expanded or collapsed.
      (setq-local tree-widget-after-toggle-functions
                  '(elisp-tred--update-label))))
  (display-buffer elisp-tred--tree-buffer)
  (elisp-tred-refresh-tree-buffer))

(defun elisp-tred--kill-tree-buffer ()
  (when (buffer-live-p elisp-tred--tree-buffer)
    (kill-buffer elisp-tred--tree-buffer)))

(defun elisp-tred--update-label (widget)
  "Update the text label for the given tree node WIDGET.

This function allows showing different labels on a tree node,
depending on whether the node is collapsed or expanded."
  (let* ((node (widget-get widget :treesit-node))
         (node-type (treesit-node-type node)))
    (when t ;(eq node-type "list")
      (let* ((button (widget-get widget :node))
             (open (widget-get widget :open))
             (new-label (if open "expanded!" "collapsed")))
        (widget-put button :tag new-label)))))

(defun elisp-tred--get-label (node)
  (let ((node-type (treesit-node-type node)))
    (concat (pcase node-type
              ("symbol" (treesit-node-text node))
              (")" ")"))
            (format " [%s]" node-type))))

(defun elisp-tred--get-tree-widget (node)
  "Return the tree widget definition corresponding to treesit node
NODE.

The tree widget definition is used render the treesit nodes as
collapsible UI widget in the tree buffer."
  `(tree-widget
    :node (push-button
           :tag ,(elisp-tred--get-label node)
           :button-face default
           :format "%[%t%]\n")
    :treesit-node ,node
    :expander elisp-tred--get-child-widgets))

(defun elisp-tred--get-child-widgets (widget)
  "Get the widget definitions for the children of the given tree node
WIDGET.

This function is called when expanding a tree node in the UI."
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
