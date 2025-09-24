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

(defun elisp-tred--leaf-node-p (treesit-node)
  (equal (treesit-node-type treesit-node) "symbol"))

(defun elisp-tred--get-relevant-children (node)
  "Get children of NODE, filtering out parentheses and other structural elements."
  (seq-filter (lambda (child)
                (let ((type (treesit-node-type child)))
                  (not (member type '("(" ")" "\n" " ")))))
              (treesit-node-children node)))

(defun elisp-tred--render-tree (node &optional prefix is-last)
  "Render NODE as a tree with PREFIX and connection status IS-LAST."
  (let* ((children (elisp-tred--get-relevant-children node))
         (is-leaf (elisp-tred--leaf-node-p node))
         (connector (cond
                     ((null prefix) "╭─")
                     (is-last "╰─")
                     (t "├─")))
         (child-prefix (concat prefix (cond
                                       ((null prefix) "")
                                       (is-last "  ")
                                       (t "│ ")))))

    (cond
     ;; For leaf nodes (symbols), just render the symbol
     (is-leaf
      (when prefix (insert prefix))
      (insert connector " " (treesit-node-text node t) "\n"))

     ;; For list nodes, don't render the list text, just process children
     ((> (length children) 0)
      (when prefix (insert prefix))
      (insert connector)
      ;; Handle list structure properly
      (let ((first-child (car children))
            (rest-children (cdr children)))
        (cond
         ;; Single leaf child - show inline without branching
         ((and (= (length children) 1) (elisp-tred--leaf-node-p first-child))
          (insert " " (treesit-node-text first-child t) "\n"))

         ;; First child is leaf with other children - show with branching (except for root)
         ((elisp-tred--leaf-node-p first-child)
          (if (null prefix)
              ;; Root level - show function name inline
              (progn
                (insert " " (treesit-node-text first-child t) "\n")
                (when rest-children
                  (let ((child-count (length rest-children)))
                    (dotimes (i child-count)
                      (let ((child (nth i rest-children))
                            (is-last-child (= i (1- child-count))))
                        (elisp-tred--render-tree child child-prefix is-last-child))))))
            ;; Non-root level - show with branching
            (progn
              (insert "┬─ " (treesit-node-text first-child t) "\n")
              (when rest-children
                (let ((child-count (length rest-children)))
                  (dotimes (i child-count)
                    (let ((child (nth i rest-children))
                          (is-last-child (= i (1- child-count))))
                      (elisp-tred--render-tree child child-prefix is-last-child))))))))

         ;; First child is not a leaf - full branching
         (t
          (insert "┬─\n")
          (let ((child-count (length children)))
            (dotimes (i child-count)
              (let ((child (nth i children))
                    (is-last-child (= i (1- child-count))))
                (elisp-tred--render-tree child child-prefix is-last-child)))))))))))

(defun elisp-tred-refresh-tree-buffer ()
  (interactive)
  (let ((root-node (treesit-buffer-root-node 'elisptred)))
    (with-current-buffer elisp-tred--tree-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((top-level-forms (elisp-tred--get-relevant-children root-node)))
          (dotimes (i (length top-level-forms))
            (let ((form (nth i top-level-forms))
                  (is-last (= i (1- (length top-level-forms)))))
              (elisp-tred--render-tree form nil is-last))))))))
