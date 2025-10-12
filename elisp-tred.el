(require 'treesit)

(defvar elisp-tred-mode)

(defvar elisp-tred-indent-size 2
  "Number of space characters to indent, for each successive level of the tree.")

(defvar-local elisp-tred--tree-buffer nil)

(defvar-local elisp-tred-max-label-length 128
  "The maximum length of a tree node label. For the sake of
performance, labels longer than this length will be truncated with an
ellipsis (\"...\").

It is important to impose a max length on the tree node labels because
when a node is collapsed, it shows the full lisp code for its subtree
in a single line, which can be very long indeed.")

(define-minor-mode elisp-tred-mode
  "Minor mode for Elisp tree editing."
  :lighter "ET"
  (if elisp-tred-mode
      (elisp-tred--enable)
    (elisp-tred--kill-tree-buffer)))

(defvar-keymap elisp-tred--tree-mode-map
  "TAB" #'elisp-tred-toggle-node
  "<backtab>" #'elisp-tred-collapse-parent)

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
                 (format "*elisp-tred: %s*"
                         (buffer-name))))
    (with-current-buffer elisp-tred--tree-buffer
      (elisp-tred--tree-mode)
      ;; Register a callback to update the labels of certain tree
      ;; nodes, when they are expanded or collapsed.
      (setq-local tree-widget-after-toggle-functions
                  '(elisp-tred--update-label))))
  (display-buffer elisp-tred--tree-buffer)
  (elisp-tred-refresh-tree-buffer))


(defun elisp-tred--get-toplevel-treesit-node (node)
  "Return the treesit node for the top-level form that contains the
given treesit node (NODE)."
  (let* ((parent-node (treesit-node-parent node))
         (parent-type (treesit-node-type parent-node)))
    (if (equal parent-type "source_file")
        node
      (elisp-tred--get-toplevel-treesit-node parent-node))))

(defun elisp-tred--get-toplevel-form-at-point ()
  "Return the treesit node for the top-level form that contains
POINT."
  (let* ((node (treesit-node-at (point))))
    (elisp-tred--get-toplevel-treesit-node node)))

(defun elisp-tred--treesit-init ()
  (unless (treesit-available-p)
    (user-error "Emacs was not built with tree-sitter support"))
  (unless (treesit-language-available-p 'elisptred)
    (user-error "Missing tree-sitter grammar for elisptred"))
  (unless (treesit-ready-p 'elisptred)
    (user-error "Failed to load treesit with elisptred grammar (buffer too large?)"))
  ;; Note: Passing `t' to `treesit-parser-create' forces Emacs to
  ;; recreate the parser from the latest tree-sitter grammar library
  ;; on disk (e.g. `~/.emacs.d/tree-sitter/libtree-sitter-elisptred.so' on
  ;; Linux). The default behaviour is to reuse the parser for the
  ;; buffer if it already exists, which caused me *a lot* of confusion
  ;; during development, because my grammar changes wouldn't take
  ;; effect until I restarted emacs.
  (treesit-parser-create 'elisptred (current-buffer) t))

(defun elisp-tred-jump-to-tree ()
  "Open elisp-tred buffer and show tree for current top-level elisp
form surrounding POINT."
  (interactive)
  (elisp-tred--treesit-init)
  (when-let* ((tree-buffer (get-buffer-create "*elisp-tred*"))
              (pos (point))
              (root-node (elisp-tred--get-toplevel-form-at-point)))
    (with-current-buffer tree-buffer
      (elisp-tred--tree-mode)
      ;; Register a callback to update the labels of certain tree
      ;; nodes, when they are expanded or collapsed.
      (setq-local tree-widget-after-toggle-functions
                  '(elisp-tred--update-label))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (widget-create (elisp-tred--get-tree-widget root-node))))
    ;; Default to displaying the tree buffer in the same window as the
    ;; elisp source buffer, unless the user overrides it in their
    ;; `display-buffer-alist'.
    (display-buffer tree-buffer '(display-buffer-same-window))))

(defun elisp-tred--kill-tree-buffer ()
  (when (buffer-live-p elisp-tred--tree-buffer)
    (kill-buffer elisp-tred--tree-buffer)))

(defun elisp-tred--get-icon-widget-for-current-line ()
  (save-excursion
    (beginning-of-line)
    (let ((line-number (line-number-at-pos (point))))
      (unless (widget-at (point)) (widget-forward 1))
      (when (and (widget-at (point))
                 (equal line-number (line-number-at-pos (point))))
		(widget-at (point))))))

(defun elisp-tred-toggle-node ()
  "Toggle the expanded/collapsed state of the tree node on the current
line."
  (interactive)
  (when-let* ((icon-widget (elisp-tred--get-icon-widget-for-current-line))
              (pos (widget-get icon-widget :from)))
    (widget-button-press pos)))

(defun elisp-tred-collapse-parent ()
  "Move up to the parent tree node (if any) and collapse it."
  (interactive)
  (when-let* ((icon-widget (elisp-tred--get-icon-widget-for-current-line))
              (tree-widget (widget-get icon-widget :parent))
              (parent-tree-widget (widget-get tree-widget :parent))
              (pos (widget-get parent-tree-widget :from)))
    (goto-char pos)
    (widget-button-press pos)))

(defun elisp-tred--update-label (widget)
  "Update the text label for the given tree node WIDGET.

This function allows showing different labels on a tree node,
depending on whether the node is collapsed or expanded."
  (let* ((node (widget-get widget :treesit-node))
         (node-type (treesit-node-type node)))
    (when t ;(eq node-type "list")
      (let* ((button (widget-get widget :node))
             (open (widget-get widget :open))
             (new-label (if open
                            (elisp-tred--get-expanded-label node)
                          (elisp-tred--get-collapsed-label node))))
        (widget-put button :tag new-label)
        ;; HACK: I don't understand what the line below does, but it's
        ;; necessary in order for the tree widget label to be updated.
        (widget-value-set widget open)))))

(defun elisp-tred--remove-newlines-and-collapse-spaces (str)
  "Remove all newlines and collapse duplicate spaces in STR."
  (let ((no-newlines (replace-regexp-in-string "\n" " " str)))
    (replace-regexp-in-string "\\s-+" " " no-newlines)))

(defun elisp-tred--get-text-for-child-range (node index count)
  "Get the source code text that corresponds to COUNT
consecutive child nodes of treesit NODE, starting at child index
INDEX."
  (let* ((children (treesit-node-children node))
         (index (min index (1- (length children))))
         (count (min count (length children)))
         (start-child (nth index children))
         (end-child (nth (1- (+ index count)) children))
         (start-pos (treesit-node-start start-child))
         (end-pos (treesit-node-end end-child)))
    (with-current-buffer (treesit-node-buffer node)
      (elisp-tred--remove-newlines-and-collapse-spaces
       (buffer-substring start-pos end-pos)))))

(defun elisp-tred--get-num-children-in-expanded-label (node)
  "Get the number of initial children that should be included within
the label for treesit node NODE, instead of being created as separate
child node widgets.

As a simple example, let us suppose that NODE correponds to the list
`(one two three)'.

If this function returns 0, it means the subtree for NODE should be
rendered as follows:

[-] (
 |- one
 |- two
 |- three)

On the other, if this function returns 1, it means the subtree for
NODE should be rendered as:

[-] (one
 |- two
 |- three)

The second option above is generally more readable and concise, but in
the case where the first child of NODE is a list (e.g. `((one) two
three)'), then option 1 is more readable (in the author's opinion)."
  (when-let* ((node-type (treesit-node-type node))
              ;; Note: We skip child 0 because it is a "(" literal.
              (child (nth 1 (treesit-node-children node)))
              (child-type (treesit-node-type child)))
    (if (equal child-type "list") 0 1)))

(defun elisp-tred--get-expanded-label (node)
  "Return the text label for a treesit node (NODE) when
it is expanded."
  (let ((num-children-in-label (elisp-tred--get-num-children-in-expanded-label node)))
    (concat (elisp-tred--get-text-for-child-range node 0 (1+ num-children-in-label))
            ;; (format " [%s]" node-type)
            )))

(defun elisp-tred--calc-number-of-closing-parens (node)
  "Calculate the number of closing parens (`)') that we need
to append to the label for treesit node NODE, in order to balance
open parens (`(') in parent and ancestor nodes.

Note that it is only necessary to append closing parens if If NODE is
the last child of it's parent node (and so on recursively up the
tree). If NODE is not the last child of its parent, we always return
0."
  (if (elisp-tred--is-last-child node)
      (if-let* ((parent (treesit-node-parent node)))
          (1+ (elisp-tred--calc-number-of-closing-parens parent))
        1)
      0))

(defun elisp-tred--get-collapsed-label (node)
  "Return the text label for a treesit node (NODE) when
it is collapsed."
  (let* ((label (treesit-node-text node))
         (truncated (> (length label) elisp-tred-max-label-length))
         (label (if truncated (substring label 0 elisp-tred-max-label-length) label))
         (label (if truncated (concat label "...") label))
         (label (elisp-tred--remove-newlines-and-collapse-spaces label))
         (num-closing-parens (elisp-tred--calc-number-of-closing-parens node)))
    (concat label
            (make-string num-closing-parens ?\))
            ;; (format " [%s]" (treesit-node-type node))
            )))

(defun elisp-tred--get-tree-widget (node)
  "Return the tree widget definition corresponding to treesit node
NODE.

The tree widget definition is used render the treesit nodes as
collapsible UI widget in the tree buffer."
  (if (eql (treesit-node-child-count node) 0)
	  `(item :tag ,(elisp-tred--get-collapsed-label node))
    `(tree-widget
      :node (item :tag ,(elisp-tred--get-collapsed-label node))
      :treesit-node ,node
      ;; Below, we explicitly set the keymaps for the tree icon widgets,
      ;; so that they are the same as the default keymap for the mode
      ;; (i.e. `elisp-tred--tree-mode-map').
      ;;
      ;; This ensures that the keybindings work consistently, regardless
      ;; of where the cursor happens to be positioned on the current
      ;; line.
      ;;
      ;; For example, I want to ensure that the TAB key always works to
      ;; toggle the expanded/collapsed state of the node on the current
      ;; line.
      :open-icon (tree-widget-open-icon :keymap elisp-tred--tree-mode-map)
      :close-icon (tree-widget-close-icon :keymap elisp-tred--tree-mode-map)
      :empty-icon (tree-widget-empty-icon :keymap elisp-tred--tree-mode-map)
      :leaf-icon (tree-widget-leaf-icon :glyph-name "handle" :keymap elisp-tred--tree-mode-map)
      :expander elisp-tred--get-child-widgets)))

(defun elisp-tred--is-last-child (node)
  (when-let* ((parent (treesit-node-parent node))
              (num-children (treesit-node-child-count parent))
              (child-index (treesit-node-index node)))
    ;; Note: We're subtracting 2 here because the true
    ;; last child is the literal closing paren `)'.
    (eql child-index (- num-children 2))))

(defun elisp-tred--get-child-widgets (widget)
  "Get the widget definitions for the children of the given tree node
WIDGET.

This function is called when expanding a tree node in the UI."
  (let* ((node (widget-get widget :treesit-node))
         (num-children-in-label (elisp-tred--get-num-children-in-expanded-label node)))
    (mapcar 'elisp-tred--get-tree-widget
            ;; skip children that already shown within label of parent node
            (nthcdr num-children-in-label (elisp-tred--get-children node)))))

(defun elisp-tred-refresh-tree-buffer ()
  (interactive)
  (let ((root-node (treesit-buffer-root-node 'elisptred)))
    (with-current-buffer elisp-tred--tree-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
		(widget-create (elisp-tred--get-tree-widget root-node))))))

(defun elisp-tred--get-children (node)
  "Get children of NODE, filtering out parentheses and other structural elements."
  (seq-filter (lambda (child)
                (let ((type (treesit-node-type child)))
                  (not (member type '("(" ")")))))
              (treesit-node-children node)))

(provide 'elisp-tred)