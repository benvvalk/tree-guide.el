(require 'treesit)

(defvar-local elisp-tred-max-label-length 128
  "The maximum length of a tree node label. For the sake of
performance, labels longer than this length will be truncated with an
ellipsis (\"...\").

It is important to impose a max length on the tree node labels because
when a node is collapsed, it shows the full lisp code for its subtree
in a single line, which can be very long indeed.")

(defvar-keymap elisp-tred--tree-mode-map
  "TAB" #'elisp-tred-toggle-node
  "<backtab>" #'elisp-tred-collapse-parent)

(define-derived-mode elisp-tred--tree-mode special-mode
  "TM"
  "Mode for displaying lisp code as a tree."
  nil)

(defun elisp-tred--get-toplevel-node-with-same-start-pos (node)
  (let* ((start-pos (treesit-node-start node))
         (parent-node (treesit-node-parent node))
         (parent-type (treesit-node-type parent-node))
         (parent-pos (treesit-node-start parent-node)))
    (if (or (/= parent-pos start-pos)
            (equal parent-type "source_file"))
        node
      (elisp-tred--get-toplevel-node-with-same-start-pos parent-node))))

(defun elisp-tred--treesit-node-at (pos)
  "Return the node closest to root that starts exactly at POS.

This function is similar to `treesit-node-at', except that in the case
where there are multiple treesit nodes that start at POS, we return
the node that is closest to the root, rather than the leaf node. The
other difference from `treesit-node-at' is that we only return a node if
its starting position exactly matches POS, whereas `treesit-node-at'
will return a nearby leaf node if there isn't an exact match."
  (let* ((node (treesit-node-at pos))
         (node-pos (treesit-node-start node)))
    (when (eql node-pos pos)
      (elisp-tred--get-toplevel-node-with-same-start-pos node))))

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

(defvar elisp-tred--tree-mapping-rules
  `(

    ;; If the node is a sequence (list or vector), and the first
    ;; child is also a sequence (i.e. a list or vector), use a bare
    ;; "(" or "[" for the expanded parent label, and show the first
    ;; child on its own line.
    ;;
    ;; For example, render the list `((one) two three)' as;
    ;;
    ;; [-] (
    ;;  |-- (one)
    ;;  |-- two
    ;;  |-- three)
    ;;
    ;; rather than:
    ;;
    ;; [-] ((one)
    ;;  |-- two
    ;;  |-- three)
    ;;
    ;; We don't want to embed the first element in the parent label
    ;; (second diagram) because it prevents us from recursively
    ;; expanding the first child (`(one)' in the example above), which
    ;; could be an arbitrarily complex list.
    (:description "list where first element is a sequence (list or vector)"
     :capture-fn
     (lambda (node)
        (when (member (treesit-node-type node) '("list" "vector"))
          (when-let* ((child0 (treesit-node-child node 0 t))
                      (child0-type (treesit-node-type child0)))
            (when (member child0-type '("list" "vector"))
              (treesit-node-children node t)))))
     :expanded-label-fn
     (lambda (node captures)
        (pcase (treesit-node-type node)
          ("list" "(")
          ("vector" "[")
          (_ (error "capture-fn: unhandled case"))))
     )

    ;; If a sequence has two or more elements, show the first element
    ;; as part of the parent node label. For example, render the list
    ;; `(one two tree)' as:
    ;;
    ;; [-] (one
    ;;  |-- two
    ;;  |-- three)
    ;;
    ;; rather than:
    ;;
    ;; [-] (
    ;;  |-- one
    ;;  |-- two
    ;;  |-- tree)
    ;;
    ;; It's a matter of taste, but I find putting the opening paren
    ;; ("(") on its own line really wastes a lot of vertical space and
    ;; hurts readability.
    ;;
    ;; One exception is when the first element of the list is itself a
    ;; list (or a vector). But that case is handled by a previous rule
    ;; above this one.
    (:description "a sequence (list or vector) with two or more elements"
     :capture-fn
     (lambda (node)
       (when (and (member (treesit-node-type node) (list "list" "vector"))
                   (>= (treesit-node-child-count node t) 2))
              (treesit-node-children node t)))
     :expanded-label-fn
     (lambda (node captures)
        (let* ((child0 (car captures))
               (child0-text (treesit-node-text child0)))
          (pcase (treesit-node-type node)
            ("list" (concat "(" child0-text))
            ("vector" (concat "[" child0-text))
            (_ (error "capture-fn: unhandled case")))))
     :child-nodes-fn
     (lambda (node captures)
        (cdr captures)))

    (:description "a sequence (list or vector)"
     :capture-fn
     (lambda (node)
        (when (member (treesit-node-type node) (list "list" "vector"))
          (treesit-node-children node t)))
     :expanded-label-fn
     (lambda (node captures)
        (pcase (treesit-node-type node)
          ("list" "(")
          ("vector" "[")
          (_ (error "capture-fn: unhandled case")))))

    )

  "A list of rules for mapping the structure of the tree-sitter parse
tree to the structure of the elisp-tred tree. Generally speaking,
directly mapping the tree-sitter parse tree to the elisp-tred tree is
not very practical, because it results in a lot of unwanted
intermediate nodes. So we need to to reshape the parse tree in various
ways before we show it to the user.

 When we are building the elisp-tred from the tree-sitter parse tree,
we want two things as we visit each tree-sitter node: (1) What text
should we use to represent the current tree-sitter node in the
elisp-tred tree?, and (2) Which child nodes of the current tree-sitter
node should we process to generate the children in the elisp tred
tree?  The purpose of the tree-mapping rules is to answer these two
questions for the different types of nodes we encounter in the
tree-sitter parse tree. The `:expander-label-fn' (see below) answers
question (1), and the `:child-nodes-fn' answers question (2).

Each tree-mapping rule is a plist consisting of the following
properties:

:description' (optional) - An optional string that is used only for
debugging purposes and which describes the type of treesit node that
is matched by this rule (e.g. \"a list with 2 or more elements\").

`:capture-fn' (optional) - A function that is used to determine if a
given treesit node is a match for this tree-mapping rule (e.g. \"Is
it a list with 2 or more elements?\").  The `:capture-fn' function
takes a single argument, which is the treesit node to be tested. If
the `:capture-fn' determines that the treesit node is a match
(e.g. it is a list with 2 or more elements), the return value is the
list of \"captures\", i.e. a list of treesit nodes that will be
passed as an argument to the `:expanded-label-fn' and the
`:child-nodes-fn'.  In the case that a given treesit node does not
match this tree-mapping rule, `:capture-fn' should returns `nil' to
indicate a non-match. Specifying a `:capture-fn' is optional, and
defaults to a function that returns all named children of the given
treesit node (i.e. it makes the rule match any treesit node).

`:expanded-label-fn' (optional) - A function that is used to generate
the label text for the elisp-tred tree node when it is in expanded
state.  For example, a list node might show a \"(\" when it is
expanded, and the full list contents when it is collapsed.  The
`:expanded-label-fn' takes two arguments: (1) the root treesit NODE
that matched this tree-mapping rule, and (2) the CAPTURES list, which
is the list of treesit nodes returned by the `:capture-fn' (see
above). The `:expanded-label-fn' is optional and will default to just
returning the entire source code text corresponding the target treesit
node.  Side note: there is no `:collapsed-label-fn' that corresponds
to `:expanded-label-fn' because the labels for collapsed tree nodes
are always the same -- they show the entire elisp code for the
subtree, collapsed to a single line.

`:child-nodes-fn' (optional) - A function that returns the treesit nodes
for the the child widgets of the current node in the elisp-tred tree.
This function takes two arguments: (1) the root treesit NODE that
matched this tree-mapping rule, and (2) the CAPTURES list, which is
the list of treesit nodes returned by the `:capture-fn' (see
above). The `:child-nodes-fn' is optional and defaults to returning
all named children of the matched treesit node.")

(defun elisp-tred--get-tree-mapping-rule (node)
  (catch 'break
    (dolist (rule elisp-tred--tree-mapping-rules)
	  (let* ((capture-fn (plist-get rule :capture-fn))
             (captures (if capture-fn
                           (funcall capture-fn node)
                         (treesit-node-children node t))))
        (when captures
          (throw 'break rule))))))

(defun elisp-tred--get-tree-mapping-rule-at-pos (pos)
  (when-let* ((node (elisp-tred--treesit-node-at pos)))
    (elisp-tred--get-tree-mapping-rule node)))

(defun elisp-tred--get-expanded-label (node)
  "Return the text label for a treesit node (NODE) when
it is expanded."
  (if-let* ((rule (elisp-tred--get-tree-mapping-rule node))
            (label-fn (plist-get rule :expanded-label-fn)))
      (let* ((capture-fn (plist-get rule :capture-fn))
             (captures (if capture-fn
                           (funcall capture-fn node)
                         (treesit-node-children node t))))
        (funcall label-fn node captures))
    (treesit-node-text node)))

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

(defun elisp-tred--get-child-nodes (node)
  "Return the child nodes of NODE.

Note that this function doesn't not necessarily return the same thing
as `(treesit-node-children node)'. It implements custom rules for
filtering child nodes, as specified by
`elisp-tred--tree-mapping-rules'.
"
  (if-let* ((rule (elisp-tred--get-tree-mapping-rule node))
            (child-nodes-fn (plist-get rule :child-nodes-fn)))
      (let* ((capture-fn (plist-get rule :capture-fn))
             (captures (if capture-fn
                           (funcall capture-fn node)
                         (treesit-node-children node t))))
        (funcall child-nodes-fn node captures))
    (treesit-node-children node t)))

(defun elisp-tred--get-child-widgets (widget)
  "Get the widget definitions for the children of the given tree node
WIDGET.

This function is called when expanding a tree node in the UI."
  (let* ((node (widget-get widget :treesit-node))
         (child-nodes (elisp-tred--get-child-nodes node)))
    (mapcar 'elisp-tred--get-tree-widget child-nodes)))

(provide 'elisp-tred)