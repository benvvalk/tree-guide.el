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

    ;; If the first element of a list is a sequence (list or vector),
    ;; use a bare "(" for the expanded parent label, and show the
    ;; the first element (list or vector) on its own line.
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
    ;; Embedding the first element in the parent label (second
    ;; diagram) would prevent us from recursively expanding `(one)',
    ;; which could be an arbitrarily complex list.
    (:description "list where first element is a sequence (list or vector)"
     :capture-query ((list :anchor [(list) (vector)] @child (_) :* @child))
     :expanded-label-fn ,(lambda (captures) "("))

    ;; If the first element of a vector is a sequence (list or vector)
    ;; use a bare "[" for the expanded parent label, and show the
    ;; first element (list or vector) on its own line.
    ;;
    ;; See the previous rule for further explanation, since it is
    ;; very similar.
    (:description "vector where first element is a sequence (list or vector)"
     :capture-query ((vector :anchor [(list) (vector)] @child (_) :* @child))
     :expanded-label-fn ,(lambda (captures) "["))

    ;; If a list has two or more elements, show the first element as
    ;; part of the parent node label. For example, render the list
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
    ;; list (or a vector). But that case is handled by previous rules
    ;; above this one.
    (:description "a list with two or more elements"
     :capture-query ((list :anchor (_) @child (_) :+ @child))
     :capture-nodes-only t
     :expanded-label-fn ,(lambda (captures) (concat "(" (treesit-node-text (car captures))))
     :child-nodes-fn ,(lambda (captures) (cdr captures)))

    (:description "a list"
     :capture-query ((list) @node)
     :expanded-label-fn ,(lambda (captures) "("))

    (:description "default rule"
     :capture-query ((_) @node))

    )

  "A list of query-based rules for mapping the structure of the
tree-sitter parse tree to the structure of the interactive tree shown
in the elisp-tred buffer. Using a simple one-to-one mapping
(i.e. rendering the tree-sitter parse tree verbatim) is possible but
not that useful in practice, because it results in a lot of
intermediate nodes that make the tree tedious to navigate.

`elisp-tred--tree-mapping-rules' is a list of rules, where each rule
is a plist contained the following properties:

:description - A string that describes the rule. This is only for
making debugging easier.

:capture-query - A tree-sitter \"capture query\" that is passed to
`treesit-query-capture'. The rule matches if the query a non-empty
result.

:capture-nodes-only - This is passed as the NODES-ONLY argument of
`treesit-query-capture', and affects how the query results (i.e.
captured treesit nodes) are passed to the `:expanded-label-fn' and
`:child-nodes-fn' functions.

:expanded-label-fn - A function that is used to generate the label
text for the tree node when it is in expanded state.  Note that there
is no corresponding `:collapsed-label-fn' because the labels on
collapsed tree nodes are always the same -- they show the entire
elisp code for the subtree, collapsed to a single line.

:child-nodes-fn - A function that returns the treesit nodes for the
the child widgets of the current node, in the elisp tred tree.  This
function takes a single argument, which is output of the
`treesit-query-capture' function, i.e. the list of captured treesit
nodes.")

(defun elisp-tred--get-tree-mapping-rule (node)
  (catch 'break
    (dolist (rule elisp-tred--tree-mapping-rules)
	  (let* ((pos (treesit-node-start node))
                  (query (plist-get rule :capture-query))
                  (nodes-only (plist-get rule :capture-nodes-only))
                  (captures (treesit-query-capture node query pos (1+ pos) nodes-only)))
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
      (let* ((query (plist-get rule :capture-query))
             (nodes-only (plist-get rule :capture-nodes-only))
             (pos (treesit-node-start node))
             (captures (treesit-query-capture node query pos (1+ pos) nodes-only)))
        (funcall label-fn captures))
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
            (query (plist-get rule :capture-query))
			(nodes-only (plist-get rule :capture-nodes-only))
            (pos (treesit-node-start node))
            (captures (treesit-query-capture node query pos (1+ pos) nodes-only))
            (child-nodes-fn (plist-get rule :child-nodes-fn)))
      (funcall child-nodes-fn captures)
    (treesit-node-children node t)))

(defun elisp-tred--get-child-widgets (widget)
  "Get the widget definitions for the children of the given tree node
WIDGET.

This function is called when expanding a tree node in the UI."
  (let* ((node (widget-get widget :treesit-node))
         (child-nodes (elisp-tred--get-child-nodes node)))
    (mapcar 'elisp-tred--get-tree-widget child-nodes)))

(defun elisp-tred-show-tree-mapping-rule-at-point ()
  (interactive)
  (if-let* ((rule (elisp-tred--get-tree-mapping-rule-at-pos (point))))
      (message "matched: \"%s\"" (plist-get rule :description))
    (message "no match")))

(defun elisp-tred-show-tree-mapping ()
  "Show the result of applying tree mapping rules to the current position.

This command helps interactively test and develop tree mapping rules by:
1. Finding the treesit node at point
2. Attempting to match each rule in `elisp-tred--tree-mapping-rules'
3. Displaying the results in a buffer showing which rules matched and their output"
  (interactive)

  (elisp-tred--treesit-init)

  (when-let* ((node (elisp-tred--treesit-node-at (point))))
    ;; Try each rule against the node
    (let* ((results-buffer (get-buffer-create "*elisp-tred-mapping*"))
           (results '()))
      (dolist (rule elisp-tred--tree-mapping-rules)
        (let* ((description (plist-get rule :description))
               (query (plist-get rule :capture-query))
               (capture-nodes-only (plist-get rule :capture-nodes-only))
               (expanded-label (plist-get rule :expanded-label))
               (child-nodes-fn (plist-get rule :child-nodes-fn))
               (captures (treesit-query-capture node query (point) (1+ (point)) capture-nodes-only))
               (matched (not (null captures))))

          (push (list :description description
                      :matched matched
                      :captures captures
                      :capture-nodes-only capture-nodes-only
                      :expanded-label expanded-label
                      :child-nodes-fn child-nodes-fn)
                results)))

      ;; Display results
      (with-current-buffer results-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Tree Mapping Rules Test\n"))
          (insert (format "=======================\n\n"))
          (insert (format "Node at point: %s\n" (treesit-node-type node)))
          (insert (format "Node text: %s\n\n"
                          (elisp-tred--remove-newlines-and-collapse-spaces
                           (treesit-node-text node))))

          (dolist (result (reverse results))
            (let ((description (plist-get result :description))
                  (matched (plist-get result :matched))
                  (captures (plist-get result :captures))
                  (capture-nodes-only (plist-get result :capture-nodes-only))
                  (expanded-label (plist-get result :expanded-label))
                  (child-nodes-fn (plist-get result :child-nodes-fn)))

              (insert (format "Rule: %s\n" description))
              (insert (format "  Status: %s\n" (if matched "MATCHED" "did not match")))

              (when matched
                (insert (format "  Captures: %d\n" (length captures)))
                (dolist (capture captures)
                  (let* ((capture-name (unless capture-nodes-only (car capture)))
                         (capture-node (if capture-nodes-only capture (cdr capture)))
                         (capture-text (elisp-tred--remove-newlines-and-collapse-spaces
                                        (treesit-node-text capture-node))))
                    (if capture-nodes-only
                        (insert (format "    - %s\n" capture-text))
                      (insert (format "    - %s: %s\n" capture-name capture-text)))))

                (when expanded-label
                  (insert (format "  Label: %s\n" expanded-label)))

                (when child-nodes-fn
                  (let ((children (funcall child-nodes-fn captures)))
                    (insert (format "  Children: %d\n" (length children)))
                    (dolist (child children)
                      (let ((child-text (elisp-tred--remove-newlines-and-collapse-spaces
                                         (treesit-node-text child))))
                        (insert (format "    - %s\n" child-text)))))))

              (insert "\n")))

          (goto-char (point-min))
          (special-mode)))

      (display-buffer results-buffer))))

(provide 'elisp-tred)