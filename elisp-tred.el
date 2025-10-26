(require 'treesit)

(defvar-local elisp-tred-max-label-length 128
  "The maximum length of a tree node label. For the sake of
performance, labels longer than this length will be truncated with an
ellipsis (\"...\").

It is important to impose a max length on the tree node labels because
when a node is collapsed, it shows the full lisp code for its subtree
in a single line, which can be very long indeed.")

(defvar-keymap elisp-tred-mode-map
  "TAB" #'elisp-tred-toggle-node
  "<backtab>" #'elisp-tred-collapse-parent)

(define-derived-mode elisp-tred-mode special-mode
  "TM"
  "Mode for displaying lisp code as a tree."
  (setq-local
   ;; Remove default space between tree node icon and
   ;; label. For lists/vectors, we use the opening
   ;; bracket "("/"[" for the tree node icon, it looks
   ;; weird to introduce a space before the list/vector
   ;; contents.
   tree-widget-space-width 0))

(define-widget 'elisp-tred-node-label 'item
  "A custom widget that is used for the tree node labels.

This is widget is the same as an `item' widget, except that I add a
`button' overlay so that `widget-forward' will jump to it. (By
default, `widget-forward' only jumps to interactive widgets such as
buttons and editable fields.)"
  :create 'elisp-tred--node-label-create)

(defun elisp-tred--node-label-create (widget)
  "Create an item widget with a `button' overlay, so that
`widget-forward' will jump to it. (By default, `widget-forward' only
jumps to interactive widgets such as buttons and editable fields.)"
  (widget-default-create widget)
  ;; Add a `button' overlay so we can use `widget-forward' to jump to
  ;; our tree node labels. For example, I use `widget-forward' to
  ;; implement `elisp-tred--node-widget-for-current-line'.
  ;;
  ;; Normally, `widget-forward' advances the cursor to the next
  ;; *interactive* widget in the buffer (i.e. a button or a field).
  (when-let* ((from (widget-get widget :from))
              (to (widget-get widget :to)))
    (let ((overlay (make-overlay from to)))
      (overlay-put overlay 'button widget)
      (overlay-put overlay 'evaporate t))))

(define-widget 'elisp-tred-empty-icon 'tree-widget-icon
  "Empty (zero-width) icon widget.

By default, tree-widget.el renders diamond-shaped button widgets for
each tree node, which can be mouse-clicked to expand/collapse the
nodes. I like the look/feel of the tree much better without the button
widgets, so I use this zero-width icon to hide them."
  :tag "")

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
      (elisp-tred-mode)
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

(defun elisp-tred--node-widget-for-current-line ()
  (save-excursion
    (beginning-of-line)
    (let* ((line-number (line-number-at-pos (point)))
           (widget (widget-at)))
      ;; Find the last widget on the current line.
      ;; If we are rendering an icon/button widget for each tree node,
      ;; that widget will appear before (to the left of) the node
      ;; widget.
      ;;
      ;; Note `widget-forward' throws an error if there are no more
      ;; widgets in the buffer, so we surround the `widget-forward'
      ;; calls with `condition-case' and fall through on the first
      ;; error.
      (condition-case nil
          (progn
            (widget-forward 1)
            (while (and (widget-at) (= line-number (line-number-at-pos)))
              (setq widget (widget-at))
              (widget-forward 1)))
        (error)) ; if widget-forward fails, fall through
      widget)))

(defun elisp-tred--tree-widget-for-current-line ()
  (when-let* ((node-widget (elisp-tred--node-widget-for-current-line)))
    ;; For internal tree-widget nodes, `:parent' points to a tree
    ;; widget for the current line. For leaf nodes, `:parent' points to
    ;; the tree widget for the parent in the tree, which is on a
    ;; different line. Pretty confusing!
    (unless (widget-get node-widget :leaf-p)
      (widget-get node-widget :parent))))

(defun elisp-tred-toggle-node ()
  "Toggle the expanded/collapsed state of the tree node on the current line."
  (interactive)
  (when-let* ((tree-widget (elisp-tred--tree-widget-for-current-line))
              (label-widget (widget-get tree-widget :node))
              (treesit-node (elisp-tred--treesit-node tree-widget)))
    (let* ((open (widget-get tree-widget :open))
           (new-label (if open
                          (elisp-tred--get-collapsed-label treesit-node)
                        (elisp-tred--get-expanded-label treesit-node))))
      ;; Update the tree node label
      (widget-put label-widget :tag new-label)
      ;; Toggle the :open property
      (widget-put tree-widget :open (not open))
      ;; Redraw the widget
      (widget-apply tree-widget :value-set (not open)))))

(defun elisp-tred-collapse-parent ()
  "Move up to the parent tree node (if any) and collapse it."
  (interactive)
  (when-let* ((icon-widget (elisp-tred--get-icon-widget-for-current-line))
              (tree-widget (widget-get icon-widget :parent))
              (parent-tree-widget (widget-get tree-widget :parent))
              (pos (widget-get parent-tree-widget :from)))
    (goto-char pos)
    (widget-button-press pos)))

(defun elisp-tred--treesit-node (widget)
  "Return the treesit node corresponding to WIDGET.

WIDGET can be any one of the following:

* the label widget for a tree node (i.e. the `:node' widget)
* the icon/button widget for a tree node
* the main tree widget for a tree node, which is the common
  `:parent' of the icon and label widgets."
  (if-let* ((treesit-node (widget-get widget :treesit-node)))
      ;; WIDGET is the label widget for the tree node (i.e. the
      ;; `:node' widget)
      treesit-node
    ;; WIDGET is either the main tree widget or the icon/button
    ;; widget. In both cases, the widget has a `:node' property that
    ;; points to the label widget for the tree node.
    (when-let* ((node-widget (widget-get widget :node)))
      (widget-get node-widget :treesit-node))))

(defun elisp-tred--remove-newlines-and-collapse-spaces (str)
  "Remove all newlines and collapse duplicate spaces in STR."
  (let ((no-newlines (replace-regexp-in-string "\n" " " str)))
    (replace-regexp-in-string "\\s-+" " " no-newlines)))

(defun elisp-tred--quoted-p (node)
  "Return `t' if NODE is treesit node for a quoted form (e.g. a quoted
list), or `nil' otherwise."
  (equal (treesit-node-type node) "quote"))

(defun elisp-tred--unquote (node)
  "If NODE is treesit node for a quoted form (e.g. a quoted list),
return the child treesit node for the unquoted form. Otherwise return
NODE unmodified."
  (if (elisp-tred--quoted-p node)
      (treesit-node-child node 0 t)
    node))

(defun elisp-tred--sequence-p (node)
  "If NODE is a treesit node for a sequence (list or vector) or a quoted
sequence, return `t'. Otherwise return `nil'."
  (member (treesit-node-type (elisp-tred--unquote node)) '("list" "vector")))

(defun elisp-tred--sequence-length (node)
  "If NODE is a treesit node for a sequence (list or vector) or a
quoted sequence, return the number of elements in the
sequence. Otherwise return `nil'."
  (when (elisp-tred--sequence-p node)
    (treesit-node-child-count (elisp-tred--unquote node) t)))

(defun elisp-tred--sequence-children (node)
  "If NODE is a treesit node for a sequence (list or vector) or a
quoted sequence, return the treesit nodes for elements of the
sequence. Otherwise, return `nil'."
  (when (elisp-tred--sequence-p node)
    (treesit-node-children (elisp-tred--unquote node) t)))

(defun elisp-tred--sequence-car (node)
  "If NODE is a treesit node for a sequence (list or vector) or a
quoted sequence, return the treesit node for the first element
of the sequence. Otherwise, return `nil'."
  (car (elisp-tred--sequence-children node)))

(defun elisp-tred--sequence-cdr (node)
  "If NODE is a treesit node for a sequence (list or vector) or a
quoted sequence, return the treesit nodes for all elements
of the sequence except the first. Otherwise, return `nil'."
  (cdr (elisp-tred--sequence-children node)))

(defun elisp-tred--left-bracket-char (node)
  "If NODE is a treesit node for a sequence (list or vector) or a
quoted sequence, return a string containing the left (opening) bracket character.

For lists and quoted lists, the return value is \"(\".

For vectors and quoted vectors, the return value is \"[\"."
  (when (elisp-tred--sequence-p node)
    (if (elisp-tred--quoted-p node)
        (elisp-tred--left-bracket-char (elisp-tred--unquote node))
      (pcase (treesit-node-type node)
        ("list" "(")
        ("vector" "[")
        (_ (error "unhandled case"))))))

(defun elisp-tred--right-bracket-char (node)
  "If NODE is a treesit node for a sequence (list or vector) or a
quoted sequence, return a string containing the right (closing) bracket character.

For lists and quoted lists, the return value is \")\".

For vectors and quoted vectors, the return value is \"]\"."
  (when (elisp-tred--sequence-p node)
    (if (elisp-tred--quoted-p node)
        (elisp-tred--right-bracket-char (elisp-tred--unquote node))
      (pcase (treesit-node-type node)
        ("list" ")")
        ("vector" "]")
        (_ (error "unhandled case"))))))

(defun elisp-tred--quote-char (node)
  "If NODE is a treesit node for a quoted form (e.g. a quoted list),
return the quote character, which will be one of: \"'\", \"`\", or \"#'\".

If NODE is not a treesit node for a quoted form, return `nil'."
  (when (elisp-tred--quoted-p node)
    (treesit-node-text (treesit-node-child node 0))))

(defvar elisp-tred--tree-mapping-rules
  `(
    ;; If a sequence (list or vector) has two or more elements, and
    ;; the first element is not a sequence or a comment, show the
    ;; first element as part of the parent node label rather than as
    ;; it's own child element.
    ;;
    ;; For example, render the list `(one two tree)' as:
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
    ;; ("(") on its own line wastes too much vertical space and
    ;; hurts readability of the code.
    (:description "a sequence (list or vector) with two or more elements,
 and the first element is not a sequence or a comment"

     :match-fn
     (lambda (node)
	   (when (and (elisp-tred--sequence-p node)
                  (>= (elisp-tred--sequence-length node) 2))
         (let* ((child0 (elisp-tred--sequence-car node))
                (child0-type (treesit-node-type child0)))
           (and (not (elisp-tred--sequence-p child0))
                (not (equal child0-type "comment"))))))

     :expanded-label-fn
     (lambda (node)
       (let* ((quote-char (elisp-tred--quote-char node)))
         (when-let* ((left-bracket (elisp-tred--left-bracket-char node))
                     (child0 (elisp-tred--sequence-car node))
                     (child0-text (treesit-node-text child0)))
           (concat quote-char
                   left-bracket
                   child0-text))))

     :child-nodes-fn
     (lambda (node)
       (elisp-tred--sequence-cdr node)))

    (:description "a sequence (list or vector) or quoted sequence"
     :match-fn
     (lambda (node)
       (elisp-tred--sequence-p node))
     :expanded-label-fn
     (lambda (node)
       (let* ((quote-char (elisp-tred--quote-char node)))
         (when-let* ((left-bracket (elisp-tred--left-bracket-char node)))
           (concat quote-char left-bracket)))))


    )

  "A list of rules for mapping the structure of the tree-sitter parse
tree to the structure of the elisp-tred tree. Generally speaking,
directly mapping the tree-sitter parse tree to the elisp-tred tree is
not very practical, because it results in a lot of unwanted
intermediate nodes. So we need to to reshape the parse tree in various
ways before we show it to the user.

When we are building the elisp-tred from the tree-sitter parse tree,
we need to know two things as we visit each tree-sitter node: (1) What
text should we use to represent the current tree-sitter node in the
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

`:match-fn' (required) - A function that is used to determine if a
given treesit node is a match for this tree-mapping rule (e.g. \"Is
it a list with 2 or more elements?\"). The `:match-fn' function
takes a single argument, which is the treesit node to be tested. If
the `:match-fn' determines that the treesit node is a match
(e.g. it is a list with 2 or more elements) it should return
a non-nil value. `:match-fn' is the only required property
for a tree-mapping rule.

`:expanded-label-fn' (optional) - A function that is used to generate
the label text for the elisp-tred tree node when it is in expanded
state. For example, a elisp-tred node might show a \"(\" when it is
expanded, and the full list contents when it is collapsed. The
`:expanded-label-fn' takes a single argument NODE, which is a treesit
node that matched this tree mapping rule (as determined by
`:match-fn'). The `:expanded-label-fn' is optional and will default to
just returning the entire source code text corresponding to NODE.
Side note: There is no `:collapsed-label-fn' that corresponds to
`:expanded-label-fn' because the labels for collapsed elisp-tred nodes
are always follow the same rule -- they show the entire elisp code for
the subtree, collapsed to a single line.

`:child-nodes-fn' (optional) - A function that returns the treesit nodes
for the the child widgets of the current node in the elisp-tred tree.
The `:expanded-label-fn' takes a single argument NODE, which is a treesit
node that matched this tree mapping rule (as determined by
`:match-fn'). The `:child-nodes-fn' is optional and defaults to returning
all named children of NODE.")

(defun elisp-tred--get-tree-mapping-rule (node)
  (catch 'break
    (dolist (rule elisp-tred--tree-mapping-rules)
	  (when-let* ((match-fn (plist-get rule :match-fn))
                  (match-p (funcall match-fn node)))
        (throw 'break rule)))))

(defun elisp-tred--get-tree-mapping-rule-at-pos (pos)
  (when-let* ((node (elisp-tred--treesit-node-at pos)))
    (elisp-tred--get-tree-mapping-rule node)))

(defun elisp-tred--get-expanded-label (node)
  "Return the text label for a treesit node (NODE) when
it is expanded."
  (if-let* ((rule (elisp-tred--get-tree-mapping-rule node))
            (label-fn (plist-get rule :expanded-label-fn)))
      (funcall label-fn node)
    ""))

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

(defun elisp-tred--get-collapsed-label-for-sequence (node)
  "Given a treesit node NODE for a sequence (list or vector), return
the label for the node in its collapsed state, i.e. when its children
are hidden in the elisp-tred tree.

The collapsed label for a sequence node is its elisp source code text,
with newlines and consecutive spaces removed, and truncated to a
maximum length of `elisp-tred-max-label-length' (for performance
reasons). In the case of truncation, an ellipsis (\"...\") will be
used to represent the omitted forms. When truncating, we remove entire
balanced forms, rather than simply chopping the string at the maximum
character length, to ensure that parentheses remain balanced in the
elisp-tred buffer."
  ;; Implementation note: You may be wondering why we have
  ;; `right-bracket' here but no `left-bracket'. The reason is that we
  ;; use the left bracket as the icon for the tree node (see
  ;; `elisp-tred--get-tree-widget').
  (let* ((quote-char (elisp-tred--quote-char node))
         (left-bracket (elisp-tred--left-bracket-char node))
         (right-bracket (elisp-tred--right-bracket-char node))
         (children (elisp-tred--sequence-children node))
         (max-len elisp-tred-max-label-length))
    (concat
     quote-char
     left-bracket
     (catch 'truncated
       (seq-reduce
        (lambda (acc child)
          ;; Omit comments (strings starting with `;') from the
          ;; collapsed elisp code.  Since comments extend to the end
          ;; of their line, it doesn't make sense to embed them in the
          ;; label.
          (if (equal (treesit-node-type child) "comment")
              acc
            (let* ((text (treesit-node-text child))
                   (collapsed (elisp-tred--remove-newlines-and-collapse-spaces text))
                   (sep (if (string-empty-p acc) "" " "))
                   (new-acc (concat acc sep collapsed)))
              (if (> (length new-acc) max-len)
                  (throw 'truncated (concat acc "..."))
                new-acc))))
        children
        ""))
     right-bracket)))

(defun elisp-tred--get-collapsed-label (node)
  "Return the text label for a treesit node (NODE) when
it is collapsed."
  (let* ((label (if (elisp-tred--sequence-p node)
                    (elisp-tred--get-collapsed-label-for-sequence node)
                  (elisp-tred--remove-newlines-and-collapse-spaces
                   (treesit-node-text node))))
         (num-closing-parens (elisp-tred--calc-number-of-closing-parens node)))
    (concat label
            (make-string num-closing-parens ?\))
            ;; (format " [%s]" (treesit-node-type node))
            )))

(defun elisp-tred--leaf-p (node)
  "Return t if treesit NODE should be rendered as a leaf in the
elisp-tred tree."
  (let ((unquoted (elisp-tred--unquote node)))
    (eq (treesit-node-child-count unquoted) 0)))

(defun elisp-tred--get-tree-widget (node)
  "Return the tree widget definition corresponding to treesit node
NODE.

The tree widget definition is used render the treesit nodes as
collapsible UI widget in the tree buffer."
  (if (elisp-tred--leaf-p node)
	  `(elisp-tred-node-label
        :tag ,(elisp-tred--get-collapsed-label node)
        :treesit-node ,node
        :leaf-p t)
    `(tree-widget
      :node (elisp-tred-node-label
             :tag ,(elisp-tred--get-collapsed-label node)
             :treesit-node ,node
             :leaf-p nil)
      :open-icon (elisp-tred-empty-icon)
      :close-icon (elisp-tred-empty-icon)
      :empty-icon (elisp-tred-empty-icon)
      :leaf-icon (elisp-tred-empty-icon)
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
      (funcall child-nodes-fn node)
    (treesit-node-children node t)))

(defun elisp-tred--get-child-widgets (widget)
  "Get the widget definitions for the children of the given tree node
WIDGET.

This function is called when expanding a tree node in the UI."
  (let* ((node (elisp-tred--treesit-node widget))
         (child-nodes (elisp-tred--get-child-nodes node)))
    (mapcar 'elisp-tred--get-tree-widget child-nodes)))

(provide 'elisp-tred)