;; Some notes about `tree-widget.el':
;;
;; This mode uses Emacs' built-in `tree-widget.el' to render the
;; interactive tree. `tree-widget.el' is battle-hardened and
;; versatile, but it's internal data structures can be difficult to
;; understand. As a reminder to myself and others, here are a few
;; notes:
;;
;; Internal (non-leaf) tree nodes have THREE widgets:
;; 1. Main tree-widget - the container widget, has :open/:args/:node/:children/:buttons properties
;; 2. Icon widget - the [+]/[-] button, stored in :buttons, :parent points to main tree-widget
;; 3. Node widget - the label, first element of :children, :parent points to main tree-widget
;;
;; There is no container tree-widget for leaf nodes. Leaf nodes are
;; bare widgets of any type (e.g. an `item' widget).
;;
;; For internal nodes, the main tree widget has no visible
;; representation in the buffer, and it can only be accessed through
;; the `:parent' property of the icon and node widgets. In the case of
;; `elisp-tred', I hide the icon widgets by using a custom zero-width
;; widget called `elisp-tred-empty-icon', so it is only the node
;; widgets that actually have a representation in the buffer.

(require 'seq)
(require 'treesit)
(require 'tree-widget)
(require 'xref)

(defconst elisp-tred-grammar-version "0.0.1"
  "The version of the `tree-sitter-elisptred' grammar that is intended
to be used with this version elisp-tred.")

(defvar-local elisp-tred-max-label-length 128
  "The maximum length of a tree node label. For the sake of
performance, labels longer than this length will be truncated with an
ellipsis (\"...\").

It is important to impose a max length on the tree node labels because
when a node is collapsed, it shows the full lisp code for its subtree
in a single line, which can be very long indeed.")

(defvar-local elisp-tred--current-node-overlay nil
  "Overlay used to highlight the tree node label on the current line.")

;;; `xref' backend
;;
;; The code in this section implements the `xref' backend for
;; `elisp-tred'. This allows users to call `xref-find-definitions' on
;; a symbol in the `elisp-tred' tree, to jump to the definition of the
;; lisp function/variable/macro under the cursor.
;;
;; `M-.' (`xref-find-definitions') in `elisp-tred' behaves very
;; similarly to `M-.' in `emacs-lisp-mode' buffers, except that the
;; target function/variable/macro is displayed in a new `elisp-tred'
;; buffer, rather than jumping directly to the elisp source code
;; file. This allows the user to keep browsing the code as a tree,
;; when jumping between elisp functions. One notable exception
;; behaviour is that jumping to the definition of a C function will
;; jump to the C source code file, because `elisp-tred' only knows how
;; to create interactive trees for elisp code. (Perhaps someday it
;; will support C code as well.)

(defun elisp-tred--is-c-source-xref-p (xref)
  "Return t if XREF item points to a C source definition.
The elisp xref backend marks C source definitions with file paths
starting with \"src/\" (e.g., \"src/data.c\")."
  (when-let* ((location (xref-item-location xref)))
    ;; Check if this is an elisp-specific location (has a file field)
    (and (xref-elisp-location-p location)
         (when-let* ((file (xref-elisp-location-file location)))
           (string-prefix-p "src/" file)))))

(defun elisp-tred--xref-show-definition (xref)
  "Show a single xref definition by opening it in an elisp-tred buffer.

XREF is an xref item to display. For C source definitions, uses the
default xref behavior. For elisp sources, opens the definition in an
elisp-tred buffer."
  (if (elisp-tred--is-c-source-xref-p xref)
      ;; C source - use default xref behavior
      (xref-pop-to-location xref)
    ;; Elisp source - open in elisp-tred
    (xref-push-marker-stack)
    (let* ((location (xref-item-location xref))
           (marker (xref-location-marker location))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      (with-current-buffer buffer
        (goto-char pos)
        (elisp-tred)))))

(defun elisp-tred--xref-show-definitions (fetcher alist)
  "Show xref definitions by opening them in elisp-tred buffers.

This is a custom implementation of `xref-show-definitions-function'
that opens the target definition in an elisp-tred buffer instead of
jumping directly to the source file.

FETCHER is a function that returns a list of xref items.
ALIST is an association list of additional parameters."
  (let* ((xrefs (funcall fetcher))
         (xref-count (length xrefs)))
    (cond
     ;; No definitions found
     ((= xref-count 0)
      (user-error "No definitions found"))

     ;; Single definition - open it in elisp-tred
     ((= xref-count 1)
      (elisp-tred--xref-show-definition (car xrefs)))

     ;; Multiple definitions - let user choose, then open in elisp-tred
     (t
      (let* ((collection (mapcar
                         (lambda (xref)
                           (cons (xref-item-summary xref) xref))
                         xrefs))
             (choice (completing-read "Choose definition: " collection nil t))
             (xref (cdr (assoc choice collection))))
        (elisp-tred--xref-show-definition xref))))))

;;; Custom "widgets"
;;
;; This section defines a few simple custom UI widgets, which are
;; based Emacs' built-in widgets system [1]. Widgets are used to
;; implement the interactive UI for `customize', but that's the only
;; place I've ever seen them used in practice.
;;
;; Note: Understanding Emacs' widget system is fairly important for
;; understanding how `elisp-tred' works, because the elisp-tred tree is
;; implemented using `tree-widget.el', which is one of built-in widgets
;; provided by Emacs.
;;
;; [1]: "The Emacs Widget Library", https://www.gnu.org/software/emacs/manual/html_mono/widget.html.

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

(defun elisp-tred--install-grammar (&optional suppress-warnings-p)
  "Install the right tree-sitter grammar from GitHub.

Here, the \"right tree-sitter grammar\" means that:

(1) The grammar version matches `elisp-tred-grammar-version'.
(2) The tree-sitter ABI version for the grammar is supported by the
user's build of Emacs.

The grammar version and ABI version are embedded in the git tags for
the grammar releases (e.g. `0.0.1-abi-14').

If SUPPRESS-WARNINGS-P is non-nil, then suppress all warnings during
the internal call to `treesit-install-language-grammar'. I added this
option because I encountered some cases where
`treesit-install-language-grammar' was showing some really misleading
warning messages that were likely to be confusing for the user. In
particular, on Linux I observed that `tree-install-language-grammar'
will show an ABI mismatch warning after successfully installing a
compatible treesit grammar over top of an incompatible grammar
(!). That happens because the grammar on disk is not reloaded until
Emacs is restarted."
  (let* ((library-abi-version-max (treesit-library-abi-version))
         (git-tag (format "%s-abi-%s" elisp-tred-grammar-version library-abi-version-max)))
    (push
     `(elisptred . ("https://github.com/benvvalk/tree-sitter-elisptred.git" ,git-tag))
     treesit-language-source-alist)
    (if suppress-warnings-p
        (with-suppressed-warnings (treesit-install-language-grammar 'elisptred))
      (treesit-install-language-grammar 'elisptred))))

(defun elisp-tred--treesit-init ()
  (unless (treesit-available-p)
    (user-error "Emacs was not compiled with tree-sitter support"))
  ;; Note: If `treesit-language-available-p' is true, it
  ;; means that both of the following are true:
  ;;
  ;; (1) Emacs found the shared library file for the grammar
  ;; (e.g. `~/.emacs.d/tree-sitter/tree-sitter-elisptred.so' on
  ;; Linux).
  ;;
  ;; (2) The tree-sitter ABI version of the shared library is
  ;; compatible with the user's Emacs binary. (This is what is meant
  ;; by "loadable" in the docstring for `treesit-language-available-p').
  ;;
  ;; Note 1: The range of tree-sitter ABI versions that are
  ;; supported by Emacs is determined by the version of the
  ;; tree-sitter library that Emacs was compiled with. You can get
  ;; the minimum and maximum supported tree-sitter ABI versions by
  ;; evaluating `(treesit-library-abi-version t)' and
  ;; `(treesit-library-abi-version)', respectively.
  ;;
  ;; Note 2: Emacs does *not* "hot reload" the shared libraries for
  ;; the grammar if they change on disk. Each grammar is loaded *once*
  ;; when it is first needed, and thereafter it cannot be unloaded or
  ;; reloaded for the rest of Emacs' lifetime.  It makes testings and
  ;; debugging tree-sitter grammars really awkward and error-prone.
  (let* ((result (treesit-language-available-p 'elisptred t))
         (error-p (not (car result)))
         (error-type (cadr result))
         (version-mismatch-p (eq error-type 'version-mismatch)))
    (when error-p
      (if (y-or-n-p "Install tree-sitter grammar for elisp-tred?")
          (progn
            ;; In the case of a `version-mismatch' error, I pass `t'
            ;; for the optional SUPPRESS-WARNINGS-P argument because
            ;; it prints some really misleading warnings.  See the
            ;; docstring of `elisp-tred--install-grammar' for further
            ;; explanation.
            (elisp-tred--install-grammar version-mismatch-p)
            (when version-mismatch-p
              (user-error "Note: Elisp-tred will not work until you restart Emacs (to reload the grammar).")))
        (user-error "Elisp-tred aborted"))))
  (unless (treesit-ready-p 'elisptred)
    (user-error "Failed to load elisp-tred grammar (buffer too large?)"))
  (let ((parser (treesit-parser-create 'elisptred (current-buffer) t)))
    ;; Set up a hook to call the `elisp-tred--on-treesit-reparse'
    ;; function whenever the `treesit' parse tree changes. This allows
    ;; us to keep the overlays for the tree guides in sync with the
    ;; structure of the elisp code.
    ;;
    ;; Note: At first, I expected that `treesit' would automatically
    ;; reparse the buffer and call `elisp-tred--on-treesit-reparse'
    ;; whenever the user edited text in the buffer. However, `treesit'
    ;; does not work this way! Instead, `treesit' lazily reparses the
    ;; buffer when any elisp code makes an API call that accesses the
    ;; `treesit' parse tree (e.g. by calling
    ;; `treesit-parser-root-node').
    ;;
    ;; Since we need to trigger the `treesit' reparse ourselves by
    ;; making `treesit' API calls, we set up another hook for
    ;; `pre-redisplay-functions' to do that on a repeating basis.  In
    ;; addition, we call `elisp-tred--force-treesit-reparse' below to
    ;; perform the initial `treesit' parse after enabling
    ;; `elisp-tred-overlay-mode'.
	(treesit-parser-add-notifier parser #'elisp-tred--on-treesit-reparse)
    (add-hook 'pre-redisplay-functions #'elisp-tred--pre-redisplay nil t)
    (elisp-tred--force-treesit-reparse)))

(defun elisp-tred--buffer-name (treesit-node)
  "Return a buffer name for an elisp-tred buffer that is rooted at
TREESIT-NODE."
  (if-let* (((equal (treesit-node-type treesit-node) "list"))
            (child0 (treesit-node-child treesit-node 0 t))
            (child0-text (treesit-node-text child0))
            ((member child0-text '("defun" "defmacro")))
            (child1 (treesit-node-child treesit-node 1 t))
            (function-name (treesit-node-text child1)))
      (format "*elisp-tred: %s*" function-name)
	(format "*elisp-tred*")))

(defun elisp-tred ()
  "Open elisp-tred buffer and show tree for current top-level elisp
form surrounding POINT."
  (interactive)
  (elisp-tred--treesit-init)
  (when-let* ((root-node (treesit-buffer-root-node 'elisptred))
              (bufname (elisp-tred--buffer-name root-node))
              (tree-buffer (get-buffer-create bufname))
              (pos (point)))
    ;; Force immediate syntax highlighting of the entire elisp source
    ;; code buffer, using Emacs' built-in syntax highlighting.  This
    ;; ensures that the code used to label the tree nodes is always
    ;; syntax-highlighted. By default, Emacs does "just-in-time"
    ;; syntax highlighting of elisp code, which means that code only
    ;; gets syntax-highlighted when it becomes visible to the user
    ;; (i.e. when the user scrolls to the code in a window). If we
    ;; don't force ahead-of-time syntax highlighting, the lack of
    ;; syntax highlighting becomes particularly noticeable if we are
    ;; using `M-.'  (`xref-find-definition') to jump between function
    ;; definitions across buffers.
    (when jit-lock-mode
      (message "elisp-tred: syntax highlighting...")
      (let ((result (benchmark-run (jit-lock-fontify-now (point-min) (point-max)))))
        (message "elisp-tred: syntax highlighting...done (%.3f sec)" (car result))))
    (with-current-buffer tree-buffer
      (elisp-tred-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (message "elisp-tred: creating root widget...")
		(let ((result (benchmark-run (widget-create (elisp-tred--get-tree-widget root-node)))))
          (message "elisp-tred: creating root widget...done (%.3f sec)" (car result)))
        (message "elisp-tred: building tree...")
        (let ((result (benchmark-run (elisp-tred--goto-source-code-pos-in-tree pos))))
          (message "elisp-tred: building tree...done (%.3f sec)" (car result)))))
    ;; Default to displaying the tree buffer in the same window as the
    ;; elisp source buffer, unless the user overrides it in their
    ;; `display-buffer-alist'.
    (display-buffer tree-buffer '(display-buffer-same-window))))

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

(defun elisp-tred--treesit-node-for-current-line ()
  (when-let* ((node-widget (elisp-tred--node-widget-for-current-line)))
    (elisp-tred--treesit-node node-widget)))

(defun elisp-tred--tree-widget-for-current-line ()
  "Return the main tree widget on the current-line, if any.

This function will return `nil' if we are on a line that represents a
leaf node in the tree. Leaf node lines don't have a tree widget, they only
have a `item' widget that is used for the tree node label."
  (when-let* ((node-widget (elisp-tred--node-widget-for-current-line)))
    ;; For internal tree-widget nodes, `:parent' points to a tree
    ;; widget for the current line. For leaf nodes, `:parent' points to
    ;; the tree widget for the parent in the tree, which is on a
    ;; different line. Pretty confusing!
    (unless (widget-get node-widget :leaf-p)
      (widget-get node-widget :parent))))

(defun elisp-tred--set-tree-widget-expanded (tree-widget expanded)
  "Set the expanded/collapsed state of TREE-WIDGET.

When EXPANDED is non-nil, expand the tree widget and show its children.
When EXPANDED is nil, collapse the tree widget and hide its children."
  (when-let* ((label-widget (widget-get tree-widget :node))
              (treesit-node (elisp-tred--treesit-node tree-widget)))
    ;; Do nothing if `expanded' already matches the current
    ;; expanded/collapsed state of the tree-widget.
    (when (not (equal expanded (widget-get tree-widget :open)))
      ;; Before collapsing the node, call
      ;; `tree-widget-children-value-save' to recursively save the
      ;; collapsed/expanded states of the descendants. This ensures
      ;; that the subtree looks exactly the same the next time we
      ;; expand it. (Without this step, the children would all be in
      ;; collapsed state the next time we expand the node).
      ;;
      ;; `tree-widget' implementation notes:
      ;;
      ;; * When a node is collapsed, the text lines for the descendant
      ;; nodes are completely *deleted* from the buffer (!), not
      ;; hidden.
      ;;
      ;; * `tree-widget-children-value-save' saves the
      ;; collapsed/expanded states of the descendants by updating the
      ;; values of the `:open' property in the widget definitions for
      ;; the children, which are stored `:args' property of the
      ;; parent. The next time the parent is expanded, the descendant
      ;; nodes are redrawn from scratch in the buffer, from the
      ;; updated child definitions.
      ;;
      ;; * For a related code example, see `tree-widget-action', the
      ;; built-in function for toggling the collapsed/expanded state
      ;; of a `tree-widget'. It also calls
      ;; `tree-widget-children-value-save' to save the
      ;; collapsed/expanded state of the descendants.
      (or expanded
          (tree-widget-children-value-save tree-widget))
      (let* ((new-label (if expanded
                            (elisp-tred--get-expanded-label treesit-node)
                          (elisp-tred--get-collapsed-label treesit-node))))
       ;; Update the tree node label
       (widget-put label-widget :tag new-label)
       ;; Update the :open property
       (widget-put tree-widget :open expanded)
       ;; Redraw the widget.
       ;; It's not super intuitive, but setting the "value" of the
       ;; tree-widget to nil or non-nil triggers a redraw of the
       ;; widget in its collapsed or expanded state, respectively.
       (widget-apply tree-widget :value-set expanded)))))

(defun elisp-tred--expand-tree-widget-recursively (tree-widget)
  "Expand TREE-WIDGET and its children recursively."
  (let ((treesit-node (elisp-tred--treesit-node tree-widget)))
    ;; Special case: Don't auto-expand single-item lists/vectors,
    ;; because it wastes vertical space without improving code
    ;; readability.
    (unless (elisp-tred--single-item-sequence-p treesit-node)
      (elisp-tred--set-tree-widget-expanded tree-widget t)
      (dolist (child-tree-widget (elisp-tred--visible-child-tree-widgets tree-widget))
        (elisp-tred--expand-tree-widget-recursively child-tree-widget)))))

(defun elisp-tred-toggle-node-recursively ()
  "If the tree widget on the current line is collapsed, expand
it and its children recursively. It the tree widget is expanded,
collapse it.

If there is no tree widget on the current line, do nothing."
  (interactive)
  (when-let* ((tree-widget (elisp-tred--tree-widget-for-current-line)))
    (if (widget-get tree-widget :open)
        (elisp-tred--set-tree-widget-expanded tree-widget nil)
      (elisp-tred--expand-tree-widget-recursively tree-widget))))

(defun elisp-tred-toggle-node ()
  "Toggle the expanded/collapsed state of the tree node on the current line."
  (interactive)
  (when-let* ((tree-widget (elisp-tred--tree-widget-for-current-line)))
    (let ((open (widget-get tree-widget :open)))
      (elisp-tred--set-tree-widget-expanded tree-widget (not open)))))

(defun elisp-tred--parent-node-widget (node-widget)
  "Return the parent node widget of NODE-WIDGET."
  ;; Some reminders about the `tree-widget' data structure,
  ;; to help understand the code below.
  ;;
  ;; Internal (non-leaf) tree nodes have THREE associated widgets:
  ;; 1. Main tree-widget - the container widget
  ;; 2. Icon widget - the [+]/[-] button, `:parent' points to main tree-widget
  ;; 3. Node widget - the label, `:parent' points to main tree-widget
  ;;
  ;; Leaf nodes are bare widgets (`item', `button', etc.)  any
  ;; container. For leaf nodes, `:parent' points to the `tree-widget'
  ;; at the _parent level_ in the tree (i.e. one level up!).
  (let ((leaf-p (widget-get node-widget :leaf-p))
        (parent1 (widget-get node-widget :parent)))
    (if leaf-p
        (car (widget-get parent1 :children))
      (let ((parent2 (widget-get parent1 :parent)))
        (car (widget-get parent2 :children))))))

(defun elisp-tred--visible-child-tree-widgets (tree-widget)
  "If the TREE-WIDGET is currently expanded (i.e. its children are
visible), return a list child tree widgets (i.e. non-leaf
children). Otherwise return `nil'."
  (when (widget-get tree-widget :open)
    ;; Some reminders about the `tree-widget' data structure,
    ;; to help understand the code below:
    ;;
    ;; * One would expect the `:children' property of a
    ;; `tree-widget' to simply be a list of the (converted)
    ;; widgets for the children. However, there is an extra
    ;; widget added to the front of the list. The `car' of
    ;; `:children' is the (converted) `:node' widget of the
    ;; parent node, and the `cdr' of `:children' is actual
    ;; list of (converted) widgets for the children.
    ;;
    ;; * There are two cases for the widget types of the
    ;; children. If the child is an internal node (i.e. not a
    ;; leaf node) its widget type will be
    ;; `tree-widget'. However, if the child is a leaf node, it
    ;; will be a basic widget type (`button', `item', etc.). In
    ;; the case of `elisp-tred-mode', leaf nodes are always
    ;; `item' widgets.
    (let ((child-widgets (cdr (widget-get tree-widget :children))))
      (seq-filter #'tree-widget-p child-widgets))))

(defun elisp-tred--visible-child-node-widgets (node-widget)
  "If the NODE-WIDGET is currently expanded (i.e. its children are
visible), return a list its child node widgets. Otherwise return
`nil'."
  (let ((leaf-p (widget-get node-widget :leaf-p)))
    (unless leaf-p
      (let* ((tree-widget (widget-get node-widget :parent))
             (expanded-p (widget-get tree-widget :open)))
        (when expanded-p
          ;; Some reminders about the `tree-widget' data structure,
          ;; to help understand the code below:
          ;;
          ;; * One would expect the `:children' property of a
          ;; `tree-widget' to simply be a list of the (converted)
          ;; widgets for the children. However, there is an extra
          ;; widget added to the front of the list. The `car' of
          ;; `:children' is the (converted) `:node' widget of the
          ;; parent node, and the `cdr' of `:children' is actual
          ;; list of (converted) widgets for the children.
          ;;
          ;; * There are two cases for the widget types of the
          ;; children. If the child is an internal node (i.e. not a
          ;; leaf node) its widget type will be
          ;; `tree-widget'. However, if the child is a leaf node, it
          ;; will be a basic widget type (`button', `item', etc.). In
          ;; the case of `elisp-tred-mode', leaf nodes are always
          ;; `item' widgets.
          (let ((child-widgets (cdr (widget-get tree-widget :children))))
            (mapcar (lambda (widget)
                      (if (tree-widget-p widget)
                          (car (widget-get widget :children))
                        widget))
                    child-widgets)))))))

(defun elisp-tred--child-node-widgets (node-widget)
  "Return the node widget definitions for the children of NODE-WIDGET.

Note that the current expanded/collapsed state of NODE-WIDGET has no
effect on the widgets returned by this function. If you need a
function that returns `nil' when NODE-WIDGET is collapsed, use
`elisp-tred--visible-child-node-widgets' instead."
  (unless (widget-get node-widget :leaf-p)
    (let* ((child-widgets (elisp-tred--get-child-widgets node-widget)))
      ;; If the child widget is a leaf node, `child-widget' is an
      ;; `item' widget and we can return it as is.
      ;;
      ;; If the child widget is an internal (non-leaf) node, then it
      ;; is a container `tree-widget', and we need to get the node
      ;; widget (i.e. the label widget for the tree node) from the
      ;; `:node' property.
      (mapcar (lambda (child-widget)
                (if (widget-get child-widget :leaf-p)
                    child-widget
                  (widget-get child-widget :node)))
              child-widgets))))

(defun elisp-tred--sibling-node-widgets (node-widget)
  "Return the list of NODE-WIDGET's siblings, including itself."
  (when-let ((parent-node-widget (elisp-tred--parent-node-widget node-widget)))
	(elisp-tred--visible-child-node-widgets parent-node-widget)))

(defun elisp-tred--sibling-index-for-node-widget (node-widget)
  (let ((sibling-node-widgets (elisp-tred--sibling-node-widgets node-widget))
        (equal-fn (lambda (widget1 widget2)
                   (= (widget-get widget1 :from)
                      (widget-get widget2 :from)))))
    (seq-position sibling-node-widgets node-widget equal-fn)))

(defun elisp-tred--prev-sibling-node-widget (node-widget)
  (let ((sibling-node-widgets (elisp-tred--sibling-node-widgets node-widget))
        (sibling-index (elisp-tred--sibling-index-for-node-widget node-widget)))
    ;; Note: To my surprise, `nth' returns the first element of the
    ;; list when the index is negative, so we need to explicitly check
    ;; if `sibling-index' is > 0 here.
    (when (> sibling-index 0)
      (nth (1- sibling-index) sibling-node-widgets))))

(defun elisp-tred--next-sibling-node-widget (node-widget)
  (let ((sibling-node-widgets (elisp-tred--sibling-node-widgets node-widget))
        (sibling-index (elisp-tred--sibling-index-for-node-widget node-widget)))
    ;; Note: `nth' returns `nil' if we use an index that is larger
    ;; than number of elements minus one, so there's no need to do
    ;; an explicit bounds check here.
    (nth (1+ sibling-index) sibling-node-widgets)))

(defun elisp-tred-goto-prev-sibling ()
  "Move the cursor to the prev sibling node in the tree.
Position the cursor on the first character of the tree node label.
If there is no prev sibling, leave the cursor where it is and
display a message."
  (interactive)
  (if-let* ((node-widget (elisp-tred--node-widget-for-current-line))
            (prev-sibling-widget (elisp-tred--prev-sibling-node-widget node-widget))
            (target-pos (widget-get prev-sibling-widget :from)))
      (goto-char target-pos)
    (user-error "No prev sibling")))

(defun elisp-tred-goto-next-sibling ()
  "Move the cursor to the next sibling node in the tree.
Position the cursor on the first character of the tree node label.
If there is no next sibling, leave the cursor where it is and
display a message."
  (interactive)
  (if-let* ((node-widget (elisp-tred--node-widget-for-current-line))
            (next-sibling-widget (elisp-tred--next-sibling-node-widget node-widget))
            (target-pos (widget-get next-sibling-widget :from)))
      (goto-char target-pos)
    (user-error "No next sibling")))

(defun elisp-tred-goto-parent ()
  "Move the cursor to the parent node in the tree.  Position the
cursor on the first character of the tree node label. If there is no
parent, leave the cursor where it is and display a message."
  (interactive)
  (if-let* ((node-widget (elisp-tred--node-widget-for-current-line))
            (parent-node-widget (elisp-tred--parent-node-widget node-widget))
            (target-pos (widget-get parent-node-widget :from)))
      (goto-char target-pos)
    (user-error "No parent")))

(defun elisp-tred-goto-parent-and-collapse ()
  "Move the cursor to the parent node in the tree.  Position the
cursor on the first character of the tree node label. If there is no
parent, leave the cursor where it is and display a message."
  (interactive)
  (elisp-tred-goto-parent)
  (when-let* ((tree-widget (elisp-tred--tree-widget-for-current-line)))
    (elisp-tred--set-tree-widget-expanded tree-widget nil)))

(defun elisp-tred-goto-first-child ()
  "Move the cursor to the cursor to the first child of the current
node, expanding the current node as needed. If the current node has no
children (i.e. it is a leaf node), leave the cursor where it is and
show a message."
  (interactive)
  (if-let ((tree-widget (elisp-tred--tree-widget-for-current-line)))
      (if (widget-get tree-widget :open)
          (when-let* ((child0 (cadr (widget-get tree-widget :children)))
                      (target-pos (widget-get child0 :from)))
            (goto-char target-pos))
        (user-error "Children are not visible"))
    (user-error "No children")))

(defun elisp-tred-goto-first-child-and-expand ()
  "Move the cursor to the cursor to the first child of the current
node, expanding the current node as needed. If the current node has no
children (i.e. it is a leaf node), leave the cursor where it is and
show a message."
  (interactive)
  (if-let ((tree-widget (elisp-tred--tree-widget-for-current-line)))
      (progn (elisp-tred--set-tree-widget-expanded tree-widget t)
             (elisp-tred-goto-first-child))
    (user-error "No children")))

(defun elisp-tred--update-current-node-highlight ()
  "Update the overlay that highlights the tree node label on the current line.
Creates the overlay if it doesn't exist, or moves it to the current line's
node widget if it does exist. Removes the overlay if no node widget is found
on the current line."
  (when-let* ((node-widget (elisp-tred--node-widget-for-current-line))
              (start (widget-get node-widget :from))
              (end (widget-get node-widget :to)))
    ;; Exclude the trailing newline from the highlight by using (1- end).
    ;; Widget boundaries include the newline that follows the widget text.
    (let ((highlight-end (1- end)))
      ;; Create overlay if it doesn't exist
      (unless elisp-tred--current-node-overlay
        (setq elisp-tred--current-node-overlay (make-overlay start highlight-end))
        (overlay-put elisp-tred--current-node-overlay 'face 'highlight)
        (overlay-put elisp-tred--current-node-overlay 'priority 100))
      ;; Move overlay to current node widget
      (move-overlay elisp-tred--current-node-overlay start highlight-end)))
  ;; Remove overlay if no node widget on current line
  (when (and elisp-tred--current-node-overlay
             (not (elisp-tred--node-widget-for-current-line)))
    (delete-overlay elisp-tred--current-node-overlay)
    (setq elisp-tred--current-node-overlay nil)))

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

(defun elisp-tred--single-item-sequence-p (node)
  "Return non-nil if the treesit node NODE represents a list or vector
with a single element."
  (and (elisp-tred--sequence-p node)
       (eq (elisp-tred--sequence-length node) 1)))

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
quoted sequence, return a string containing the left (opening) bracket
character.

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
quoted sequence, return a string containing the right (closing)
bracket character.

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

(defvar elisp-tred--newline-regex
  "\\(\r\n\\|\n\\|\r\\)"
  "Regular expression that matches newlines on Linux, Mac, and Windows.")

(defun elisp-tred--multiline-string-p (treesit-node)
  "Return true if TREESIT-NODE is a string that contains
one or more newlines."
  (when (equal (treesit-node-type treesit-node) "string")
    (let* ((str (treesit-node-text treesit-node))
           (lines (split-string str elisp-tred--newline-regex)))
		(> (length lines) 1))))

(defun elisp-tred--linewise-widgets-for-string (treesit-node)
  "For a TREESIT-NODE of type \"string\", return one `item' widget per
line of text in the string. In other words, split on newlines and
create a widget for each part."
  (let ((str (treesit-node-text treesit-node))
        (treesit-start-pos (treesit-node-start treesit-node))
        (str-pos 0)
        child-widgets)
    (while (string-match elisp-tred--newline-regex str str-pos)
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (push (elisp-tred--get-tree-widget
               treesit-node
               (+ treesit-start-pos str-pos)
               (+ treesit-start-pos start))
              child-widgets)
        (setq str-pos end)))
    ;; Handle the last line, in the case that it doesn't
    ;; have a trailing newline.
    (when (< str-pos (length str))
      (push (elisp-tred--get-tree-widget
             treesit-node
             (+ treesit-start-pos str-pos)
             (+ treesit-start-pos (length str)))
            child-widgets))
    (nreverse child-widgets)))

(defun elisp-tred--child-widgets-for-sequence (treesit-node)
  "Return the child widgets of TREESIT-NODE, where TREESIT-NODE
represents a lisp sequence (i.e. a list or a vector).

In general, this function just gets the named children of TREESIT-NODE
and creates a widget for each. However, in the case of multi-line
strings (e.g. the docstring for a `defun'), we split the string into a
separate tree node widgets for each line. This helps greatly with
readability of docstrings, as otherwise they would collapsed to a
single line and truncated to a maximum length."
  (let ((child-nodes (treesit-node-children treesit-node t))
        child-widgets)
    (dolist (child-node child-nodes)
      ;; Split multi-line strings into one widget per line. Without
      ;; this, docstrings are collapsed to a single line and truncated
      ;; to `elisp-tred-max-label-length', which prevents the user
      ;; from reading documentation.
      (if (elisp-tred--multiline-string-p child-node)
          (dolist (widget (elisp-tred--linewise-widgets-for-string child-node))
                  (push widget child-widgets))
        (push (elisp-tred--get-tree-widget child-node) child-widgets)))
    (nreverse child-widgets)))

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
     (lambda (node &optional from to)
	   (when (and (elisp-tred--sequence-p node)
                  (>= (elisp-tred--sequence-length node) 2))
         (let* ((child0 (elisp-tred--sequence-car node))
                (child0-type (treesit-node-type child0)))
           (and (not (elisp-tred--sequence-p child0))
                (not (equal child0-type "comment"))))))

     :collapsed-label-fn elisp-tred--get-collapsed-label-for-sequence

     :expanded-label-fn
     (lambda (node)
       (let* ((quote-char (elisp-tred--quote-char node)))
         (when-let* ((left-bracket (elisp-tred--left-bracket-char node))
                     (child0 (elisp-tred--sequence-car node))
                     (child0-text (treesit-node-text child0)))
           (concat quote-char
                   left-bracket
                   child0-text))))

     :child-widgets-fn
     (lambda (treesit-node)
       ;; Remove widget for first element (e.g. `setq', `defun'),
       ;; because it is already shown on the parent line along with the
       ;; opening paren/bracket.
       (cdr (elisp-tred--child-widgets-for-sequence treesit-node))))

    (:description "a sequence (list or vector) or quoted sequence"

     :match-fn
     (lambda (node &optional from to)
       (elisp-tred--sequence-p node))

     :collapsed-label-fn elisp-tred--get-collapsed-label-for-sequence

     :expanded-label-fn
     (lambda (node)
       (let* ((quote-char (elisp-tred--quote-char node)))
         (when-let* ((left-bracket (elisp-tred--left-bracket-char node)))
           (concat quote-char left-bracket))))

     :child-widgets-fn elisp-tred--child-widgets-for-sequence))
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

`:collapsed-label-fn' (optional) - A function that is used to generate
the label text for the elisp-tred tree node when it is in collapsed
state. For example, a elisp-tred node might show a \"(\" when it is
expanded, and the full list contents when it is collapsed. The
`:collapsed-label-fn' takes a single argument NODE, which is a treesit
node that matched this tree mapping rule (as determined by
`:match-fn'). The `:collapsed-label-fn' is optional and will default to
just returning the entire source code text corresponding to NODE, with
newlines and duplicate spaces removed.

`:expanded-label-fn' (optional) - A function that is used to generate
the label text for the elisp-tred tree node when it is in expanded
state. For example, a elisp-tred node might show a \"(\" when it is
expanded, and the full list contents when it is collapsed. The
`:expanded-label-fn' takes a single argument NODE, which is a treesit
node that matched this tree mapping rule (as determined by
`:match-fn'). The `:expanded-label-fn' is optional and will default to
just returning the entire source code text corresponding to NODE, with
newlines and duplicate spaces removed.

`:child-widgets-fn' (optional) - A function that returns the child
widgets of the current node in the elisp-tred tree. The
`:child-widgets-fn' takes a single argument NODE, which is a treesit
node that matched this tree mapping rule (as determined by
`:match-fn'). The `:child-nodes-fn' is optional and defaults to
returning widgets for all name treesit children of NODE.")

(defun elisp-tred--get-tree-mapping-rule (node &optional from to)
  (catch 'break
    (dolist (rule elisp-tred--tree-mapping-rules)
	  (when-let* ((match-fn (plist-get rule :match-fn))
                  (match-p (funcall match-fn node from to)))
        (throw 'break rule)))))

(defun elisp-tred--get-expanded-label (node)
  "Return the text label for a treesit node (NODE) when
it is expanded."
  (if-let* ((rule (elisp-tred--get-tree-mapping-rule node))
            (label-fn (plist-get rule :expanded-label-fn)))
      (funcall label-fn node)
    ""))

(defun elisp-tred--closing-brackets-string (node)
  "Build the closing brackets string for the given treesit node NODE,
which is a sequence of `]' and `)' characters for closing vectors and
lists, respectively.

For the sake of displaying the elisp code compactly, we display the
closing brackets for lists/vectors on the same line as the last
list/vector item.  If NODE is not the last item of a list/vector
(i.e. the last child or the parent), then this function will return
`nil'."
  (when (and node (elisp-tred--is-last-child node))
      (when-let* ((parent-node (treesit-node-parent node))
                  (parent-node-type (treesit-node-type parent-node)))
        (concat (elisp-tred--closing-brackets-string parent-node)
                (pcase parent-node-type
		  		  ("list" ")")
                  ("vector" "]"))))))

(defun elisp-tred--get-collapsed-label-for-sequence (node &optional from to)
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
     right-bracket
     (elisp-tred--closing-brackets-string node))))

(defun elisp-tred--get-collapsed-label (treesit-node &optional from to)
  "Return the text label for a treesit node (NODE) when
it is collapsed."
  (if-let* ((rule (elisp-tred--get-tree-mapping-rule treesit-node))
            (collapsed-label-fn (plist-get rule :collapsed-label-fn)))
	  (funcall collapsed-label-fn treesit-node from to)
    (let ((text (treesit-node-text treesit-node))
          (start-pos (treesit-node-start treesit-node)))
      (concat
       (if (and from to)
           (substring text (- from start-pos) (- to start-pos))
         (concat (elisp-tred--remove-newlines-and-collapse-spaces text)
                 (elisp-tred--closing-brackets-string treesit-node)))))))

(defun elisp-tred--leaf-p (node &optional from to)
  "Return t if treesit NODE should be rendered as a leaf in the
elisp-tred tree."
  (if-let* ((rule (elisp-tred--get-tree-mapping-rule node from to))
            (child-widgets-fn (plist-get rule :child-widgets-fn)))
      (null (funcall child-widgets-fn node))
    (let ((unquoted (elisp-tred--unquote node)))
      (eq (treesit-node-child-count unquoted) 0))))

(defun elisp-tred--get-tree-widget (node &optional from to)
  "Return the widget definition for treesit node NODE.

The widget definition is used render the treesit node as collapsible
tree widget in the elisp-tree buffer.

The FROM and TO arguments specify a subrange of the NODE's full range
in the elisp source code buffer, as returned by
`treesit-node-start'/`treesit-node-end'.  One place where subranges
are used is to create separate widgets for each line in a multi-line
string (for the sake of readability)."
  (if (elisp-tred--leaf-p node from to)
	  `(elisp-tred-node-label
        :tag ,(elisp-tred--get-collapsed-label node from to)
        :treesit-node ,node
        :treesit-node-from ,from
        :treesit-node-to ,to
        :leaf-p t)
    (let ((half-width-space (propertize " " 'display '(space :width 0.5)))
          (shadow-face (lambda (text) (propertize text 'face 'shadow))))
      `(tree-widget
       :node (elisp-tred-node-label
              :tag ,(elisp-tred--get-collapsed-label node from to)
              :treesit-node ,node
              :treesit-node-from ,from
              :treesit-node-to ,to
              :leaf-p nil)
       :open-icon (elisp-tred-empty-icon)
       :close-icon (elisp-tred-empty-icon)
       :empty-icon (elisp-tred-empty-icon)
       :leaf-icon (elisp-tred-empty-icon)
       :guide (tree-widget-guide :tag ,(funcall shadow-face "├"))
       :no-guide (tree-widget-guide :tag ,(funcall shadow-face " "))
       :nohandle-guide (tree-widget-guide :tag ,(funcall shadow-face "│"))
       :handle (tree-widget-guide :tag  ,(funcall shadow-face (concat "─" half-width-space)))
       :no-handle (tree-widget-guide :tag ,(funcall shadow-face (concat " " half-width-space)))
       :end-guide (tree-widget-guide :tag ,(funcall shadow-face "╰"))
       :expander elisp-tred--get-child-widgets))))

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
  (let* ((node-widget (widget-get widget :node))
         (node (elisp-tred--treesit-node widget))
         (from (widget-get node-widget :treesit-node-from))
         (to (widget-get node-widget :treesit-node-to)))
    (if-let* ((rule (elisp-tred--get-tree-mapping-rule node from to))
              (child-widgets-fn (plist-get rule :child-widgets-fn)))
        ;; There is custom rule for creating/filtering child widgets.
        (funcall child-widgets-fn node)
      ;; Default behaviour: Since there is no custom rule, create a
      ;; widget for each (named) child in the tree-sitter parse tree.
      (let ((child-nodes (treesit-node-children node t)))
        (mapcar #'elisp-tred--get-tree-widget child-nodes)))))

(defun elisp-tred--node-contains-pos (node pos)
  "Return t if treesit NODE contains buffer position POS."
  (and node
       (>= pos (treesit-node-start node))
       (<= pos (treesit-node-end node))))

(defun elisp-tred--expand-tree-widget-if-needed (icon-widget)
  "Expand ICON-WIDGET if it is currently collapsed."
  (unless (widget-get icon-widget :open)
    (when-let* ((icon-widget (car (widget-get icon-widget :buttons)))
                (pos (widget-get icon-widget :from)))
        (widget-button-press pos))))

(defun elisp-tred--goto-source-code-pos-in-tree (pos)
  "Navigate to the deepest (leaf-most) tree node that corresponds
to buffer position POS in the elisp source code buffer, expanding
ancestor tree nodes as needed. Then navigate to the exact position
within the tree node label that corresponds to POS."
  (goto-char (point-min))
  ;; Find the line of deepest (leaf-most) tree node containing POS.
  (catch 'done
    (while (not (eobp))
      (when-let* ((node-widget (elisp-tred--node-widget-for-current-line))
                  (treesit-node (widget-get node-widget :treesit-node)))
        (when (elisp-tred--node-contains-pos treesit-node pos)
          ;; Check if any child node also contains POS
          (let ((child-node-widgets (elisp-tred--child-node-widgets node-widget))
                (found-child nil))
            (dolist (child-node-widget child-node-widgets)
              (when-let ((child-treesit-node (widget-get child-node-widget :treesit-node)))
                (when (elisp-tred--node-contains-pos child-treesit-node pos)
                 (setq found-child t))))

            (if found-child
                ;; One of the children is a better match, so expand
                ;; the tree widget on the current line and fall through
                ;; to continue searching on subsequent lines.
                (elisp-tred--set-tree-widget-expanded
                 (elisp-tred--tree-widget-for-current-line) t)
              ;; Found the target node, move point to exact position in label
              (let* ((node-start (treesit-node-start treesit-node))
                     (node-from (or (widget-get node-widget :treesit-node-from) node-start))
                     (label-start (widget-get node-widget :from))
                     (label-end (widget-get node-widget :to))
                     ;; Calculate offset of POS within the source node portion
                     (offset-in-node (- pos node-from))
                     ;; Calculate the target position in the tree buffer label
                     (target-pos (+ label-start offset-in-node)))
                ;; Clamp to label boundaries to handle edge cases
                ;; Use max of (label-start, label-end - 1) to handle empty/short labels
                (goto-char (min (max target-pos label-start) (max label-start (1- label-end)))))
              (throw 'done t)))))
      (forward-line 1))))

(defun elisp-tred-jump-to-source-buffer ()
  "Jump to the position in the elisp source code buffer that corresponds
to the current cursor position in the elisp-tred buffer."
  (interactive)
  (when-let* ((node-widget (elisp-tred--node-widget-for-current-line))
              (node (elisp-tred--treesit-node node-widget))
              (source-buffer (treesit-node-buffer node)))
    (let* ((label-start (widget-get node-widget :from))
           (label-end (widget-get node-widget :to))
           (cursor-pos (point))
           ;; Calculate offset of cursor within the label
           (offset-in-label (- cursor-pos label-start))
           ;; Calculate the target position in the source buffer
           (node-start (treesit-node-start node))
           (node-from (or (widget-get node-widget :treesit-node-from) node-start))
           (node-end (treesit-node-end node))
           (node-to (or (widget-get node-widget :treesit-node-to) (1- node-end)))
           (target-pos (+ node-from offset-in-label)))
      ;; Clamp to node boundaries to handle edge cases
      (let ((clamped-pos (min (max target-pos node-from) node-to)))
        (with-current-buffer source-buffer
            (goto-char clamped-pos))
        (display-buffer source-buffer '(display-buffer-same-window))))))

;;; New overlay-based implementation

;;;; Tree-sitter

(defun elisp-tred--force-treesit-reparse ()
    "Force `treesit' to reparse the buffer.

Note: `treesit' does not automatically reparse the buffer whenever the
user makes an edit. Instead, it lazily reparses the buffer next time
some elisp code makes an API call that accesses `treesit' parse tree,
e.g. by calling `treesit-parser-root-node'."
    (when-let ((parsers (elisp-tred--treesit-parsers)))
      (dolist (parser parsers)
        ;; Note: Any `treesit' API call that accesses the parse tree
        ;; should also work here (see Note above).
        (treesit-parser-root-node parser))))

(defvar-local elisp-tred--pre-redisplay-tick nil
  "The last `buffer-chars-modified-tick' that we've processed.  This
is used to work the bug/quirk that Emacs calls its redisplay hooks
multiple times for the same redisplay event, and the exact number of
hook invocations isn't even predictable.")

(defun elisp-tred--pre-redisplay (&rest _)
  "Force reparse of treesit parser, which will trigger notifiers.
This function is added to `pre-redisplay-functions' to ensure that
the parse tree is updated before redisplay, similar to how treesit.el
handles font-lock updates."
  (unless (eq elisp-tred--pre-redisplay-tick (buffer-chars-modified-tick))
    (elisp-tred--force-treesit-reparse)
    (setq elisp-tred--pre-redisplay-tick (buffer-chars-modified-tick))))

(defun elisp-tred--on-treesit-reparse (ranges _parser)
  "Update elisp-tred overlays (e.g. tree guides) when the
`treesit' parse tree changes."
  (elisp-tred--remove-overlays)
  (elisp-tred--create-whitespace-overlays)
  (let ((root-node (treesit-buffer-root-node 'elisptred)))
    (elisp-tred--create-tree-guide-overlays root-node)))

(defun elisp-tred--treesit-parsers ()
  "Return the `treesit' parser object for `elisp-tred'."
  (let ((parsers (treesit-parser-list)))
    (seq-filter (lambda (parser)
                (eq 'elisptred (treesit-parser-language parser)))
              parsers)))

(defun elisp-tred--treesit-teardown ()
  "Destroy `treesit' parser object for `elisp-tred', and disable
all treesit-related hook functions."
  (when-let ((parsers (elisp-tred--treesit-parsers)))
    (dolist (parser parsers)
      (treesit-parser-remove-notifier parser #'elisp-tred--on-treesit-reparse)
      (remove-hook 'pre-redisplay-functions #'elisp-tred--pre-redisplay t)
      (treesit-parser-delete parser))))

;;;; Tree guide rendering

(setq elisp-tred--guide "│ ")
(setq elisp-tred--guide-with-handle "├─")
(setq elisp-tred--guide-with-handle-last "╰─")
(setq elisp-tred--no-guide "  ")

(defun elisp-tred--last-child-p (node)
  "Return non-nil if the treesit node NODE is the last named child
of its parent node.

Note: If NODE has no parent treesit node (i.e. it is the root node of
the treesit parse tree), this function will return `nil'."
  (when-let* ((parent (treesit-node-parent node))
              (siblings (treesit-node-children parent t)))
    (treesit-node-eq node (car (last siblings)))))

(defun elisp-tred--guide-flags (node &optional flags)
  "Return the `guide flags' for treesit node NODE.

The `guide flags' are a list consisting of one flag per ancestor node,
plus one flag for NODE itself. The flags indicate which tree guide
characters should be rendered at the beginning of the line for NODE. A
value of `t' indicates that a guide (e.g. `|') should be drawn in that
position, whereas a value of `nil' indicates that a guide should not
be drawn in that position (e.g. ` ')."
  (if-let ((parent (treesit-node-parent node)))
    (let ((flag (not (elisp-tred--last-child-p node))))
      (push flag flags)
      (elisp-tred--guide-flags parent flags))
    flags))

(defun elisp-tred--guide-flags-to-string (flags)
  "Convert the guide flags FLAGS to a string that will be shown in the
buffer.

See the docstring for `elisp-tred--guide-flags' for an explanation
about the purpose of the guide flags."
  (when flags
    (let* ((ancestor-flags (butlast flags))
           (last-flag (car (last flags))))
      (concat (mapconcat (lambda (flag)
                           (if flag
                               elisp-tred--guide
                             elisp-tred--no-guide))
                         ancestor-flags)
              (if last-flag
                  elisp-tred--guide-with-handle
                elisp-tred--guide-with-handle-last)))))

(defun elisp-tred--tree-guides-string (node)
  "Return the string of tree guide characters (pipes and spaces) that
should be inserted at the beginning of the line for treesit node
NODE."
  (when-let ((parent (treesit-node-parent node)))
    (let ((flags (elisp-tred--guide-flags parent)))
      (concat (elisp-tred--guide-flags-to-string flags)
              (if (elisp-tred--last-child-p node)
                  elisp-tred--guide-with-handle-last
                elisp-tred--guide-with-handle)))))

(defun elisp-tred--create-tree-guide-overlay (node guide-flags)
  "Insert the tree guide overlay at the beginning of the line
for treesit node NODE. "
  (when-let* ((guide-string (elisp-tred--guide-flags-to-string guide-flags))
              (start (treesit-node-start node))
              (end (treesit-node-end node))
              (overlay (make-overlay start end)))
    (overlay-put overlay 'category 'elisp-tred-guide)
    (overlay-put overlay 'before-string (concat "\n" guide-string))))

(defun elisp-tred--create-tree-guide-overlays (node &optional guide-flags)
  "Create the tree guide overlays for treesit node NODE and all of its
descendants.

GUIDE-FLAGS is a list of booleans, one per ancestor node, that is used
to construct the tree guide lines."
  (elisp-tred--create-tree-guide-overlay node guide-flags)
  (let* ((children (treesit-node-children node t)))
    (dolist (child children)
      (let* ((guide-flag (not (elisp-tred--last-child-p child)))
             (guide-flags (append guide-flags (list guide-flag))))
        (elisp-tred--create-tree-guide-overlays child guide-flags)))))

(defun elisp-tred--create-whitespace-overlays ()
  "Create overlays to hide non-significant whitespace characters.

Uses tree-sitter to identify which whitespace is significant (e.g.,
spaces within strings and comments) vs non-significant (e.g.,
indentation and spacing between tokens). Only non-significant
whitespace is hidden."
  (let ((root-node (treesit-buffer-root-node 'elisptred)))
    (elisp-tred--hide-non-significant-whitespace root-node)))

(defun elisp-tred--hide-non-significant-whitespace (node)
  "Hide all non-significant whitespace using overlays.

`Non-significant whitespace' means whitespace that is not contained
within a string or comment (e.g. indentation and newlines). We want
to hide all such whitespace so that we display the tree structure in a
consistent and predictable manner."
  (let ((children (treesit-node-children node t)))
    (when children
      ;; Hide whitespace before the first child
      (let* ((first-child (car children))
             (gap-start (treesit-node-start node))
             (gap-end (treesit-node-start first-child)))
        (elisp-tred--hide-whitespace-in-range gap-start gap-end))

      ;; Hide whitespace between consecutive children
      (let ((prev-child (car children)))
        (dolist (curr-child (cdr children))
          (let ((gap-start (treesit-node-end prev-child))
                (gap-end (treesit-node-start curr-child)))
            (elisp-tred--hide-whitespace-in-range gap-start gap-end))
          (setq prev-child curr-child)))

      ;; Hide whitespace after the last child
      (let* ((last-child (car (last children)))
             (gap-start (treesit-node-end last-child))
             (gap-end (treesit-node-end node)))
        (elisp-tred--hide-whitespace-in-range gap-start gap-end))

      ;; Recursively process children
      (dolist (child children)
        (elisp-tred--hide-non-significant-whitespace child)))))

(defun elisp-tred--hide-whitespace-in-range (start end)
  "Hide all whitespace characters in the range from START to END."
  (when (< start end)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "[ \t\n\r]+" end t)
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put overlay 'category 'elisp-tred-whitespace)
          (overlay-put overlay 'invisible t))))))

(defun elisp-tred--remove-overlays ()
  "Remove all overlays created by elisp-tred.

Elisp-tred creates overlays to: (1) show the tree guides as virtual
text, and (2) to hide whitespace characters. There purpose of the
latter is to ensure a predictable/consistent layout of the tree."
    (remove-overlays nil nil 'category 'elisp-tred-guide)
    (remove-overlays nil nil 'category 'elisp-tred-whitespace))

;;;; Minor mode definition

(define-minor-mode elisp-tred-mode
  "Display and edit elisp code as a tree."
  :lighter " Tred"
  (if elisp-tred-mode
      (elisp-tred--treesit-init)
    (elisp-tred--remove-overlays)
    (elisp-tred--treesit-teardown)))

(provide 'elisp-tred)