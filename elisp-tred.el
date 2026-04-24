;;; ...  -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'seq)
(require 'track-changes)
(require 'treesit)

(defconst elisp-tred-grammar-version "1.0.0"
  "The version of the `tree-sitter-elisptred' grammar that is intended
to be used with this version elisp-tred.")

(defvar-local elisp-tred-max-label-length 80
  "The maximum length of a tree node label. For the sake of
performance, labels longer than this length will be truncated with an
ellipsis (\"...\").

It is important to impose a max length on the tree node labels because
when a node is collapsed, it shows the full lisp code for its subtree
in a single line, which can be very long indeed.")

(defvar elisp-tred--newline-regex
  "\\(\r\n\\|\n\\|\r\\)"
  "Regular expression that matches newlines on Linux, Mac, and Windows.")

(defvar elisp-tred--whitespace-regex
  "[ \t\n\r\f]+"
  "Regular expression for a sequence of one or more whitespace
characters, including tabs, newlines, and page breaks.")

;;; Tree-sitter

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
	;; Don't create or update Elisp-Tred overlays (e.g. tree guides)
	;; in the shadow buffer.
    ;;
    ;; We don't have any way to keep the Elisp-Tred overlays in the
	;; shadow buffer up-to-date with respect to user edits, because
	;; the shadow buffer does not have its own shadow buffer (that
	;; would be infinite recursion).
    (unless elisp-tred--update-shadow-buffer-p
      ;; Set up hooks for updating Elisp-Tred overlays (e.g. tree
      ;; guides) when the user edits the buffer.
      (add-hook 'pre-redisplay-functions #'elisp-tred--pre-redisplay nil t)
      ;; We pass `ignore' here because we don't use the Track-Changes.
	  ;; Instead, we check for pending changes in `elisp-tred--pre-redisplay'.
      (setq elisp-tred--update-change-tracker-id (track-changes-register #'ignore))
      ;; Create initial Elisp-Tred overlays for the entire buffer.
      (let ((root-node (treesit-parser-root-node parser)))
        (elisp-tred--create-overlays root-node nil)))))

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
      (remove-hook 'pre-redisplay-functions #'elisp-tred--pre-redisplay t)
      (when elisp-tred--update-change-tracker-id
        (track-changes-unregister elisp-tred--update-change-tracker-id))
      (treesit-parser-delete parser))))

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

(defun elisp-tred--node-for-current-line ()
  "Return the largest treesit node for the current line.

If there are multiple treesit nodes shown on the current line, we
return the largest node,i.e. the node closest to the root. For
example, if the current line contains both the opening paren (`(') for
a list and als the first element of the list, we would return the
treesit node for the list."
  (save-excursion
    (beginning-of-visual-line)
    (elisp-tred--treesit-node-at (point))))

(defun elisp-tred--treesit-node-overlaps-range-p (node beg end)
  "Return non-nil if the treesit node NODE overlaps the buffer range
[BEG, END]."
  (let ((node-beg (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (elisp-tred--ranges-overlap-p node-beg node-end beg end)))

(defun elisp-tred--treesit-top-level-node-for-pos (pos)
  "Return the treesit node for the top-level elisp form (e.g. `defun',
`defvar') that contains buffer position POS.

This function will return nil if POS is located within the whitespace
between top-level elisp forms."
  (car (elisp-tred--treesit-top-level-nodes-overlapping-range pos pos)))

(defun elisp-tred--treesit-top-level-nodes-overlapping-range (beg end)
  "Return a list of top level treesit nodes overlapping the buffer
range [BEG, END].

The top level treesit nodes correspond to top level elisp forms such
as `defun', `defmacro', `defvar', etc."
  (let* ((root-node (treesit-buffer-root-node 'elisptred))
         (top-level-node (treesit-node-first-child-for-pos root-node beg t))
         result)
    (while (and top-level-node
                (elisp-tred--treesit-node-overlaps-range-p top-level-node beg end))
      (push top-level-node result)
      (setq top-level-node (treesit-node-next-sibling top-level-node t)))
    (nreverse result)))

(defun elisp-tred--treesit-node-range (node)
  "Return the buffer range for treesit node NODE as a cons cell (BEG
. END)."
  (let ((beg (treesit-node-start node))
        (end (treesit-node-end node)))
    (cons beg end)))

(defun elisp-tred--treesit-nodes-range-union (nodes)
  "Return the union of the buffer ranges for NODES,
a list of treesit nodes.

The range is returned as a cons cell (BEG . END)."
  (let (range)
    (dolist (node nodes)
      (let ((node-range (elisp-tred--treesit-node-range node)))
        (setq range (elisp-tred--range-union range node-range))))
    range))

(defun elisp-tred--treesit-node-first-in-buffer-p (node)
  "Return non-nil if treesit node NODE is on the first line of the
buffer, and is preceded only by indentation whitespace.

The first elisp form in the file is a special case when creating tree
guides, because normally we create tree guides after the newline that
ends the previous line."
  (let ((beg (treesit-node-start node)))
    (save-excursion
      (goto-char beg)
      (let ((line-beg (point-at-bol)))
        (and (= line-beg (point-min))
             (= (+ line-beg (current-indentation)) beg))))))

(defun elisp-tred--treesit-node-top-level-p (node)
  "Return non-nil if treesit node NODE is a top-level elisp form
(e.g. `defun', `defmacro'), i.e. a child node of the root
`source_file' node.

Note: The function returns nil if NODE is the root `source_file'
node."
  (let ((parent (treesit-node-parent node))
        (root-node (treesit-buffer-root-node 'elisptred)))
    (treesit-node-eq parent root-node)))

(defun elisp-tred--treesit-node-atom-p (node)
  "Return non-nil if treesit node NODE is a lisp atom (e.g. a string,
a symbol name), as opposed to a nested type (e.g. a list, a vector)."
  (let ((type (treesit-node-type node)))
    (not (member type
                 '("list"
                   "vector"
                   "hash_table"
                   "bytecode"
                   "string_text_properties"
                   "quote"
                   "unquote"
                   "unquote_splice")))))

(defun elisp-tred--treesit-node-defun-defmacro-defvar-name-p (node)
  "Return non-nil if treesit node NODE is the declared name for a
function, macro, or variable."
  (when-let* ((parent (treesit-node-parent node))
              (parent-type (treesit-node-type parent))
              (children (treesit-filter-child parent #'elisp-tred--treesit-sexp-p t))
              (child0 (nth 0 children))
              (child0-text (treesit-node-text child0 t))
              (child1 (nth 1 children)))
	(and (treesit-node-eq node child1)
         (string= parent-type "list")
         (member child0-text '("defun" "defmacro" "defvar" "defvar-local" "defcustom")))))

(defun elisp-tred--treesit-node-defun-defmacro-arglist-p (node)
  "Return non-nil if treesit node NODE is the argument list for
a `defun' or `defmacro' declaration."
  (when-let* ((node-type (treesit-node-type node))
              (parent (treesit-node-parent node))
              (parent-type (treesit-node-type parent))
              (children (treesit-filter-child parent #'elisp-tred--treesit-sexp-p t))
              (child0 (nth 0 children))
              (child0-text (treesit-node-text child0 t))
              (child2 (nth 2 children)))
	(and (treesit-node-eq node child2)
         (string= parent-type "list")
         (string= node-type "list")
         (member child0-text '("defun" "defmacro")))))

(defun elisp-tred--treesit-node-defvar-atomic-value-p (node)
  "Return non-nil if treesit node NODE is the default variable value
in a `defvar'/`defvar-local'/`defcustom' declaration, and the value is
an atomic type (e.g. a string, a float).

Return nil if NODE is a non-atomic type (e.g. a list, a vector)."
  (when (elisp-tred--treesit-node-atom-p node)
    (when-let* ((parent (treesit-node-parent node))
                (parent-type (treesit-node-type parent))
                (children (treesit-filter-child parent #'elisp-tred--treesit-sexp-p t))
                (child0 (nth 0 children))
                (child0-text (treesit-node-text child0 t))
                (child2 (nth 2 children)))
	  (and (treesit-node-eq node child2)
           (string= parent-type "list")
           (member child0-text '("defvar" "defvar-local" "defcustom"))))))

(defun elisp-tred--treesit-node-defun-defmacro-arg-p (node)
  (when-let (parent (treesit-node-parent node))
    (elisp-tred--treesit-node-defun-defmacro-arglist-p parent)))

(defun elisp-tred--treesit-node-let-declarations-list-p (node)
  "Return non-nil if treesit node NODE is the list of
variable declarations for a `let' form."
  (when-let* ((parent (treesit-node-parent node))
              (parent-type (treesit-node-type parent))
              (children (treesit-filter-child parent #'elisp-tred--treesit-sexp-p t))
              (child0 (nth 0 children))
              (child0-text (treesit-node-text child0 t))
              (child1 (nth 1 children)))
    (and (treesit-node-eq node child1)
         (string= parent-type "list")
         (member child0-text '("let" "let*" "if-let" "if-let*" "when-let" "when-let*")))))

(defun elisp-tred--treesit-sexp-p (node)
  "Return non-nil if treesit node NODE is a sexp (symbolic
expression).

Generally, we use this predicate to filter out whitespace nodes
(i.e. treesit node types `newline' and `horizontal_whitespace') using
`treesit-filter-child'. The whitespace nodes are useful for certain
operations (e.g. whitespace formatting), but in many cases we want
write the code as if the whitespace nodes don't exist."
  (let ((node-type (treesit-node-type node)))
    (not (member node-type '("horizontal_whitespace" "newline")))))

(defun elisp-tred--treesit-node-newline-p (node)
  "Return non-nil if treesit node NODE is a newline.

Exception: We return nil for the newline character that appears at the
last character position in the buffer (if any), because we don't want
to create a tree guide for the final empty line."
  (and (string= (treesit-node-type node) "newline")
       ;; exclude final newline in file, because
       ;; we don't want to create a tree guide for that
       (/= (treesit-node-end node) (point-max))))

(defun elisp-tred--treesit-node-sexp-or-newline-p (node)
  "Return non-nil if treesit node NODE is an elisp sexp or a newline,
excluding the newline that appears at the last character position in
the buffer."
  (or (elisp-tred--treesit-sexp-p node)
      (elisp-tred--treesit-node-newline-p node)))

(defun elisp-tred--treesit-node-quoted-or-unquoted-p (node)
  "Return non-nil if treesit node NODE is a quoted or
unquoted elisp form.

Examples:

'(1 2 3)
`(1 2 3)
,(message \"hello!\")
@,(1 2 3)"
  (when-let* ((parent (treesit-node-parent node))
              (parent-type (treesit-node-type parent)))
    (member parent-type '("quote" "unquote" "unquote_splice"))))

(defun elisp-tred--treesit-node-structural-diff-p (node1 node2)
  "Return non-nil if treesit node NODE1 has a different structure than
treesit NODE2.

NODE1 and NODE2 are considered to have the same structure if they have
same treesit node types and number of children, recursively.

Note that the buffer ranges of NODE1 and NODE2 can be different while
still having the same tree structure. For example, this can happen
with adding whitespace between list members, which does not change the
structure of the treesit parse tree."
  (catch 'done
    (let ((node1-type (treesit-node-type node1))
          (node2-type (treesit-node-type node2)))
      (when (not (string= node1-type node2-type))
        (throw 'done t))
	  (let ((node1-children (treesit-filter-child node1 #'elisp-tred--treesit-sexp-p t))
            (node2-children (treesit-filter-child node2 #'elisp-tred--treesit-sexp-p t)))
        (elisp-tred--treesit-nodes-structural-diff-p node1-children node2-children)))))

(defun elisp-tred--treesit-nodes-structural-diff-p (nodes1 nodes2)
  "Return non-nil if two lists of treesit nodes, NODES1 and NODES2,
are structurally different.

NODES1 and NODES2 are considered to have the same structure if NODES1
and NODES2 have the same number of nodes, and the corresponding pairs
of nodes have the same structure.

A pair of treesit nodes are considered to have the same structure if
they have the same treesit node types (e.g. `list') and have the same
number of children, recursively. The comparison between individual
pairs of nodes is done with
`elisp-tred--treesit-node-structural-diff-p'."
  (or (/= (length nodes1) (length nodes2))
      (cl-some #'elisp-tred--treesit-node-structural-diff-p nodes1 nodes2)))

(defun elisp-tred--treesit-traversal (node visitor-func)
  "For the buffer region corresponding to treesit node NODE,
invoke VISITOR-FUNC on consecutive subregions that correspond to:

(1) The opening chars of a treesit NODE, e.g. the opening paren
`(' of the list `(one two)'.
(2) The whitespace separating adjacent parent/child/sibling treesit
nodes, e.g. the space ` ' between `one' and `two' in the list `(one
two)'.
(3) The closing chars of a treesit NODE, e.g. the closing paren
`)' of the list `(one two)'.
(4) The full range of chars for a treesit NODE, in the case that NODE
is a leaf node, e.g. the symbol `one' in the list `(one two)'.

VISITOR-FUNC is invoked with the following arguments:

* REGION-TYPE: one of `opening-chars', `whitespace', `closing-chars',
               or `all-chars'.
* NODE: target treesit NODE (nil when REGION-TYPE is `whitespace')
* BEG: start of the buffer region
* END: end of the buffer region

If the return value of VISITOR-FUNC is nil, the traversal will be
halted early. Otherwise, the traversal continues."
  (catch 'done
    (if-let ((children (treesit-filter-child node #'elisp-tred--treesit-sexp-p t)))
       (progn
         ;; Visit opening chars for parent, whitespace between parent
         ;; and first child (if any), and first child
         (let* ((first-child (car children))
                (gap-start (treesit-node-start node))
                (gap-end (treesit-node-start first-child)))
           (when (< gap-start gap-end)
             (save-excursion
               (goto-char gap-start)
               (if (re-search-forward elisp-tred--whitespace-regex gap-end t)
                   (let ((whitespace-start (match-beginning 0))
                         (whitespace-end (match-end 0)))
                     ;; Whitespace should extend to start of first child.
                     (cl-assert (eq whitespace-end gap-end))
                     (when (< gap-start whitespace-start)
				       (unless (funcall visitor-func 'opening-chars node gap-start whitespace-start)
                         (throw 'done nil)))
                     (when (< whitespace-start gap-end)
                       (unless (funcall visitor-func 'whitespace nil whitespace-start gap-end)
                         (throw 'done nil))))
                 ;; else: no whitespace found between parent and first child
                 (unless (funcall visitor-func 'opening-chars node gap-start gap-end)
                   (throw 'done nil)))))
           ;; recurse
           (elisp-tred--treesit-traversal first-child visitor-func))

         ;; Visit whitespace before children 2..N
         (let ((prev-child (car children)))
           (dolist (curr-child (cdr children))
             (let ((gap-start (treesit-node-end prev-child))
                   (gap-end (treesit-node-start curr-child)))
          	   (when (< gap-start gap-end)
                 ;; Gaps between siblings should only contain whitespace chars.
                 (cl-assert (elisp-tred--whitespace-p gap-start gap-end))
                 (unless (funcall visitor-func 'whitespace nil gap-start gap-end)
                   (throw 'done nil))))
		     ;; recurse
             (elisp-tred--treesit-traversal curr-child visitor-func)
             ;; set up for next loop iteration
             (setq prev-child curr-child)))

         ;; Visit whitespace between last child and parent (if any),
         ;; and closing chars of parent.
         (let* ((last-child (car (last children)))
                (gap-start (treesit-node-end last-child))
                (gap-end (treesit-node-end node)))
           (when (< gap-start gap-end)
             (save-excursion
               (goto-char gap-start)
               (if (re-search-forward elisp-tred--whitespace-regex gap-end t)
                   (let ((whitespace-start (match-beginning 0))
                         (whitespace-end (match-end 0)))
                     ;; Whitespace should start immediately after last child.
                     (cl-assert (eq whitespace-start gap-start))
				     (when (< gap-start whitespace-end)
                       (unless (funcall visitor-func 'whitespace nil gap-start whitespace-end)
                         (throw 'done nil)))
                     (when (< whitespace-end gap-end)
                       (unless (funcall visitor-func 'closing-chars node whitespace-end gap-end)
                         (throw 'done nil))))
                 ;; else: no whitespace found between last child and parent
                 (unless (funcall visitor-func 'closing-chars node gap-start gap-end)
                   (throw 'done nil)))))))
     ;; else: treesit node has no named children (leaf node)
      (funcall visitor-func 'all-chars node (treesit-node-start node) (treesit-node-end node)))))

(defun elisp-tred--treesit-traversal-test (node)
  "The function I used to test and debug
`elisp-tred--in-order-traversal'."
  (cl-labels ((visitor-func (region-type node beg end)
                (message "region (%s, %s), type = %s, node: %s"
                         beg
                         end
                         (symbol-name region-type)
                         (if node (treesit-node-type node) "nil"))))
    (elisp-tred--treesit-traversal node #'visitor-func)))

;;; Tree guide rendering

(defcustom elisp-tred-tree-guide-face 'shadow
  "The face for tree guide characters.")

(defvar elisp-tred--guide-spacer
  (propertize " " 'display '(space :width 0.5))
  "Horizontal space between tree guide handle and elisp code.")

(defun elisp-tred--make-tree-guide-string (str)
  "Apply formatting for tree guide strings."
  (propertize (concat str elisp-tred--guide-spacer)
              'face
              elisp-tred-tree-guide-face))

(defvar elisp-tred--guide
  (elisp-tred--make-tree-guide-string "│ ")
  "Vertical tree guide")

(defvar elisp-tred--guide-with-handle
  (elisp-tred--make-tree-guide-string "├─")
  "Vertical tree guide with handle")

(defvar elisp-tred--guide-with-handle-last
  (elisp-tred--make-tree-guide-string "╰─")
  "Vertical tree guide with handle, for last child")

(defvar elisp-tred--no-guide
  (elisp-tred--make-tree-guide-string "  ")
  "Horizontal space to replace vertical tree guide, for alignment")

(defun elisp-tred--first-child-p (node)
  "Return non-nil if the treesit node NODE is the first named child of
its parent node.

Note: If NODE has no parent treesit node (i.e. it is the root node of
the treesit parse tree), this function will return `nil'."
  (when-let* ((parent (treesit-node-parent node))
              (siblings (treesit-filter-child parent #'elisp-tred--treesit-sexp-p t)))
    (treesit-node-eq node (car siblings))))

(defun elisp-tred--last-child-p (node)
  "Return non-nil if the treesit node NODE is the last named child
of its parent node.

Note: If NODE has no parent treesit node (i.e. it is the root node of
the treesit parse tree), this function will return `nil'."
  (when-let* ((parent (treesit-node-parent node))
              (siblings (treesit-filter-child parent #'elisp-tred--treesit-sexp-p t)))
    (treesit-node-eq node (car (last siblings)))))

(defun elisp-tred--tree-guide-flags (node &optional flags)
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
      (elisp-tred--tree-guide-flags parent flags))
    flags))

(defun elisp-tred--tree-guide-flags-to-string (flags)
  "Convert the guide flags FLAGS to a string that will be shown in the
buffer.

See the docstring for `elisp-tred--tree-guide-flags' for an explanation
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

(defun elisp-tred--tree-guide-string (node)
  (when (elisp-tred--tree-guide-p node)
    (let ((guide-flags (elisp-tred--tree-guide-flags node)))
      (concat "\n" (elisp-tred--tree-guide-flags-to-string guide-flags)))))

(defun elisp-tred--tree-guide-string-at (&optional pos)
  "Copy the string of tree guide characters that appear immediately
before POS."
  (when-let* ((pos (or pos (point)))
              (overlay (elisp-tred--tree-guide-overlay-at pos))
              (guide-string (overlay-get overlay 'before-string)))
    (substring-no-properties guide-string)))

(defun elisp-tred--create-tree-guide-overlay-at (beg end folded guide-string)
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'category 'elisp-tred-guide)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'elisp-tred-folded folded)
    (overlay-put overlay 'before-string guide-string)))

(defun elisp-tred--create-tree-guide-overlays-for-string (node folded guide-flags)
  "Create tree guide overlays for a string, corresponding to
treesit node NODE.

We handle tree guides for strings specially, because strings can span
multiple lines in the buffer (e.g. docstrings), whereas other treesit
node types (e.g. symbol) always correspond to a single line in the
buffer. For multi-line strings, we create separate tree guide overlays
for each line, but we only show a handle for the first line."
  (let* ((guide-string-line0 (elisp-tred--tree-guide-flags-to-string guide-flags))
         (start (treesit-node-start node))
         (end (treesit-node-end node))
         (newline-prefix (unless (elisp-tred--treesit-node-first-in-buffer-p node) "\n")))
    (save-excursion
      (goto-char start)
      (if (re-search-forward elisp-tred--newline-regex end t)
          ;; If string has multiple lines, create a separate tree
          ;; guide overlay for each line.
          (let* ((guide-length (length elisp-tred--guide))
                 (last-child-p (elisp-tred--last-child-p node))
                 ;; Guide string for string lines 2..N (if any).
                 ;; Replace the guide chars for the last level, in order to
                 ;; remove the handle.
                 (guide-string-rest (concat (substring guide-string-line0 0 (- guide-length))
                                            (if last-child-p
                                                elisp-tred--no-guide
                                              elisp-tred--guide))))
            (elisp-tred--create-tree-guide-overlay-at start (point) folded (concat newline-prefix guide-string-line0))
            (let ((point-prev (point)))
              (while (re-search-forward elisp-tred--newline-regex end t)
                (elisp-tred--create-tree-guide-overlay-at point-prev (point) folded guide-string-rest)
                (setq point-prev (point)))
              (elisp-tred--create-tree-guide-overlay-at point-prev end folded guide-string-rest)))
        ;; else: not a multi-line string
        (elisp-tred--create-tree-guide-overlay-at start end folded (concat newline-prefix guide-string-line0))))))

(defun elisp-tred--create-tree-guide-overlays-for-node (node folded guide-flags)
  "Insert the tree guide overlay at the beginning of the line
for treesit node NODE. "
  (when-let ((guide-string (elisp-tred--tree-guide-flags-to-string guide-flags)))
    (let* ((node-type (treesit-node-type node))
           (start (treesit-node-start node))
           (end (treesit-node-end node))
           (newline-prefix (unless (elisp-tred--treesit-node-first-in-buffer-p node) "\n")))
     (if (equal node-type "string")
         (elisp-tred--create-tree-guide-overlays-for-string node folded guide-flags)
       (elisp-tred--create-tree-guide-overlay-at start end folded (concat newline-prefix guide-string))))))

(defun elisp-tred--tree-guide-overlay (node)
  "Return the tree guide overlay for treesit node NODE, or nil if NODE
does not have a tree guide overlay."
  (let* ((start (treesit-node-start node))
         (end (treesit-node-end node))
         (overlays (overlays-in start end)))
    (seq-find (lambda (overlay)
                (and (= (overlay-start overlay) start)
                     (= (overlay-end overlay) end)
                     (eq (overlay-get overlay 'category) 'elisp-tred-guide)))
              overlays)))

(defun elisp-tred--tree-guide-overlay-at (&optional pos)
  "Return the tree guide overlay that starts at POS, or `nil' if there
is no such overlay."
  (let* ((pos (or pos (point)))
         (overlays (overlays-in pos (1+ pos))))
    (seq-find (lambda (overlay)
                (and (= (overlay-start overlay) pos)
                     (eq (overlay-get overlay 'category) 'elisp-tred-guide)))
              overlays)))

(defun elisp-tred--tree-guide-p (node)
  "Return `t' if we should insert a tree guide overlay before
treesit node NODE, or `nil' otherwise."
  (not
      (or
       ;; Show open paren for `let' declarations on same line
       ;; as `let'.
       (elisp-tred--treesit-node-let-declarations-list-p node)
       ;; Show quoted lists/vectors on the same line as their
       ;; preceding quote/unquote.
       (elisp-tred--treesit-node-quoted-or-unquoted-p node)
       ;; 'defun'/`defmacro'/`defvar' forms:
       ;; Disable tree guides for the three list elements, so that
       ;; they are shown on the same line.
       ;; Example: "(defun hello-world (name)...".
       (elisp-tred--treesit-node-defun-defmacro-defvar-name-p node)
       (elisp-tred--treesit-node-defun-defmacro-arglist-p node)
       (elisp-tred--treesit-node-defun-defmacro-arg-p node)
       (elisp-tred--treesit-node-defvar-atomic-value-p node)
       ;; In most cases (see exceptions below), render the first child
       ;; of a list/vector on the same line as its opening
       ;; paren/bracket. This mimics how elisp code is typically
       ;; formatted, and greatly improves the vertical compactness of
       ;; the tree.
       ;;
       ;; For example, we render `(a b c)' as:
       ;;
       ;; ╰─ (a
       ;;    ├─ b
       ;;    ╰─ c)
       ;;
       ;; instead of:
       ;;
       ;; ╰─ (
       ;;    ├─ a
       ;;    ├─ b
       ;;    ╰─ c)
       ;;
       ;; Exceptions:
       ;;
       ;; (1) If the first child is a top-level node, i.e. it is a
       ;; child of the `source_file' root node, we render it
       ;; with a tree guide. Examples: a comment (`;; ...')  or
       ;; `defun' declaration on the first line of an elisp file.
       ;;
       ;; (2) If the first child is a complex type (e.g. a nested
       ;; list) we render it on a new line with its own tree guide. We
       ;; test for this case by checking if the first child has > 0
       ;; children.
       (and (elisp-tred--first-child-p node)
            (not (elisp-tred--treesit-node-top-level-p node))
            (eq (treesit-node-child-count node t) 0)))))

(defun elisp-tred--tree-guide-overlay-for-line (pos)
  "Return the tree guide overlay for the line containing POS, if any."
  (save-excursion
    (goto-char pos)
    (let ((overlays (overlays-in (line-beginning-position) (line-end-position))))
      (seq-find (lambda (overlay)
                  (eq (overlay-get overlay 'category) 'elisp-tred-guide))
                overlays))))

(defun elisp-tred--create-tree-guide-overlay-for-line-at-pos (pos guide-flags)
  "Create a tree guide overlay for the line containing POS.

If a tree guide overlay for the line containing POS already exists, do
nothing."
  ;; don't create tree guide overlay if it already exists
  (unless (elisp-tred--tree-guide-overlay-for-line pos)
    (when-let* ((guide-string (elisp-tred--tree-guide-flags-to-string guide-flags)))
      (save-excursion
        (goto-char pos)
        (beginning-of-line)
        (let* ((line-end (line-end-position))
               (overlay (make-overlay (point) (1+ line-end) nil t)))
          (overlay-put overlay 'category 'elisp-tred-guide)
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'line-prefix guide-string))))))

(defun elisp-tred--create-tree-guide-overlays (node _folded &optional guide-flags)
  "Create the tree guide overlays for treesit node NODE and all of its
descendants.

GUIDE-FLAGS is a list of booleans, one per ancestor node, that is used
to construct the tree guide lines."
  (let ((newline-p (elisp-tred--treesit-node-newline-p node))
        (first-in-buffer-p (elisp-tred--treesit-node-first-in-buffer-p node)))
    (when (or newline-p first-in-buffer-p)
      ;; We want the tree guide overlay to start *after* the newline
	  ;; that ends the previous line. The only exception is the tree
	  ;; guide that we create for the first line in the buffer, because
	  ;; there is no preceding newline in that case.
      (let ((pos (if newline-p
                     (treesit-node-end node)
                   (treesit-node-start node))))
        (elisp-tred-indent--hide-indentation pos)
        (elisp-tred--create-tree-guide-overlay-for-line-at-pos pos guide-flags))))
  (let* ((children (treesit-filter-child node #'elisp-tred--treesit-node-sexp-or-newline-p t))
         (children-newlines (treesit-filter-child node #'elisp-tred--treesit-node-newline-p t))
         (child-newline-last (car (last children-newlines)))
         (guide-flag t))
    (dolist (child children)
      (when (or (null children-newlines)
                (treesit-node-eq child child-newline-last))
        (setq guide-flag nil))
      (let* ((guide-flags (append guide-flags (list guide-flag))))
        (elisp-tred--create-tree-guide-overlays child _folded guide-flags)))))

(defun elisp-tred--tree-guide-at-point-p ()
  "Return non-nil if there is a tree guide overlay at point.

The existence of a tree guide overlay at point means that we
are at the beginning of a (visual) line."
  (let ((overlays (overlays-in (point) (1+ (point)))))
    (seq-find (lambda (overlay)
                (and (= (overlay-start overlay) (point))
                     (eq (overlay-get overlay 'category) 'elisp-tred-guide)))
			  overlays)))

(defun elisp-tred--tree-guide-pos-prev ()
  "Return the position of the previous tree guide overlay, relative to
point.

Note: This function will skip over the tree guide overlay located at
point (if any), and return the previous one instead."
  (catch 'done
    (save-excursion
      (while (not (bobp))
        (goto-char (previous-overlay-change (point)))
        (when (elisp-tred--tree-guide-at-point-p)
          (throw 'done (point)))))))

(defun elisp-tred--tree-guide-pos-next ()
  "Return the position of the next tree guide overlay, relative to
point.

Note: This function will skip over the tree guide overlay located at
point (if any), and return the next one instead."
  (catch 'done
    (save-excursion
      (while (not (eobp))
        (goto-char (next-overlay-change (point)))
        (when (elisp-tred--tree-guide-at-point-p)
          (throw 'done (point)))))))

;;; Whitespace rendering
;;
;; We use overlays to both hide real whitespace and to add virtual
;; whitespace (e.g. newlines), to ensure that the tree is always
;; rendered in a consistent manner with respect to the structure of
;; the code, rather than the user's personal choice of
;; whitespace/indentation.

(defun elisp-tred--whitespace-p (beg end)
  "Return non-nil if the buffer region between BEG and END
contains only whitespace characters."
  (save-excursion
    (goto-char beg)
    (when (re-search-forward elisp-tred--whitespace-regex end t)
      (and (eq (match-beginning 0) beg)
           (eq (match-end 0) end)))))

(defun elisp-tred--create-whitespace-overlays (node folded)
  "Hide all non-significant whitespace using overlays.

`Non-significant whitespace' means whitespace that is not contained
within a string or comment (e.g. indentation and newlines). We want
to hide all such whitespace so that we display the tree structure in a
consistent and predictable manner."
  (when-let ((children (treesit-filter-child node #'elisp-tred--treesit-sexp-p t)))
    (let* ((first-child (car children))
           (gap-start (treesit-node-start node))
           (gap-end (treesit-node-start first-child))
           (replacement (when (or folded (not (elisp-tred--tree-guide-p first-child))) " ")))
      (elisp-tred--hide-whitespace-in-range gap-start gap-end replacement))

    ;; Hide whitespace between consecutive children
    (let ((prev-child (car children)))
      (dolist (curr-child (cdr children))
        (let* ((gap-start (treesit-node-end prev-child))
               (gap-end (treesit-node-start curr-child))
               (replacement (when (or folded (not (elisp-tred--tree-guide-p curr-child))) " ")))
          (elisp-tred--hide-whitespace-in-range gap-start gap-end replacement))
        (setq prev-child curr-child)))

    ;; Hide whitespace after the last child
    (let* ((last-child (car (last children)))
           (gap-start (treesit-node-end last-child))
           (gap-end (treesit-node-end node))
           (replacement (when folded " ")))
      (elisp-tred--hide-whitespace-in-range gap-start gap-end replacement))

    ;; Recursively process children
    (dolist (child children)
      (elisp-tred--create-whitespace-overlays child folded))))

(defun elisp-tred--hide-whitespace-in-range (start end &optional replacement)
  "Hide each contiguous sequence of whitespace characters in the range
of START to END, using overlays.

If the optional string argument REPLACEMENT is provided, visually
replace each contiguous sequence of whitespace characters with
REPLACEMENT, using overlays. Otherwise, make the whitespace sequences
fully invisible."
  (when (< start end)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "[ \t\n\r]+" end t)
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0) nil t)))
          (overlay-put overlay 'category 'elisp-tred-whitespace)
          (if replacement
              (overlay-put overlay 'display replacement)
            (overlay-put overlay 'invisible t)))))))

;;; Indentation

(defun elisp-tred-indent--hide-indentation (pos)
  "Create an overlay that hides the leading whitespace on the line
containing POS.

We hide the leading whitespace so that we can display the tree guide
characters instead."
  (save-excursion
    (goto-char pos)
    (let* ((overlay-beg (line-beginning-position))
           (overlay-end (+ overlay-beg (current-indentation)))
           (overlay (make-overlay overlay-beg overlay-end nil t nil)))
      (overlay-put overlay 'category 'elisp-tred-whitespace)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'invisible t))))

(defun elisp-tred-indent--advice (orig-fn)
  "Advice for Emacs' built-in indent functions
(e.g. `lisp-indent-line', `lisp-indent-region'), that temporarily
unhides indentation whitespace on all lines.

Normally we hide the indentation whitespace and show the tree guides
instead. However, Emacs' built-in indent functions don't calculate the
correct column values unless they can 'see' the indentation whitespace
on the previous line.

To work around this, we add an advice that temporarily unhides all
invisible text, by setting `buffer-invisibility-spec' to nil."
  (let (buffer-invisibility-spec)
    (funcall orig-fn)))

(defun elisp-tred-indent--advice-init ()
  "Add advice to Emacs' built-in line/region indentation functions, so
that they work correctly in Elisp-Tred mode.

See the docstring for `elisp-tred-indent--advice' for further
explanation."
  (advice-add indent-line-function :around #'elisp-tred-indent--advice)
  (advice-add indent-region-function :around #'elisp-tred-indent--advice))

(defun elisp-tred-indent--advice-teardown ()
  "Remove advice from Emacs' built-in line/region indentation
functions.

See the docstring for `elisp-tred-indent--advice' for further
explanation."
  (advice-remove indent-line-function #'elisp-tred-indent--advice)
  (advice-remove indent-region-function #'elisp-tred-indent--advice))

;;; Folding
;;
;; Implements collapsing/expanding of the current line using the TAB
;; key.

(defvar-local elisp-tred--fold-toggle-top-level-state nil
  "Non-nil if all top-level nodes (e.g. `defun' forms) are currently folded.

This variable is toggled by `elisp-tred-fold-toggle-top-level'.")

(defun elisp-tred--create-fold-overlay (beg end)
  "Create an overlay for a folded multi-line string, where lines 2..N
are hidden and an ellipsis (`...') is shown instead."
  (let ((overlay (make-overlay beg end nil t)))
    (overlay-put overlay 'category 'elisp-tred-fold)
    (overlay-put overlay 'display "...")
    (overlay-put overlay 'cursor-sensor-functions (list #'elisp-tred--fold-cursor-sensor-function))
    overlay))

(defun elisp-tred--first-newline-pos (node)
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (save-excursion
      (goto-char node-start)
      (when (re-search-forward elisp-tred--newline-regex node-end t)
        (match-beginning 0)))))

(defun elisp-tred--create-fold-overlay-for-string (node)
  "Create an overlay that `folds' the string corresponding to NODE,
where NODE is a treesit node with type `string'.

If NODE is a multi-line string, a new overlay is create that hides
lines 2..N and replaces them with an ellipsis (`...'). If NODE is a
single-line string, nothing needs to be done and therefore no overlay
is created."
  (let* ((start (treesit-node-start node))
         (end (treesit-node-end node))
         (overlay (make-overlay start start nil t)))
    (save-excursion
      (goto-char start)
      (when (re-search-forward elisp-tred--newline-regex end t)
        (goto-char (match-beginning 0))
        ;; use `(1- end)' so closing quotes (`"') are not hidden
        (elisp-tred--create-fold-overlay (point) (1- end))))))

(defun elisp-tred--create-fold-overlays-for-strings (node)
  "Fold all multi-line strings under treesit node NODE, such that only
the first line is shown, followed by an ellipsis (`...')."
  (when (equal (treesit-node-type node) "string")
    (elisp-tred--create-fold-overlay-for-string node))
  (mapc #'elisp-tred--create-fold-overlays-for-strings
      (treesit-filter-child node #'elisp-tred--treesit-sexp-p t)))

(defun elisp-tred--fold-end-pos (node)
  "Return the position the last character we are allowed to hide
when folding treesit node NODE, plus one.

In many cases, the last character we are allowed to hide is the
character before `(treesit-node-end node)'.  However, is the case
where NODE is the last child of its parent, we also need to include
the closing characters of the parent node (e.g. the closing paren of a
list), and so on recursively, up to the root of the treesit parse
tree."
  (if (elisp-tred--last-child-p node)
      (elisp-tred--fold-end-pos (treesit-node-parent node))
    (treesit-node-end node)))

(defun elisp-tred--fold-visitor-create (root-node)
  "Create and return a visitor function that we can use with
`elisp-tred--treesit-traversal', that will fold the subtree rooted at
treesit node NODE.

NOTE: We need the extra level of indirection provided by this
function-that-returns-a-function because
`elisp-tred--treesit-traversal' does not invoke the visitor function
with all of the arguments we need. In particular, we also need to know
the total number of visible characters we have visited so far
(i.e. the working length), and the root node for the subtree that we
are folding."
  (let ((fold-end-pos (elisp-tred--fold-end-pos root-node))
        (length 0))
    (lambda (region-type node beg end)
      (let ((node-type (treesit-node-type node))
            (region-length (- end beg)))
        (cond
          ;; Special handling for whitespace.
          ;; Collapse any contiguous sequence of whitespace chars to
          ;; single space (" ").
          ((eq region-type 'whitespace)
           (if (>= length elisp-tred-max-label-length)
               ;; Optimization: Return `nil' to halt further traversal.
               (progn (elisp-tred--create-fold-overlay beg fold-end-pos) nil)
             (elisp-tred--hide-whitespace-in-range beg end " ")
             (setq length (1+ length))))
          ;; Special handling for comments.
          ;; Hide comments doesn't make sense to show them within folded nodes.
          ((equal node-type "comment")
           (unless (treesit-node-eq node root-node)
             (elisp-tred--create-fold-overlay beg end)))
          ;; Special handling for strings.
          ;;
          ;; Unlike other treesit nodes types (e.g. constants,
          ;; symbols) that are only displayed if they fully fit within
          ;; `elisp-tred-max-label-length', allow truncating the
          ;; string if needed. This is especially helpful for reading
          ;; the docstrings of folded function/variable/macro nodes.
          ;;
          ;; Exception: In the case that the user is folding the
          ;; string node directly (i.e. the string is `root-node'), we
          ;; truncate at the first newline rather than
          ;; `elisp-tred-max-label-length', because that is more
          ;; intuitive behaviour.
          ((equal node-type "string")
           (if (treesit-node-eq node root-node)
               (elisp-tred--create-fold-overlay-for-string node)
             (let* ((max-length (- elisp-tred-max-label-length length))
                    (max-length-pos (+ beg max-length))
                    (first-newline-pos (elisp-tred--first-newline-pos node)))
               (if (or first-newline-pos (> region-length max-length))
                   (let ((fold-start-pos (if first-newline-pos
                                             (min first-newline-pos max-length-pos)
                                           max-length-pos)))
                     (elisp-tred--create-fold-overlay fold-start-pos fold-end-pos))
                 ;; whole string fits under length limit
                 (setq length (+ length region-length))))))
          ;; Default behaviour for all other treesit node types:
          ;; Append characters for `node' only if it fits within
          ;; length limit. Otherwise hide the remainder of the treesit
          ;; subtree and finish traversal.
          (t (if (> (+ length region-length) elisp-tred-max-label-length)
            ;; Optimization: Return `nil' to halt further traversal.
            (progn (elisp-tred--create-fold-overlay beg fold-end-pos) nil)
          (setq length (+ length region-length)))))))))

(defun elisp-tred--create-fold-overlays (node)
  "Fold the subtree rooted at treesit node NODE, by collapsing the
elisp code to a single line, and truncating characters beyond the
length limit specified by `elisp-tred-max-label-length'."
  (let ((visitor-func (elisp-tred--fold-visitor-create node)))
    (elisp-tred--treesit-traversal node visitor-func)))

(defun elisp-tred--folded-p (node)
  "Return non-nil if treesit node NODE is currently folded."
  (when-let* ((overlay (elisp-tred--tree-guide-overlay node)))
    (overlay-get overlay 'elisp-tred-folded)))

(defun elisp-tred--set-node-folded (node folded)
  "Set folded state of treesit node NODE."
  (when (not (eq (elisp-tred--folded-p node) folded))
    (elisp-tred--remove-overlays node)
    (elisp-tred--create-overlays node folded)))

(defun elisp-tred--set-current-line-folded (folded)
  "Set folded state of treesit node on current line."
  (when-let ((node (elisp-tred--node-for-current-line)))
    (elisp-tred--set-node-folded node folded)))

(defun elisp-tred--fold-cursor-sensor-function (_window _old-pos direction)
  "Automatically unfold the treesit node for a top-level elisp form
(e.g. `defun', `defvar') when the cursor enters folded text is hidden.

This function is triggered by `cursor-sensor-functions' text property.
For an explanation of the arguments, see the documentation for
`cursor-sensor-mode'."
  (when (eq direction 'entered)
    (when-let ((top-level-node (elisp-tred--treesit-top-level-node-for-pos (point))))
      (elisp-tred--set-node-folded top-level-node nil))))

(defun elisp-tred-fold-toggle-current-line ()
  "Toggle the folded/unfolded state of the current line."
  (interactive)
  (when-let* ((node (elisp-tred--node-for-current-line)))
    (let ((folded (elisp-tred--folded-p node)))
      (elisp-tred--set-node-folded node (not folded)))))

(defun elisp-tred-fold-toggle-top-level ()
  "Toggle the folded/unfolded state of all top-level nodes
(e.g. `defun', `defmacro', `defvar', etc.)"
  (interactive)
  ;; set new global folding state
  (setq elisp-tred--fold-toggle-top-level-state (not elisp-tred--fold-toggle-top-level-state))
  ;; update fold overlays to match new state
  (let ((root-node (treesit-buffer-root-node 'elisptred)))
    (dolist (top-level-node (treesit-filter-child root-node #'elisp-tred--treesit-sexp-p t))
      (elisp-tred--set-node-folded top-level-node elisp-tred--fold-toggle-top-level-state))))

;;; High-level overlay functions
;;
;; High-level functions for creating/removing elisp-tred overlays
;; (tree guides and whitespace).

(defun elisp-tred--remove-all-overlays-in-buffer ()
  "Remove all overlays created by elisp-tred.

Elisp-tred creates overlays to: (1) show the tree guides as virtual
text, and (2) to hide whitespace characters. The purpose of the latter
is to ensure a consistent rendering of the tree based on code
structure, rather than the author's personal preferences for
whitespace/indentation."
    (remove-overlays nil nil 'category 'elisp-tred-guide)
    (remove-overlays nil nil 'category 'elisp-tred-whitespace)
    (remove-overlays nil nil 'category 'elisp-tred-fold))

(defun elisp-tred--remove-overlays (node)
  "Remove tree guide and whitespace overlays for treesit node NODE."
  (let ((start (treesit-node-start node))
        (end (elisp-tred--fold-end-pos node)))
    (remove-overlays start end 'category 'elisp-tred-guide)
    (remove-overlays start end 'category 'elisp-tred-whitespace)
    (remove-overlays start end 'category 'elisp-tred-fold)))

(defun elisp-tred--create-overlays (node folded)
  "Create tree guide and whitespace overlays for treesit node NODE."
  (let ((tree-guide-flags (elisp-tred--tree-guide-flags node))
        (start (treesit-node-start node))
        (end (treesit-node-end node)))
    ;; (if folded
    ;;     ;; (elisp-tred--create-fold-overlays-for-strings node)
    ;;     (elisp-tred--create-fold-overlays node)
    ;;   (elisp-tred--create-whitespace-overlays node nil))
    (elisp-tred--create-tree-guide-overlays node folded tree-guide-flags)))

(defun elisp-tred--update-overlays (node)
  "Update all elisp-tred overlays for treesit node NODE, namely tree
guide overlays, whitespace overlays, and fold overlays.

We need to update the overlays after the user makes an edit to elisp
code, in order to ensure that overlays (e.g. tree guides) stay in sync
with the structure of the code."
  (let ((folded-p (elisp-tred--folded-p node)))
    (elisp-tred--remove-overlays node)
    (elisp-tred--create-overlays node folded-p)))

;;; Buffer/column position calculations

(defvar-local elisp-tred--pos-goal-column nil
  "The target column pos we will move to, when moving up/down a line.

The value of this variable remains the same during any sequence
of repeated movement commands in the same direction (e.g.
`elisp-tred-line-previous').")

(defun elisp-tred--pos-buffer-to-column (buffer-pos)
  "Return the column position of the character at BUFFER-POS.

If BUFFER-POS corresponds to an invisible character, return the the
column position of the first visible character that precedes it."
  (catch 'done
    (save-excursion
      (goto-char buffer-pos)
      ;; edge case: already at beginning of line
      (when (elisp-tred--tree-guide-at-point-p)
        (throw 'done 0))
      (let ((line-start-pos (or (elisp-tred--tree-guide-pos-prev) (point-min)))
            (point-prev (point))
            (column-pos 0))
        (goto-char (previous-single-char-property-change point-prev 'invisible nil line-start-pos))
        (while (/= point-prev (point))
          (unless (invisible-p (point))
            (setq column-pos (+ column-pos (- point-prev (point)))))
          (setq point-prev (point))
          (goto-char (previous-single-char-property-change (point) 'invisible nil line-start-pos)))
        column-pos))))

(defun elisp-tred--pos-column-to-buffer (column-pos)
  "Return the buffer position for the character at COLUMN-POS on the
current line.

If COLUMN-POS is greater than the number of visible characters on the
current line, return the buffer position corresponding to the last
visible character on the current line."
  (catch 'done
    (save-excursion
      ;; go to start of line
      (unless (elisp-tred--tree-guide-at-point-p)
        (if-let ((pos (elisp-tred--tree-guide-pos-prev)))
            (goto-char pos)
          (throw 'done nil)))
      (let ((bound (or (elisp-tred--tree-guide-pos-next) (point-max)))
            (distance column-pos)
            last-visible-pos)
        (while (< (point) bound)
          (if (invisible-p (point))
              (goto-char (next-single-char-property-change (point) 'invisible))
            (let* ((next-invisible-pos (next-single-char-property-change (point) 'invisible nil bound))
                   (visible-chars (- next-invisible-pos (point))))
              (if (> visible-chars distance)
                  (throw 'done (+ (point) distance))
                (setq distance (- distance visible-chars))
                (setq last-visible-pos (1- next-invisible-pos))
                (goto-char next-invisible-pos)))))
        ;; `column-pos' was greater than number of visible chars on line.
        ;; Return position of last visible char on line.
        last-visible-pos))))

(defun elisp-tred--pos-line-start (&optional pos)
  "Return the position of the first visible character, in the (visual)
line containing POS.

If POS is `nil', using `(point)' instead."
  (catch 'done
    (save-excursion
      (when pos (goto-char pos))
      (if (elisp-tred--tree-guide-at-point-p)
          (throw 'done (point))
        (throw 'done (elisp-tred--tree-guide-pos-prev))))))

(defun elisp-tred--pos-line-end (&optional pos)
  "Return the position one after the last visible character, in the
(visual) line containing POS.

If POS is `nil', using `(point)' instead."
  (catch 'done
    (save-excursion
      (when pos (goto-char pos))
      (throw 'done (1+ (elisp-tred--pos-column-to-buffer most-positive-fixnum))))))

(defun elisp-tred--pos-line-prev (goal-column-pos)
  "Calculate the buffer position for column position GOAL-COLUMN-POS
on the next line (relative to point).

If there is no next line, return nil.

If GOAL-COLUMN-POS is greater than the number of visible characters on
the next line, return the buffer position corresponding to the last
visible character on the next line."
  (catch 'done
    (save-excursion
      ;; go to start of current line
      (unless (elisp-tred--tree-guide-at-point-p)
        (if-let ((pos (elisp-tred--tree-guide-pos-prev)))
           (goto-char pos)
         (throw 'done nil)))
      ;; go to start of previous line
      (if-let ((pos (elisp-tred--tree-guide-pos-prev)))
          (goto-char pos)
        (throw 'done nil))
      (elisp-tred--go-to-column-pos goal-column-pos)
      (point))))

(defun elisp-tred--pos-line-next (goal-column-pos)
  "Calculate the buffer position for column position GOAL-COLUMN-POS
on the previous line (relative to point).

If there is no previous line, return nil.

If GOAL-COLUMN-POS is greater than the number of visible characters on
the previous line, return the buffer position corresponding to the
last visible character on the previous line."
  (catch 'done
    (save-excursion
      (if-let ((pos (elisp-tred--tree-guide-pos-next)))
          (goto-char pos)
        (throw 'done nil))
      (elisp-tred--go-to-column-pos goal-column-pos)
      (point))))

;;; Movement commands
;;
;; I implement my own line movement commands for this mode because
;; they perform better and have more predictable behaviour.
;;
;; Using Emacs' default `previous-line'/`next-line' commands with
;; `elisp-tred-mode' is a very confusing/frustrating experience,
;; because `elisp-tred-mode' changes the visual location of newlines
;; via overlays.
;;
;; In theory, Emacs' built-in `visual-line-mode' should solve the
;; problem of navigating by visual newlines, but in practice I find
;; `visual-line-mode' to be very laggy in `elisp-tred-mode', and the
;; cursor often jumps to unexpected places.

(defun elisp-tred--go-to-column-pos (column-pos)
  "Go the COLUMN-POS on the current line.

If COLUMN-POS is greater than the number of visible characters on the
current line, go to the last visible character on the current line
instead."
  (when-let ((pos (elisp-tred--pos-column-to-buffer column-pos)))
    (goto-char pos)))

;;; Helper functions for buffer ranges

(defun elisp-tred--range-contains-pos-p (beg end pos)
  "Return non-nil if range [BEG, END] contains POS."
  (and (>= pos beg) (<= pos end)))

(defun elisp-tred--range-union (range1 range2)
  "Return the union of RANGE1, RANGE2, and any interval that separates
them.

In other words, always return a single continuous range that spans
RANGE1 and RANGE2, even when RANGE1 and RANGE2 don't overlap.

RANGE1, RANGE2, and the return value are cons cells, where the car
is the start of the range and the cdr is the end of the range.

RANGE1 and/or RANGE2 may be nil. If both RANGE1 and RANGE2 are nil,
then the returned range is nil. If only RANGE1 is nil then the
returned range is RANGE2. If only RANGE2 is nil, then the returned
range is RANGE1."
  (cond
   ((not range1) range2)
   ((not range2) range1)
   ((and range1 range2)
    (let ((range1-beg (car range1))
            (range1-end (cdr range1))
            (range2-beg (car range2))
            (range2-end (cdr range2)))
        (cons (min range1-beg range2-beg)
              (max range1-end range2-end))))))

(defun elisp-tred--ranges-overlap-p (range1-beg range1-end range2-beg range2-end)
  "Return non-nil if the range [RANGE1-BEG, RANGE1-END] overlaps
[RANGE2-BEG, RANGE2-END]."
  (or (elisp-tred--range-contains-pos-p range1-beg range1-end range2-beg)
      (elisp-tred--range-contains-pos-p range1-beg range1-end range2-end)
      (elisp-tred--range-contains-pos-p range2-beg range2-end range1-beg)
      (elisp-tred--range-contains-pos-p range2-beg range2-end range1-end)))

(defun elisp-tred--range-extend-to-top-level-treesit-nodes (beg end)
  "Extend the range [BEG, END] to combined range of the top-level
treesit nodes (e.g. `defun', `defmacro') that overlap [BEG, END]."
  (let ((overlapping-top-level-nodes (elisp-tred--treesit-top-level-nodes-overlapping-range beg end)))
    (elisp-tred--treesit-nodes-range-union overlapping-top-level-nodes)))

;;; Live updates during editing

(defvar-local elisp-tred--update-change-tracker-id nil
  "The change tracker ID returned by `track-changes-register'.

Passed to `track-changes-fetch' to retrieve a description of the
latest changes the user has made to the buffer (if any), so we can
Elisp-Tred tree structure in sync with the elisp code.")

(defvar-local elisp-tred--update-shadow-buffer nil
  "The shadow buffer for this Elisp-Tred buffer. The shadow buffer is
a hidden clone of an Elisp-Tred buffer that is used for
change-tracking.

The text content of the shadow buffer is kept in sync with the main
buffer, minus the user's most recent buffer edit (as captured by the
built-in Track-Changes library).

You can determine if the current buffer is the main Elisp-Tred or the
shadow buffer by checking the value of
`elisp-tred--update-shadow-buffer-p'.  You can also visually
distinguish the main buffer from the shadow buffer, because the shadow
buffer does not have any overlays (e.g. tree guides).")

(defvar-local elisp-tred--update-shadow-buffer-p nil
  "Non-nil if this buffer is a shadow buffer. The shadow buffer is a
hidden clone of an Elisp-Tred buffer that is used for
change-tracking.")

(defun elisp-tred--update-shadow-buffer-init ()
  "Create the shadow buffer for the current Elisp-Tred buffer.

The shadow buffer is a hidden clone of an Elisp-Tred buffer that is
used for change-tracking."
  ;; prevent infinite recursion (don't create a shadow buffer
  ;; for the shadow buffer)
  (unless elisp-tred--update-shadow-buffer-p
    (let* ((buffer (current-buffer))
          (buffer-content (buffer-string))
          (buffer-mode major-mode)
          (shadow-buffer-name (format "*elisp-tred-shadow: %s*" (buffer-name)))
          (shadow-buffer (get-buffer-create shadow-buffer-name t)))
	 (with-current-buffer shadow-buffer
       (erase-buffer)
       (insert buffer-content)
	   (funcall buffer-mode)
       (setq elisp-tred--update-shadow-buffer-p t)
       (elisp-tred-mode))
     (setq elisp-tred--update-shadow-buffer shadow-buffer))))

(defun elisp-tred--update-shadow-buffer-teardown ()
  "Destroy the shadow buffer for the current Elisp-Tred buffer.

The shadow buffer is a hidden clone of an Elisp-Tred buffer that is
used for change-tracking."
  (when elisp-tred--update-shadow-buffer
	(kill-buffer elisp-tred--update-shadow-buffer)))

(defun elisp-tred--update-shadow-buffer-apply-change (change)
  "Apply a buffer edit to the shadow buffer, as described by CHANGE.

CHANGE is a list consisting of:

(1) BEG, the start position of the changed text
(2) END, the end position of the changed text
(3) BEFORE, a string containing the previous text content of the (BEG,
END) range

We call this function to re-synchronize the shadow buffer after
applying a change in the main Elisp-Tred buffer."
  (let* ((beg (nth 0 change))
         (end (nth 1 change))
         (before (nth 2 change))
         (after (buffer-substring-no-properties beg end)))
    (with-current-buffer elisp-tred--update-shadow-buffer
      (goto-char beg)
	  (delete-char (length before))
      (insert after))))

(defun elisp-tred--update-range-calculate (change)
  "Return the buffer range for which elisp-tred overlays need to be
updated, in response to the user's most recent buffer edit, as
described by CHANGE.

CHANGE is a list consisting of:

(1) BEG, the start position of the changed text
(2) END, the end position of the changed text
(3) BEFORE, a string containing the previous text content of the (BEG,
END) range

The purpose of the updating the Elisp-Tred overlays is to ensure that
the structure of the Elisp-Tred tree remains in sync with the
structure of the elisp code.

Determining the correct/minimal buffer range for updating the
Elisp-Tred overlays is a challenging problem. The trickiest cases
occur when the user performs an edit that results in unbalanced
parentheses (e.g. adding/deleting a single paren), which in certain
cases can change the tree structure for entire rest of the buffer,
i.e. from the first modified character to the end of the buffer. It
really helps to think about some small examples to understand
this. For example, consider what happens when we delete the closing
paren after the `b' in the following list:

`(a (b) c)'

Initially, the tree looks like:

╰─ (a
   ├─ (b)
   ╰─ c)

But after deleing closing paren, resulting in `(a (b c)', the tree
looks like:

╰─ (a
   ╰─ (b
      ╰─ c)

To help with the calculation of the update range, Elisp-Tred's
maintains a \"shadow buffer\", which is an exact clone of the main
buffer, but without the user's most recent edit applied. This
allows us to easily compare the state of the treesit parse tree
before and after the user's edit.

To simplify the calculation of the buffer update range, we always
update top-level elisp forms (e.g. `defun', `defvar', etc.) as atomic
units. In other words, each top-level elisp form will either be fully
included or fully excluded from the buffer update region, and never
anything in between. This isn't necessarily optimal, but it helps make
the code easier to understand and reason about.

The current algorithm to compute the buffer update range is as
follows:

(1) Calculate `top-level-nodes-before', the list of top-level
treesit nodes that overlap CHANGE in the shadow buffer.
(2) Calculate `top-level-nodes-after', the list of top-level
treesit nodes that overlap CHANGE in the main Elisp-Tred buffer.
(3) Calculate `update-range-before', the buffer range that corresponds
to `top-level-nodes-before' in the shadow buffer.
(4) Calculate `update-range-before-adjusted', which is
`update-range-before', but with the end position updated to reflect
the net number of characters that were added/deleted during the user's
most recent edit operation. In other words, adjust the region so that
it matches the main Elisp-Tred buffer, rather than the shadow buffer.
(5) Calculate `update-range-after', the buffer range that corresponds
to `top-level-nodes-after' in the main Elisp-Tred buffer.
(6) Calculate `update-range-union', the union of `update-range-before'
and `update-range-after'.
(7) Return `update-range-union', but with the start position
clamped to the start position of CHANGE."
  (let* ((beg (nth 0 change))
         (end (nth 1 change))
         (before (nth 2 change))
         (top-level-nodes-before
          (with-current-buffer elisp-tred--update-shadow-buffer
            (elisp-tred--treesit-top-level-nodes-overlapping-range beg (+ beg (length before)))))
         (top-level-nodes-after
          (elisp-tred--treesit-top-level-nodes-overlapping-range beg end))
         (update-range-before (elisp-tred--treesit-nodes-range-union top-level-nodes-before))
         (update-range-before-beg (car update-range-before))
         (update-range-before-end (cdr update-range-before))
         (change-chars-delta (- (- end beg) (length before)))
         (update-range-before-adjusted (cons update-range-before-beg (+ update-range-before-end change-chars-delta)))
         (update-range-after (elisp-tred--treesit-nodes-range-union top-level-nodes-after))
		 (update-range-union (elisp-tred--range-union update-range-before-adjusted update-range-after)))
    ;; Clamp update region to beginning of CHANGE.
	;; A buffer edit can only affect the tree structure in buffer
	;; region *after* the edit position, not before.
    (cons beg (cdr update-range-union))))

(defvar-local elisp-tred--pre-redisplay-tick nil
  "The last `buffer-chars-modified-tick' that we've processed.  This
is used to work the bug/quirk that Emacs calls its redisplay hooks
multiple times for the same redisplay event, and the exact number of
hook invocations isn't even predictable.")

(defun elisp-tred--update-tree-structure-changed-p (change)
  "Return non-nil if the given buffer edit CHANGE would change the
structure of the treesit parse tree.

Certain types of buffer edits, such adding whitespace or renaming a
symbol, do not have any effect on the treesit parse tree."
  (let* ((beg (nth 0 change))
         (end (nth 1 change))
         (before (nth 2 change))
         (top-level-nodes-before
          (with-current-buffer elisp-tred--update-shadow-buffer
            (elisp-tred--treesit-top-level-nodes-overlapping-range beg (+ beg (length before)))))
         (top-level-nodes-after
          (elisp-tred--treesit-top-level-nodes-overlapping-range beg end)))
    (elisp-tred--treesit-nodes-structural-diff-p
     top-level-nodes-before
     top-level-nodes-after)))

(defun elisp-tred--update-change-get ()
  "Return the current set of pending user buffer edits, combined into
a single change description.

A buffer edit is \"pending\" if we have not yet made the corresponding
updates to the Elisp-Tred overlays, to ensure that the structure of
the Elisp-Tred tree stays in sync with the structure of the elisp
code.

The returned change description is a list consisting of:

(1) BEG, the start position of the changed text
(2) END, the end position of the changed text
(3) BEFORE, a string containing the previous text content of the (BEG,
END) range"
  (let* ((change-tracker-id elisp-tred--update-change-tracker-id)
         (fetch-changes-callback #'elisp-tred--update-track-changes-fetch-callback))
	(track-changes-fetch change-tracker-id fetch-changes-callback)))

(defun elisp-tred--update ()
  "Update elisp-tred overlays so the tree structure reflects the
user's most recent edits to the buffer."
  (when-let* ((change (elisp-tred--update-change-get))
              (update-range (elisp-tred--update-range-calculate change))
              (update-beg (car update-range))
              (update-end (cdr update-range))
              (nodes-to-update (elisp-tred--treesit-top-level-nodes-overlapping-range update-beg update-end)))
    (message "change: %s" change)
    (dolist (node nodes-to-update)
      (elisp-tred--update-overlays node))
    ;; re-sync shadow buffer to main buffer, in preparation
    ;; for user's next edit
    (elisp-tred--update-shadow-buffer-apply-change change)))

(defun elisp-tred--pre-redisplay (&rest _)
  "Force reparse of treesit parser, which will trigger notifiers.
This function is added to `pre-redisplay-functions' to ensure that
the parse tree is updated before redisplay, similar to how treesit.el
handles font-lock updates."
  (unless (eq elisp-tred--pre-redisplay-tick (buffer-chars-modified-tick))
    (elisp-tred--update)
    (setq elisp-tred--pre-redisplay-tick (buffer-chars-modified-tick))))

(defun elisp-tred--update-track-changes-fetch-callback (beg end before)
  (message "track-changes-fetch: (%s, %s) (before: \"%s\")" beg end before)
  (list beg end before))

;;; Render formatted elisp-tred tree to kill ring, buffer, string
;;
;; Note: When the user copies text from an elisp-tred buffer, they get
;; the raw text from the underlying file, not the formatted tree.
;; That is just how overlays work in Emacs -- the user cannot position
;; the cursor on the virtual characters added by overlays (e.g. tree
;; guide characters), which means that the text is neither selectable
;; nor copyable.
;;
;; This section implements specialized functions that make it possible
;; to copy the formatted elisp-tred tree, which is super useful for
;; documentation and tests.

(defun elisp-tred--render-region-to-string (beg end)
  "Return a multi-line string containing the formatted elisp-tred
tree, for the (full) lines overlapping the active region (BEG, END)."
  (let ((beg (elisp-tred--pos-line-start beg))
        (end (elisp-tred--pos-line-end end))
        yank-start-pos
        yanked-text-parts)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (if (invisible-p (point))
            ;; yank preceding visible chars (if any)
            (when yank-start-pos
              (push (buffer-substring-no-properties yank-start-pos (point)) yanked-text-parts)
              (setq yank-start-pos nil))
          ;; else: current text is visible
          (when (elisp-tred--tree-guide-at-point-p)
            ;; yank previously accumulated text before tree guide
            (when yank-start-pos
              (push (buffer-substring-no-properties yank-start-pos (point)) yanked-text-parts)
              (setq yank-start-pos nil))
            (push (elisp-tred--tree-guide-string-at) yanked-text-parts))
          (unless yank-start-pos
            (setq yank-start-pos (point))))
		(goto-char (next-overlay-change (point))))
	  ;; yank final part of last line
      (when (and yank-start-pos (< yank-start-pos end))
        (push (buffer-substring-no-properties yank-start-pos end) yanked-text-parts)))
    (apply #'concat (nreverse yanked-text-parts))))

(defun elisp-tred--render-buffer-to-string (&optional buffer)
  "Return a multi-line string with formatted elisp-tred tree from BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (unless elisp-tred-mode
      (user-error "target buffer is not an elisp-tred buffer"))
    (elisp-tred--render-region-to-string (point-min) (point-max))))

(defun elisp-tred--render-region-as-kill (beg end)
  "Copy the selected lines to the kill ring, with all elisp-tred
formatting in tact.

In other words, copy the lines as they appear to the user after
applying overlays for tree guides, whitespace-hiding, and folding. In
contrast, a normal Emacs copy operation with `copy-region-as-kill'
copies the characters from the underlying buffer without any overlays
applied.

This function is useful for copying tree renderings from elisp-tred
buffers, for use in documentation and tests."
  (interactive "r")
  (kill-new (elisp-tred--render-region-to-string beg end)))

(defun elisp-tred-render-region-replace (beg end)
  "Replace the selected elisp form(s) with the formatted Elisp-Tred
tree."
  (interactive "r")
  (let* ((orig-text (buffer-substring-no-properties beg end))
         (replacement-text (with-temp-buffer
                             (insert orig-text)
                             (elisp-tred-mode)
                             (elisp-tred--render-buffer-to-string))))
    (save-excursion
      	(goto-char beg)
		(delete-char (- end beg))
	    (insert replacement-text))))

(defun elisp-tred--render-elisp-form-to-buffer-append (quoted-elisp-form buffer)
  "Append the formatted elisp-tred tree for QUOTED-ELISP-FORM to end
of BUFFER."
  (with-current-buffer buffer
      (goto-char (point-max))
      (prin1 quoted-elisp-form (current-buffer))
	  (elisp-tred-mode)))

(defun elisp-tred--render-elisp-form-to-string (quoted-elisp-form)
  "Return the formatted elisp-tred tree for QUOTED-ELISP-FORM as a
multi-line string."
  (let ((buffer (get-buffer-create "*elisp-tred--render-elisp-form-to-string**")))
    (with-current-buffer buffer
      (erase-buffer)
      (elisp-tred--render-elisp-form-to-buffer-append quoted-elisp-form (current-buffer))
      (elisp-tred--render-region-to-string (point-min) (point-max)))))

(defun elisp-tred--render-overlay-diagram (&optional buffer)
  "Render an ASCII diagram of overlays in BUFFER (or current buffer).

The diagram shows:
- A horizontal axis with buffer positions and characters
- A row for `point' position
- A row for each overlay category showing overlay spans
- A row showing the `invisible' property state

Each overlay is shown with '[' at its start position and ']' at its
end position, with '-' characters filling the span.

Returns the output buffer."
  (let* ((source-buffer (or buffer (current-buffer)))
         (source-buffer-name (buffer-name source-buffer))
         (output-buffer-name (format "*overlays: %s*" source-buffer-name))
         (output-buffer (get-buffer-create output-buffer-name)))
    (with-current-buffer source-buffer
      (let* ((num-chars (1- (point-max)))  ; Number of characters in buffer
             (all-overlays (overlays-in (point-min) (point-max)))
             (overlay-categories '())
             (current-point (point)))

        ;; Collect all unique overlay categories
        (dolist (ov all-overlays)
          (when-let ((cat (overlay-get ov 'category)))
            (unless (member cat overlay-categories)
              (push cat overlay-categories))))
        (setq overlay-categories (nreverse overlay-categories))

        (with-current-buffer output-buffer
          (erase-buffer)

          (let ((label-width 25))  ; Fixed width for row labels

            ;; Row: buffer positions (between characters)
            (insert (truncate-string-to-width "pos:" label-width nil ?\s))
            ;; Position before first character
            (insert "1")
            ;; Positions between and after characters
            (dotimes (i num-chars)
              (let ((pos (+ i 2)))
                (insert " ")
                (if (< pos 10)
                    (insert (number-to-string pos))
                  (insert (number-to-string (mod pos 10))))))
            (insert "\n")

            ;; Row: buffer characters (with spaces between)
            (insert (truncate-string-to-width "char:" label-width nil ?\s))
            (insert " ")  ; Initial space to align with positions
            (dotimes (i num-chars)
              (let ((char (with-current-buffer source-buffer
                            (char-after (1+ i)))))
                (insert (if (or (not char) (eq char ?\n) (eq char ?\r) (eq char ?\t))
                            "."
                          (char-to-string char)))
                (insert " ")))
            (insert "\n")

            ;; Rows for each overlay category
            (dolist (cat overlay-categories)
              (let ((cat-name (symbol-name cat))
                    rows)
                ;; Truncate or pad category name to fixed width
                (setq cat-name (truncate-string-to-width cat-name label-width nil ?\s))

                ;; Mark overlays in the row
                (dolist (ov all-overlays)
                  (when (eq (overlay-get ov 'category) cat)
                    (let* ((start (max (point-min) (overlay-start ov)))
                           (end (min (point-max) (overlay-end ov)))
                           ;; Find first existing row where the
                           ;; overlay doesn't overlap with an existing
                           ;; overlay. If overlay doesn't fit in an
                           ;; existing row, create a new row instead.
                           (row (or (seq-find (lambda (row)
                                                (seq-every-p (lambda (pos)
                                                               (eq (aref row (* 2 (1- pos))) ?\.))
                                                             (number-sequence start end)))
                                              rows)
                                    (let ((new-row (with-temp-buffer
                                                     (insert ".")
                                                     (dotimes (i num-chars)
                                                       (insert " ."))
                                                     (buffer-string))))
                                      (setq rows (append rows (list new-row)))
                                      new-row))))
                      (when (<= start end)
                        ;; Convert buffer position to diagram position
                        ;; Buffer pos N maps to diagram position 2*(N-1)
                        (let ((diagram-start (* 2 (1- start)))
                              (diagram-end (* 2 (1- end))))
                          ;; Mark start position
                          (when (and (>= start (point-min)) (<= start (point-max)))
                            (aset row diagram-start ?\[))
                          ;; Mark middle positions (skip every other position for spaces)
                          (let ((i (+ diagram-start 2)))
                            (while (< i diagram-end)
                              (aset row i ?\-)
                              (setq i (+ i 2))))
                          ;; Mark end position
                          (when (and (>= end (point-min)) (<= end (point-max)))
                            (aset row diagram-end ?\])))))))

                (dolist (row rows)
                  (insert cat-name)
                  (insert row)
                  (insert "\n"))))

            ;; Row: invisible-p state
            (insert (truncate-string-to-width "invisible-p:" label-width nil ?\s))
            (dotimes (i (1+ num-chars))
              (let ((pos (1+ i)))
                (insert (if (with-current-buffer source-buffer
                              (invisible-p pos))
                            "I" ".")
                        " ")))
            (insert "\n")

            ;; Row: point position
            (insert (truncate-string-to-width "point:" label-width nil ?\s))
            (dotimes (i (1+ num-chars))
              (insert (if (= (1+ i) current-point) "^" " ") " "))
            (insert "\n")))))
    output-buffer))

(defun elisp-tred-render-overlay-diagram ()
  "Render and display an ASCII diagram of overlays in the current buffer.

Creates a diagram showing buffer positions, characters, point location,
overlay spans by category, and invisible-p state. The diagram is
displayed in a buffer named \"*overlays: BUFFER-NAME*\"."
  (interactive)
  (pop-to-buffer (elisp-tred--render-overlay-diagram)))

;;; Keymap

(defvar-keymap elisp-tred-mode-map
  "<backtab>" #'elisp-tred-fold-toggle-top-level
  "TAB" #'elisp-tred-fold-toggle-current-line)

;;; Minor mode definition

(defvar-local elisp-tred--mode-local-vars-saved nil
  "The previous buffer-local values of any variables that were
modified during intialization of `elisp-tred-mode'. The value is a
list of cons cells, where the CAR of each cons cell is the variable
name, and the CDR is the variable value.

`elisp-tred--mode-local-vars-saved' is restore the user's previous values
for buffer-local variables when `elisp-tred-mode' is disabled. For
example, `elisp-tred-mode' always sets `truncate-lines' to `t' to
disable line-wrapping, but the user might have set a different buffer
local value for `truncate-lines', prior enabling `elisp-tred-mode'.")

(defun elisp-tred--mode-local-var-save (var)
  "Save the value of buffer-local variable VAR in
`elisp-tred--mode-local-vars-saved'. If VAR does not have a buffer-local
binding, then this function does nothing."
  (when (local-variable-p var)
    (unless (assoc var elisp-tred--mode-local-vars-saved #'eq)
     (push (cons var (symbol-value var))
           elisp-tred--mode-local-vars-saved))))

(defun elisp-tred--mode-local-var-saved-value (var)
  "Return the value of buffer-local variable VAR that was saved by
`elisp-tred--mode-local-var-save', or nil if the VAR was never
saved by `elisp-tred--mode-local-var-save'."
  (when-let ((binding (assoc var elisp-tred--mode-local-vars-saved)))
    (cdr binding)))

(defun elisp-tred--mode-local-var-restore (var)
  "Restore the value of buffer-local variable VAR from
`elisp-tred--mode-local-vars-saved'. If VAR was not previously saved by
calling `elisp-tred--mode-local-var-save' on VAR, then this function does
nothing."
  (kill-local-variable var)
  (let ((saved-value (elisp-tred--mode-local-var-saved-value var)))
    (set (make-local-variable var) saved-value)))

(defun elisp-tred--mode-init ()
  (elisp-tred-indent--advice-init)
  (elisp-tred--update-shadow-buffer-init)
  (elisp-tred--treesit-init)
  ;; save user's buffer-local vars before modifying
  (elisp-tred--mode-local-var-save 'truncate-lines)
  (elisp-tred--mode-local-var-save 'truncate-partial-width-windows)
  (elisp-tred--mode-local-var-save 'cursor-sensor-mode)
  ;; disable line-wrapping, because it makes
  ;; reading and navigating the tree more difficult
  (setq-local truncate-lines t)
  (setq-local truncate-partial-width-windows t)
  (cursor-sensor-mode))

(defun elisp-tred--mode-teardown ()
  "Delete Elisp-Tred overlays, destroy `elisptred' treesit parser,
and restore `visual-line-mode' to its original value before enabling
`elisp-tred-mode'."
  (elisp-tred-indent--advice-teardown)
  (elisp-tred--update-shadow-buffer-teardown)
  (elisp-tred--remove-all-overlays-in-buffer)
  (elisp-tred--treesit-teardown)
  ;; restore user's buffer-local vars
  (elisp-tred--mode-local-var-restore 'truncate-lines)
  (elisp-tred--mode-local-var-restore 'truncate-partial-width-windows)
  (unless (elisp-tred--mode-local-var-saved-value cursor-sensor-mode)
    (cursor-sensor-mode -1)))

(define-minor-mode elisp-tred-mode
  "Display and edit elisp code as a tree."
  :lighter " Tred"
  (if elisp-tred-mode
      (elisp-tred--mode-init)
    (elisp-tred--mode-teardown)))

(provide 'elisp-tred)