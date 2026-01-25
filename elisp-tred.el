(require 'seq)
(require 'treesit)

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

(defvar elisp-tred--newline-regex
  "\\(\r\n\\|\n\\|\r\\)"
  "Regular expression that matches newlines on Linux, Mac, and Windows.")

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
    ;; NOTE: For now, disable automatic treesit re-parsing on buffer
    ;; changes, until I can address the slowness I am seeing with
    ;; scrolling/paging in a static buffer (parsed once on load).  I
    ;; believe the scrolling slowness is caused by using a large
    ;; number of overlays, which I have heard Emacs does not handle
    ;; well.
    ;;
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
	;;(treesit-parser-add-notifier parser #'elisp-tred--on-treesit-reparse)
    ;;(add-hook 'pre-redisplay-functions #'elisp-tred--pre-redisplay nil t)
    ;;(elisp-tred--force-treesit-reparse)
    (elisp-tred--on-treesit-reparse nil nil)))

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
  (elisp-tred--remove-all-overlays-in-buffer)
  (let ((root-node (treesit-buffer-root-node 'elisptred)))
    (elisp-tred--create-overlays root-node nil)))

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
              (siblings (treesit-node-children parent t)))
    (treesit-node-eq node (car siblings))))

(defun elisp-tred--last-child-p (node)
  "Return non-nil if the treesit node NODE is the last named child
of its parent node.

Note: If NODE has no parent treesit node (i.e. it is the root node of
the treesit parse tree), this function will return `nil'."
  (when-let* ((parent (treesit-node-parent node))
              (siblings (treesit-node-children parent t)))
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

(defun elisp-tred--create-tree-guide-overlay-at (pos guide-string)
  (let ((overlay (make-overlay pos pos)))
    (overlay-put overlay 'category 'elisp-tred-guide)
    (overlay-put overlay 'elisp-tred-folded folded)
    (overlay-put overlay 'before-string (concat guide-string))))

(defun elisp-tred--create-tree-guide-overlays-for-string (node folded guide-flags)
  "Create tree guide overlays for a string, corresponding to
treesit node NODE.

We handle tree guides for strings specially, because strings can span
multiple lines in the buffer (e.g. docstrings), whereas other treesit
node types (e.g. symbol) always correspond to a single line in the
buffer. For multi-line strings, we create separate tree guide overlays
for each line, but we only show a handle for the first line."
  (let* ((last-child-p (elisp-tred--last-child-p node))
         (guide-string-line0 (elisp-tred--tree-guide-flags-to-string guide-flags))
         (guide-length (length elisp-tred--guide))
         ;; Guide string for string lines 2..N (if any).
         ;; Replace the guide chars for the last level, in order to
         ;; remove the handle.
         (guide-string-rest (concat (substring guide-string-line0 0 (- guide-length))
                                    (if last-child-p
                                        elisp-tred--no-guide
                                      elisp-tred--guide)))
         (start (treesit-node-start node))
         (end (treesit-node-end node))
         (overlay (make-overlay start start)))
    (save-excursion
      (goto-char start)
      (elisp-tred--create-tree-guide-overlay-at (point) (concat "\n" guide-string-line0))
      (while (re-search-forward elisp-tred--newline-regex end t)
        (goto-char (match-end 0))
        (elisp-tred--create-tree-guide-overlay-at (point) guide-string-rest)))))

(defun elisp-tred--create-tree-guide-overlays-for-node (node folded guide-flags)
  "Insert the tree guide overlay at the beginning of the line
for treesit node NODE. "
  (when-let* ((node-type (treesit-node-type node))
              (guide-string (elisp-tred--tree-guide-flags-to-string guide-flags))
              (start (treesit-node-start node)))
    (if (equal node-type "string")
        (elisp-tred--create-tree-guide-overlays-for-string node folded guide-flags)
     (elisp-tred--create-tree-guide-overlay-at start (concat "\n" guide-string)))))

(defun elisp-tred--tree-guide-overlay-p (overlay)
  "Return non-nil if OVERLAY is a tree guide overlay, or nil
otherwise."
  (eq (overlay-get overlay 'category) 'elisp-tred-guide))

(defun elisp-tred--tree-guide-overlay (node)
  "Return the tree guide overlay for treesit node NODE, or nil if NODE
does not have a tree guide overlay."
  (let* ((start (treesit-node-start node))
         (overlays (overlays-in start start)))
    (seq-find #'elisp-tred--tree-guide-overlay-p overlays)))

(defun elisp-tred--tree-guide-p (node)
  "Return `t' if we should insert a tree guide overlay before
treesit node NODE, or `nil' otherwise."
  (unless
      ;; Omit tree guide for first child of a sequence (list or
      ;; vector), so that it is shown on the same line as its opening
      ;; paren/bracket.
      (and (elisp-tred--first-child-p node)
           (not (elisp-tred--sequence-p node)))
    t))

(defun elisp-tred--create-tree-guide-overlays (node folded &optional guide-flags)
  "Create the tree guide overlays for treesit node NODE and all of its
descendants.

GUIDE-FLAGS is a list of booleans, one per ancestor node, that is used
to construct the tree guide lines."
  (when (elisp-tred--tree-guide-p node)
    (elisp-tred--create-tree-guide-overlays-for-node node folded guide-flags))
  (unless folded
    (let* ((children (treesit-node-children node t)))
     (dolist (child children)
       (let* ((guide-flag (not (elisp-tred--last-child-p child)))
              (guide-flags (append guide-flags (list guide-flag))))
         (elisp-tred--create-tree-guide-overlays child folded guide-flags))))))

;;; Whitespace rendering
;;
;; We use overlays to both hide real whitespace and to add virtual
;; whitespace (e.g. newlines), to ensure that the tree is always
;; rendered in a consistent manner with respect to the structure of
;; the code, rather than the user's personal choice of
;; whitespace/indentation.

(defun elisp-tred--create-whitespace-overlays (node folded)
  "Hide all non-significant whitespace using overlays.

`Non-significant whitespace' means whitespace that is not contained
within a string or comment (e.g. indentation and newlines). We want
to hide all such whitespace so that we display the tree structure in a
consistent and predictable manner."
  (let ((replacement (when folded " "))
        (children (treesit-node-children node t)))
    (when children
      ;; Hide whitespace before the first child
      (let* ((first-child (car children))
             (gap-start (treesit-node-start node))
             (gap-end (treesit-node-start first-child)))
        (elisp-tred--hide-whitespace-in-range gap-start gap-end replacement))

      ;; Hide whitespace between consecutive children
      (let ((prev-child (car children)))
        (dolist (curr-child (cdr children))
          (let ((gap-start (treesit-node-end prev-child))
                (gap-end (treesit-node-start curr-child)))
            (elisp-tred--hide-whitespace-in-range gap-start gap-end replacement))
          (setq prev-child curr-child)))

      ;; Hide whitespace after the last child
      (let* ((last-child (car (last children)))
             (gap-start (treesit-node-end last-child))
             (gap-end (treesit-node-end node)))
        (elisp-tred--hide-whitespace-in-range gap-start gap-end replacement))

      ;; Recursively process children
      (dolist (child children)
        (elisp-tred--create-whitespace-overlays child folded)))))

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
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put overlay 'category 'elisp-tred-whitespace)
          (if replacement
              (overlay-put overlay 'display replacement)
            (overlay-put overlay 'invisible t)))))))

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
        (end (treesit-node-end node)))
    (remove-overlays start end 'category 'elisp-tred-guide)
    (remove-overlays start end 'category 'elisp-tred-whitespace)
    (remove-overlays start end 'category 'elisp-tred-fold)))

(defun elisp-tred--create-overlays (node folded)
  "Create tree guide and whitespace overlays for treesit node NODE."
  (let ((tree-guide-flags (elisp-tred--tree-guide-flags node))
        (start (treesit-node-start node))
        (end (treesit-node-end node)))
    (when folded
      (elisp-tred--create-fold-overlays-for-strings node))
    (elisp-tred--create-whitespace-overlays node folded)
    (elisp-tred--create-tree-guide-overlays node folded tree-guide-flags)))

;;; Folding
;;
;; Implements collapsing/expanding of the current line using the TAB
;; key.

(defun elisp-tred--create-fold-overlay (beg end)
  "Create an overlay for a folded multi-line string, where lines 2..N
are hidden and an ellipsis (`...') is shown instead."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'category 'elisp-tred-fold)
    (overlay-put overlay 'display "...")))

(defun elisp-tred--create-fold-overlay-for-string (node)
  "Create an overlay that `folds' the string corresponding to NODE,
where NODE is a treesit node with type `string'.

If NODE is a multi-line string, a new overlay is create that hides
lines 2..N and replaces them with an ellipsis (`...'). If NODE is a
single-line string, nothing needs to be done and therefore no overlay
is created."
  (let* ((start (treesit-node-start node))
         (end (treesit-node-end node))
         (overlay (make-overlay start start)))
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
      (treesit-node-children node t)))

(defun elisp-tred--folded-p (node)
  "Return non-nil if treesit node NODE is currently folded."
  (when-let* ((overlay (elisp-tred--tree-guide-overlay node)))
    (overlay-get overlay 'elisp-tred-folded)))

(defun elisp-tred--set-node-folded (node folded)
  "Set folded state of treesit node NODE."
  (elisp-tred--remove-overlays node)
  (elisp-tred--create-overlays node folded))

(defun elisp-tred--set-current-line-folded (folded)
  "Set folded state of treesit node on current line."
  (when-let ((node (elisp-tred--node-for-current-line)))
    (elisp-tred--set-node-folded node folded)))

(defun elisp-tred-toggle-current-line-folded ()
  "Toggle the folded/unfolded state of the current line."
  (interactive)
  (when-let* ((node (elisp-tred--node-for-current-line)))
    (let ((folded (elisp-tred--folded-p node)))
      (elisp-tred--set-node-folded node (not folded)))))

;;; Evil integration
;;
;; Remap j/k and up/down arrows to move by visual lines rather than
;; real newlines. In order to render the elisp syntax tree in a
;; consistent manner, `elisp-tred' uses many overlays to both add
;; visual newlines and hide real newlines. As a result, scrolling by
;; real newlines feels like buggy/non-sensical behaviour.
;;
;; Note 1: These key remappings are only enabled while
;; `elisp-tred-mode' is enabled. The user's original j/k/up/down
;; keybindings are restored when `elisp-tred-mode' is disabled, even
;; if they are custom keybindings.
;;
;; Note 2: Setting `evil-respect-visual-line-mode' to `t' accomplishes
;; a similar result to these remapping. However, my rebindings remap
;; both the j/k keys and the up/down arrow keys, whereas
;; `evil-respect-visual-line-mode' only remaps the j/k keys [1].
;;
;; [1]: https://github.com/emacs-evil/evil/issues/1971

(with-eval-after-load 'evil
  (dolist (state '(normal insert visual motion))
    (evil-define-minor-mode-key state 'elisp-tred-mode
      (kbd "<down>") #'evil-next-visual-line
      (kbd "<up>") #'evil-previous-visual-line))
  (dolist (state '(normal visual motion))
    (evil-define-minor-mode-key state 'elisp-tred-mode
      (kbd "TAB") #'elisp-tred-toggle-current-line-folded
      (kbd "j") #'evil-next-visual-line
      (kbd "k") #'evil-previous-visual-line)))

;;; Keymap

(defvar-keymap elisp-tred-mode-map
  "TAB" #'elisp-tred-toggle-current-line-folded)

;;; Minor mode definition

(defvar-local elisp-tred--saved-local-vars nil
  "The previous buffer-local values of any variables that were
modified during intialization of `elisp-tred-mode'. The value is a
list of cons cells, where the CAR of each cons cell is the variable
name, and the CDR is the variable value.

`elisp-tred--saved-local-vars' is restore the user's previous values
for buffer-local variables when `elisp-tred-mode' is disabled. For
example, `elisp-tred-mode' always sets `truncate-lines' to `t' to
disable line-wrapping, but the user might have set a different buffer
local value for `truncate-lines', prior enabling `elisp-tred-mode'.")

(defun elisp-tred--save-local-var (var)
  "Save the value of buffer-local variable VAR in
`elisp-tred--saved-local-vars'. If VAR does not have a buffer-local
binding, then this function does nothing."
  (when (local-variable-p var)
    (unless (assoc var elisp-tred--saved-local-vars #'eq)
     (push (cons var (symbol-value var))
           elisp-tred--saved-local-vars))))

(defun elisp-tred--restore-local-var (var)
  "Restore the value of buffer-local variable VAR from
`elisp-tred--saved-local-vars'. If VAR was not previously saved by
calling `elisp-tred--save-local-var' on VAR, then this function does
nothing."
  (kill-local-variable var)
  (when-let ((binding (assoc var elisp-tred--saved-local-vars)))
    (let ((saved-value (cdr binding)))
      (set (make-local-variable var) saved-value))))

(defun elisp-tred--init ()
  (elisp-tred--treesit-init)
  ;; save user's buffer-local vars before modifying
  (elisp-tred--save-local-var 'truncate-lines)
  (elisp-tred--save-local-var 'truncate-partial-width-windows)
  ;; disable line-wrapping, because it makes
  ;; reading and navigating the tree more difficult
  (setq-local truncate-lines t)
  (setq-local truncate-partial-width-windows t))

(defun elisp-tred--teardown ()
  "Delete Elisp-Tred overlays, destroy `elisptred' treesit parser,
and restore `visual-line-mode' to its original value before enabling
`elisp-tred-mode'."
  (elisp-tred--remove-all-overlays-in-buffer)
  (elisp-tred--treesit-teardown)
  ;; restore user's buffer-local vars
  (elisp-tred--restore-local-var 'truncate-lines)
  (elisp-tred--restore-local-var 'truncate-partial-width-windows))

(define-minor-mode elisp-tred-mode
  "Display and edit elisp code as a tree."
  :lighter " Tred"
  (if elisp-tred-mode
      (elisp-tred--init)
    (elisp-tred--teardown)))

(provide 'elisp-tred)