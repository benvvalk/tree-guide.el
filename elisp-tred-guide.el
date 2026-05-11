(require 'easy-mmode) ;; for `define-minor-mode'

;;; Guide rendering
;;
;; For each logical line in the buffer, create a guide overlay.

(defcustom elisp-tred-guide-min-handle-width 1
  "Minimum width of guide handle, in characters.")

(defcustom elisp-tred-guide-enable-top-level-guides nil
  "Show guides for top level sexps.")

(defvar elisp-tred-guide--guide-char-with-handle "├")
(defvar elisp-tred-guide--guide-char-with-handle-last "╰")
(defvar elisp-tred-guide--guide-char-without-handle "│")
(defvar elisp-tred-guide--guide-char-handle "─")
(defvar elisp-tred-guide--guide-char-space " ")

(defun elisp-tred-guide--beginning-next-sexp ()
  "Move point to the start of the next sibling sexp and return the new
value of point. If there is no next sibling sexp, point remains
unchanged and the return value is nil.

This function is similar to the built-in `forward-sexp', with the
following differences:

(1) `forward-sexp' moves point to the end of the next sibling sexp,
but this function moves to the start of the next sibling sexp.

(2) `forward-sexp' has an inconsistent return value. When passed a
 positive argument (to move forwards over N sexps), it returns the new
value of point. When passed a negative value (to move backwards over N
sexps), it always returns nil."
  (let ((orig-pos (point)))
    (condition-case _
        (progn
          ;; Move forward over current sexp
          (forward-sexp)
          ;; Skip any whitespace/comments to find the next sibling
          (skip-chars-forward " \t\n")
          ;; Check for end-of-buffer or unbalanced closing delimiter
          (if (or (eobp)
                  (looking-at "[])}]")) ;; unbalanced closing delimiter
              (progn
                ;; No next sibling, restore position
                (goto-char orig-pos)
                nil)
            ;; We found a next sibling, and we're at its beginning
            t))
      (scan-error ;; thrown when we encounter an unbalanced paren
       (goto-char orig-pos)
       nil))))

(defun elisp-tred-guide--last-line-at-current-depth-p ()
  "Return non-nil if point is on the last line of the enclosing sexp.

Example:

(a
 (b
  c))

If point is before `(a', the return value is non-nil.

If point is before `(b', the return value is nil.

If point is before `c', the return value is non-nil."
  (let ((bol (line-beginning-position)))
    (save-excursion
      (while (and (elisp-tred-guide--beginning-next-sexp)
                  (= (line-beginning-position) bol)))
      (= (line-beginning-position) bol))))

(defun elisp-tred-guide--compute-guide-columns (&optional guide-columns)
  "Compute the column positions for the guides on the current line.

The GUIDE-COLUMNS argument is used internally for passing intermediate
results during recursive calls, and should normally be omitted when
calling this function from other functions.

The return value is a list of cons cells, where the CAR is the column
position for a guide, and the CDR is the LAST-CHILD flag. The
LAST-CHILD flag is non-nil when is ancestor sexp that owns the guide
is the last child of its parent."
  (save-excursion
    (let* ((parser-state (syntax-ppss))
           (parent-sexp-beg (nth 1 parser-state))
           (last-line-at-current-depth-p (elisp-tred-guide--last-line-at-current-depth-p))
           ;; Note: In order for `current-column' to return the
           ;; correct value (i.e. the number of indentation spaces on
           ;; current line), we need to temporarily make the hidden
           ;; indentation whitespace visible, by setting
           ;; `buffer-invisibility-spec' to nil.
           (guide-column (let (buffer-invisibility-spec) (current-column))))
      (push (cons guide-column last-line-at-current-depth-p) guide-columns)
      (if parent-sexp-beg
          (progn
            (goto-char parent-sexp-beg)
            (elisp-tred-guide--compute-guide-columns guide-columns))
        guide-columns))))

(defun elisp-tred-guide--make-guide-string (guide-columns)
  "Make a guide string from GUIDE-COLUMNS.

For example, if GUIDE-COLUMNS is ((1) (3) (6 . t)), the return value
will be '| | ╰'.

See the docstring for `elisp-tred-guide--compute-guide-columns' for
further information about the structure/meaning of GUIDE-COLUMNS."
  (let (guide-string-parts)
    (dotimes (i (length guide-columns))
      (let* ((guide-column-prev (when (> i 0) (car (nth (1- i) guide-columns))))
             (guide-column (car (nth i guide-columns)))
             (guide-column-next (car (nth (1+ i) guide-columns)))
             (guide-last-p (cdr (nth i guide-columns)))
             (guide-char (if guide-column-next
                             (if guide-last-p
                                 elisp-tred-guide--guide-char-space
                               elisp-tred-guide--guide-char-without-handle)
                           (if guide-last-p
                               elisp-tred-guide--guide-char-with-handle-last
                             elisp-tred-guide--guide-char-with-handle)))
             (guide-extension-char (if guide-column-next
                                       elisp-tred-guide--guide-char-space
                                     elisp-tred-guide--guide-char-handle))
             (handle-width (if guide-column-prev
                               (max (1- (- guide-column guide-column-prev))
                                    elisp-tred-guide-min-handle-width)
                             elisp-tred-guide-min-handle-width)))
        (when (or (> i 0) elisp-tred-guide-enable-top-level-guides)
          (push (concat guide-char
                        (string-join
                         (make-list handle-width guide-extension-char)))
                guide-string-parts))))
    (string-join (nreverse guide-string-parts))))

(defun elisp-tred-guide--hide-indentation-for-current-line ()
  "Create an overlay that hides the leading whitespace on the current
line, i.e. the indentation whitespace. If there is no leading
whitespace on the current line, do nothing."
  (let ((indentation (current-indentation)))
    (when (> indentation 0)
      (save-excursion
        (let* ((overlay-beg (line-beginning-position))
               (overlay-end (+ overlay-beg indentation))
               (overlay (make-overlay overlay-beg overlay-end nil t nil)))
          (overlay-put overlay 'category 'elisp-tred-indentation)
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'invisible t))))))

(defun elisp-tred-guide--create-for-current-line ()
  "Create an overlay which displays the guide string for the
current line."
  (let (buffer-invisibility-spec)
    (move-to-column (current-indentation)))
  (let* ((guide-columns (elisp-tred-guide--compute-guide-columns))
         (guide-string (elisp-tred-guide--make-guide-string guide-columns))
         (overlay-beg (line-beginning-position))
         (overlay-end (min (1+ (line-end-position)) (point-max)))
         (overlay (make-overlay overlay-beg overlay-end)))
    (overlay-put overlay 'category 'elisp-tred-guide)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'line-prefix guide-string)))

(defun elisp-tred-guide--destroy-all ()
  "Destroy all overlays related to Elisp-Tred-Guide in the current
buffer."
  (remove-overlays nil nil 'category 'elisp-tred-guide)
  (remove-overlays nil nil 'category 'elisp-tred-indentation))

;;; Live update algorithm
;;
;; When the user edits the buffer, instantly update the guides to
;; reflect the new buffer contents.
;;
;; Key points for understanding the live update algorithm:
;;
;; (1) The update algorithm is line-based, because there is a
;; one-to-one relationship between guide overlays and logical lines in
;; the buffer.
;;
;; (2) We keep track of which lines are "dirty" (i.e. which guides
;; need to be updated) in `elisp-tred-guide--dirty-line-ranges'.
;;
;; (3) Whenever the user makes an edit to the buffer, we mark *every
;; line* in the buffer as dirty. This sounds ridiculous, but it is
;; actually reasonably efficient if we update the dirty lines
;; incrementally over time (see point 4).
;;
;; (4) We use Emacs' built-in `pre-redisplay-functions' hook to
;; incrementally update the dirty lines. This hook gets called
;; immediately before Emacs' re-renders the text in the visible
;; portion of the current window, at which time we determine which
;; dirty lines are visible and update them.

(defvar-local elisp-tred-guide--dirty-line-ranges nil
  "The line ranges where the guides are out-of-date due the user
making edits in the buffer.")

(defun elisp-tred-guide--mark-all-buffer-lines-dirty ()
  "Mark all lines in the buffer as 'dirty', meaning that the guides on
those lines need to be updated to correctly reflect the structure of
the lisp code in the buffer."
  (setq elisp-tred-guide--dirty-line-ranges
        (list (cons
               (line-number-at-pos (point-min))
               (line-number-at-pos (point-max))))))

(defvar-local elisp-tred-guide--buffer-chars-modified-tick nil
  "The last `buffer-chars-modified-tick' that we've processed. This
value is used to work around the bug/quirk that Emacs calls its
redisplay hooks an unpredictable number of times for the same
redisplay event.")

(defun elisp-tred-guide--create-guides-in-line-range (range)
  "Create guides for the line numbers contained in RANGE."
  (let ((line-beg (car range))
        (line-end (cdr range)))
    (save-excursion
	 (goto-line line-beg)
     (while (and (<= (line-number-at-pos) line-end)
                 (not (eobp)))
       (elisp-tred-guide--hide-indentation-for-current-line)
       (elisp-tred-guide--create-for-current-line)
       (forward-line)))))

(defun elisp-tred-guide--destroy-guides-in-line-range (range)
  "Destroy guides for the line numbers contained in RANGE."
  (let* ((line-beg (car range))
         (line-end (cdr range))
         (beg (save-excursion
                (goto-line line-beg)
                (line-beginning-position)))
         (end (save-excursion
                (goto-line line-end)
                ;; `1+' because guide overlays include the newline at
                ;; the end of the line, if present. (This gives us a
                ;; character to attach the guide overlays to on empty
                ;; lines.)
                (min (1+ (line-end-position))
                     (point-max)))))
    (remove-overlays beg end 'category 'elisp-tred-guide)
    (remove-overlays beg end 'category 'elisp-tred-indentation)))

(defun elisp-tred-guide--line-ranges-subtract (range1 range2)
  "Subtract line range RANGE2 from line range RANGE1, and return the
result as a list of line ranges.

Each line range in the returned list is a cons cell, where the CAR is
the starting line number and the CDR is the ending line number
(inclusive).

Note that subtracting RANGE2 from RANGE1 may result 0, 1, or 2 line
ranges, depending on how RANGE1 and RANGE2 overlap.

Example: If RANGE1 is '(1 . 5) and RANGE2 is '(3 . 4), then the result
is a list of two ranges: '((1 . 2) (5 . 5))."
  (let* ((beg1 (car range1))
         (beg2 (car range2))
         (end1 (cdr range1))
         (end2 (cdr range2))
         result-ranges)
    ;; If there is no overlap between `range1' and `range2', return
    ;; `range1' unaltered.
    (if (or (> beg1 end2) (> beg2 end1))
        (list range1)
      ;; Otherwise, compute subtraction which can result in 0-2
      ;; ranges. Result will be 0 ranges (i.e. nil) if `range2'
      ;; completely covers `range1'.
      (when (> end1 end2)
        (push (cons (1+ end2) end1) result-ranges))
      (when (< beg1 beg2)
        (push (cons beg1 (1- beg2)) result-ranges))
      result-ranges)))

(defun elisp-tred-guide--line-ranges-intersect (range1 range2)
  "Return the line range that is the intersection of line ranges
RANGE1 and RANGE2. The result is returned as a cons cell, where the
CAR is the starting line number and the CDR is the end line number
(inclusive).

Example: If RANGE1 is '(1 . 5) and RANGE2 is '(4 . 9), then the result
is '(4 . 5)."
  (let* ((beg1 (car range1))
         (beg2 (car range2))
         (end1 (cdr range1))
         (end2 (cdr range2))
         (result-beg (max beg1 beg2))
         (result-end (min end1 end2)))
    ;; If `result-beg' > `result-end', it means that `range1' and
    ;; `range2' do not intersect.
    (when (<= result-beg result-end)
      (cons result-beg result-end))))

(defun elisp-tred-guide--visible-line-range ()
  "Return the range of line numbers that are visible in the current
window as a cons cell, where the CAR is the starting line number and
the CDR is the ending line number (inclusive)."
  (let ((line-beg (save-excursion
                    (goto-char (window-start))
                    (line-number-at-pos)))
        (line-end (save-excursion
                    (goto-char (window-end))
                    (line-number-at-pos))))
    (cons line-beg line-end)))

(defun elisp-tred-guide--mark-line-ranges-as-clean (line-ranges)
  "Remove the 'dirty' status from LINE-RANGES.

LINE-RANGES is a list of cons cells where the CAR is the starting line
number and the CDR is the ending line number (inclusive)."
  (dolist (line-range line-ranges)
    (setq elisp-tred-guide--dirty-line-ranges
          (mapcan (lambda (dirty-line-range)
                    (elisp-tred-guide--line-ranges-subtract dirty-line-range line-range))
                  elisp-tred-guide--dirty-line-ranges))))

(defun elisp-tred-guide--compute-visible-line-ranges-that-are-dirty ()
  "Return a list of line ranges that are both visible in the current
window and marked as dirty."
  (let ((visible-line-range (elisp-tred-guide--visible-line-range))
        result)
    (dolist (dirty-line-range elisp-tred-guide--dirty-line-ranges)
      (when-let ((intersection-range (elisp-tred-guide--line-ranges-intersect
                                      visible-line-range
                                      dirty-line-range)))
        (push intersection-range result)))
    result))

(defun elisp-tred-guide--update-visible-lines-that-are-dirty ()
  "Update the guides for the lines that are both visible in the
current window and marked as 'dirty'."
  (let ((visible-line-ranges-that-are-dirty
         (elisp-tred-guide--compute-visible-line-ranges-that-are-dirty)))
    (dolist (dirty-line-range visible-line-ranges-that-are-dirty)
      (elisp-tred-guide--destroy-guides-in-line-range dirty-line-range)
      (elisp-tred-guide--create-guides-in-line-range dirty-line-range))
    (elisp-tred-guide--mark-line-ranges-as-clean visible-line-ranges-that-are-dirty)))

(defun elisp-tred-guide--pre-redisplay-hook (&rest _)
  "Update the guides, in response to user edits in the buffer.

This function is called by the built-in `pre-redisplay-functions'
hook, which gets called immediately before the Emacs display engine
re-renders the text in the current window."
  ;; Note: We need to check `buffer-chars-modified-tick' here to avoid
  ;; doing unnecessary work, because Emacs may invoke
  ;; `pre-redisplay-functions' multiple times for the same redisplay
  ;; event.
  (unless (eq elisp-tred-guide--buffer-chars-modified-tick (buffer-chars-modified-tick))
    ;; If the buffer contents have changed in any way, naively
    ;; mark all lines in the buffer as dirty.
    (elisp-tred-guide--mark-all-buffer-lines-dirty)
    (setq elisp-tred-guide--buffer-chars-modified-tick (buffer-chars-modified-tick)))
  (elisp-tred-guide--update-visible-lines-that-are-dirty))

;;; Integration with indentation functions
;;
;; This mode hides the leading indentation whitespace on each line, so
;; that it can show the guides instead. However, some
;; modes/commands/functions require indentation whitespace to be
;; visible in order to behave correctly. The most important example
;; are Emacs' built-in `indent-line-function' and
;; `indent-region-function'.
;;
;; For such indentation-sensitive functions, we need to temporarily
;; unhide all invisible text by setting `buffer-invisibility-spec' to
;; nil.

(defcustom elisp-tred-guide-regexps-for-commands-that-require-visible-indentation
  '("lispy")
  "A list of regexps for commands that require indentation whitespace
to be visible, in order to function correctly.

Normally, all indentation whitespace is hidden, and the guides are
shown instead. However, some commands don't function correctly when
indentation whitespace is hidden, most notably Emacs' built-in
indentation functions (`indent-line-function' and
`indent-region-function').

Indentation whitespace is temporarily unhidden for commands that match
this regexp list, by setting `buffer-invisibility-spec' to nil.")

(defun elisp-tred-guide--indent-advice (orig-fn &rest args)
  "Advice for Emacs' built-in indent functions
(e.g. `indent-line-function', `indent-region-function'), that temporarily
unhides indentation whitespace on all lines.

Normally we hide the indentation whitespace and show the tree guides
instead. However, Emacs' built-in indent functions don't calculate the
correct column values unless they can 'see' the indentation whitespace
on the previous line.

To work around this, we add an advice that temporarily unhides all
invisible text, by setting `buffer-invisibility-spec' to nil."
  (let (buffer-invisibility-spec)
    (apply orig-fn args)))

(defun elisp-tred-guide--indent-advice-init ()
  "Add advice to Emacs' built-in line/region indentation functions, so
that they work correctly in Elisp-Tred mode.

See the docstring for `elisp-tred-guide--indent-advice' for further
explanation."
  (advice-add indent-line-function :around #'elisp-tred-guide--indent-advice)
  (advice-add indent-region-function :around #'elisp-tred-guide--indent-advice))

(defun elisp-tred-guide--indent-advice-teardown ()
  "Remove advice from Emacs' built-in line/region indentation
functions.

See the docstring for `elisp-tred-guide--indent-advice' for further
explanation."
  (advice-remove indent-line-function #'elisp-tred-guide--indent-advice)
  (advice-remove indent-region-function #'elisp-tred-guide--indent-advice))

(defun elisp-tred-guide--indent-pre-command-hook ()
  (let ((command-name (symbol-name this-command)))
    (when (seq-find
           (lambda (regexp)
             (string-match regexp command-name nil t))
           elisp-tred-guide-regexps-for-commands-that-require-visible-indentation)
      (setq-local buffer-invisibility-spec nil))))

(defun elisp-tred-guide--indent-post-command-hook ()
  (setq-local buffer-invisibility-spec t))

(defun elisp-tred-guide--indent-command-hooks-init ()
  (add-hook 'pre-command-hook #'elisp-tred-guide--indent-pre-command-hook nil t)
  (add-hook 'post-command-hook #'elisp-tred-guide--indent-post-command-hook nil t))

(defun elisp-tred-guide--indent-command-hooks-teardown ()
  (remove-hook 'pre-command-hook #'elisp-tred-guide--indent-pre-command-hook t)
  (remove-hook 'post-command-hook #'elisp-tred-guide--indent-post-command-hook t))

;;; Minor mode definition

(defun elisp-tred-guide--mode-init ()
  "Performs necessary initialization when enabling Elisp-Tred-Guide
mode."
  (add-hook 'pre-redisplay-functions #'elisp-tred-guide--pre-redisplay-hook nil t)
  (elisp-tred-guide--indent-advice-init)
  (elisp-tred-guide--indent-command-hooks-init)
  (elisp-tred-guide--mark-all-buffer-lines-dirty))

(defun elisp-tred-guide--mode-teardown ()
  "Perform necessary teardown when disabling Elisp-Tred-Guide mode."
  (remove-hook 'pre-redisplay-functions #'elisp-tred-guide--pre-redisplay-hook t)
  (elisp-tred-guide--indent-advice-init)
  (elisp-tred-guide--indent-command-hooks-teardown)
  (elisp-tred-guide--destroy-all))

(define-minor-mode elisp-tred-guide-mode
  "Display tree guides for elisp code."
  :lighter nil
  (if elisp-tred-guide-mode
      (elisp-tred-guide--mode-init)
    (elisp-tred-guide--mode-teardown)))

(provide 'elisp-tred-guide)