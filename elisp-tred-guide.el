;;; -*- lexical-binding: t -*-

(require 'easy-mmode) ;; for `define-minor-mode'

;;; Guide rendering
;;
;; For each logical line in the buffer, create a guide overlay.

(defcustom elisp-tred-guide-handle-width 1
  "Minimum width of guide handle, in characters.")

(defcustom elisp-tred-guide-min-depth 1
  "The minimum depth for which to render guides.

Top-level sexps are at depth 0, children of top-level sexps are at
depth 1, and so on.")

(defvar elisp-tred-guide--guide-char-with-handle "├")
(defvar elisp-tred-guide--guide-char-with-handle-last "╰")
(defvar elisp-tred-guide--guide-char-without-handle "│")
(defvar elisp-tred-guide--guide-char-handle "─")
(defvar elisp-tred-guide--guide-char-space " ")

(defun elisp-tred-guide--parent-sexp-end-position ()
  "Return the end position of the parent sexp containing point.

If point is not contained within a sexp, i.e. it is located
before/after/between top-level sexps, return the end-of-buffer
position (i.e. the value returned by `point-max')."
  (let* ((ppss (syntax-ppss))
       (parent-sexp-beg (nth 1 ppss)))
    ;; If `parent-sexp-beg' is nil, it means that point is located between
    ;; top-level sexps. In that case, we treat the entire buffer as the
    ;; parent sexp, and return `(point-max)'.
    (if (null parent-sexp-beg)
        (point-max)
      ;; Else: Point is located inside one or more sexps, i.e. point is not
      ;; located between top-level sexps. Find the end of the
      ;; parent sexp using `scan-sexps'.
      ;;
      ;; Some notes about `scan-sexps' error cases:
      ;;
      ;; (1) If `scan-sexps' encounters an unbalanced close paren, it will
      ;; throw a `scan-error'.
      ;;
      ;; (2) If `scan-sexps' reaches the end of the buffer without seeing
      ;; the expected number of close parens, i.e. there are unbalanced
      ;; open parens, it will throw a `scan-error'.
      ;;
      ;; (3) If `scan-sexps' encounters the end of the buffer before moving
      ;; over COUNT balanced sexps, the return value is nil.
      ;;
      ;; Cases (1) and (3) should never occur in this function, because we
      ;; always start the scan on an open paren, and we pass 1 for the COUNT
      ;; argument.
      ;;
      ;; Case (2) is possible, in which case we return `(point-max)'.
      (condition-case _
          (scan-sexps parent-sexp-beg 1)
        (scan-error (point-max))))))

(defun elisp-tred-guide--last-line-at-current-depth-p ()
  "Return non-nil if there is no next sibling sexp, string, comment, or
blank line that appears on a subsequent line.

Point should be positioned before the opening paren of the sexp
that you want to test. Note that this function can return different
values for different positions of point within the same line.

Implementation note: We scan line-by-line, rather than moving by sexp,
because we want to treat comment lines and blank lines as first class
siblings."
  (catch 'done
    (save-excursion
      (let* ((parent-sexp-beg (nth 1 (syntax-ppss)))
             (parent-sexp-end (elisp-tred-guide--parent-sexp-end-position))
             (parent-sexp-end-line-number (line-number-at-pos parent-sexp-end)))
        (while (< (line-number-at-pos) parent-sexp-end-line-number)
          (forward-line)
          ;; When we encounter a sibling line (i.e. a line with the same parent
          ;; start position)
          (when (eql (nth 1 (syntax-ppss)) parent-sexp-beg)
            (throw 'done nil)))
        ;; Return t, because we reached the last line of the parent
        ;; sexp, before encountering another sibling line.
        t))))

(defun elisp-tred-guide--compute-guide-offset-and-type (sexp-beg parent-sexp-beg)
  "Calculate the guide offset and type for the sexp beginning at buffer
position SEXP-BEG. PARENT-SEXP-BEG is the start position of the parent
sexp that contains SEXP-BEG, or nil if SEXP-BEG is a top-level sexp.

The return value is a cons cell of the form (OFFSET
. GUIDE-TYPE-LAST-P), where OFFSET is an integer specifying how many
space characters to insert before the guide, and GUIDE-TYPE-LAST-P is
a boolean indicating if SEXP-BEG is the last child line of its parent
sexp."
  (save-excursion
    (goto-char sexp-beg)
    (let* (buffer-invisibility-spec
           (parent-on-same-line-p (when parent-sexp-beg
                                        (save-excursion
                                          ;; fast jump to beginning of line
                                          (forward-line 0)
                                          (<= (point) parent-sexp-beg))))
           (guide-type-last-p (elisp-tred-guide--last-line-at-current-depth-p)))
      ;; If: parent sexp starts on same line, guide offset is offset from
      ;; start of parent sexp, minus 1 to make room for parent guide
      ;; char (i.e. `|' or ` ').
      ;;
      ;; Else: parent sexp starts on a previous line. Guide offset is
      ;; distance from first non-whitespace char on line, plus
      ;; width of parent guide's handle char(s).
      (cons (if parent-on-same-line-p
                (1- (- sexp-beg parent-sexp-beg))
              (let ((indentation (current-indentation))
                    (column-pos (save-excursion
                                  (goto-char sexp-beg)
                                  (current-column))))
                (+ (- column-pos indentation)
                   elisp-tred-guide-handle-width)))
            guide-type-last-p))))

(defun elisp-tred-guide--compute-guide-offsets-and-types (&optional guide-offsets-and-types)
  "Compute the column positions for the guides on the current line.

The GUIDE-OFFSETS-AND-TYPES argument is used internally for passing intermediate
results during recursive calls, and should normally be omitted when
calling this function from other functions.

The return value is a list of cons cells, where the CAR is the column
position for a guide, and the CDR is the LAST-CHILD flag. The
LAST-CHILD flag is non-nil when is ancestor sexp that owns the guide
is the last child of its parent."
  (save-excursion
    ;; Note: I set `buffer-invisibility-spec' to nil here to work around a
    ;; strange intermittent bug, where `goto-char' sometimes moves point
    ;; to the beginning of the line, rather than the requested target
    ;; location (i.e. the open paren of the parent sexp). It is probably
    ;; related to the following excerpt of the "Invisible Text" section in
    ;; the Emacs manual [1]:
    ;;
    ;;   "If a command ends with point inside or at the boundary of
    ;;   invisible text, the main editing loop relocates point to one of
    ;;   the two ends of the invisible text. Emacs chooses the direction
    ;;   of relocation so that it is the same as the overall movement
    ;;   direction of the command..."
    ;;
    ;; I've tried just setting `buffer-invisibility-spec' to nil for both
    ;; `current-column' and `goto-char' but the bug still occurred, so
    ;; I'm not exactly sure where the problem is.
    ;;
    ;; [1]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Invisible-Text.html
    (let* (buffer-invisibility-spec
           (parser-state (syntax-ppss))
           (parent-sexp-beg (nth 1 parser-state))
           (guide-offset (elisp-tred-guide--compute-guide-offset-and-type
                          (point)
                          parent-sexp-beg)))
      (push guide-offset guide-offsets-and-types)
      ;; if: there is no parent sexp, finish and return the result
      (if (null parent-sexp-beg)
          guide-offsets-and-types
        ;; else: move point to beginning of parent sexp and recurse
        (goto-char parent-sexp-beg)
        (elisp-tred-guide--compute-guide-offsets-and-types guide-offsets-and-types)))))

(defun elisp-tred-guide--make-guide-string (guide-offsets-and-types)
  "Make a guide string from GUIDE-OFFSETS-AND-TYPES.

For example, if GUIDE-OFFSETS-AND-TYPES is ((1) (3) (6 . t)), the return value
will be '| | ╰'.

See the docstring for `elisp-tred-guide--compute-guide-offsets-and-types' for
further information about the structure/meaning of GUIDE-OFFSETS-AND-TYPES."
  (let (guide-string-parts
        (num-guides (length guide-offsets-and-types)))
    (dotimes (i num-guides)
      (let* ((rightmost-guide-p (>= i (1- num-guides)))
             (guide-offset (car (nth i guide-offsets-and-types)))
             (guide-type-last-p (cdr (nth i guide-offsets-and-types)))
             (guide-char (if rightmost-guide-p
                              (if guide-type-last-p
                                  elisp-tred-guide--guide-char-with-handle-last
                                elisp-tred-guide--guide-char-with-handle)
                            (if guide-type-last-p
                                elisp-tred-guide--guide-char-space
                              elisp-tred-guide--guide-char-without-handle)))
             (num-padding-chars (max
                                 guide-offset
                                 (if rightmost-guide-p
                                     elisp-tred-guide-handle-width
                                   0)))
             (padding-char (if rightmost-guide-p
                               elisp-tred-guide--guide-char-handle
                             elisp-tred-guide--guide-char-space)))
        (when (>= i elisp-tred-guide-min-depth)
          (push (concat
                guide-char
                (string-join (make-list num-padding-chars padding-char)))
               guide-string-parts))))
    (string-join (nreverse guide-string-parts))))

(defun elisp-tred-guide--update-or-create-indentation-overlay-for-current-line ()
  "Create or update the indentation overlay for the current line.

The purpose of the indentation overlays is to hide the leading
whitespace on each line, so that we can show the tree guides
instead. The indentation overlays need to be updated whenever lines
are re-indented, and whenever new lines are inserted in the buffer.

This function returns non-nil if it makes any changes related to the
indentation overlay on the current line, such as creating a missing
indentation overlay, updating the start/end positions of an existing
overlay, or deleting duplicate/extraneous overlays that should no
longer exist. (Extraneous overlays can be introduced by the user's
buffer-editing activity, such as inserting a newline that splits an
existing line, or deleting the whitespace at the beginning of a line.)

A return value of nil means that the indentation overlay for the
current line already exists and is up-to-date, and there were no
extraneous overlays that needed to be removed."
  ;; When invoking `current-indentation':
  ;;
  ;; (1) We temporarily set `buffer-invisibility-spec' to nil because
  ;; `current-indentation' ignores invisible whitespace.
  ;;
  ;; (2) We temporarily set `tab-width' to 1 because we want know the
  ;; literal number of whitespace characters at the beginning of the
  ;; line, not the resulting visual width after expanding TABs.
  (let* ((indent-column (let ((tab-width 1)
                              buffer-invisibility-spec)
                          (current-indentation)))
         (line-end (save-excursion (forward-line 1) (point)))
         (line-beg (save-excursion (forward-line 0) (point)))
         (existing-overlays (seq-filter
                             (lambda (overlay)
                               (eq (overlay-get overlay 'category)
                                   'elisp-tred-indentation))
                             (overlays-in line-beg line-end))))
    ;; If: there is no leading whitespace on current line, we should
    ;; delete any existing indentation overlays. The return value should
    ;; be non-nil if any overlays were deleted, to indicate that an update
    ;; occurred.
    (if (= indent-column 0)
        (when existing-overlays
          (mapc #'delete-overlay existing-overlays))
      ;; Else: current line starts with one or more whitespace characters.
      ;;
      ;; If there is exactly one existing overlay whose start/end position
      ;; exactly matches the start/end of the leading whitespace on the
      ;; current line, then nothing needs to be updated, and we should
      ;; return nil to indicate that no update occurred.
      ;;
      ;; Otherwise, delete all existing overlays, create a new
      ;; overlay with the correct start/end positions, and return
      ;; non-nil to indicate that an update occurred.
      (unless (and (= (length existing-overlays) 1)
                   (= (overlay-start (car existing-overlays)) line-beg)
                   (= (overlay-end (car existing-overlays)) (+ line-beg indent-column)))
        (mapc #'delete-overlay existing-overlays)
        (let* ((overlay-end (+ line-beg indent-column))
               (overlay (make-overlay line-beg overlay-end nil t nil)))
          (overlay-put overlay 'category 'elisp-tred-indentation)
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'invisible 'elisp-tred-guide))))))

(defun elisp-tred-guide--update-or-create-guide-overlay-for-current-line ()
  "Create or update the guide overlay for the current line.

The guide overlays display virtual tree guide characters at the
beginning of each line, which show the parent/child relationships
between sexps in the buffer. The guide overlays need to be updated
when the parent/child relationships between sexps change, and when new
lines are inserted in the buffer.

This function returns non-nil if it makes any changes related to the
guide overlay for the current line, such as: creating a missing guide
overlay, updating the guide characters displayed by an existing
overlay, or deleting duplicate/extranedeous overlays that should no
longer exist. (Extraneous overlays can be introduced by the user's
buffer-editing activity, such as inserting a newline that splits an
existing line, or deleting the whitespace at the beginning of a line.)

A return value of nil means that the guide overlay for the current
line already existing, is displaying the correct guide characters, and
there were no extraneous overlays that needed to be removed."
  (save-excursion
    ;; Notes:
    ;; 
    ;; (1) `elisp-tred-guide--compute-guide-offsets-and-types' requires
    ;; point to be located immediately before the first non-whitespace
    ;; character on the current line.
    ;;
    ;; (2) We need to temporarily set `buffer-invisibility-spec' to nil
    ;; because `current-indentation' ignores invisible whitespace chars.
    (let (buffer-invisibility-spec)
      (move-to-column (current-indentation)))
    (let* ((guide-offsets-and-types (elisp-tred-guide--compute-guide-offsets-and-types))
           (guide-string (elisp-tred-guide--make-guide-string guide-offsets-and-types))
           (line-end (save-excursion (forward-line 1) (point)))
           (line-beg (save-excursion (forward-line 0) (point)))
           (existing-overlays (seq-filter
                               (lambda (overlay)
                                 (eq (overlay-get overlay 'category)
                                     'elisp-tred-guide))
                               (overlays-in line-beg line-end))))
      ;; If: number of guides on the current line is <=
      ;; `elisp-tred-guide-min-depth', there are no guides that need to be
      ;; displayed at the beginning of this line. We need to delete any
      ;; existing overlays, and return non-nil if any overlays were deleted.
      (if (<= (length guide-offsets-and-types) elisp-tred-guide-min-depth)
          (when existing-overlays
            (mapc #'delete-overlay existing-overlays))
        ;; Else: One or more guides need to be displayed at the
        ;; beginning of this line.
        ;;
        ;; Unless: there is exactly one existing overlay and it is already
        ;; displaying the correct guide string, delete all existing overlays
        ;; and create a new one with the correct guide string.
        (unless (and (= (length existing-overlays) 1)
                     (string-equal (overlay-get (car existing-overlays) 'line-prefix) guide-string))
          (mapc #'delete-overlay existing-overlays)
          (let* ((overlay (make-overlay line-beg line-end nil nil t)))
            (overlay-put overlay 'category 'elisp-tred-guide)
            (overlay-put overlay 'evaporate t)
            (overlay-put overlay 'line-prefix guide-string)))))))

(defun elisp-tred-guide--update-or-create-overlays-for-current-line ()
  "Create or update the indentation and guide overlays for the current line."
  (let ((indentation-overlay-updated-p
         (elisp-tred-guide--update-or-create-indentation-overlay-for-current-line))
        (guide-overlay-updated-p
         (elisp-tred-guide--update-or-create-guide-overlay-for-current-line)))
    (or indentation-overlay-updated-p
        guide-overlay-updated-p)))

(defun elisp-tred-guide--update-or-create-overlays-for-change-region (change-region)
  "Update indentation and guide overlays in response to a buffer edit.

CHANGE-REGION is a cons cell of the form (BEG . END), which describes
the buffer range where the text content has changed.

This function halts its work as soon as any user input occurs, such as
pressing a key. This is important to keep Emacs responsive, because
some types of buffer edits can require updating a large number of
lines. For example, inserting an unbalanced open paren (`(') at the
beginning of the buffer requires updating the indentation and guide
overlays on every (logical) line of the buffer!

If this function is interrupted by user input, it will return a list
of CHANGE-REGIONs that we can feed back into this function to resume
the work. This ensures that we are able to make incremental progress
on very large updates.

 This function returns nil if it successfully completes all
indentation/guide overlay updates without being interrupted by
user input."
  ;; The RESUME-* variables are used to record where we should
  ;; resume updating lines, if we are interrupted by user input
  ;; (e.g. a key press):
  ;;
  ;; * RESUME-BEFORE: The buffer position we should work backwards from,
;; when updating lines that precede CHANGE-REGION.
;;
  ;; * RESUME-BEG / RESUME-AFTER: The sub-range of CHANGE-REGION where
  ;; we still need to update the indentation/guide overlays.
  ;;
  ;; * RESUME-AFTER: The buffer position we should work forwards from,
  ;; when updating lines that follow CHANGE-REGION.
  (let* ((beg (car change-region))
         (end (cdr change-region))
         (resume-before-pos (save-excursion
                                  (goto-char beg)
                                  (when (= (forward-line -1) 0)
                                    (point))))
         (resume-before (set-marker (make-marker) resume-before-pos))
         (resume-beg (set-marker (make-marker) beg))
         (resume-end (set-marker (make-marker) end))
         (resume-after-pos (save-excursion
                                  (goto-char end)
                                  (forward-line 1)
                                  (when (not (eobp))
                                    (point))))
         (resume-after (set-marker (make-marker) resume-after-pos)))
    (save-excursion
      ;; We use `while-no-input' to interrupt the work when Emacs receives
;; user input (e.g. a key press).
      (while-no-input
        ;; Go to beginning of first line overlapping the change
        ;; region.
        (goto-char beg)
        (forward-line 0)
        ;; Unconditionally update all lines that overlap the change
        ;; region.
		(while (and (not (eobp)) (<= (point) end))
          (elisp-tred-guide--update-or-create-overlays-for-current-line)
          (forward-line)
          ;; Save progress after updating each line, in case we are
          ;; interrupted by user input.
          (set-marker resume-beg (point))
          (when (> (point) end)
            (set-marker resume-end (marker-position resume-beg))))
        (set-marker resume-beg nil)
        (set-marker resume-end nil)
        ;; Update lines following the change region one-by-one,
        ;; until we encounter a line where the existing indentation
        ;; and guide overlays are already up-to-date, or we reach the
        ;; end of the buffer.
        (when (marker-position resume-after)
          (while (and (not (eobp))
                      (elisp-tred-guide--update-or-create-overlays-for-current-line))
           (forward-line 1)
           ;; Save progress after updating each line, in case we are
           ;; interrupted by user input.
           (set-marker resume-after (point))))
        (set-marker resume-after nil)
        ;; Update lines preceding the change region one-by-one,
        ;; until we encounter a line where the existing indentation
        ;; and guide overlays are already up-to-date, or we reach the
        ;; beginning of the buffer.
        (when (marker-position resume-before)
		  (goto-char resume-before)
          (while (and (not (bobp))
                      (elisp-tred-guide--update-or-create-overlays-for-current-line))
            (forward-line -1)
            ;; Save progress after updating each line, in case we are
            ;; interrupted by user input.
            (set-marker resume-before (point)))
          (set-marker resume-before nil))))
    ;; Return a list of ranges that tells us where we need to resume
    ;; the line updates on the next idle timer event, if we were
    ;; interrupted. We may need to return up to two ranges, because
    ;; we need to update the lines both before and after the change
    ;; region.
    ;;
    ;; If we managed to update all lines before being interrupted by
    ;; user input, return nil.
    (let (resume-ranges)
      ;; If: `resume-beg' and `resume-end' are non-nil, it means we
      ;; didn't finish updating the lines within the change region. We
      ;; don't need to explicitly include
      ;; `resume-before'/`resume-after' in the returned list of
      ;; ranges, because those ranges will automatically be recreated.
      (if (and (marker-position resume-beg) (marker-position resume-end))
          (push (cons resume-beg resume-end) resume-ranges)
        ;; Else: We finished updating the lines within the change region.
        ;; Return the ranges for `resume-before' and/or `resume-after',
        ;; depending on how many lines we managed to update before
        ;; getting interrupted.
        (when (marker-position resume-after)
          (push (cons resume-after resume-after) resume-ranges))
        (when (marker-position resume-before)
          (push (cons resume-before resume-before) resume-ranges)))
      resume-ranges)))

(defun elisp-tred-guide--delete-all-overlays ()
  "Destroy all overlays related to Elisp-Tred-Guide in the current
buffer."
  (remove-overlays nil nil 'category 'elisp-tred-guide)
  (remove-overlays nil nil 'category 'elisp-tred-indentation))

;;; Live update algorithm
;;
;; When the user edits the buffer, we need to quickly update the
;; indentation/guide overlays to match the new buffer contents.
;;
;; Key points for understanding the live update algorithm:
;;
;; (1) The update algorithm is line-based, because there is always one
;; indentation overlay per logical line, and one guide overlay per
;; logical line.
;;
;; (2) We use an `after-change-functions' hook to remember the regions
;; where the user has edited the buffer, and then update the
;; lines in those regions using an idle timer.
;;
;; (3) We surround the update work with a `while-no-input' macro, so
;; that Emacs does not become unresponsive to user input while we are
;; updating the overlays. This is important because certain types of
;; buffer edits (e.g. inserting a single unbalanced paren) can affect
;; the overlays for a large number of lines (every line in the file,
;; the worse case).

(defvar-local elisp-tred-guide--change-list nil
  "A list of regions where text has changed.
  Each element in the list is a cons cell of the form (BEG . END).")

(defvar-local elisp-tred-guide--update-timer nil
  "Idle timer that updates the guides, when the user edits the buffer.")

(defun elisp-tred-guide--process-updates-while-no-input (buffer)
  "Update the indentation/guide overlays in response to buffer edits.

To keep Emacs responsive, this function halts work when
any user input occurs (e.g. a key press)."
  (with-current-buffer buffer
    (catch 'done
      (while-let ((change-region (pop elisp-tred-guide--change-list)))
        ;; When
        ;; `elisp-tred-guide--update-or-create-overlays-for-change-region'
        ;; returns a non-nil value for `resume-ranges', it means that
        ;; line updates were interrupted by user input.
        (when-let ((resume-ranges (elisp-tred-guide--update-or-create-overlays-for-change-region change-region)))
          (dolist (resume-range resume-ranges)
            (push resume-range elisp-tred-guide--change-list))
          ;; Return nil to indicate that we were interrupted.
          (throw 'done nil)))
      ;; Return t to indicate that we processed all pending
      ;; buffer changes.
      t)))

(defun elisp-tred-guide--update-timer-rearm ()
  "Reset idle timer for updating indentation and guide overlays."
  (elisp-tred-guide--update-timer-teardown)
  (setq elisp-tred-guide--update-timer
        (run-with-idle-timer 0.05 ;; seconds after Emacs becomes idle
                             t ;; repeat
                             #'elisp-tred-guide--process-updates-while-no-input
                             (current-buffer))))

(defun elisp-tred-guide--update-timer-teardown ()
  "Stop idle timer for updating indentation and guide overlays."
  (when (timerp elisp-tred-guide--update-timer)
    (cancel-timer elisp-tred-guide--update-timer)
    (setq elisp-tred-guide--update-timer nil)))

(defun elisp-tred-guide--record-buffer-change (beg end _length)
  "Record that a buffer change occurred between BEG and END.
This function is invoked by `after-change-functions'."
  (push (cons (set-marker (make-marker) beg) (set-marker (make-marker) end))
        elisp-tred-guide--change-list)
  ;; For reasons I don't fully understand, I need to re-arm the idle
;; timer here, to ensure that it fires reliably after each buffer
;; edit.  Otherwise, the timer sometimes fails to fire after editing
;; the buffer, and guides don't get updated until the user presses an
;; additional key.
  ;; 
  ;; I have seen other modes implement the same workaround. For example,
;; see discussion at [1], or the source code for
;; `aggressive-indent-mode'.
  ;;
  ;; [1]: https://emacs.stackexchange.com/a/71615
  (elisp-tred-guide--update-timer-rearm))

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
      (remove-from-invisibility-spec 'elisp-tred-guide))))

(defun elisp-tred-guide--indent-post-command-hook ()
  (add-to-invisibility-spec 'elisp-tred-guide)
  ;; If point is inside invisible text, move point to the first visible
  ;; character on the line.
  ;;
  ;; This code is added to work around some buggy/quirky behaviour of
  ;; Emacs' line movement commands when interacting with invisible
  ;; text. For example, if I position the cursor at the beginning of a
  ;; `defun' line, and then move down by one line, the cursor _looks_
  ;; like it moves to the first visible character on the next line, but
  ;; the value returned by `current-column' is 0 (even when
  ;; `buffer-invisibility-spec' is nil).
  ;;
  ;; This isn't a huge problem, but it prevents `lispy' from working as
  ;; expected, because `lispy' commands can only be triggered when the
  ;; cursor is positioned before the open paren of a sexp.  It also
  ;; prevents `show-paren-mode' from working as intended.
  (let (line-end-pos (line-end-position))
    (while (and (not (eolp))
                (invisible-p (point)))
      (goto-char (next-char-property-change (point) line-end-pos)))))

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
  (add-to-invisibility-spec 'elisp-tred-guide)
  (elisp-tred-guide--indent-advice-init)
  (elisp-tred-guide--indent-command-hooks-init)
  (elisp-tred-guide--update-timer-rearm)
  (add-hook 'after-change-functions #'elisp-tred-guide--record-buffer-change nil t)
  (add-hook 'kill-buffer-hook #'elisp-tred-guide--update-timer-teardown nil t)
  (setq elisp-tred-guide--change-list (list (cons (point-min-marker) (point-max-marker)))))

(defun elisp-tred-guide--mode-teardown ()
  "Perform necessary teardown when disabling Elisp-Tred-Guide mode."
  (remove-from-invisibility-spec 'elisp-tred-guide)
  (elisp-tred-guide--indent-advice-teardown)
  (elisp-tred-guide--indent-command-hooks-teardown)
  (elisp-tred-guide--update-timer-teardown)
  (elisp-tred-guide--delete-all-overlays)
  (remove-hook 'after-change-functions #'elisp-tred-guide--record-buffer-change t)
  (remove-hook 'kill-buffer-hook #'elisp-tred-guide--update-timer-teardown t))

(define-minor-mode elisp-tred-guide-mode
  "Display tree guides for elisp code."
  :lighter nil
  (if elisp-tred-guide-mode
      (elisp-tred-guide--mode-init)
    (elisp-tred-guide--mode-teardown)))

(provide 'elisp-tred-guide)