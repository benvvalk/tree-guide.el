;;; -*- lexical-binding: t -*-

(require 'easy-mmode) ;; for `define-minor-mode'

;;; Guide rendering
;;
;; For each logical line in the buffer, create a guide overlay.

(defcustom tree-guide-min-handle-width 1
  "Minimum width of guide handle, in characters.")

(defcustom tree-guide-min-depth 1
  "The minimum depth for which to render guides.

Top-level sexps are at depth 0, children of top-level sexps are at
depth 1, and so on.")

(defcustom tree-guide-face 'shadow
  "The face used to render the tree guides.")

(defvar tree-guide--guide-char-with-handle "├")
(defvar tree-guide--guide-char-with-handle-last "╰")
(defvar tree-guide--guide-char-without-handle "│")
(defvar tree-guide--guide-char-handle "─")
(defvar tree-guide--guide-char-space " ")

(defun tree-guide--parent-sexp-end-position ()
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

(defun tree-guide--last-line-at-current-depth-p ()
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
             (parent-sexp-end (tree-guide--parent-sexp-end-position))
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

(defun tree-guide--compute-guide-offsets-and-flags-for-current-line ()
  "Compute the guide offsets and flags for the current line.

The return value is a list of cons cells, where the CAR is the column
position for a guide, and the CDR is the LAST-CHILD flag. The
LAST-CHILD flag is non-nil when is ancestor sexp that owns the guide
is the last child of its parent."
  (save-excursion
    ;; Note: I set `buffer-invisibility-spec' to nil here to work
    ;; around a strange intermittent bug, where `goto-char' sometimes
    ;; moves point to the beginning of the line, rather than the
    ;; target position (i.e. the open paren of the parent sexp). It is
    ;; probably related to the following excerpt of the "Invisible
    ;; Text" section in the Emacs manual [1]:
    ;;
    ;;   "If a command ends with point inside or at the boundary of
    ;;   invisible text, the main editing loop relocates point to one of
    ;;   the two ends of the invisible text. Emacs chooses the direction
    ;;   of relocation so that it is the same as the overall movement
    ;;   direction of the command..."
    ;;
    ;; [1]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Invisible-Text.html
    (let* (buffer-invisibility-spec
           guide-offsets-and-flags
           done)
      ;; set initial position of point to first non-whitespace char
      ;; on current line
      (move-to-column (current-indentation))
      (while (not done)
        (let* ((parent-sexp-beg (nth 1 (syntax-ppss)))
               (last-child-p (tree-guide--last-line-at-current-depth-p))
               (parent-on-same-line-p (when parent-sexp-beg
                                        (save-excursion
                                          ;; fast jump to beginning of line
                                          (forward-line 0)
                                          (<= (point) parent-sexp-beg))))
               (parent-column (if parent-sexp-beg
                                  (save-excursion
                                    (goto-char parent-sexp-beg)
                                    (current-column))
                                0))
               (guide-offset (- (current-column) parent-column)))
          (push (list guide-offset parent-on-same-line-p last-child-p)
                guide-offsets-and-flags)
          ;; Set up for next `while' loop iteration, by moving point to
          ;; beginning of parent sexp. If there is no parent sexp,
          ;; we are done.
          (if parent-sexp-beg
              (goto-char parent-sexp-beg)
            (setq done t))))
      guide-offsets-and-flags)))

(defun tree-guide--make-guide-string (guide-offsets-and-flags)
  "Make a guide string from GUIDE-OFFSETS-AND-FLAGS.

For example, if GUIDE-OFFSETS-AND-FLAGS is ((1) (3) (6 . t)), the return value
will be '| | ╰'.

See the docstring for `tree-guide--compute-guide-offsets-and-flags' for
further information about the structure/meaning of GUIDE-OFFSETS-AND-FLAGS."
  (let (guide-string-parts
        (num-guides (length guide-offsets-and-flags)))
    (dotimes (i num-guides)
      (let* ((rightmost-guide-p (>= i (1- num-guides)))
             (guide-offset-and-flags (nth i guide-offsets-and-flags))
             (guide-offset (nth 0 guide-offset-and-flags))
             (parent-on-same-line-p (nth 1 guide-offset-and-flags))
             (guide-type-last-p (nth 2 guide-offset-and-flags))
             (guide-char (if rightmost-guide-p
                             (if guide-type-last-p
                                 tree-guide--guide-char-with-handle-last
                               tree-guide--guide-char-with-handle)
                           (if guide-type-last-p
                               tree-guide--guide-char-space
                             tree-guide--guide-char-without-handle)))
             (num-padding-chars (if parent-on-same-line-p
                                    (max (1- guide-offset) 0)
                                  (max
                                   (1- guide-offset)
                                   tree-guide-min-handle-width)))
             (padding-char (if rightmost-guide-p
                               tree-guide--guide-char-handle
                             tree-guide--guide-char-space)))
        (when (>= i tree-guide-min-depth)
          (push (propertize
                 (concat
                  guide-char
                  (string-join (make-list num-padding-chars padding-char)))
                 'face
                 tree-guide-face)
                guide-string-parts))))
    (string-join (nreverse guide-string-parts))))

(defun tree-guide--update-or-create-indentation-overlay-for-current-line ()
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
          (overlay-put overlay 'invisible 'tree-guide))))))

(defun tree-guide--update-or-create-guide-overlay-for-current-line ()
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
    ;; (1) `tree-guide--compute-guide-offsets-and-types' requires
    ;; point to be located immediately before the first non-whitespace
    ;; character on the current line.
    ;;
    ;; (2) We need to temporarily set `buffer-invisibility-spec' to nil
    ;; because `current-indentation' ignores invisible whitespace chars.
    (let (buffer-invisibility-spec)
      (move-to-column (current-indentation)))
    (let* ((guide-offsets-and-flags (tree-guide--compute-guide-offsets-and-flags-for-current-line))
           (guide-string (tree-guide--make-guide-string guide-offsets-and-flags))
           (line-end (save-excursion (forward-line 1) (point)))
           (line-beg (save-excursion (forward-line 0) (point)))
           (existing-overlays (seq-filter
                               (lambda (overlay)
                                 (eq (overlay-get overlay 'category)
                                     'tree-guide))
                               (overlays-in line-beg line-end))))
      ;; If: number of guides on the current line is <=
      ;; `tree-guide-min-depth', there are no guides that need to be
      ;; displayed at the beginning of this line. We need to delete any
      ;; existing overlays, and return non-nil if any overlays were deleted.
      (if (<= (length guide-offsets-and-flags) tree-guide-min-depth)
          (when existing-overlays
            (mapc #'delete-overlay existing-overlays))
        ;; Else: One or more guides need to be displayed at the
        ;; beginning of this line.
        ;;
        ;; Unless: there is exactly one existing overlay and it
        ;; already has the correct start/end positions and guide
        ;; string, delete all existing overlays and create a new
        ;; overlay.
        (let ((existing-overlay (car existing-overlays)))
          (unless (and (= (length existing-overlays) 1)
                       (= (overlay-start existing-overlay) line-beg)
                       (= (overlay-end existing-overlay) line-end)
                       (string-equal (overlay-get existing-overlay 'line-prefix) guide-string))
           (mapc #'delete-overlay existing-overlays)
           (let* ((overlay (make-overlay line-beg line-end nil nil t)))
             (overlay-put overlay 'category 'tree-guide)
             (overlay-put overlay 'evaporate t)
             (overlay-put overlay 'line-prefix guide-string))))))))

(defun tree-guide--update-or-create-overlays-for-current-line ()
  "Create or update the indentation and guide overlays for the current line."
  (let ((indentation-overlay-updated-p
         (tree-guide--update-or-create-indentation-overlay-for-current-line))
        (guide-overlay-updated-p
         (tree-guide--update-or-create-guide-overlay-for-current-line)))
    (or indentation-overlay-updated-p
        guide-overlay-updated-p)))

(defun tree-guide--update-or-create-overlays-for-change-region (change-region &optional update-bounds)
  "Update indentation and guide overlays in response to a buffer edit.

CHANGE-REGION is a cons cell of the form (BEG . END), which describes
the buffer range where the text content has changed.

This function halts its work as soon as any user input occurs, such as
pressing a key. This is important to keep Emacs responsive, because
some types of buffer edits can require updating a large number of
lines. For example, inserting an unbalanced open paren (`(') at the
beginning of the buffer requires updating the indentation and guide
overlays on every (logical) line of the buffer!

The return value of this function is a cons cell of the
form (INTERRUPTED-P . RESUME-RANGES), where INTERRUPTED-P
is non-nil if updates were interrupted by user input,
and RESUME-RANGES is a list of CHANGE-REGION values that can be
passed back into this function (one by one) to complete the updates.
The use of RESUME-RANGES ensures that we are able to make incremental progress
on very large updates.

By default, this function updates all lines in CHANGE-REGION
unconditionally, and then continues to update successive lines
before/after CHANGE-REGION until it encounters a line that is already
up-to-date, i.e. a line where the recomputed indentation/guide
overlays are identical to the existing overlays.

However, if the optional argument UPDATE-BOUNDS is provided, only
logical lines that overlap UPDATE-BOUNDS will be updated, and any
buffer ranges outside of of those lines will be returned as items in
RESUME-RANGES. The main purpose of UPDATE-BOUNDS is to restrict
updates to on-screen lines, so that on-screen updates happen as
quickly as possible."
  ;; Extend CHANGE-REGION and UPDATE-BOUNDS to include the entirety of
  ;; their first and last logical lines. This prevents subtle bugs
  ;; where the updates to the first/last lines might be incorrectly
  ;; skipped.
  (let* ((change-beg (save-excursion
                       (goto-char (car change-region))
                       (forward-line 0)
                       (point)))
         (change-end (save-excursion
                       (goto-char (cdr change-region))
                       (forward-line 1)
                       (point)))
         (update-beg (if update-bounds
                         (save-excursion
                           (goto-char (car update-bounds))
                           (forward-line 0)
                           (point))
                       (point-min)))
         (update-end (if update-bounds
                         (save-excursion
                           (goto-char (cdr update-bounds))
                           (forward-line 1)
                           (point))
                       (point-max)))
         ;; indicates if we were interrupted by user input, before we could
         ;; update all logical lines that overlap UPDATE-BOUNDS
         interrupted-p
         ;; `resume-ranges' tells us which ranges still need to be
         ;; updated, in the case that we are interrupted by user input.
         resume-ranges)
    ;; If CHANGE-REGION is outside of UPDATE-BOUNDS, there is no work
    ;; to do. We simply need to return the original value
    ;; CHANGE-REGION as sole item of `resume-ranges', so that the
    ;; update work will be completed later.
    (if (null (tree-guide--range-intersect
               (cons change-beg change-end)
               (cons update-beg update-end)))
        (push change-region resume-ranges)
      (let* ((beg (max change-beg update-beg))
             (end (min change-end update-end))
             ;; The `resume-*' variables are used to record where we should
             ;; resume updating lines, if we are interrupted by user input
             ;; (e.g. a key press):
             ;;
             ;; * `resume-before': The buffer position we should work backwards from,
             ;; when updating lines that precede CHANGE-REGION.
             ;;
             ;; * `resume-beg'/`resume-end': The sub-range of CHANGE-REGION where
             ;; we still need to update the indentation/guide overlays.
             ;;
             ;; * `resume-after': The buffer position we should work forwards from,
             ;; when updating lines that follow CHANGE-REGION.
             (resume-before (make-marker))
             (resume-beg (set-marker (make-marker) beg))
             (resume-end (set-marker (make-marker) end))
             (resume-after (make-marker)))
        ;; If `change-region' extends beyond `update-bounds', add the
        ;; out-of-bounds subranges to `resume-ranges', to ensure that
        ;; they will be updated later.
        (when (< change-beg update-beg)
          (push (cons
                 (set-marker (make-marker) change-beg)
                 (set-marker (make-marker) update-beg))
                resume-ranges))
        (when (> change-end update-end)
          (push (cons
                 (set-marker (make-marker) update-end)
                 (set-marker (make-marker) change-end))
                resume-ranges))
        (save-excursion
          ;; We use `while-no-input' to interrupt the work when Emacs receives
          ;; user input (e.g. a key press).
          (setq interrupted-p t)
          (while-no-input
            ;; Go to start of `change-region'.
            (goto-char beg)
            ;; Unconditionally update all lines that overlap `change-region'.
	        (while (and (not (eobp)) (< (point) end))
              (tree-guide--update-or-create-overlays-for-current-line)
              ;; Note: We need to set `resume-before' after we update
              ;; the first line of `change-region', because after that
              ;; there will be a unknown gap of one or more lines
              ;; between `resume-before' and `resume-beg'.
              (when (= (point) beg)
                (set-marker resume-before (save-excursion
                                            (when (= (forward-line -1) 0)
                                              (point)))))
              (forward-line)
              (when (< (point) end)
                (set-marker resume-beg (point))))
            (unless (eobp)
              (set-marker resume-after (point)))
            ;; Set `resume-beg'/`resume-end' to nil, to indicate that
            ;; `change-region' is now fully up-to-date.
            (set-marker resume-beg nil)
            (set-marker resume-end nil)
            ;; Update lines following the change region one-by-one,
            ;; until we encounter a line where the existing indentation
            ;; and guide overlays are already up-to-date, or we reach the
            ;; end of the buffer.
            (while (and (marker-position resume-after)
                        (< (point) update-end))
              (if (not (tree-guide--update-or-create-overlays-for-current-line))
                  (set-marker resume-after nil)
                (forward-line 1)
                (if (eobp)
                    (set-marker resume-after nil)
                  (set-marker resume-after (point)))))
            ;; Update lines preceding the change region one-by-one,
            ;; until we encounter a line where the existing indentation
            ;; and guide overlays are already up-to-date, or we reach the
            ;; beginning of the buffer.
            (when (marker-position resume-before)
		      (goto-char resume-before)
              (while (and (marker-position resume-before)
                          (>= (point) update-beg))
                (if (not (tree-guide--update-or-create-overlays-for-current-line))
                    (set-marker resume-before nil)
                  (forward-line -1)
                  (if (bobp)
                      (set-marker resume-before nil)
                    (set-marker resume-before (point))))))
            ;; Set `interrupted-p' to nil, to indicate that we finished
            ;; all line updates without being interrupted by user input.
            (setq interrupted-p nil)))
        ;; Return a list of ranges that tells us where we need to resume
        ;; the line updates next time, if we were interrupted by input. We
        ;; may need to return up to three ranges, because we also need to
        ;; update the lines before and after the change region.
        ;;
        ;; If we managed to update all lines before being interrupted,
        ;; return nil.
        (when (marker-position resume-before)
          (push (cons resume-before resume-before) resume-ranges))
        (when (and (marker-position resume-beg) (marker-position resume-end))
          (push (cons resume-beg resume-end) resume-ranges))
        (when (marker-position resume-after)
          (push (cons resume-after resume-after) resume-ranges))))
    (cons interrupted-p resume-ranges)))

(defun tree-guide--delete-all-overlays ()
  "Destroy all overlays related to tree-Guide in the current
buffer."
  (remove-overlays nil nil 'category 'tree-guide)
  (remove-overlays nil nil 'category 'elisp-tred-indentation))

;;; Operations on line/character ranges
;; (used by live update algorithm below)

(defun tree-guide--make-marker-range (beg end)
  "Create a marker range from BEG and END.

A marker range is a cons cell of the form (BEG . END), where both BEG
and END are markers.

 Both BEG and END may be provided as either a number or a marker, In
the case that BEG/END is a marker, it will used as-is in the returned
range (a new marker will not be created)."
  (let* ((marker-beg (if (markerp beg)
                         beg
                       (set-marker (make-marker) beg)))
         (marker-end (if (markerp end)
                         end
                       (set-marker (make-marker) end))))
    (cons marker-beg marker-end)))

(defun tree-guide--range-to-markers (range)
  "Convert RANGE to a marker range.

RANGE should be a cons cell of the form (BEG . END), where each of
BEG and END may be either a number or a marker.

In the case that both BEG and END are already markers, this function
will simply return RANGE unchanged."
  (when range
    (tree-guide--make-marker-range
     (car range)
     (cdr range))))

(defun tree-guide--range-contains-p (range1 range2)
  "Return non-nil if RANGE1 contains RANGE2."
  (when (and range1 range2)
    (let ((beg1 (car range1))
          (beg2 (car range2))
          (end1 (cdr range1))
          (end2 (cdr range2)))
      (and (<= beg1 beg2) (>= end1 end2)))))

(defun tree-guide--range-intersect (range1 range2 &optional allow-zero-width-p)
  "Return the line range that is the intersection of line ranges RANGE1
and RANGE2.

 The returned value is a cons cell, where the CAR is the starting line
number and the CDR is the end line number (inclusive). If the ranges
don't intersect, the return value will be nil.

Example: If RANGE1 is '(1 . 5) and RANGE2 is '(4 . 9), then the result
is '(4 . 5)."
  (when (and range1 range2)
    (let* ((beg1 (car range1))
           (beg2 (car range2))
           (end1 (cdr range1))
           (end2 (cdr range2))
           (max-beg (max beg1 beg2))
           (min-end (min end1 end2)))
      ;; If `max-beg' > `min-end', it means that `range1' and
      ;; `range2' do not intersect.
      (when (or (< max-beg min-end)
                (and allow-zero-width-p (= max-beg min-end)))
        (cons max-beg min-end)))))

(defun tree-guide--range-merge (range1 range2)
  "Return the merged range for RANGE1 and RANGE2.

Return nil if RANGE1 and RANGE2 are not overlapping or directly
adjacent."
  (let* ((beg1 (car range1))
         (beg2 (car range2))
         (end1 (cdr range1))
         (end2 (cdr range2))
         (max-beg (max beg1 beg2))
         (min-end (min end1 end2)))
    ;; When ranges intersect or are adjacent.
    (when (<= max-beg (1+ min-end))
      (cons (min beg1 beg2) (max end1 end2)))))

(defun tree-guide--sorted-range-list-insert (range range-list)
  "Insert RANGE into the sorted range list RANGE-LIST, and
return the new sorted range list.

If RANGE overlaps existing ranges in RANGE-LIST, it will be
merged with those ranges.

Note: This function modifies the original list RANGE-LIST. In other
words, the returned list reuses the cons cells from RANGE-LIST."
  (let ((merged-range range)
        result)
    (while (and range-list merged-range)
      (let ((current-range (car range-list)))
        (if (< (cdr merged-range) (1- (car current-range)))
            ;; If: `merged-range' ends before `current-range',
            ;; insert `merged-range' at current position and finish.
            (progn
              (push (tree-guide--range-to-markers merged-range) result)
              (setq result (nconc (nreverse range-list) result))
              ;; signal to exit `while' loop
              (setq merged-range nil))
          ;; Else: Check if `merged-range' overlaps `current-range'.
          ;; If `merge-result' is non-nil, it means the ranges overlap.
          ;; If `merge-result' is nil, it means that `merged-range'
          ;; comes after `current-range'. Insert `current-range'
          ;; at current position and continue.
          (if-let ((merge-result (tree-guide--range-to-markers
                                  (tree-guide--range-merge
                                   merged-range
                                   current-range))))
              (setq merged-range merge-result)
            (push current-range result))
          ;; Prepare for next `while' loop iteration.
          (pop range-list))))
    ;; Edge case: If `merged-range' has not yet been inserted, it means
    ;; that `merged-range' needs to be inserted in the last position.
    (when merged-range
      (push merged-range result))
    (nreverse result)))

(defun tree-guide--sorted-range-list-subtract (range range-list)
  "Subtract RANGE from the sorted range list RANGE-LIST, and return
the new sorted range list.

Note: This function modifies the original list RANGE-LIST. In other
words, the returned list reuses the cons cells from RANGE-LIST."
  (let ((beg (car range))
        (end (cdr range))
        result)
    (dolist (current-range range-list)
      (let ((current-beg (car current-range))
            (current-end (cdr current-range)))
        (if (or (< current-end beg) (> current-beg end))
            ;; If ranges don't overlap, keep current range.
            (push current-range result)
          ;; Else, ranges overlap.
          ;; Subtract `range' from `current-range', which may split
          ;; `current-range' into two ranges.
          (when (< current-beg beg)
            (push (tree-guide--make-marker-range current-beg (1- beg))
                  result))
          (when (> current-end end)
            (push (tree-guide--make-marker-range (1+ end) current-end)
                  result)))))
    (nreverse result)))

(defun tree-guide--sorted-range-list-intersect (range range-list)
  "Return the intersection ranges of RANGE and RANGE-LIST.

The intersections are returned as a sorted range list."
  (let (result)
    (dolist (current-range range-list)
      (when-let ((intersection (tree-guide--range-intersect current-range range)))
        (setq result
              (tree-guide--sorted-range-list-insert intersection result))))
    result))

(defun tree-guide--sorted-range-lists-intersect (range-list1 range-list2 &optional allow-zero-width-p)
  "Return the intersection ranges of RANGE-LIST1 and RANGE-LIST2."
  (let (result)
    (dolist (range1 range-list1)
      (dolist (range2 range-list2)
        (when-let (intersection (tree-guide--range-intersect range1 range2 allow-zero-width-p))
          (setq result
                (tree-guide--sorted-range-list-insert intersection result)))))
    result))

(defun tree-guide--sorted-range-list-find-containing (range range-list)
  "Return the unique range in RANGE-LIST that contains RANGE.

There will be at most one range in RANGE-LIST that contains RANGE,
because RANGE-LIST is assumed to be a sorted and non-redundant range
list.

If there is no range in RANGE-LIST that contains RANGE, the return
value will be nil."
  (catch 'done
    (dolist (current-range range-list)
      (when (tree-guide--range-contains-p current-range range)
        ;; Note: There can be at most one containing range in RANGE-LIST,
        ;; because RANGE-LIST is assumed to be a sorted range list with no
        ;; redundant/overlapping ranges.
        (throw 'done current-range)))
    ;; Return nil to indicate that we did not find a range in
    ;; RANGE-LIST that contains RANGE.
    nil))

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

(defvar-local tree-guide--change-list nil
  "A list of regions where text has changed.
  Each element in the list is a cons cell of the form (BEG . END).")

(defvar-local tree-guide--update-timer nil
  "Idle timer that updates the guides, when the user edits the buffer.")

(defun tree-guide--on-screen-range-list (buffer)
  "Return the list of buffer ranges that are on-screen.

  \"On-screen\" buffer ranges are character ranges that are
currently visible to the user in one or more Emacs windows."
  (let (on-screen-range-list
        (window-list (get-buffer-window-list buffer)))
    (dolist (window window-list)
      (setq on-screen-range-list
            (tree-guide--sorted-range-list-insert
             (cons (window-start window) (window-end window))
             on-screen-range-list)))
    on-screen-range-list))

(defun tree-guide--process-on-screen-updates-while-no-input (buffer)
  "Update any on-screen lines of BUFFER that are out-of-date.

\"On-screen\" lines are lines that are currently visible to the user
in one or more Emacs windows.

\"Out-of-date\" lines are lines whose indentation/guide overlays are
no longer correct with respect text content of the buffer, due to user
edits.

To keep Emacs responsive, this function will halt its processing if
any user input occurs (e.g. a key press).

The return value of this function is non-nil if one or more on-screen
lines were updated, regardless of whether the processing was
interrupted by user input."
  (let (did-work-p)
    (catch 'done
      (with-current-buffer buffer
        (when-let* ((on-screen-range-list (tree-guide--on-screen-range-list buffer))
                    (on-screen-change-list (tree-guide--sorted-range-lists-intersect
                                            on-screen-range-list
                                            tree-guide--change-list
                                            t)))
          (setq did-work-p t)
          (dolist (on-screen-change on-screen-change-list)
            (let* ((update-bounds (tree-guide--sorted-range-list-find-containing
                                   on-screen-change
                                   on-screen-range-list))
                   (update-result (tree-guide--update-or-create-overlays-for-change-region
                                   on-screen-change
                                   update-bounds))
                   (interrupted-p (car update-result))
                   (resume-ranges (cdr update-result)))
              (setq tree-guide--change-list
                    (tree-guide--sorted-range-list-subtract
                     on-screen-change
                     tree-guide--change-list))
              (dolist (resume-range resume-ranges)
                (setq tree-guide--change-list
                      (tree-guide--sorted-range-list-insert
                       resume-range
                       tree-guide--change-list)))
              (when interrupted-p
                (throw 'done did-work-p)))))))
    did-work-p))

(defun tree-guide--process-updates-while-no-input (buffer)
  "Update any lines of BUFFER that are out-of-date.

\"Out-of-date\" lines are lines whose indentation/guide overlays are
no longer correct with respect text content of the buffer, due to user
edits.

To keep Emacs responsive, this function will halt its processing if
any user input occurs (e.g. a key press).

The return value of this function is non-nil if all out-of-date lines
were successfully updated without being interrupted by user input."
  (if (tree-guide--process-on-screen-updates-while-no-input buffer)
      ;; If we updated one or more on-screen lines, briefly return control
      ;; to Emacs so that it can immediately refresh the display, before
      ;; continuing with off-screen updates.
      (run-with-timer 0 nil #'tree-guide--process-updates-while-no-input buffer)
    (with-current-buffer buffer
      (catch 'done
        (while-let ((change-region (pop tree-guide--change-list)))
          ;; When
          ;; `tree-guide--update-or-create-overlays-for-change-region'
          ;; returns a non-nil value for `resume-ranges', it means that
          ;; line updates were interrupted by user input.
          (let* ((update-result (tree-guide--update-or-create-overlays-for-change-region change-region))
                 (interrupted-p (car update-result))
                 (resume-ranges (cdr update-result)))
            (dolist (resume-range resume-ranges)
              (setq tree-guide--change-list
                    (tree-guide--sorted-range-list-insert
                     resume-range
                     tree-guide--change-list)))
            ;; Return nil to indicate that we were interrupted.
            (when interrupted-p
              (throw 'done nil))))
        ;; Return t to indicate that we processed all pending
        ;; buffer changes.
        t))))

(defun tree-guide--update-timer-rearm ()
  "Reset idle timer for updating indentation and guide overlays."
  (tree-guide--update-timer-teardown)
  (setq tree-guide--update-timer
        (run-with-idle-timer 0.05 ;; seconds after Emacs becomes idle
                             t ;; repeat
                             #'tree-guide--process-updates-while-no-input
                             (current-buffer))))

(defun tree-guide--update-timer-teardown ()
  "Stop idle timer for updating indentation and guide overlays."
  (when (timerp tree-guide--update-timer)
    (cancel-timer tree-guide--update-timer)
    (setq tree-guide--update-timer nil)))

(defun tree-guide--record-buffer-change (beg end _length)
  "Record that a buffer change occurred between BEG and END.
This function is invoked by `after-change-functions'."
  (push (cons (set-marker (make-marker) beg) (set-marker (make-marker) end))
        tree-guide--change-list)
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
  (tree-guide--update-timer-rearm))

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

(defcustom tree-guide-regexps-for-commands-that-require-visible-indentation
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

(defun tree-guide--indent-advice (orig-fn &rest args)
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

(defun tree-guide--indent-advice-init ()
  "Add advice to Emacs' built-in line/region indentation functions, so
that they work correctly in Elisp-Tred mode.

See the docstring for `tree-guide--indent-advice' for further
explanation."
  (advice-add indent-line-function :around #'tree-guide--indent-advice)
  (advice-add indent-region-function :around #'tree-guide--indent-advice))

(defun tree-guide--indent-advice-teardown ()
  "Remove advice from Emacs' built-in line/region indentation
functions.

See the docstring for `tree-guide--indent-advice' for further
explanation."
  (advice-remove indent-line-function #'tree-guide--indent-advice)
  (advice-remove indent-region-function #'tree-guide--indent-advice))

(defun tree-guide--indent-pre-command-hook ()
  (let ((command-name (symbol-name this-command)))
    (when (seq-find
           (lambda (regexp)
             (string-match regexp command-name nil t))
           tree-guide-regexps-for-commands-that-require-visible-indentation)
      (remove-from-invisibility-spec 'tree-guide))))

(defun tree-guide--indent-post-command-hook ()
  (add-to-invisibility-spec 'tree-guide)
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

(defun tree-guide--indent-command-hooks-init ()
  (add-hook 'pre-command-hook #'tree-guide--indent-pre-command-hook nil t)
  (add-hook 'post-command-hook #'tree-guide--indent-post-command-hook nil t))

(defun tree-guide--indent-command-hooks-teardown ()
  (remove-hook 'pre-command-hook #'tree-guide--indent-pre-command-hook t)
  (remove-hook 'post-command-hook #'tree-guide--indent-post-command-hook t))

;;; Minor mode definition

(defun tree-guide--mode-init ()
  "Performs necessary initialization when enabling tree-Guide
mode."
  (add-to-invisibility-spec 'tree-guide)
  (tree-guide--indent-advice-init)
  (tree-guide--indent-command-hooks-init)
  (add-hook 'after-change-functions #'tree-guide--record-buffer-change nil t)
  (add-hook 'kill-buffer-hook #'tree-guide--update-timer-teardown nil t)
  ;; add change region for entire buffer, so that initial guides get
  ;; created on each line
  (setq tree-guide--change-list (list (cons (point-min-marker) (point-max-marker))))
  (tree-guide--update-timer-rearm))

(defun tree-guide--mode-teardown ()
  "Perform necessary teardown when disabling tree-Guide mode."
  (remove-from-invisibility-spec 'tree-guide)
  (tree-guide--indent-advice-teardown)
  (tree-guide--indent-command-hooks-teardown)
  (tree-guide--update-timer-teardown)
  (tree-guide--delete-all-overlays)
  (remove-hook 'after-change-functions #'tree-guide--record-buffer-change t)
  (remove-hook 'kill-buffer-hook #'tree-guide--update-timer-teardown t))

(define-minor-mode tree-guide-mode
  "Display tree guides for elisp code."
  :lighter nil
  (if tree-guide-mode
      (tree-guide--mode-init)
    (tree-guide--mode-teardown)))

(provide 'tree-guide)