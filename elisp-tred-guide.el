(require 'easy-mmode) ;; for `define-minor-mode'

(defcustom elisp-tred-guide-min-handle-width 1
  "Minimum width of guide handle, in characters.")

(defvar elisp-tred-guide--guide-char-with-handle "├")
(defvar elisp-tred-guide--guide-char-with-handle-last "╰")
(defvar elisp-tred-guide--guide-char-without-handle "|")
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

For example, if GUIDE-COLUMNS is ((1) (3) (6 .t)), the return value
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
        (push (concat guide-char
                      (string-join
                       (make-list handle-width guide-extension-char)))
              guide-string-parts)))
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

(defun elisp-tred-guide--create-all ()
  "Create an overlay for each line in the buffer, to display the
guides."
  (save-excursion
    (goto-char (point-min))
    ;; for each line
    (while (not (eobp))
      (message "%s: %s"
               (line-number-at-pos)
               (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
      (elisp-tred-guide--hide-indentation-for-current-line)
      (elisp-tred-guide--create-for-current-line)
      (forward-line))))

(defun elisp-tred-guide--destroy-all ()
  "Destroy all overlays related to Elisp-Tred-Guide in the current
buffer."
  (remove-overlays nil nil 'category 'elisp-tred-guide)
  (remove-overlays nil nil 'category 'elisp-tred-indentation))

(define-minor-mode elisp-tred-guide-mode
  "Display tree guides for elisp code."
  :lighter nil
  (if elisp-tred-guide-mode
      (elisp-tred-guide--create-all)
    (elisp-tred-guide--destroy-all)))

(provide 'elisp-tred-guide)