(require 'elisp-tred-guide)
(require 'ert)

(ert-deftest elisp-tred-guide--test--beginning-next-sexp--valid-next-sexp ()
  (let ((elisp-valid-next-sexp "(a) (b)")
        (buffer (get-buffer-create "elisp-tred-guide: test*")))
    (with-current-buffer buffer
      ;; Test 1: If there is a next sibling sexp, point should move to
      ;; its open paren.
      (erase-buffer)
      (insert elisp-valid-next-sexp)
      (goto-char 1)
      (elisp-tred-guide--beginning-next-sexp)
      (should (= (point) 5)))))

(ert-deftest elisp-tred-guide--test--beginning-next-sexp--no-next-sexp ()
  (let ((elisp-no-next-sexp-and-trailing-whitespace "(a) \n")
        (buffer (get-buffer-create "elisp-tred-guide: test*")))
    (with-current-buffer buffer
      ;; Test 2: If there is no next sibling sexp, point should remain
      ;; unchanged.
      (erase-buffer)
      (insert elisp-no-next-sexp-and-trailing-whitespace)
      (goto-char 1)
      (elisp-tred-guide--beginning-next-sexp)
      (should (= (point) 1)))))

(ert-deftest elisp-tred-guide--test--beginning-next-sexp--unbalanced-open-paren ()
  (let ((elisp-unbalanced-open-paren (concat "(a) (b"))
        (buffer (get-buffer-create "elisp-tred-guide: test*")))
    (with-current-buffer buffer
      ;; Test 3: If there is a next sibling sexp, but its open paren
      ;; is unbalanced, we should still move to it. This makes the
      ;; guides to appear as follows:
      ;;
      ;; ├─ (a)
      ;; ╰─ (b
      (erase-buffer)
      (insert elisp-unbalanced-open-paren)
      (goto-char 1)
      (elisp-tred-guide--beginning-next-sexp)
      (should (= (point) 5)))))

(ert-deftest elisp-tred-guide--test--beginning-next-sexp--unbalanced-close-paren ()
  (let ((elisp-unbalanced-close-paren (concat "(a)) (b)"))
        (buffer (get-buffer-create "elisp-tred-guide: test*")))
    (with-current-buffer buffer
      ;; Test 4: If the starting sexp has an extra closed paren, point
      ;; should remain unchanged. This makes the guides appear as
      ;; follows:
      ;; 
      ;; ├─ (a))
      ;; ╰─ (b)
      (erase-buffer)
      (insert elisp-unbalanced-close-paren)
      (goto-char 1)
      (elisp-tred-guide--beginning-next-sexp)
      (should (= (point) 1)))))

(ert-deftest elisp-tred-guide--test--compute-guide-columns ()
  (let ((elisp (concat "(ab cd\n"
                       "    (ef\n"
                       "     gh))\n"
                       "(ij)"))
        (buffer (get-buffer-create "*elisp-tred-guide: test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert elisp)
      (goto-line 3)
      (move-to-column (current-indentation))
      (should
       (equal (elisp-tred-guide--compute-guide-columns)
              '((0) (4 . t) (5 . t)))))))

(ert-deftest elisp-tred-guide--test--make-guide-string ()
  (let ((elisp-tred-guide-min-handle-width 1))
    (should
     (string=
      (elisp-tred-guide--make-guide-string '((0) (3 . t)))
      (concat
       elisp-tred-guide--guide-char-without-handle
       (string-join (make-list elisp-tred-guide-min-handle-width
                               elisp-tred-guide--guide-char-space))
       elisp-tred-guide--guide-char-with-handle-last
       (string-join (make-list 2 elisp-tred-guide--guide-char-handle)))))))