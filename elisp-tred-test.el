(require 'elisp-tred)
(require 'ert)

(defun elisp-tred--test-buffer-create (quoted-elisp-form)
  "Create an elisp-tred buffer whose entire content is
QUOTED-ELISP-FORM."
  (let ((buffer (get-buffer-create "*elisp-tred-test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (prin1 quoted-elisp-form (current-buffer))
      (elisp-tred-mode))
    buffer))

(ert-deftest elisp-tred--test-render-list-simple ()
  "Test tree rendering for simple list."
  (should
   (string=
    (elisp-tred--render-elisp-form-to-string '(a b))
    (concat "╰─ (a\n"
            "   ╰─ b)"))))

(ert-deftest elisp-tred--test-edit-delete-open-paren-simple ()
  (let ((buffer (elisp-tred--test-buffer-create '(a b))))
    ;; test buffer state before
    (should
     (string=
      (elisp-tred--render-buffer-to-string buffer)
      (concat "╰─ (a\n"
              "   ╰─ b)")))
	;; perform edit: delete open paren '('
    (with-current-buffer buffer
	  (goto-char (point-min))
      (delete-char 1)
      (elisp-tred--update-buffer))
    ;; test buffer state after
    ;;
    ;; NOTE: unbalanced closing paren ')' appears on its own line
    ;; because it is a treesit error node
    (should
     (string=
      (elisp-tred--render-buffer-to-string buffer)
      (concat "├─ a\n"
              "├─ b\n"
              "╰─ )")))))