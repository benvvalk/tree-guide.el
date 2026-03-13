(require 'elisp-tred)
(require 'ert)

(ert-deftest elisp-tred--test-render-list-simple ()
  "Test tree rendering for simple list."
  (should
   (string=
    (elisp-tred--render-elisp-form-to-string '(a b c))
    (concat "╰─ (a\n"
            "   ├─ b\n"
            "   ╰─ c)"))))
