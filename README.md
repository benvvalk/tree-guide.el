## Troubleshooting

Troubleshooting tree-sitter grammar errors in Emacs is a bit annoying.

If you are seeing tree-sitter-related errors (e.g. ABI version errors), you should manually delete
`~/.emacs.d/tree-sitter/tree-sitter-elisptred.so` and restart Emacs. The next time you use `elisp-tred`, it will prompt you to install the grammar, and everything should work fine after that.

Restarting Emacs is important when troubleshooting tree-sitter problems, because Emacs doesn't automatically reload the tree-sitter grammars (shared libraries) when they change on disk. Emacs will only load a grammar  *once* when it is first used, and thereafter you can't unload it or reload it for the rest of Emacs' lifetime.