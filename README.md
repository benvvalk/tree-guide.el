# Description

`tree-guide-mode` renders decorative guide lines in lisp source code buffers, which makes the tree structure of lisp code more explicit. In my opinion, it makes lisp code much easier to read and edit.

![Screenshot of tree-guide-mode on some example elisp code](images/screenshot.png)

# Features & Caveats

Features:

* local minor mode, easily toggled on/off with `M-x tree-guide-mode`
* guides update in real time while editing the code
* integrates well with structural editing packages (e.g. `lispy`)
* performs well on large files
* no dependency on `tree-sitter` (uses Emacs' built-in lisp-parsing functions)

Caveats:

* harder to see code indentation problems (I recommend using `aggressive-indent-mode` to address this)
* flickering during guide updates might be distracting
* only tested with elisp code so far

# Installation

`tree-guide.el` is not on MELPA yet. In the meantime, you will need to manually `git clone` the repo:

```bash
mkdir -p ~/git
cd ~/git
git clone https://github.com/benvvalk/tree-guide.el.git
```

Then add the `git clone` directory to your elisp load path.

For example, you can add the following `use-package` expression to your `~/emacs.d/init.el`:

```elisp
(use-package tree-guide
  ;; add `git clone` directory to elisp load path
  :load-path "~/git/tree-guide.el/"
  ;; optional: automatically enable `tree-guide-mode` in *.el files 
  :hook emacs-lisp-mode)
```

# How it Works

To determine the structure of the lisp parse tree, `tree-guide-mode` uses Emacs' built-in [syntax-ppss](https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser-State.html) function. Among other info, `syntax-ppss` returns the start position of the parent expression surrounding POINT, e.g. the position of the open paren ("`(`") for the list containing POINT. `tree-guide-mode` uses this information to walk up the lisp parse tree, by moving POINT to the start position returned by `syntax-ppss` and then recursively calling `syntax-ppss`, until the root of lisp parse tree is reached. (This traversal is implemented in `tree-guide--compute-guide-offsets-and-flags-for-current-line`.)

To display the tree guide characters, `tree-guide-mode` uses an Emacs feature called [overlays](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html), which provides the ability to display additional "virtual" characters in the buffer, i.e. characters which do not actually exist in the underlying buffer. Interestingly, overlays also provide the ability to _hide_ real characters that exist in the underlying buffer, and `tree-guide-mode` makes use of both abilities. For each line in the buffer, `tree-guide-mode` creates two overlays: (1) an overlay to hide the string of leading space/TAB characters at the beginning of the line (real characters), and (2) an overlay to display the guide string (virtual characters, e.g. "`│ ├─`"). The function that creates/updates these two overlays for current line is `tree-guide--update-or-create-overlays-for-current-line`.

To update the tree guides in reponse to user edits, `tree-guide-mode` uses an [after-change-functions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Change-Hooks.html) hook to record the buffer ranges where text has been edited, and process the guide updates "in the background" (i.e. when the user is not typing) using an [idle timer](https://www.gnu.org/software/emacs/manual/html_node/elisp/Idle-Timers.html). To make the update algorithm as easy-to-understand as possible, the  update logic is line-based. When updating the guide overlays for an edited buffer range, first the lines overlapping the edited range are updated, and then the updates are incrementally expanded to include lines before and after the edited range, stopping only when a line is encountered where the guide overlay is already up-to-date (i.e. the newly computed guide string is identical to the existing guide string). The update algorithm needs to consider lines before and after the edited buffer range because buffer edits can have far-reaching effects on the lisp parse tree, and it is difficult to predict exactly how which lines will be affected. For example, inserting a single open paren character ("`(`") at the beginning of a buffer will increase the nesting level of every lisp expression that follows it, and will therefore require updating the guide overlays on every line of the buffer (!). Such large updates do not pose a problem in terms of performance/responsiveness, because the guide overlays are updated incrementally using an idle timer, and on-screen lines are always updated before off-screen lines.