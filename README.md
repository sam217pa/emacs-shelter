# Shelter

`shelter` is an emacs package that aims to provide context-sensitive
commands, and more broadly, keybindings in situations that are
“sheltered” from the surrounding context.

It does that mostly through the help of the `camp` package, which
provides macros for defining self-inserting commands which action
depends on the local context around point (see shelter-lisp.el for
examples). The [`camp`](#camp) package provides commands that wraps
together the otherwise (mostly) self-sufficient packages like
[`pause`](#pause), [`tent`](#tent) and [`fort`](#fort).

## Camp

Camp is implemented as what I call a “sharp” minor mode, a minor mode
which keymap depends on the major mode it is associated to (much like
the [`selected`](https://github.com/Kungsgeten/selected.el) package).

Basically, that means it is a minor mode whose keybindings overrides
most of other keybindings until it is deactivated, and whose
keybindings depends on the major mode of the associated buffer. In
fact, what camp does at activation is look out for a keymap named
`camp-MM-map`, where MM is the associated major-mode with no ‘-mode’
suffix. When it finds one, this keymap is activated. (This
functionality, that `camp` shares with `fort`, is provided by the
[`aeolian`](#aeolian) package.)

Camp provides three macros:

### `camp`:

`camp` is a wrapper around `cond` that returns _nil_ if none of its
clauses have a truthy value. Clauses are simple arguments in the
`cl-loop` style. They are grouped by four: the two firsts defines the
context, the two lasts defines the action.

Context keyword can be one of:
- `at RE`: returns _t_ when point is at (before) regular expression RE
- `bk RE`: returns _t_ when point looks back (is after) regular
  expression RE.
- `if CLAUSE`: returns _t_ if clause does.

Action keywords can be one of:
- `do (sexp)`: evaluates _sexp_ regularly.
- `call 'fun`: evaluates _fun_ via `funcall` (which means sexp
doesn’t have to be an interactive command).
- `cmd 'fun`: evaluates _fun_ as if called interactively.
- `burn (sexp)`: evaluates _sexp_ but burn camp before that, by
deleting last entered character.
- `tent (body)`: wraps _body_ in a `tent` call (see [`tent`](#tent)
below).
- `pause (body)`: wraps _body_ in a `pause` call (see
[`pause`](#pause) below).

Example:
```lisp
(camp
 at "^#" do (message "yes")
 bk "\)" cmd 'doctor)
```

(Should send you to a listening friend.)

### **`defcamp`**:

`defcamp` defines an interactive command that is self-inserting, that
is that defaults to inserting the key bound to it no clause of its
body evaluates to t (where clauses are passed to the
[`camp`](#camp-1)) macro.

For instance, in the following example, <kbd>m</kbd> is bound to
`camp-lisp-mark`, which means that <kbd>m</kbd> will insert `m` as
usual in the buffer, unless point is before `(` or after `)`, in which
case the following or preceding SEXP is marked.

Example:

```lisp
(defcamp camp-lisp-mark
  "Mark sexp at point"
  at "\(" do (mark-sexp  1)
  bk "\)" do (mark-sexp -1))
(define-key emacs-lisp-mode-map (kbd "m") #'camp-lisp-mark)  
```

### **`camp-defkey`**:

`camp-defkey` goes basically the other way as `defcamp`: it binds
a key to a `camp` body for a given keymap. The equivalent of previous
example would be:

```lisp
(camp-defkey "m" emacs-lisp 
  "Mark sexp at point"
  at "\(" do (mark-sexp  1)
  bk "\)" do (mark-sexp -1))
```

It has the advantage of being able to bind more stuff to a single key
(where <kbd>m</kbd> could stand as “mark” or “move” or “migrate” or
whatever), but being tied to a specific camp keymap
(camp-emacs-lisp-map in that case). That implies that other mode
(like common-lisp or scheme) can’t use it directly.

### **`camp-define-keys`**

An example from
[shelter-lisp.el](https://github.com/sam217pa/emacs-shelter/blob/master/shelter-lisp.el)
should help:

```lisp
(camp-define-keys
 :map emacs-lisp
 :simple
 ("e" 'camp-emacs-lisp-eval
  "n" 'camp-emacs-lisp-next
  "p" 'camp-emacs-lisp-prev
  "b" 'camp-emacs-lisp-bwd
  "f" 'camp-emacs-lisp-fwd
  "x" 'camp-emacs-lisp-exec
  "m" 'camp-lisp-mark))
```

`camp-define-keys` simply adds all the keybindings definition below to
the camp-keymap defined by `:map`. Here <kbd>e</kbd> is bound to
`camp-emacs-lisp-eval` in the `camp-emacs-lisp-map`: Here is what it
does when I press <kbd>e</kbd>.

![camp1](camp1.mp4)

## Tent

As the `tent` feature just got showed up in the previous screencast,
`tent` is a single self-sufficient package with no dependency outside
core emacs features like `cl-lib`.

Tent provides a single macro, `tent` that closely ressembles the
descriptions of an hydra (see the
[`hydra`](https://github.com/abo-abo/hydra) package), though less
sophisticated.

Here is the tent definition of the above-demonstrated tent:

```lisp
(tent 
 '(("b" eval-buffer "buffer")
   ("e" eval-last-sexp "last sexp")
   ("E" pp-eval-last-sexp "pretty last sexp")
   ("x" eval-defun "defun")
   ("m" pp-macroexpand-last-sexp "macro expand")))
```

Each clause contains three elements, the keybinding, the command and
a short description. When tent is evaluated, all keybindings are added
to a transient keymap that overrides every other keymaps, and where
commands are bound to their respective keys. Any other key pressed
deactivates the tent, <kbd>C-g</kbd> in particular. Tent takes care of
tabulating every description and display them in a two column table in
an overlay right below point.

WARNING: as of now, tent tends not to work propperly when the description hits
the window right edge, or the end of buffer.

## Pause

Pause is another attempt at providing commands with simple menu
displayed at point, much like an hydra displayed at point.

It is showcased in the following video:

![pause-1](camp2.mp4)

Pause provides one macro, `pause`, with first argument specifying if
the transient map should stay active as long as keybindings in it are
activated. Next argument is the text displayed in the menu, which can
be arbitrary text, though it is advised to use the `pause-prompt`
function, that decorates text enclosed in special cookies like `[]`,
`{}` or `()`. Next is a simple keybinding definition, with the body of
an interactive command. 

WARNING: for reasons not entirely clear to me, pause commands
not-defined in a context where lexical-binding is set to _t_ doesn’t
work. 

## Fort



## Aeolian

# License

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
