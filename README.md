[![MELPA](https://melpa.org/packages/atom-one-dark-theme-badge.svg)](https://melpa.org/#/atom-one-dark-theme)
[![MELPA Stable](https://stable.melpa.org/packages/atom-one-dark-theme-badge.svg)](https://stable.melpa.org/#/atom-one-dark-theme)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# Atom One Dark Theme for Emacs

This is a port of the Atom One Dark theme from
[Atom](https://github.com/atom/atom), which was sunset in December
2022. The original One Dark theme for Atom can be found here:

* [Atom One Dark UI](https://github.com/atom/one-dark-ui)
* [Atom One Dark Syntax](https://github.com/atom/one-dark-syntax)

The theme covers a wide range of built-in and third-party modes. If a
mode you use is still missing, pull requests are extremely welcomed! :)

## Installation

### From Packages (MELPA)

Make sure you have enabled the [MELPA](https://melpa.org/) package
archive in your Emacs configuration.

<kbd>m-x</kbd> `package-install` <kbd>RET</kbd> `atom-one-dark-theme`

### Manual

First, clone this repo:

```console
$ git clone https://github.com/jonathanchu/atom-one-dark-theme.git
```

Then, add this to your Emacs config:

```elisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/path/to/atom-one-dark-theme/")
```

## Configuration

Load the theme with:

<kbd>load-theme</kbd> `atom-one-dark`

To load this theme on Emacs startup and make it the default, add this
to your Emacs config:

```elisp
(load-theme 'atom-one-dark t)
```

## Customization

### Per-mode face remapping

A few modes (notably `html-mode` and `js2-mode`) do not define faces of
their own and instead reuse the standard font-lock faces, which makes
them impossible to theme without affecting every other mode. To work
around this, the theme remaps a small number of faces buffer-locally
based on the major mode:

| Mode | Remapped faces |
| --- | --- |
| `js2-mode` | `font-lock-constant-face`, `font-lock-doc-face`, `font-lock-variable-name-face` |
| `html-mode` | `font-lock-function-name-face`, `font-lock-variable-name-face` |

Modes derived from these are remapped as well, so `mhtml-mode` (the
default major mode for HTML files) and `js2-jsx-mode` are covered too.

If you would rather see the same faces in every mode, disable it:

```elisp
(setq atom-one-dark-theme-force-faces-for-mode nil)
```

## Terminal Support

The theme ships two palettes and picks one per display. Terminals that
report exactly 256 colors get `color-NNN` values chosen to sit closest
to the intended shades; graphical frames and 24-bit terminals get the
real hex values.

If Emacs looks noticeably worse in your terminal than in the GUI, your
terminal is probably not advertising 256 colors. Add this to your
`.bashrc` or `.zshrc`, then start Emacs again:

```console
export TERM=xterm-256color
```

For fish, the equivalent in `config.fish` is:

```console
set -x TERM xterm-256color
```

Under a daemon the theme is loaded before any display exists, so it
rebuilds the palette against the first client frame and re-applies
itself. Starting `emacsclient` in a terminal after `emacs --daemon`
gives you the terminal palette rather than the graphical one.

## Screenshots
![Atom One Dark theme screenshot](https://i.imgur.com/qDnlEYc.png)
