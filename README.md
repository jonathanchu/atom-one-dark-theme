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

### With `use-package`

If you install packages declaratively, `use-package` handles both the
install and the load:

```elisp
(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t))
```

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

## Mode Support

Beyond the core font-lock faces — including the newer ones used by the
tree-sitter major modes — the theme styles roughly 76 packages and
built-in modes.

<details>
<summary>Full list</summary>

* `ace-jump`
* `ace-window`
* `ansi-color`
* `calendar`
* `centaur-tabs`
* `cider`
* `company-mode`
* `compilation`
* `counsel`
* `custom`
* `desktop-entry`
* `dictionary`
* `diff-hl`
* `dired-async`
* `dired-mode`
* `doom-modeline`
* `ediff`
* `elfeed`
* `elixir`
* `epa`
* `erc`
* `eshell`
* `eww`
* `fill-column-indicator`
* `flx-ido`
* `flycheck`
* `flymake`
* `flyspell`
* font-latex (AUCTeX)
* `git-commit`
* `git-gutter`
* `gomoku`
* `gud`
* `guix`
* `helm`
* `hlinum`
* `ido`
* `isearch`
* `ivy`
* `jabber`
* `js2-mode`
* `line-number`
* `linum`
* `lispy`
* `lispyville`
* `magit`
* `man`
* `message`
* `mode-line`
* `notmuch`
* `nxml`
* `org-mode`
* `perspective`
* `powerline`
* `rainbow-delimiters`
* `rbenv`
* `realgud`
* `regexp-builder`
* `rmsbolt`
* `rpm-spec-mode`
* `ruler-mode`
* `sh-mode`
* `show-paren`
* `sly`
* `smartparens`
* `solaire-mode`
* `spaceline`
* `swiper`
* `tab-bar-mode`
* `tab-line-mode`
* `tabbar`
* `tetris`
* `undo-tree`
* `web-mode`
* `window-divider`
* `woman`

</details>

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

## Contributing

Bug reports and pull requests are welcome on the [issue tracker][issues].

The theme is a single file, `atom-one-dark-theme.el`. Faces are grouped
by the package they belong to, in one `custom-theme-set-faces` form, and
each face draws its colors from the palette rather than hard-coding hex
values. Adding support for a mode usually means appending one more
group in the same style.

[issues]: https://github.com/jonathanchu/atom-one-dark-theme/issues

## Contributors

This theme is the work of many people. See [AUTHORS](AUTHORS) for the
full list, and thank you to everyone who has sent a patch.

## License

Distributed under the terms of the [GNU General Public License][gpl],
either version 3 or (at your option) any later version. See
[LICENSE](LICENSE) for the full text.

[gpl]: https://www.gnu.org/licenses/gpl-3.0
