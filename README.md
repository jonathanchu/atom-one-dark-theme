[![MELPA](http://melpa.org/packages/atom-one-dark-theme-badge.svg)](http://melpa.org/#/atom-one-dark-theme)

# Atom One Dark Theme for Emacs

This is a port of the Atom One Dark theme from
[Atom.io](https://atom.io). The original One Dark theme for Atom can
be found here:

* [Atom One Dark UI](https://atom.io/themes/one-dark-ui)
* [Atom One Dark Syntax](https://atom.io/themes/one-dark-syntax)

This is a work in progress. I have yet to include theme faces for many
popular modes. Pull requests are extremely welcomed! :)

## Installation

### From Packages (MELPA)

Make sure you have enabled the [MELPA](http://melpa.org/) package
archive in your Emacs configuration.

<kbd>m-x</kbd> `package-install` <kbd>RET</kbd> `atom-one-dark-theme`

### Manual

## Configuration

```elisp
(load-theme 'atom-one-dark t)
```

If you installed manually you'll need to include this theme and add to
the custom theme load path:

```elisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/path/to/atom-one-dark-theme/
```

## Screenshots
![Atom One Dark theme screenshot](http://i.imgur.com/qDnlEYc.png)
