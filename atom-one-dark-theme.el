;;; atom-one-dark-theme.el --- Atom One Dark color theme
;;
;; Copyright 2015 Jonathan Chu
;;
;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/atom-one-dark-theme
;; Version: 0.0.1
;;
;;; Commentary
;;
;; An Emacs port of the Atom One Dark theme from Atom.io.
;;
;;; Code

(deftheme atom-one-dark
  "Atom One Dark - An Emacs port of the Atom One Dark theme from Atom.io.")

(custom-theme-set-faces
  'atom-one-dark

  '(default ((t (:foreground "#abb2bf" :background "#31343F" ))))
  '(cursor ((t (:background "#8599FF" ))))
  '(fringe ((t (:background "#31343F" ))))
  '(region ((t (:background "#504945" ))))
  '(highlight ((t (:background "#484b5b"))))
  '(secondary-selection ((t (:background "#262626" ))))
  '(query-replace ((t (:inherit (isearch)))))
  '(minibuffer-prompt ((t (:foreground "#FF8000"))))
  '(region ((t (:background "grey70"))))

  '(font-lock-builtin-face ((t (:foreground "#56b6c2" ))))
  '(font-lock-comment-face ((t (:foreground "#5C6370" ))))
  '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
  '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
  '(font-lock-function-name-face ((t (:foreground "#61afef" ))))
  '(font-lock-keyword-face ((t (:foreground "#c678dd" ))))
  '(font-lock-preprocessor-face ((t (:foreground "#8996A8"))))
  '(font-lock-string-face ((t (:foreground "#98c379" ))))
  '(font-lock-type-face ((t (:foreground "#CFCB90" ))))
  '(font-lock-constant-face ((t (:foreground "#d19a66" ))))
  '(font-lock-variable-name-face ((t (:foreground "#e5c07b" ))))
  '(font-lock-warning-face ((t (:foreground "#ff982d" :bold t ))))

  ;; mode-line
  '(mode-line ((t (:background "#21252B" :foreground "#96CBFE"))))
  '(mode-line-buffer-id ((t (:weight bold))))
  '(mode-line-emphasis ((t (:weight bold))))
  '(mode-line-inactive ((t (:background "#4b4c4d"))))

  ;; isearch
  '(isearch (
             (((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3"))
             (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2"))
             (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4"))
             (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t)))
            )
  '(isearch-fail (
                  (((class color) (min-colors 88) (background light)) (:background "RosyBrown1"))
                  (((class color) (min-colors 88) (background dark)) (:background "red4"))
                  (((class color) (min-colors 16)) (:background "red"))
                  (((class color) (min-colors 8)) (:background "red"))
                  (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t)))
                 )

  ;; diff-hl (https://github.com/dgutov/diff-hl)
  '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
  '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
  '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

  ;; dired-mode
  '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
  '(dired-flagged ((t (:inherit (diff-hl-delete)))))
  '(dired-symlink ((t (:foreground "#FD5FF1"))))

  ;; flx-ido
  '(flx-highlight-face ((t (:inherit (link) :weight bold))))
  )

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'atom-one-dark)

;;; atom-one-dark-theme.el ends here
