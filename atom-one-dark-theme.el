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

  '(font-lock-builtin-face ((t (:foreground "#56b6c2" ))))
  '(font-lock-comment-face ((t (:foreground "#5C6370" ))))
  '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
  '(font-lock-function-name-face ((t (:foreground "#61afef" ))))
  '(font-lock-keyword-face ((t (:foreground "#c678dd" ))))
  '(font-lock-string-face ((t (:foreground "#98c379" ))))
  '(font-lock-type-face ((t (:foreground "#CFCB90" ))))
  '(font-lock-constant-face ((t (:foreground "#d19a66" ))))
  '(font-lock-variable-name-face ((t (:foreground "#e5c07b" ))))
  '(font-lock-warning-face ((t (:foreground "#ff982d" :bold t ))))

  ;; mode-line
  '(mode-line ((t (:background "#21252B" :foreground "#96CBFE"))))
  '(mode-line-buffer-id ((t (:weight bold))))
  '(mode-line-emphasis ((t (:weight bold))))
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
