;;; atom-one-dark-theme.el --- Atom One Dark color theme
;;
;; Copyright 2015 Jonathan Chu
;;
;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/atom-one-dark-theme
;; Version: 0.3.1
;;
;;; Commentary:
;;
;; An Emacs port of the Atom One Dark theme from Atom.io.
;;
;;; Code:

(deftheme atom-one-dark
  "Atom One Dark - An Emacs port of the Atom One Dark theme from Atom.io.")

(defvar atom-one-dark-colors-alist
  '(("atom-one-dark-accent"   . "#528BFF")
    ("atom-one-dark-fg"       . "#ABB2BF")
    ("atom-one-dark-bg"       . "#282C34")
    ("atom-one-dark-bg-1"     . "#121417")
    ("atom-one-dark-mono-1"   . "#ABB2BF")
    ("atom-one-dark-mono-2"   . "#828997")
    ("atom-one-dark-mono-3"   . "#5C6370")
    ("atom-one-dark-cyan"     . "#56B6C2")
    ("atom-one-dark-blue"     . "#61AFEF")
    ("atom-one-dark-purple"   . "#C678DD")
    ("atom-one-dark-green"    . "#98C379")
    ("atom-one-dark-red-1"    . "#E06C75")
    ("atom-one-dark-red-2"    . "#BE5046")
    ("atom-one-dark-orange-1" . "#D19A66")
    ("atom-one-dark-orange-2" . "#E5C07B")
    ("atom-one-dark-gray"     . "#3E4451")
    ("atom-one-dark-silver"   . "#AAAAAA")
    ("atom-one-dark-black"    . "#0F1011"))
  "List of Atom One Dark colors.")

(defmacro atom-one-dark-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    atom-one-dark-colors-alist))
     ,@body))

(atom-one-dark-with-color-variables
  (custom-theme-set-faces
   'atom-one-dark

   `(default ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg))))
   `(cursor ((t (:background ,atom-one-dark-accent))))
   `(fringe ((t (:background ,atom-one-dark-bg))))
   `(region ((t (:background ,atom-one-dark-gray))))
   `(highlight ((t (:background ,atom-one-dark-gray))))
   `(secondary-selection ((t (:background ,atom-one-dark-bg-1))))
   `(query-replace ((t (:inherit (isearch)))))
   `(minibuffer-prompt ((t (:foreground ,atom-one-dark-silver))))

   `(font-lock-builtin-face ((t (:foreground ,atom-one-dark-cyan))))
   `(font-lock-comment-face ((t (:foreground ,atom-one-dark-mono-3))))
   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,atom-one-dark-blue))))
   `(font-lock-keyword-face ((t (:foreground ,atom-one-dark-purple))))
   `(font-lock-preprocessor-face ((t (:foreground ,atom-one-dark-mono-2))))
   `(font-lock-string-face ((t (:foreground ,atom-one-dark-green))))
   `(font-lock-type-face ((t (:foreground ,atom-one-dark-orange-2))))
   `(font-lock-constant-face ((t (:foreground ,atom-one-dark-orange-1))))
   `(font-lock-variable-name-face ((t (:foreground ,atom-one-dark-red-1))))
   `(font-lock-warning-face ((t (:foreground ,atom-one-dark-mono-3 :bold t))))

   ;; mode-line
   `(mode-line ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-silver))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background ,atom-one-dark-gray))))

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
   ))

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'atom-one-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; atom-one-dark-theme.el ends here
