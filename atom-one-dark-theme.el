;;; atom-one-dark-theme.el --- Atom One Dark color theme

;; Copyright 2015-2017 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/atom-one-dark-theme
;; Version: 0.4.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs port of the Atom One Dark theme from Atom.io.

;;; Code:

(deftheme atom-one-dark
  "Atom One Dark - An Emacs port of the Atom One Dark theme from Atom.io.")

(defvar atom-one-dark-colors-alist
  '(("atom-one-dark-accent"   . "#528BFF")
    ("atom-one-dark-fg"       . "#ABB2BF")
    ("atom-one-dark-bg"       . "#282C34")
    ("atom-one-dark-bg-1"     . "#121417")
    ("atom-one-dark-bg-hl"    . "#2F343D")
    ("atom-one-dark-gutter"   . "#666D7A")
    ("atom-one-dark-accent"   . "#AEB9F5")
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
   `(success ((t (:foreground ,atom-one-dark-green))))
   `(warning ((t (:foreground ,atom-one-dark-orange-2))))
   `(error ((t (:foreground ,atom-one-dark-red-1 :weight bold))))
   `(link ((t (:foreground ,atom-one-dark-blue :underline t :weight bold))))
   `(link-visited ((t (:foreground ,atom-one-dark-blue :underline t :weight normal))))
   `(cursor ((t (:background ,atom-one-dark-accent))))
   `(fringe ((t (:background ,atom-one-dark-bg))))
   `(region ((t (:background ,atom-one-dark-gray))))
   `(highlight ((t (:background ,atom-one-dark-gray))))
   `(hl-line ((t (:background ,atom-one-dark-bg-hl))))
   `(vertical-border ((t (:foreground ,atom-one-dark-mono-3))))
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
   `(font-lock-constant-face ((t (:foreground ,atom-one-dark-cyan))))
   `(font-lock-variable-name-face ((t (:foreground ,atom-one-dark-red-1))))
   `(font-lock-warning-face ((t (:foreground ,atom-one-dark-mono-3 :bold t))))

   ;; mode-line
   `(mode-line ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-silver))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background ,atom-one-dark-gray))))

   ;; ido
   `(ido-first-match ((t (:foreground ,atom-one-dark-purple :weight bold))))
   `(ido-only-match ((t (:foreground ,atom-one-dark-red-1 :weight bold))))
   `(ido-subdir ((t (:foreground ,atom-one-dark-blue))))
   `(ido-virtual ((t (:foreground ,atom-one-dark-mono-3))))

   ;; ace-jump
   `(ace-jump-face-background ((t (:foreground ,atom-one-dark-mono-3 :background ,atom-one-dark-bg-1 :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,atom-one-dark-red-1 :background ,atom-one-dark-bg-1 :inverse-video nil))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg-1))))
   `(company-tooltip-annotation ((t (:foreground ,atom-one-dark-mono-2 :background ,atom-one-dark-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-gray))))
   `(company-tooltip-mouse ((t (:background ,atom-one-dark-gray))))
   `(company-tooltip-common ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg-1))))
   `(company-tooltip-common-selection ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-gray))))
   `(company-preview ((t (:background ,atom-one-dark-bg))))
   `(company-preview-common ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg))))
   `(company-scrollbar-fg ((t (:background ,atom-one-dark-mono-1))))
   `(company-scrollbar-bg ((t (:background ,atom-one-dark-bg-1))))

   ;; compilation
   `(compilation-face ((t (:foreground ,atom-one-dark-fg))))
   `(compilation-line-number ((t (:foreground ,atom-one-dark-mono-2))))
   `(compilation-column-number ((t (:foreground ,atom-one-dark-mono-2))))

   ;; isearch
   `(isearch ((t (:foreground ,atom-one-dark-bg :background ,atom-one-dark-purple))))
   `(isearch-fail ((t (:foreground ,atom-one-dark-red-2 :background nil))))
   `(lazy-highlight ((t (:foreground ,atom-one-dark-purple :background ,atom-one-dark-bg-1 :underline ,atom-one-dark-purple))))

   ;; diff-hl (https://github.com/dgutov/diff-hl)
   '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
   '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
   '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

   ;; dired-mode
   '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
   '(dired-flagged ((t (:inherit (diff-hl-delete)))))
   '(dired-symlink ((t (:foreground "#FD5FF1"))))

   ;; helm
   `(helm-header ((t (:foreground ,atom-one-dark-mono-2
                      :background ,atom-one-dark-bg
                      :underline nil
                      :box (:line-width 6 :color ,atom-one-dark-bg)))))
   `(helm-source-header ((t (:foreground ,atom-one-dark-orange-2
                             :background ,atom-one-dark-bg
                             :underline nil
                             :weight bold
                             :box (:line-width 6 :color ,atom-one-dark-bg)))))
   `(helm-selection ((t (:background ,atom-one-dark-gray))))
   `(helm-selection-line ((t (:background ,atom-one-dark-gray))))
   `(helm-visible-mark ((t (:foreground ,atom-one-dark-bg :foreground ,atom-one-dark-orange-2))))
   `(helm-candidate-number ((t (:foreground ,atom-one-dark-green :background ,atom-one-dark-bg-1))))
   `(helm-separator ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-red-1))))
   `(helm-M-x-key ((t (:foreground ,atom-one-dark-orange-1))))
   `(helm-bookmark-addressbook ((t (:foreground ,atom-one-dark-orange-1))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,atom-one-dark-purple))))
   `(helm-bookmark-info ((t (:foreground ,atom-one-dark-green))))
   `(helm-bookmark-man ((t (:foreground ,atom-one-dark-orange-2))))
   `(helm-bookmark-w3m ((t (:foreground ,atom-one-dark-purple))))
   `(helm-match ((t (:foreground ,atom-one-dark-orange-2))))
   `(helm-ff-directory ((t (:foreground ,atom-one-dark-cyan :background ,atom-one-dark-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,atom-one-dark-green :background ,atom-one-dark-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,atom-one-dark-red-1 :background ,atom-one-dark-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,atom-one-dark-bg :background ,atom-one-dark-orange-2 :weight normal))))
   `(helm-buffer-not-saved ((t (:foreground ,atom-one-dark-red-1))))
   `(helm-buffer-process ((t (:foreground ,atom-one-dark-mono-2))))
   `(helm-buffer-saved-out ((t (:foreground ,atom-one-dark-fg))))
   `(helm-buffer-size ((t (:foreground ,atom-one-dark-mono-2))))
   `(helm-buffer-directory ((t (:foreground ,atom-one-dark-purple))))
   `(helm-grep-cmd-line ((t (:foreground ,atom-one-dark-cyan))))
   `(helm-grep-file ((t (:foreground ,atom-one-dark-fg))))
   `(helm-grep-finish ((t (:foreground ,atom-one-dark-green))))
   `(helm-grep-lineno ((t (:foreground ,atom-one-dark-mono-2))))
   `(helm-grep-finish ((t (:foreground ,atom-one-dark-red-1))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-swoop-target-line-block-face ((t (:background ,atom-one-dark-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-line-face ((t (:background ,atom-one-dark-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-word-face ((t (:background ,atom-one-dark-purple :foreground "#ffffff"))))
   `(helm-locate-finish ((t (:foreground ,atom-one-dark-green))))
   `(info-menu-star ((t (:foreground ,atom-one-dark-red-1))))

   ;; git-commit
   `(git-commit-comment-action  ((t (:foreground ,atom-one-dark-green :weight bold))))
   `(git-commit-comment-branch  ((t (:foreground ,atom-one-dark-blue :weight bold))))
   `(git-commit-comment-heading ((t (:foreground ,atom-one-dark-orange-2 :weight bold))))

   ;; js2-mode
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,atom-one-dark-mono-1))))
   `(js2-jsdoc-tag ((t (:foreground ,atom-one-dark-purple))))
   `(js2-jsdoc-type ((t (:foreground ,atom-one-dark-orange-2))))
   `(js2-jsdoc-value((t (:foreground ,atom-one-dark-red-1))))
   `(js2-object-property ((t (:foreground ,atom-one-dark-red-1))))

   ;; magit
   `(magit-section-highlight ((t (:background ,atom-one-dark-bg-hl))))
   `(magit-section-heading ((t (:foreground ,atom-one-dark-orange-2 :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,atom-one-dark-fg :weight bold))))
   `(magit-diff-file-heading ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,atom-one-dark-gray :weight bold))))
   `(magit-diff-file-heading-selection ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg-hl :weight bold))))
   `(magit-diff-hunk-heading ((t (:foreground ,atom-one-dark-mono-2 :background ,atom-one-dark-gray))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,atom-one-dark-mono-1 :background ,atom-one-dark-mono-3))))
   `(magit-diff-hunk-heading-selection ((t (:foreground ,atom-one-dark-purple :background ,atom-one-dark-mono-3))))
   `(magit-diff-context ((t (:foreground ,atom-one-dark-fg))))
   `(magit-diff-context-highlight ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-fg))))
   `(magit-diffstat-added ((t (:foreground ,atom-one-dark-green))))
   `(magit-diffstat-removed ((t (:foreground ,atom-one-dark-red-1))))
   `(magit-process-ok ((t (:foreground ,atom-one-dark-green))))
   `(magit-process-ng ((t (:foreground ,atom-one-dark-red-1))))
   `(magit-log-author ((t (:foreground ,atom-one-dark-orange-2))))
   `(magit-log-date ((t (:foreground ,atom-one-dark-mono-2))))
   `(magit-log-graph ((t (:foreground ,atom-one-dark-silver))))
   `(magit-sequence-pick ((t (:foreground ,atom-one-dark-orange-2))))
   `(magit-sequence-stop ((t (:foreground ,atom-one-dark-green))))
   `(magit-sequence-part ((t (:foreground ,atom-one-dark-orange-1))))
   `(magit-sequence-head ((t (:foreground ,atom-one-dark-blue))))
   `(magit-sequence-drop ((t (:foreground ,atom-one-dark-red-1))))
   `(magit-sequence-done ((t (:foreground ,atom-one-dark-mono-2))))
   `(magit-sequence-onto ((t (:foreground ,atom-one-dark-mono-2))))
   `(magit-bisect-good ((t (:foreground ,atom-one-dark-green))))
   `(magit-bisect-skip ((t (:foreground ,atom-one-dark-orange-1))))
   `(magit-bisect-bad ((t (:foreground ,atom-one-dark-red-1))))
   `(magit-blame-heading ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-mono-2))))
   `(magit-blame-hash ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-purple))))
   `(magit-blame-name ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-orange-2))))
   `(magit-blame-date ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-mono-3))))
   `(magit-blame-summary ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-mono-2))))
   `(magit-dimmed ((t (:foreground ,atom-one-dark-mono-2))))
   `(magit-hash ((t (:foreground ,atom-one-dark-purple))))
   `(magit-tag  ((t (:foreground ,atom-one-dark-orange-1 :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,atom-one-dark-green :weight bold))))
   `(magit-branch-local   ((t (:foreground ,atom-one-dark-blue :weight bold))))
   `(magit-branch-current ((t (:foreground ,atom-one-dark-blue :weight bold :box t))))
   `(magit-head           ((t (:foreground ,atom-one-dark-blue :weight bold))))
   `(magit-refname        ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,atom-one-dark-green))))
   `(magit-signature-bad       ((t (:foreground ,atom-one-dark-red-1))))
   `(magit-signature-untrusted ((t (:foreground ,atom-one-dark-orange-1))))
   `(magit-cherry-unmatched    ((t (:foreground ,atom-one-dark-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,atom-one-dark-purple))))
   `(magit-reflog-commit       ((t (:foreground ,atom-one-dark-green))))
   `(magit-reflog-amend        ((t (:foreground ,atom-one-dark-purple))))
   `(magit-reflog-merge        ((t (:foreground ,atom-one-dark-green))))
   `(magit-reflog-checkout     ((t (:foreground ,atom-one-dark-blue))))
   `(magit-reflog-reset        ((t (:foreground ,atom-one-dark-red-1))))
   `(magit-reflog-rebase       ((t (:foreground ,atom-one-dark-purple))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,atom-one-dark-green))))
   `(magit-reflog-remote       ((t (:foreground ,atom-one-dark-cyan))))
   `(magit-reflog-other        ((t (:foreground ,atom-one-dark-cyan))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,atom-one-dark-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,atom-one-dark-purple))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,atom-one-dark-blue))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,atom-one-dark-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,atom-one-dark-green))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,atom-one-dark-orange-1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,atom-one-dark-orange-2))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,atom-one-dark-red-1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,atom-one-dark-red-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,atom-one-dark-mono-1))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,atom-one-dark-mono-2))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,atom-one-dark-mono-3))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,atom-one-dark-black))))

   ;; rbenv
   `(rbenv-active-ruby-face ((t (:foreground ,atom-one-dark-green))))

   ;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,atom-one-dark-red-1 :background ,atom-one-dark-gray :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,atom-one-dark-gray :weight bold))))

   ;; web-mode
   `(web-mode-symbol-face ((t (:foreground ,atom-one-dark-orange-1))))

   ;; flx-ido
   `(flx-highlight-face ((t (:inherit (link) :weight bold))))

   ;; rpm-spec-mode
   `(rpm-spec-tag-face ((t (:foreground ,atom-one-dark-blue))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground "#FFFFFF" :background ,atom-one-dark-red-2))))
   `(rpm-spec-macro-face ((t (:foreground ,atom-one-dark-orange-2))))
   `(rpm-spec-var-face ((t (:foreground ,atom-one-dark-red-1))))
   `(rpm-spec-doc-face ((t (:foreground ,atom-one-dark-purple))))
   `(rpm-spec-dir-face ((t (:foreground ,atom-one-dark-cyan))))
   `(rpm-spec-package-face ((t (:foreground ,atom-one-dark-red-2))))
   `(rpm-spec-ghost-face ((t (:foreground ,atom-one-dark-red-2))))
   `(rpm-spec-section-face ((t (:foreground ,atom-one-dark-orange-2))))

   ;; term
   `(term-color-black ((t :foreground ,atom-one-dark-mono-1)))
   `(term-color-blue ((t (:foreground ,atom-one-dark-blue))))
   `(term-color-cyan ((t :foreground ,atom-one-dark-cyan)))
   `(term-color-green ((t (:foreground ,atom-one-dark-green))))
   `(term-color-magenta ((t :foreground ,atom-one-dark-purple)))
   `(term-color-red ((t :foreground ,atom-one-dark-red-1)))
   `(term-color-white ((t :foreground ,atom-one-dark-fg)))
   `(term-color-yellow ((t (:foreground ,atom-one-dark-orange-1))))

   ;; linum
   `(linum ((t (:foreground ,atom-one-dark-gutter :background ,atom-one-dark-bg))))
   ;; hlinum
   `(linum-highlight-face ((t (:foreground ,atom-one-dark-accent :background ,atom-one-dark-bg))))

   ;; latex-mode
   `(font-latex-sectioning-0-face ((t (:foreground ,atom-one-dark-blue :height 1.0))))
   `(font-latex-sectioning-1-face ((t (:foreground ,atom-one-dark-blue :height 1.0))))
   `(font-latex-sectioning-2-face ((t (:foreground ,atom-one-dark-blue :height 1.0))))
   `(font-latex-sectioning-3-face ((t (:foreground ,atom-one-dark-blue :height 1.0))))
   `(font-latex-sectioning-4-face ((t (:foreground ,atom-one-dark-blue :height 1.0))))
   `(font-latex-sectioning-5-face ((t (:foreground ,atom-one-dark-blue :height 1.0))))
   `(font-latex-bold-face ((t (:foreground ,atom-one-dark-green :weight bold))))
   `(font-latex-italic-face ((t (:foreground ,atom-one-dark-green))))
   `(font-latex-warning-face ((t (:foreground ,atom-one-dark-red-1))))
   `(font-latex-doctex-preprocessor-face ((t (:foreground ,atom-one-dark-cyan))))

   ;; org-mode
   `(org-date ((t (:foreground ,atom-one-dark-cyan))))
   `(org-footnote ((t (:foreground ,atom-one-dark-cyan))))
   `(org-sexp-date ((t (:foreground ,atom-one-dark-cyan))))

   ;; undo-tree
   `(undo-tree-visualizer-current-face ((t (:foreground ,atom-one-dark-red-1))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,atom-one-dark-orange-1))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,atom-one-dark-cyan))))
   ))

(atom-one-dark-with-color-variables
  (custom-theme-set-variables
   'atom-one-dark
;;;;; fill-column-indicator
   `(fci-rule-color ,atom-one-dark-gray)
   ))

(defvar atom-one-dark-theme-force-faces-for-mode t
  "If t, atom-one-dark-theme will use Face Remapping to alter the theme faces for
the current buffer based on its mode in an attempt to mimick the Atom One Dark
Theme from Atom.io as best as possible.
The reason this is required is because some modes (html-mode, jyaml-mode, ...)
do not provide the necessary faces to do theming without conflicting with other
modes.
Current modes, and their faces, impacted by this variable:
* js2-mode: font-lock-constant-face, font-lock-doc-face, font-lock-variable-name-face
")

;; Many modes in Emacs do not define their own faces and instead use standard Emacs faces when it comes to theming.
;; That being said, to have a real "Atom One Dark Theme" for Emacs, we need to work around this so that these themes look
;; as much like "Atom One Dark Theme" as possible.  This means using per-buffer faces via "Face Remapping":
;;
;;   http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html
;;
;; Of course, this might be confusing to some when in one mode they see keywords highlighted in one face and in another
;; mode they see a different face.  That being said, you can set the `atom-one-dark-theme-force-faces-for-mode` variable to
;; `nil` to disable this feature.
(defun atom-one-dark-theme-change-faces-for-mode ()
  (interactive)
  (and (eq atom-one-dark-theme-force-faces-for-mode t)
       (cond
        ((member major-mode '(js2-mode))
         ;; atom-one-dark-orange-1
         (face-remap-add-relative 'font-lock-constant-face :foreground "#D19A66")
         (face-remap-add-relative 'font-lock-doc-face '(:inherit (font-lock-comment-face)))
         ;; atom-one-dark-mono-1
         (face-remap-add-relative 'font-lock-variable-name-face :foreground "#ABB2BF"))
        )))

(add-hook 'after-change-major-mode-hook 'atom-one-dark-theme-change-faces-for-mode)

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
