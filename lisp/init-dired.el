;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ** Dired
;;  There is a new way to hide details in =dired=. Seems to be enough instead of dired-details
;;  so I removed it. Also added some other goodies.
;; Config =dired= with =use-package=
(use-package dired
  :ensure nil
  :custom (dired-dwim-target t "guess a target directory")
  :hook
  (dired-mode . dired-hide-details-mode))

;; This provides a sidebar with a dired buffer for the current directory
(use-package dired-toggle
  :defer t)

;; Usually I'm not interested in dotfiles
(use-package dired-hide-dotfiles
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode))
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

;; And I like more colors
(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))

;; From http://pragmaticemacs.com/emacs/tree-style-directory-views-in-dired-with-dired-subtree/

;; The dired-subtree package (part of the magnificent dired hacks) improves on this by
;; allowing you to expand subdirectories in place, like a tree structure. To install the
;; package, use the following code:
(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

;; This sets up the keybinds so that in dired, hitting i on a subdirectory expands it in
;; place with an indented listing. You can expand sub-subdirectories in the same way, and so
;; on. Hitting ; inside an expanded subdirectory collapses it.

;; A nice overview for git repositories in dired
(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; From [[http://pragmaticemacs.com/emacs/dynamically-filter-directory-listing-with-dired-narrow/][Dynamically filter directory listing with dired-narrow | Pragmatic Emacs]] a useful
;; addition to narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; ********************************************************************************
;; purcell emacs
;; (setq-default dired-dwim-target t)

;; ;; Prefer g-prefixed coreutils version of standard utilities when available
;; (let ((gls (executable-find "gls")))
;;   (when gls (setq insert-directory-program gls)))

;; (when (maybe-require-package 'diredfl)
;;   (with-eval-after-load 'dired
;;     (diredfl-global-mode)
;;     (require 'dired-x)))

;; ;; Hook up dired-x global bindings without loading it up-front
;; (define-key ctl-x-map "\C-j" 'dired-jump)
;; (define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

;; (with-eval-after-load 'dired
;;   (setq dired-recursive-deletes 'top)
;;   (define-key dired-mode-map [mouse-2] 'dired-find-file)
;;   (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

;; (when (maybe-require-package 'diff-hl)
;;   (with-eval-after-load 'dired
;;     (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(provide 'init-dired)
;;; init-dired.el ends here
