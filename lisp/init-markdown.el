;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ** Markdown mode
;; There are a bunch of markdown files I use regularly. So a fitting mode is appropriate.
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc --from markdown -t html5 --standalone"))

;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; (when (maybe-require-package 'markdown-mode)
;;   (add-auto-mode 'markdown-mode "\\.md\\.html\\'")
;;   (with-eval-after-load 'whitespace-cleanup-mode
;;     (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)))


(provide 'init-markdown)
;;; init-markdown.el ends here
