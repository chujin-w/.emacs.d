;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Set repositories
(require 'package)

(setq-default load-prefer-newer t)

;; I want orgmode before melpa or gnu
(setq package-archives
      '(("ORG"          . "http://elpa.emacs-china.org/org/")
        ("GNU ELPA"     . "http://elpa.emacs-china.org/gnu/")
        ("MELPA"        . "http://elpa.emacs-china.org/melpa/"))
      package-archive-priorities
      '(("ORG"          . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(when (version< emacs-version "27.0")
      (package-initialize))
;; (setq package-enable-at-startup nil)

;; Install dependencies
(unless (and (package-installed-p 'delight)
	         (package-installed-p 'bind-key)
             (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'delight t)
  (package-install 'bind-key t)
  (package-install 'use-package t))
(setq-default
 use-package-always-defer t
 use-package-always-ensure t
 use-package-compute-statistics t
 use-package-verbose t)

;; no-littering is useful to de-clutter my /.emacs.d directory
(setq no-littering-etc-directory
      (expand-file-name "etc/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "var/" user-emacs-directory))

(use-package no-littering)
(require 'no-littering)

(provide 'init-elpa)
;;; init-elpa.el ends here
