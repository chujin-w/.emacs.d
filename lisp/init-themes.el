;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package custom
  :ensure nil
  :custom
  (custom-safe-themes t "Treat all themes as safe"))

;; =solarized-dark= with some customization:
;;  - make the modeline high contrast
;;  - make the fringe stand out from the background
(use-package solarized-theme
  :ensure t
  :init
  :demand t
  :disabled
  :config
  (set-face-attribute 'font-lock-constant-face nil :weight 'normal)
  (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
  (set-face-attribute 'font-lock-comment-face nil :italic t)
  (set-face-attribute 'font-lock-doc-face nil :italic t)
  (setq solarized-high-contrast-mode-line nil
        solarized-distinct-fringe-background t
        solarized-use-more-italic t
        solarized-use-less-bold nil
        solarized-distinct-doc-face t)
  (load-theme 'solarized-zenburn t))

(use-package nord-theme
  :ensure t
  :demand t
  :disabled
  :config
  (load-theme 'nord t))

(use-package zenburn-theme
  :ensure t
  :demand t
  ;;             :disabled t
  :config
  (setq zenburn-override-colors-alist
        '(("zenburn-bg+05" . "#282828")
          ("zenburn-bg+1"  . "#2F2F2F")
          ("zenburn-bg+2"  . "#3F3F3F")
          ("zenburn-bg+3"  . "#4F4F4F")))
  (setq zenburn-use-variable-pitch t) ;; use variable-pitch fonts for some headings and titles
  (setq zenburn-scale-org-headlines t) ;; scale headings in org-mode
  (setq zenburn-scale-outline-headlines t) ;; scale headings in outline-mode
  (load-theme 'zenburn t))

;; Dimmer package Description
;; This package provides a minor mode that indicates which buffer is currently active by dimming the faces in the other buffers. It does this nondestructively, and computes the dimmed faces dynamically such that your overall color scheme is shown in a muted form without requiring you to define what is a "dim" version of every face.
;; dimmer.el can be configured to adjust foreground colors (default), background colors, or both.
;; (maybe-require-package 'dimmer)


(provide 'init-themes)
;;; init-themes.el ends here
