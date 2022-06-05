;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Stop C-z from minimizing windows under OS X

(defun chujin/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'chujin/maybe-suspend-frame)


;; Suppress GUI features

(setq use-file-dialog nil)
(setq use-dialog-box nil)
;; (setq inhibit-startup-screen t)


;; Window size and features
(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

;; I don't need the big icons and prefer more screen real estate. See also
;; https://sites.google.com/site/steveyegge2/effective-emacs
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(if (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; I prefer setting things in this file rather than =custom.el=.
(use-package system-packages
  :custom
  (system-packages-noconfirm t))

;; ** Set the default font and frame config
;;    I like to see the full path of the buffer and no right fringe. The default fontset
;;    settings shall ensure that all unicode can be displayed. It's mainly there for the eye
;;    candy later. Finally inhibiting font cache compacting /should/ improve performance on
;;    Windows.
(setq default-frame-alist
      '(
        (fringe-mode (quote (1 . 1)) nil (fringe))
        (fringes-outside-margins nil t)
        (right-fringe . 0)
        (left-fringe)
        (left-fringe-width nil)
        (frame-resize-pixelwise t)
        (border-color . "black")
        ))

(setq-default frame-title-format
              (list '((buffer-file-name " %f"
                                        (dired-directory
                                         dired-directory
                                         (revert-buffer-function " %b"
                                                                 ("%b - Dir:  " default-directory)))))))

(when (display-graphic-p)
  (set-fontset-font "fontset-default" nil
                    (font-spec :size 20 :name "Symbola")))

(cond ((eq system-type 'windows-nt)
       (setq inhibit-compacting-font-caches t
             w32-use-native-image-API t)))

(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Monaco 12")
  (set-frame-parameter frame 'width 110)
  (set-frame-parameter frame 'hight 33)
  (set-frame-parameter frame 'top 50)
  (set-frame-parameter frame 'left 50)
  )

;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(when (display-graphic-p)
  (add-hook 'after-make-frame-functions 'fontify-frame))

;; ** Centaur Tabs
;; Let's try tabs again
(use-package centaur-tabs
  :demand t
  :init (setq centaur-tabs-set-bar 'over)
  :config
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-bar t
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker " ● "
        centaur-tabs-close-button " × "
        centaur-tabs-style "bar")
  (centaur-tabs-mode t)
  :bind
  ("C-<tab>" . centaur-tabs-backward)
  ("S-<tab>" . centaur-tabs-forward)
;  ("C-<prior>" . centaur-tabs-backward)
;  ("C-<next>" . centaur-tabs-forward)
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode))

;; ** All the icons
;; Show icons in buffers
;; It seems I need =font-lock+.el= for it to work on Windows.
;; (use-package font-lock+
;;   :load-path "lisp"
;;   :demand t)

(use-package all-the-icons
  :demand t)

(use-package all-the-icons-dired
  :demand t
  :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :ensure t
  :demand t
  :config
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))
  (all-the-icons-ivy-setup))


;; ** Time
;; Settings for the time in the mode line
(use-package time
  :defer t
  :ensure nil
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-day-and-date t)
  (display-time-interval 30)
  (display-time-string-forms
   (quote
    ((if display-time-day-and-date
         (format "%s %s. %s " dayname day monthname)
       "")
     (format "%s:%s%s"
             (if display-time-24hr-format 24-hours 12-hours)
             minutes
             (if display-time-24hr-format "" am-pm)))))
  :config
  (display-time-mode t))

;; ******************************************************************************
;; ** Maximize by default
;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)
 
;; (let ((no-border '(internal-border-width . 0)))
;;   (add-to-list 'default-frame-alist no-border)
;;   (add-to-list 'initial-frame-alist no-border))

;; (defun sanityinc/adjust-opacity (frame incr)
;;   "Adjust the background opacity of FRAME by increment INCR."
;;   (unless (display-graphic-p frame)
;;     (error "Cannot adjust opacity of this frame"))
;;   (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
;;          (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
;;          (newalpha (+ incr oldalpha)))
;;     (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
;;       (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

;; (when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
;;   (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

;; TODO: use seethru package instead?
;; (global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
;; (global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
;; (global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))


;; (when *is-a-mac*
;;   (when (maybe-require-package 'ns-auto-titlebar)
;;     (ns-auto-titlebar-mode)))


;; (setq frame-title-format
;;       '((:eval (if (buffer-file-name)
;;                    (abbreviate-file-name (buffer-file-name))
;;                  "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
;; (add-hook 'term-mode-hook
;;           (lambda ()
;;             (setq line-spacing 0)))

;; Change global font size easily

;; (require-package 'default-text-scale)
;; (add-hook 'after-init-hook 'default-text-scale-mode)

;; (require-package 'disable-mouse)
;; (use-package disable-mouse)

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
