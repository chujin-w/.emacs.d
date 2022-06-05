;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Every file opened and loaded by Emacs will run through this list to check for a proper
;; handler for the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Adjust garbage collection thresholds during startup, and thereafter
;; Avoid garbage collection during startup. The GC eats up quite a bit of time, easily
;; doubling the startup time. The trick is to turn up the memory threshold in order to
;; prevent it from running during startup.
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (normal-gc-cons-percent 0.1)
      (init-gc-cons-threshold (* 128 1024 1024))
      (init-gc-cons-percent 0.6))
    (setq gc-cons-threshold init-gc-cons-threshold
        gc-cons-percent init-gc-cons-percent)

;; After Emacs startup has been completed, set `gc-cons-threshold' to
;; 20MB and reset `gc-cons-percentage' to its original value.
;; Also reset `file-name-handler-alist'
    (add-hook 'emacs-startup-hook
           (lambda () (setq gc-cons-threshold normal-gc-cons-threshold
                            gc-cons-percent normal-gc-cons-percent)
                      (makunbound 'file-name-handler-alist-original))))


;; Bootstrap config
(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)
;; (require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
;; (require 'init-exec-path) ;; Set up $PATH

;; Allow users to provide an optional "init-preload-local.el"
;; (require 'init-preload-local nil t)

;; Load configs for specific features and modes
;; (require-package 'diminish)
;; (maybe-require-package 'scratch)
;; (require-package 'command-log-mode)

;; (require 'init-frame-hooks)
;; (require 'init-xterm)
(require 'init-themes)                  ; Set emacs themes
;; (require 'init-osx-keys)
(require 'init-gui-frames)              ; Set gui frames. eg. toolar, menubar, size, fonts etc...
(require 'init-dired)                   ; Dired is the main mode for Emacs file-manager operations.
                                        ; The name “Dired” stands for “directory editor”
;; (require 'init-isearch)
;; (require 'init-grep)
;; (require 'init-uniquify)
;; (require 'init-ibuffer)
;; (require 'init-flycheck)

;; (require 'init-recentf)
;; (require 'init-minibuffer)
;; (require 'init-hippie-expand)
;; (require 'init-company)
;; (require 'init-windows)
;; (require 'init-sessions)
;; (require 'init-mmm)

(require 'init-editing-utils)   ;; Set edit tools eg.linenum, tabwidth...
;; (require 'init-whitespace)

;; (require 'init-vc)
;; (require 'init-darcs)
(require 'init-git)
;; (require 'init-github)

;; (require 'init-cmake)

;; (require 'init-projectile)

;; (require 'init-compile)
;; (require 'init-crontab)
;; (require 'init-textile)
(require 'init-markdown)
;; (require 'init-csv)
;; (require 'init-erlang)
;; (require 'init-javascript)
;; (require 'init-php)
;; (require 'init-org)
;; (require 'init-nxml)
;; (require 'init-html)
;; (require 'init-css)
;; (require 'init-haml)
;; (require 'init-http)
;; (require 'init-python)
;; (require 'init-haskell)
;; (require 'init-elm)
;; (require 'init-purescript)
;; (require 'init-ruby)
;; (require 'init-rails)
;; (require 'init-sql)
;; (require 'init-ocaml)
;; (require 'init-j)
;; (require 'init-nim)
;; (require 'init-rust)
;; (require 'init-toml)
;; (require 'init-yaml)
;; (require 'init-docker)
;; (require 'init-terraform)
;; (require 'init-nix)
;; (maybe-require-package 'nginx-mode)

;; (require 'init-paredit)
;; (require 'init-lisp)
;; (require 'init-slime)
;; (require 'init-clojure)
;; (require 'init-clojure-cider)
;; (require 'init-common-lisp)

;; (when *spell-check-support-enabled*
;;   (require 'init-spelling))

;; (require 'init-misc)

;; (require 'init-folding)
;; (require 'init-dash)

;;(require 'init-twitter)
;; (require 'init-mu)
;; (require 'init-ledger)
;; Extra packages which don't require any configuration

;; (require-package 'sudo-edit)
;; (require-package 'gnuplot)
;; (require-package 'lua-mode)
;; (require-package 'htmlize)
;; (when *is-a-mac*
;;   (require-package 'osx-location))
;; (maybe-require-package 'dotenv-mode)
;; (maybe-require-package 'shfmt)

;; (when (maybe-require-package 'uptimes)
;;   (setq-default uptimes-keep-count 200)
;;   (add-hook 'after-init-hook (lambda () (require 'uptimes))))

;; (when (fboundp 'global-eldoc-mode)
;;   (add-hook 'after-init-hook 'global-eldoc-mode))

;; (require 'init-direnv)



;; Variables configured via the interactive 'customize' interface
;; ** Move customizations into their own file
;;     Customizations go to a dedicated file. Check the existence of such a file before loading so as
;;     not to cause errors on a vanilla emacs installation.
;;     (let
;;       ((customization-file (expand-file-name "custom.el" user-emacs-directory)))
;;     (when (file-exists-p customization-file)
;;       (setq custom-file customization-file)
;;       (load custom-file 'noerror)))
(when (file-exists-p custom-file)
   (load custom-file 'noerror))

;; Locales (setting them earlier in this file doesn't work in X)
;; (require 'init-locales)

;; Allow users to provide an optional "init-local" containing personal settings
;; (require 'init-local nil t)

;; ** Emacs Server
;; Useful to reuse the already running instance
;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (and (fboundp 'server-running-p)
              (server-running-p))
              (server-start))))
;;             (unless (server-running-p)
;;               (server-start))))


(provide 'init)

;; init.el ends here
