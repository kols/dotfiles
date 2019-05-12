;;; .emacs --- Emacs init file -*- lexical-binding: t; -*-

;;; Commentary:
;;;   Emacs init file
;;;

;;; Code:

;;; Speed up init.
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
;; https://gitlab.com/ambrevar/dotfiles/blob/master/.emacs.d/init.el
(defun ambrevar/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook 'ambrevar/reset-gc-cons-threshold)
;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist
        (append default-file-name-handler-alist
                file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))
(add-hook 'after-init-hook 'ambrevar/reset-file-name-handler-alist)

(setq load-prefer-newer t)

(defun kd/emacs-subdirectory (d)
  "Make dir path inside Emacs user dir for D."
  (expand-file-name d user-emacs-directory))

(defvar kd/ghq-dir "~/.ghq")
(defun kd/ghq-repo-path (domain repo)
  "Return a path of repo managed by ghq.
DOMAIN is like: `github.com'
REPO's pattern: `<user>/<repo>'."
  (expand-file-name (concat domain "/" repo) kd/ghq-dir))
(defun kd/ghq-github-repo-path (repo)
  "Return a path of github repo mamaged by ghq.
REPO's pattern: `<user>/<repo>'"
  (expand-file-name (kd/ghq-repo-path "github.com" repo)))

(defconst kd/default-local-proxy "127.0.0.1:7890")
(defun kd/turn-on-http-proxy (force &optional proxy)
  "Turn on http PROXY.  FORCE to turn on even no proxy process detected."
  (interactive "P")
  ;; Tor / Proxy: set up before package initialization.
  (when (or (member "ClashX"
                    (mapcar (lambda (p) (alist-get 'comm (process-attributes p)))
                            (list-system-processes)))
            (eq force '(4)))
    (require 'url)
    (unless url-proxy-services
      (unless proxy
        (setq proxy kd/default-local-proxy))
      (setq url-proxy-services
            `(("no_proxy" . "^\\(localhost\\|10.*\\)")
              ("http" . ,proxy)
              ("https" . ,proxy)))
      (message "Proxy turned on: %s" proxy))))

(add-to-list 'load-path (kd/emacs-subdirectory "elisp"))
(setq source-directory (kd/ghq-github-repo-path "emacs-mirror/emacs"))

;;; Package

(with-eval-after-load 'package
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("org" . "https://orgmode.org/elpa/"))))

(require 'package)
;; (call-interactively 'kd/turn-on-http-proxy)
(package-initialize)

;;;; Prerequisite packages
(let ((pre-pkgs '(auto-compile
                  bind-key
                  diminish
                  use-package))
      (refreshed nil))
  (dolist (pkg pre-pkgs)
    (unless (package-installed-p pkg)
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install pkg))))

;;;; auto-compile
(require 'auto-compile)
(auto-compile-on-save-mode 1)
(auto-compile-on-load-mode 1)

;;;; use-package
(with-eval-after-load 'use-package
  (progn
    (customize-set-variable 'use-package-enable-imenu-support t)
    (setq use-package-compute-statistics t)
    (setq use-package-verbose t)))
(eval-when-compile
  (require 'use-package))

(use-package bind-key)
(use-package diminish)

(use-package quelpa
  :ensure t
  :defer t
  :config
  (setq quelpa-melpa-recipe-stores '("~/.ghq/github.com/melpa/melpa/recipes"))
  (setq quelpa-update-melpa-p nil)
  (setq quelpa-melpa-dir "~/.ghq/github.com/melpa/melpa"))

(use-package quelpa-use-package
  :ensure t)

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-now kd/update-git-packages)
  :config (setq auto-package-update-delete-old-versions nil))

;;; Defaults

(use-package kd-defaults
  :no-require t
  :config
  (use-package better-defaults
    :ensure t)
  (setenv "LC_ALL" "en_US.UTF-8")
  (setenv "LC_CTYPE" "en_US.UTF-8")
  (setenv "LANG" "en_US.UTF-8")
  (setq current-language-environment "en_US.UTF-8")
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq gc-cons-threshold 50000000)
  (setq scroll-conservatively 10000)
  (setq scroll-preserve-screen-position t)
  (setq vc-follow-symlinks t)
  (setq custom-file "/dev/null")
  (setq history-length 200)

  ;;;; Tabs
  (setq-default tab-width 4)
  (setq indent-tabs-mode nil)
  (setq buffer-file-coding-system 'utf-8)
  (setq prefer-coding-system 'utf-8)

  (fset 'yes-or-no-p 'y-or-n-p)

  (unless noninteractive
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    (setq inhibit-startup-message t
          initial-major-mode 'fundamental-mode
          initial-scratch-message nil
          ring-bell-function 'ignore))

  (use-package warnings
    :config (setq warning-minimum-level :error)))


;;; Keybinding

(use-package kd-keybinding
  :no-require t
  :demand t
  :preface
  (defun kd/make-prefix-command (key command)
    "Bind KEY for a prefix COMMAND."
    (define-prefix-command command)
    (global-set-key key command))

  (defvar kd/toggle-map nil)
  (kd/make-prefix-command (kbd "s-t") 'kd/toggle-map)
  (defvar kd/pop-map nil)
  (kd/make-prefix-command (kbd "s-o") 'kd/pop-map)
  (defvar kd/org-map nil)
  (kd/make-prefix-command (kbd "s-r") 'kd/org-map)
  (defvar kd/compile-map nil)
  (kd/make-prefix-command (kbd "s-c") 'kd/compile-map)
  (defvar kd/errors-map nil)
  (kd/make-prefix-command (kbd "s-e") 'kd/errors-map)
  (defvar kd/eshell-map nil)
  (kd/make-prefix-command (kbd "s-s") 'kd/eshell-map)
  (defvar kd/projectile-map nil)
  (kd/make-prefix-command (kbd "s-P") 'kd/projectile-map)
  (defvar kd/tags-map nil)
  (kd/make-prefix-command (kbd "s-T") 'kd/tags-map)
  (defvar kd/testing-map nil)
  (kd/make-prefix-command (kbd "s-l") 'kd/testing-map)

  (defun kd/switch-to-previous-buffer ()
    "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

  (defun kd/kill-buffer-no-select ()
    (interactive)
    (kill-buffer nil))

  :defines (kd/toggle-map
            kd/pop-map
            kd/org-map
            kd/compile-map
            kd/errors-map
            kd/eshell-map
            kd/projectile-map)

  :commands kd/switch-to-previous-buffer
  :functions kd/make-prefix-command
  :chords ("JJ" . kd/switch-to-previous-buffer)
  :bind (:map kd/toggle-map
              ("l" . display-line-numbers-mode))
  :config (global-set-key (kbd "C-x K") #'kd/kill-buffer-no-select))


;;; Startup

;;;; envvar
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-shell-name "/usr/local/bin/zsh")
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "JAVA_HOME"))
  (exec-path-from-shell-initialize))

;;;; daemon
(use-package server
  :defer 1
  :config
  (defun kd/server-start ()
    (unless (server-running-p)
      (server-start)))
  (add-hook 'after-init-hook #'kd/server-start))

(use-package desktop
  :disabled t
  :config
  (setq desktop-dirname user-emacs-directory)
  (desktop-save-mode 1))

;;; User Interface (UI)

(use-package kd-UI
  :no-require t
  :config
  (menu-bar-mode 1)
  (column-number-mode 1)

  (use-package remap-face
    :commands (buffer-face-mode text-scale-mode face-remap-add-relative))

  (use-package simple
    :diminish visual-line-mode
    :commands (global-visual-line-mode turn-on-visual-line-mode)
    :init (add-hook 'after-init-hook #'global-visual-line-mode)
    :config (setq set-mark-command-repeat-pop t))

  (use-package shackle
    :ensure t
    :init (add-hook 'after-init-hook #'shackle-mode)
    :config
    (setq shackle-lighter "")
    (setq shackle-select-reused-windows nil)
    (setq shackle-default-alignment 'below)
    (setq shackle-default-size 0.35)

    ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-shackle.el
    (setq shackle-rules
          ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
          '(("*osx-dictionary*"            :select t                          :align below            :popup t)
            ("*info*"                      :select t                          :align right            :popup t)
            ("*Help*"                      :select t                          :align right            :popup t)
            ("\\`\\*[hH]elm.*?\\*\\'"      :regexp t                          :size 0.35 :align 'below)
            (helpful-mode                  :select t                          :align right            :popup t)
            (magit-status-mode             :select t                          :align right            :popup t)
            (magit-log-mode                :select t                          :align below            :same t))))

  (use-package doom-modeline
    :ensure t
    :commands doom-modeline-mode
    :init
    (add-hook 'after-init-hook #'doom-modeline-mode)
    :config
    (setq doom-modeline-height 25)
    (setq doom-modeline-buffer-file-name-style 'relative-from-project)
    (setq doom-modeline-icon nil)))

(use-package writeroom-mode
  :ensure t
  :bind (:map kd/toggle-map
              ("z" . writeroom-mode))
  :config
  (setq writeroom-mode-line-toggle-position 'mode-line-format)
  (setq writeroom-mode-line t)
  (setq writeroom-restore-window-config t)
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-global-effects '(writeroom-set-fullscreen
                                   writeroom-set-bottom-divider-width)))

(use-package fill-column-indicator
  :ensure t
  :commands fci-mode)

;;;; Graphic

(defconst IS-GUI (display-graphic-p))

(use-package kd-GUI
  :no-require t
  :if IS-GUI
  :config
  (horizontal-scroll-bar-mode -1)
  (scroll-bar-mode -1)

  ;;;;; Mouse
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse 't)

  ;;;;; Frame
  (setq default-frame-alist '((fullscreen . maximized)))
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))

  ;;;;; Theme
  (use-package default-black-theme
    :disabled t
    :config (load-theme 'default-black t))

  (use-package cyberpunk-theme
    :disabled t
    :ensure t
    :config
    (load-theme 'cyberpunk t)
    (custom-theme-set-faces
     'cyberpunk
     '(highlight-symbol-face
       ((t (:foreground "#d3d3d3" :background "dark magenta"))) t)
     '(go-guru-hl-identifier-face
       ((t (:foreground "#d3d3d3" :background "dark magenta"))) t)))

  (use-package zenburn-theme
    :disabled t
    :ensure t
    :config
    (load-theme 'zenburn t)
    (custom-theme-set-faces
     'zenburn
     '(highlight-symbol-face
       ((t (:foreground "#2B2B2B" :background "#8FB28F"))) t)
     '(region
       ((t (:foreground "#DCDCCC" :background "#2B2B2B"))) t)))

  (use-package hc-zenburn-theme
    :disabled t
    :ensure t
    :config
    (load-theme 'hc-zenburn t))

  (use-package color-theme-sanityinc-tomorrow
    :disabled t
    :ensure t
    :config
    (load-theme 'sanityinc-tomorrow-night t))

  (use-package prez-theme
    :disabled t
    :config
    (load-theme 'prez t))

  (use-package doom-themes
    :ensure t
    :config
    (setq doom-themes-padded-modeline t)
    (setq doom-opera-brighter-comments nil)
    (setq doom-opera-brighter-modeline nil)
    (setq doom-opera-comment-bg nil)
    (setq doom-opera-padded-modeline 1)
    (load-theme 'doom-opera t)
    (doom-themes-org-config)
    (custom-theme-set-faces
     'doom-opera
     `(region
       ((t (:background ,(doom-lighten (doom-color 'base4) 0.1))) t))
     `(ivy-current-match
       ((t (:background ,(doom-lighten (doom-color 'base4) 0.1) :underline t)) t))))

  (tool-bar-mode -1)
  (tooltip-mode -1)

  )

;;;; macOS

(defconst IS-MAC (memq window-system '(mac ns)))

(use-package kd-macOS
  :if (and IS-MAC)
  :no-require t
  :functions kd/lock-screen
  :config
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)

  (use-package osx-lib
    :defer t
    :ensure t)

  (use-package osx-dictionary
    :ensure t
    :functions kd/osx-dictionary-mode-hook-func
    :bind ("C-c f" . osx-dictionary-search-word-at-point)
    :init
    (when IS-GUI
      (defun kd/osx-dictionary-mode-hook-func ()
        (setq buffer-face-mode-face '(:family "Verdana" :height 190))
        (buffer-face-mode 1))
      (add-hook 'osx-dictionary-mode-hook #'kd/osx-dictionary-mode-hook-func))))

;;;; macOS Graphic
(use-package kd-macOS-GUI
  :if (and IS-MAC IS-GUI)
  :no-require t
  :config
  (set-frame-font (font-spec :family "Input Mono Condensed" :size 18 :weight 'light) nil t)
  (setq default-frame-alist '((ns-transparent-titlebar . t)
                              (ns-appearance . dark)
                              (fullscreen . fit-frame-to-buffer-sizes)
                              (font . "Input Mono Condensed-18:light")))
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (when (fboundp 'mac-set-frame-tab-group-property)
    (mac-set-frame-tab-group-property nil :tab-bar-visible-p nil))
  (setq mac-mouse-wheel-smooth-scroll nil))

;;; ---

(use-package whitespace
  :diminish whitespace-mode
  :commands whitespace-mode
  :bind (:map kd/toggle-map
              ("w" . whitespace-mode))
  :config
  (setq whitespace-line-column nil)
  (setq whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [9166 10])
                                      (tab-mark 9 [9654 9] [92 9]))))

(use-package fill
  :diminish auto-fill-mode
  :bind (:map kd/toggle-map
              ("f" . auto-fill-mode)))

(use-package visual-fill-column
  :ensure t
  :commands visual-fill-column-mode
  :bind (:map kd/toggle-map
              ("v" . visual-fill-column-mode)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (add-hook 'after-init-hook #'which-key-mode))

(use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package autoinsert
  :demand t
  :commands auto-insert-mode
  :functions define-auto-insert
  :init (add-hook 'after-init-hook #'auto-insert-mode)
  :config (setq auto-insert-query nil))

(use-package highlight-symbol
  :ensure t
  :commands highlight-symbol-mode
  :diminish highlight-symbol-mode)

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :commands aggressive-indent-mode
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

(use-package avy
  :ensure t
  :commands (avy-goto-char-timer avy-goto-line)
  :chords
  ("jl" . avy-goto-line)
  ("jc" . avy-goto-char-timer))

(use-package ace-window
  :ensure t
  :chords ("jw" . ace-window))

(use-package frog-jump-buffer
  :quelpa (frog-jump-buffer :fetcher github :repo "waymondo/frog-jump-buffer")
  :commands frog-jump-buffer
  :chords ("bb" . frog-jump-buffer))

;;; Window

(use-package window
  :bind (("C-x =" . balance-windows)))

(use-package windmove
  :bind (("C-s-h" . windmove-left)
         ("C-s-j" . windmove-down)
         ("C-s-k" . windmove-up)
         ("C-s-l" . windmove-right)
         ("<C-s-268632072>" . windmove-left)
         ("<C-s-268632074>" . windmove-down)
         ("<C-s-268632075>" . windmove-up)
         ("<C-s-268632076>" . windmove-right)))

(use-package winner
  :commands winner-mode
  :init (add-hook 'after-init-hook #'winner-mode)
  :config (setq winner-dont-bind-my-keys t))

(use-package zygospore
  :ensure t
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

(use-package eyebrowse
  :ensure t
  :commands eyebrowse-mode
  :bind (("C-c C--" . eyebrowse-next-window-config)
         ("C-c C-=" . eyebrowse-prev-window-config)
         ("s-w" . eyebrowse-close-window-config)
         ("s-'" . eyebrowse-last-window-config)
         ("s-1" . eyebrowse-switch-to-window-config-1)
         ("s-2" . eyebrowse-switch-to-window-config-2)
         ("s-3" . eyebrowse-switch-to-window-config-3)
         ("s-4" . eyebrowse-switch-to-window-config-4)
         ("s-5" . eyebrowse-switch-to-window-config-5))
  :init (add-hook 'after-init-hook #'eyebrowse-mode)
  :config
  (setq eyebrowse-mode-line-separator " ")
  (setq eyebrowse-mode-line-style 'always)
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-wrap-around t))

;;; Dired

(use-package dired
  :commands (dired dired-hide-details-mode)
  :diminish dired-mode
  :init
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
  :config
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-lahF")
  (setq dired-isearch-filenames t)
  (setq dired-ls-F-marks-symlinks t))

(use-package dired-x
  :commands dired-omit-mode
  :defines dired-omit-files
  :init (add-hook 'dired-mode-hook #'dired-omit-mode))

;; (use-package dired+
;;   :quelpa (dired+ :fetcher url :url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/dired+.el")
;;   :after dired)

(use-package peep-dired
  :ensure t
  :commands peep-dired
  :bind ((:map dired-mode-map
               ("P" . peep-dired))))

(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle)
         ("<f7>" . neotree-find))
  :config
  (setq neo-show-hidden-files t))

;;; Tramp

(use-package tramp
  :defer t
  :config
  (setq remote-file-name-inhibit-cache nil)
  (setq tramp-verbose 1)
  (setq tramp-default-method "ssh"))

;; ---

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands projectile-mode
  :functions projectile-find-file-hook-function
  :bind ((:map kd/projectile-map
               ("a" . projectile-add-known-project)
               ("b" . projectile-ibuffer)
               ("i" . projectile-invalidate-cache)))
  :init
  (add-hook 'after-init-hook #'projectile-mode)
  :config
  (defadvice projectile-project-root (around exlude-tramp activate)
    "This should disable projectile when visiting a remote file"
    (unless (--any? (and it (file-remote-p it))
                    (list
                     (buffer-file-name)
                     list-buffers-directory
                     default-directory
                     dired-directory))
      ad-do-it))

  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-generic-command "fd --type f --print0")
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-tags-backend 'xref)
  (setq projectile-mode-line
        '(:eval
          (if (file-remote-p default-directory)
              "P"
            (format "P[%s]" (projectile-project-name)))))
  (setq projectile-mode-line "P")
  (setq projectile-tags-file-name "/FILE_NOT_EXISTS")
  (setq projectile-sort-order 'recently-active))

(use-package rg
  :ensure t
  :bind ("C-c A" . rg-dwim))


;;; Git

(use-package magit
  :ensure t
  :bind (("s-g" . nil)
         ("s-g s" . magit-status)
         ("s-g l" . magit-log-current)
         ("s-g b" . magit-blame))
  :defines magit-status-expand-stashes
  :init
  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  :config
  (setq magit-git-executable "/usr/local/bin/git")
  (setq magit-status-expand-stashes nil))

(use-package git-commit
  :ensure t
  :commands (git-commit-setup git-commit-mode global-git-commit-mode)
  :init (add-hook 'git-commit-setup-hook #'flyspell-mode))

(use-package diff-hl
  :ensure t
  :commands
  (turn-on-diff-hl-mode
   diff-hl-magit-post-refresh
   diff-hl-dired-mode
   diff-hl-dired-mode-unless-remote))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package smartscan
  :ensure t
  :commands global-smartscan-mode
  :init (add-hook 'after-init-hook #'global-smartscan-mode)
  :config (setq smartscan-symbol-selector "symbol"))

(use-package hydra
  :ensure t
  :bind (("s-z" . hydra-goto/body)
         ("C-c w" . hydra-winner/body)
         ("<f2>" . hydra-zoom/body)
         ("C-M-o" . hydra-window-size/body))
  :config
  (defhydra hydra-goto (:color blue :hint nil)
    "
  goto   file
         -----------------------
         ._e_macs
         _d_eft
         _c_ap.org: default note
         o_r_g files
         _p_rojectile
         _s_rc
         code snippe_t_s
         re_f_erence
         [_x_]*scratch*
         the rif_l_e
         led_g_er
         _w_ork
         _z_ettel
    "
    ("e" (find-file "~/.dotfiles/.emacs"))
    ("d" deft)
    ("c" kd/default-captured-org-note)
    ("r" kd/counsel-org-find-file)
    ("s" kd/jump-to-src)
    ("t" (find-file "~/Dropbox/nvALT/snippets.org"))
    ("p" (helm-projectile-switch-project t))
    ("f" kd/jump-to-reference)
    ("x" (switch-to-buffer "*scratch*"))
    ("l" (hydra-helm-org-rifle/body))
    ("g" (find-file "~/Dropbox/ledger/ledger.beancount"))
    ("w" (find-file (concat org-directory "/work.org")))
    ("z" zd-new-file))

  (defhydra hydra-winner ()
    "winner mode"
    ("h" winner-undo "undo")
    ("l" winner-redo "redo"))

  (defhydra hydra-zoom ()
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))

  (defhydra hydra-window-size ()
    "window size"
    ("h" shrink-window-horizontally "shrink horizontal")
    ("j" enlarge-window "enlarge vertical")
    ("k" shrink-window "shrink vertical")
    ("l" enlarge-window-horizontally "enlarge horizontal")
    ("=" balance-windows "balance")))

(use-package edit-indirect
  :ensure t
  :bind ("C-c '" . edit-indirect-region))


;;; Helm

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands (helm-mode helm-hide-minibuffer-maybe)
  ;; :bind (("s-x" . helm-mini)
  ;;        ("M-y" . helm-show-kill-ring)
  ;;        ("C-." . helm-imenu)
  ;;        ("C-x C-f" . helm-find-files)
  ;;        ("C-S-s" . helm-occur)
  ;;        ("C-S-j" . helm-all-mark-rings)
  ;;        (:map isearch-mode-map
  ;;              ("M-s o" . helm-occur-from-isearch))
  ;;        (:map kd/toggle-map
  ;;              ("h" . helm-resume)))
  :init
  ;; (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  :config
  (setq helm-idle-delay 0.0)
  (setq helm-input-idle-delay 0.01)
  (setq helm-ff-skip-boring-files t)

  (setq helm-echo-input-in-header-line t)
  (setq helm-display-header-line nil)

  (setq helm-follow-mode-persistent t)
  (require 'helm-config)
  (helm-flx-mode 1)
  ;; (helm-mode 1)
  )

(use-package helm-flx
  :ensure t
  :commands helm-flx-mode)

(use-package helm-swoop
  :ensure t
  :commands (helm-swoop helm-multi-swoop-projectile helm-multi-swoop)
  :bind ((:map isearch-mode-map
               ("M-i" . helm-swoop-from-isearch))
         (:map helm-swoop-map
               ("M-i" . helm-multi-swoop-all-from-helm-swoop)))
  :config
  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-speed-or-color nil)
  (setq helm-swoop-move-to-line-cycle t)
  (setq helm-swoop-use-line-number-face t)
  (setq helm-swoop-use-fuzzy-match nil))

(use-package helm-smex
  :ensure t
  :bind (("M-X" . helm-smex-major-mode-commands)
         ([remap execute-extended-command] . helm-smex))
  :config (setq helm-smex-show-bindings t))

(use-package helm-descbinds
  :ensure t
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-projectile
  :ensure t
  :commands (helm-projectile-find-file
             helm-projectile-rg
             helm-projectile-switch-project))

;; (use-package helm-grep
;;   :after helm
;;   :bind ("C-c a" . helm-do-grep-ag)
;;   :config (setq helm-grep-ag-command "rg --color=ansi --colors=match:fg:red --colors=match:style:bold --smart-case --no-heading --line-number %s %s %s"))

(use-package helm-rg
  :ensure t
  :commands helm-rg
  ;; :bind ("C-c a" . helm-rg)
  :config (setq helm-rg-default-directory 'git-root))

(use-package helm-xref
  :ensure t
  :commands helm-xref-show-xrefs
  :config (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package helm-gtags
  :ensure t
  :diminish helm-gtags-mode
  :commands helm-gtags-mode
  ;; :bind (("C-]" . helm-gtags-find-tag)
  ;;        ("C-}" . helm-gtags-find-rtag)
  ;;        ("C-t" . helm-gtags-pop-stack)
  ;;        ("C-S-t" . helm-gtags-show-stack))
  ;; :chords (("gd" . helm-gtags-find-tag)
  ;;          ("gr" . helm-gtags-find-rtag))
  :config
  (setq helm-gtags-use-input-at-cursor t)
  (setq helm-gtags-fuzzy-match t)
  (setq helm-gtags-path-style 'relative)
  (setq helm-gtags-display-style 'detail)
  (setq helm-gtags-preselect t)
  (setq helm-gtags-ignore-case t)
  (setq helm-gtags-auto-update t)
  (setq helm-gtags-direct-helm-completing t))


(use-package helm-flycheck
  :ensure t
  :after flycheck
  :bind ((:map flycheck-command-map
               ("f" . helm-flycheck))))

(use-package helm-org-rifle
  :ensure t
  :commands (helm-org-rifle
             helm-org-rifl-org-directory
             hydra-helm-org-rifle/body)
  :init
  (defhydra hydra-helm-org-rifle ()
    "the rifle"
    ("R" helm-org-rifle "open buffers")
    ("r" helm-org-rifle-org-directory "org directory")
    ("a" helm-org-rifle-agenda-files "agenda")))

;;; Ivy, Swiper & Counsel

;;;; Ivy

(use-package flx
  :ensure t)

(use-package ivy
  :ensure t
  :demand t
  :functions ivy-read
  :bind ((:map ivy-mode-map
               ("s-x" . ivy-switch-buffer)
               ("C-c C-r" . ivy-resume)))
  :diminish ivy-mode
  :init
  (defun kd/jump-to-src (&optional initial-input)
    (interactive)
    (ivy-read "repo: "
              (split-string
               (shell-command-to-string
                (concat "ghq list -p")) "\n" t)
              :initial-input initial-input
              :action (lambda (d) (dired d))
              :caller 'kd/jump-to-src))

  (defun kd/jump-to-reference (&optional initial-input)
    (interactive)
    (ivy-read "ref: "
              '("~/.ghq/git.kernel.org/pub/scm/docs/man-pages/man-pages"
                "~/Documents/rfc")
              :action (lambda (d) (dired d))
              :caller 'kd/jump-to-reference))

  (add-hook 'after-init-hook 'ivy-mode)
  :config
  (setq ivy-height 15)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-more-chars-alist '((t . 2)))
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (define-key ivy-mode-map [remap ivy-switch-buffer] nil)
  (global-unset-key (kbd "C-x b")))

(use-package ivy-xref
  :ensure t
  :commands ivy-xref-show-xrefs
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;;;; Counsel

(use-package counsel
  :ensure t
  :after ivy
  :bind
  (("C-S-s" . counsel-grep-or-swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-." . counsel-imenu)
   ("C-c a" . counsel-rg)
   ("M-y" . counsel-yank-pop)
   ("C-h b" . counsel-descbinds)
   ("C-h y" . counsel-find-library))
  :config
  (add-to-list 'ivy-initial-inputs-alist '(counsel-rg . ivy-thing-at-point)))

(use-package xref)
(use-package gxref
  :ensure t
  :commands gxref-xref-backend
  :bind ((:map kd/tags-map
               ("c" . gxref-create-db)
               ("u" . gxref-sigle-update-db)
               ("U" . gxref-update-db))
         ("C-]" . xref-find-definitions)
         ("C-}" . xref-find-references)
         ("C-t" . xref-pop-marker-stack))
  :init (add-to-list 'xref-backend-functions 'gxref-xref-backend))

(use-package counsel-gtags
  :disabled t
  :quelpa (counsel-gtags :fetcher github :repo "kols/emacs-counsel-gtags")
  :bind (:map counsel-gtags-mode-map
              ("C-]" . counsel-gtags-find-definition)
              ("C-}" . counsel-gtags-find-reference)
              ("C-t" . counsel-gtags-go-backward))
  :chords (("gd" . counsel-gtags-find-definition)
           ("gr" . counsel-gtags-find-definition))
  :diminish counsel-gtags-mode
  :config
  (setq counsel-gtags-use-input-at-point t)
  (setq counsel-gtags-auto-select-only-candidate t)
  (add-to-list 'ivy-more-chars-alist '(counsel-gtags--read-tag . 0))
  (unbind-key "C-]" global-map))

(use-package counsel-projectile
  :ensure t
  :bind ((:map projectile-mode-map
               ("s-X" . counsel-projectile-switch-to-buffer)
               ("s-p" . counsel-projectile-find-file)
               ("C-c a" . counsel-projectile-rg))
         (:map kd/projectile-map
               ("p" . counsel-projectile-switch-project)
               ("g" . counsel-projectile-git-grep)))
  :init (add-hook 'after-init-hook #'counsel-projectile-mode))

;;;; Swiper

(use-package swiper
  :ensure t
  :after ivy
  :commands (swiper swiper-from-isearch)
  :bind ("C-s" . swiper-isearch)
  :config
  (add-to-list 'ivy-re-builders-alist '(swiper . ivy--regex-plus))
  (setq swiper-include-line-number-in-search nil)
  (setq swiper-goto-start-of-match t))


;; (use-package bookmark+
;;   :quelpa (bookmark+ :fetcher url :url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/bookmark+.el")
;;   :defer t)

(use-package isearch+
  :quelpa (isearch+ :fetcher url :url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/isearch+.el")
  :defer 0.1
  :bind (:map isearch-mode-map
              ("M-i" . swiper-from-isearch)))

(use-package anzu
  :ensure t
  :commands global-anzu-mode
  :diminish anzu-mode
  :bind (:map isearch-mode-map
              ([remap isearch-query-replace] . #'anzu-query-replace)
              ([remap isearch-query-replace-regexp] . #'anzu-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

(use-package saveplace
  :commands save-place-mode
  :init (add-hook 'after-init-hook #'save-place-mode))

(use-package restclient
  :ensure t
  :commands restclient-mode)

(use-package company-restclient
  :ensure t
  :after company
  :commands company-restclient
  :init
  (add-hook 'restclient-mode-hook #'(lambda ()
                                      (kd/local-push-company-backend #'company-restclient))))

(use-package restclient-helm
  :ensure t
  :after restclient
  :commands helm-restclient)

(use-package outline
  :diminish outline-minor-mode
  :commands outline-minor-mode
  :init (defvar outline-minor-mode-prefix "\M-#"))

(use-package outshine
  :ensure t
  :commands outshine-mode
  :config (setq outshine-use-speed-commands t))

(use-package poporg
  :ensure t
  :bind ("C-c \"" . poporg-dwim))

(use-package irfc
  :quelpa (irfc :fetcher url :url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/irfc.el")
  :commands irfc-mode
  :mode ("/rfc[0-9]+\\.txt\\'" . irfc-mode)
  :config
  (setq irfc-directory "~/Documents/rfc/rfc")
  (setq irfc-assoc-mode 1))


;;; Org-mode

(use-package org-plus-contrib
  :pin org
  :ensure t
  :defer t)

(use-package org
  :preface
  (defun kd/counsel-org-find-file (&optional directory)
    (interactive)
    (let ((default-directory (or directory org-directory)))
      (ivy-read "org file:"
                (split-string
                 (shell-command-to-string "fd -e org") "\n" t)
                :action (lambda (x)
                          (find-file
                           (expand-file-name x default-directory))))))

  (defun kd/default-captured-org-note ()
    "Move to the end of penultimate line of the last org capture note."
    (interactive)
    (find-file org-default-notes-file)
    (goto-char (point-max))
    (forward-line -2)
    (org-end-of-line))
  :commands (org-end-of-line org-narrow-to-block org-mode orgtbl-mode kd/counsel-org-find-file kd/default-captured-org-note)
  :defines (org-directory
            org-imenu-depth
            org-default-notes-file)
  :bind ((:map org-mode-map
               ("C-M-<return>" . org-insert-subheading)
               ("C-c L" . org-insert-link-global)
               ("C-c R" . org-refile))
         (:map kd/org-map
               ("l" . org-store-link)))
  :init
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file (concat org-directory "/cap.org"))
  (setq org-todo-keywords '((sequence "TODO(!)" "DONE(!)")))
  (setq org-log-into-drawer t)
  (setq org-hide-leading-stars t)
  (setq org-refile-targets `((,(concat org-directory "/work.org") :maxlevel . 2)
                             (,(concat org-directory "/snippets.org") :level . 1)
                             (,(concat org-directory "/tldr.org") :level . 1)))

  (defun kd/org-mode-hook-func ()
    (visual-line-mode -1))

  (add-hook 'org-mode-hook #'kd/org-mode-hook-func)
  :config
  (setq org-imenu-depth 3)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-confirm-babel-evaluate nil)

  ;; face
  (dolist (face org-level-faces)
    (custom-set-faces `(,face ((t (:height 1.0 :weight semi-bold))))))

  (unbind-key "C-'" org-mode-map)       ; used by `imenu-list'


  ;; link
  (org-link-set-parameters "devonthink"
                           :follow
                           (lambda (id)
                             (osx-open-url-at-point (concat "x-devonthink-item://" id))))
  (org-link-set-parameters "2do"
                           :follow
                           (lambda (id)
                             (osx-open-url-at-point (concat "twodo://x-callback-url/showTask?uid=" id))))
  (org-link-set-parameters "gmail"
                           :follow
                           (lambda (id)
                             (browse-url
                              ;; or "https://mail.google.com/mail/h/?&v=c&s=l&th="
                              ;; for html-browser
                              (concat "https://mail.google.com/mail/?shva=1#all/" id))))
  (org-link-set-parameters "elewiki"
                           :follow
                           (lambda (id)
                             (browse-url
                              (concat "https://wiki.ele.to:8090/pages/viewpage.action?pageId=" id))))

  ;; latex
  (add-to-list 'org-latex-default-packages-alist '("" "xeCJK" t ("xelatex")))

  ;; babel
  (use-package ob-ipython
    :ensure t
    :defer t
    :commands company-ob-ipython
    :init
    (defun kd/ob-ipython-hook-func ()
      (kd/local-push-company-backend #'company-ob-ipython))
    (add-hook 'inferior-python-mode-hook #'kd/ob-ipython-hook-func)
    (add-hook 'org-mode-hook #'kd/ob-ipython-hook-func))

  (use-package ob-clojure
    :defer t
    :config (setq org-babel-clojure-backend 'cider))

  (use-package ob-async
    :ensure t
    :defer t)

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ipython . t)
                                 (shell . t)
                                 (emacs-lisp . t)
                                 (plantuml . t)))
  (setq org-plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")
  (setq ob-async-no-async-languages-alist '("ipython"))
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append))

(use-package org-projectile
  :ensure t
  :bind (:map kd/projectile-map
              ("n" . org-projectile-project-todo-completing-read))
  :config
  (setq org-projectile-projects-file
        (expand-file-name "proj_notes.org" org-directory))
  (push (org-projectile-project-todo-entry) org-capture-templates)
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package ox-latex
  :defer t
  :init
  (setq org-latex-compiler "xelatex"))

(use-package org-agenda
  :bind (:map kd/org-map
              ("a" . org-agenda))
  :init
  (setq org-agenda-files (concat org-directory "/agenda_files.txt")))

(use-package org-capture
  :functions (org-capture-finalize)
  :commands org-capture
  :bind ((:map kd/org-map
               ("c" . org-capture)))
  :config
  (let ((default-capture-file "")
        (meeting-record-file (concat org-directory "/meeting.org"))
        (tldr-file (concat org-directory "/tldr.org"))
        (bookmark-file (concat org-directory "/bookmark.org")))
    (defun kd/org-bookmark-template ()
      (concat "* " (org-mac-chrome-get-frontmost-url) "%?\n  :LOGBOOK:\n  :CREATED: %U\n  :END:\n%i"))
    (setq org-capture-templates
          `(("n" "note" entry (file+olp+datetree ,default-capture-file) "* %?\n  :LOGBOOK:\n  :CREATED: %U\n  :END:\n%i")
            ("m" "meeting record" entry (file+olp+datetree ,meeting-record-file) "* %?\n  :LOGBOOK:\n  :CREATED: %U\n  :END:\n%i" :tree-type week)
            ("t" "tldr" entry (file+olp ,tldr-file "TL;DR") "* %?\n  :LOGBOOK:\n  :CREATED: %U\n  :END:\n%i" :tree-type week)
            ("u" "url bookmark" entry (file+olp ,bookmark-file "Bookmarks") #'kd/org-bookmark-template)))))

(use-package ox
  :after org
  :commands (org-export-dispatch))

(use-package ox-md
  :after org)

(use-package ox-taskjuggler
  :after org)

(use-package htmlize
  :ensure t
  :after ox
  :commands (htmlize-file
             htmlize-buffer))

(use-package ox-reveal
  :ensure t
  :after ox
  :commands (org-reveal-export-to-html
             org-reveal-export-to-html-and-browse
             org-reveal-export-current-subtree)
  :config (setq org-reveal-root "file://~/src/reveal.js"))

(use-package org-noter
  :ensure t
  :commands org-noter
  :config
  (setq org-noter-auto-save-last-location t)
  (setq org-noter-always-create-frame nil))

(use-package org-mac-link
  :if IS-MAC
  :after org
  :bind (:map kd/org-map
              ("g m" . org-mac-grab-link)
              ("g g" . kd/quick-url-note)
              ("g f" . kd/quick-file-pos-note))
  :config
  (defun kd/quick-url-note ()
    "Fastest way to capture a web page link"
    (interactive)
    (org-capture nil "n")
    (org-mac-safari-insert-frontmost-url)
    (org-capture-finalize))
  (defun kd/quick-file-pos-note ()
    "Store a org link to a file position"
    (interactive)
    (org-store-link nil)
    (org-capture nil "n")
    (org-insert-link)
    (org-capture-finalize)))

(use-package org-download
  :ensure t
  :after org
  :bind (:map kd/org-map
              ("d" . org-download-yank)))

(use-package org-plus-contrib
  :ensure t
  :after org
  :no-require t
  :config
  (use-package org-annotate-file
    :bind (:map kd/org-map
                ("n" . org-annotate-file))
    :after org
    :config
    (setq org-annotate-file-add-search t)
    (setq org-annotate-file-storage-file (concat org-directory "/annotation.org")))

  (use-package org-git-link
    :after org))

(use-package org-annotate
  :quelpa (org-annotate :fetcher github :repo "girzel/org-annotate")
  :after org)

(use-package mandoku
  :disabled t
  :ensure t
  :commands mandoku-view-mode
  :config
  (setq mandoku-base-dir "~/Documents/krp")
  (mandoku-initialize)
  (unless (< 0 (length mandoku-user-account))
    (find-file (expand-file-name "mandoku-settings.org" mandoku-user-dir ))
    (search-forward "uservalues")))


;;; Shell

(use-package eshell
  :bind (("C-`" . kd/eshell-here)
         (:map kd/eshell-map
               ("e" . eshell)
               ("v" . kd/eshell-new-other-window-horizontally)
               ("h" . kd/eshell-new-other-window)
               ("." . kd/eshell-here)))
  :init
  (defun kd/eshell-mode-hook-func ()
    (smartscan-mode -1)
    (goto-address-mode 1)
    (add-to-list 'eshell-visual-commands "ssh")
    (add-to-list 'eshell-preoutput-filter-functions #'xterm-color-filter)
    (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

    ;; alias
    (eshell/alias "lm" "ls -lahF")
    (eshell/alias "ff" "find-file $1")
    (eshell/alias "fo" "find-file-other-window $1")
    (eshell/alias "d" "dired $1")
    ;; magit
    (eshell/alias "gst" #'kd/eshell-gst)
    (eshell/alias "gd" #'magit-diff-unstaged)
    (eshell/alias "gds" #'magit-diff-staged)
    ;; grep
    (eshell/alias "hrg" "helm-rg $1"))
  (add-hook 'eshell-mode-hook #'kd/eshell-mode-hook-func)

  (defun kd/eshell-new ()
    (interactive)
    (eshell 'N))

  (defun kd/eshell-new-other-window ()
    (interactive)
    (let ((height (/ (window-total-height) 2)))
      (split-window-vertically (- height))
      (other-window 1)
      (kd/eshell-new)))

  (defun kd/eshell-new-other-window-horizontally ()
    (interactive)
    (let ((height (/ (window-total-width) 2)))
      (split-window-horizontally (- height))
      (other-window 1)
      (kd/eshell-new)))

  ;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#shell-here
  (defun kd/eshell-here ()
    "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t))))
           (eshell-buffer-name (concat "*eshell: " name "*"))
           (eshell-buffer-window (get-buffer-window eshell-buffer-name 'visible)))
      (if eshell-buffer-window
          (delete-window eshell-buffer-window)
        (split-window-vertically (- height))
        (other-window 1)
        (eshell)
        (rename-buffer eshell-buffer-name))))

  (defun kd/eshell-gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))

  (add-hook 'eshell-before-prompt-hook (lambda ()
                                         (setq xterm-color-preserve-properties t)))
  :config
  (setenv "PAGER" "cat")
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)

  ;; prompt
  (setq eshell-history-size 10000)
  (setq eshell-prompt-regexp "^[^❯\n]*❯ ")
  (defun kd/eshell-prompt-function ()
    (let ((prompt-string "❯ "))
      (concat "\n"
              (unless (= eshell-last-command-status 0)
                (propertize (concat (number-to-string eshell-last-command-status) " ") 'face 'error))
              (abbreviate-file-name (eshell/pwd))
              "\n"
              prompt-string
	          ;; (if (= (user-uid) 0) " # " " $ ")
              )))
  (setq eshell-prompt-function #'kd/eshell-prompt-function))

(use-package em-smart
  :after eshell
  :commands eshell-smart-initialize
  :init (add-hook 'eshell-mode-hook #'eshell-smart-initialize)
  :config
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t))

(use-package esh-autosuggest
  :ensure t
  :after eshell
  :commands esh-autosuggest-mode
  :init (add-hook 'eshell-mode-hook #'esh-autosuggest-mode))

(use-package sh-script
  :commands sh-mode
  :config
  (let ((cond '(sh-mode . "Strict bash options")))
    (unless (assoc cond auto-insert-alist)
      (define-auto-insert cond '(nil "#!/bin/bash\nset -euo pipefail\nIFS=$'\\n\\t'\n")))))


;;; Term

(use-package xterm-color
  :ensure t
  :commands xterm-color-filter)

(use-package shell
  :commands shell
  :init
  (defun kd/shell-mode-hook-func ()
    (bash-completion-setup)
    (kd/local-push-company-backend '(company-shell company-shell-env))

    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))
  (add-hook 'shell-mode-hook #'kd/shell-mode-hook-func)
  :config
  (setq explicit-shell-file-name "/usr/local/bin/bash")
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))

(use-package bash-completion
  :ensure t
  :commands bash-completion-setup)

(use-package company-shell
  :ensure t
  :commands (company-shell company-fish-shell company-shell-env))

(use-package eterm-256color
  :ensure t
  :commands eterm-256color-mode)

;; (use-package term
;;   :commands (term ansi-term)
;;   :init
;;   (defun kd/term-mode-hook-func ()
;;     (eterm-256color-mode 1))
;;   (add-hook 'term-mode-hook #'kd/term-mode-hook-func))

(use-package multi-term
  :ensure t
  :commands (multi-term multi-term-dedicated-open multi-term-dedicated-toggle)
  :config
  (setq multi-term-program "/usr/local/bin/zsh")
  (setenv "TERM" "eterm-color"))


;;; Integration

(use-package browse-url
  :commands browse-url-generic
  :bind (:map kd/pop-map
              ("u" . browse-url-at-point))
  :config (setq browse-url-generic-program "open"))

(use-package browse-at-remote
  :ensure t
  :bind (:map kd/pop-map
              ("r" . browse-at-remote))
  :config (dolist (elt '(("gitlab.alibaba-inc.com" . "gitlab")))
            (add-to-list 'browse-at-remote-remote-type-domains elt)))

(use-package goto-addr
  :diminish goto-address-mode
  :bind ((:map goto-address-highlight-keymap
               ("C-c C-o" . 'goto-address-at-point)))
  :commands (goto-address-mode goto-address-prog-mode))

(use-package ispell
  :defer t
  :config (setq ispell-program-name "aspell"))

(use-package flyspell
  :commands flyspell-mode
  :init (add-hook 'org-mode-hook #'flyspell-mode)
  :config
  (define-key flyspell-mode-map [remap flyspell-auto-correct-word] nil)
  (unbind-key "C-." flyspell-mode-map))

(use-package graphql-mode
  :ensure t
  :defer t)

(use-package plantuml-mode
  :ensure t
  :commands plantuml-mode
  :config (setq plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar"))

(use-package flycheck-plantuml
  :ensure t
  :commands flycheck-plantuml-setup
  :init (add-hook 'plantuml-mode-hook #'flycheck-plantuml-setup))


;;; Programming

(use-package prog-mode
  :init
  (let ((prog-minor-modes '(highlight-symbol-mode
                            turn-on-diff-hl-mode
                            counsel-gtags-mode
                            goto-address-prog-mode
                            abbrev-mode
                            hs-minor-mode
                            flycheck-mode
                            which-function-mode
                            fci-mode)))
    (dolist (mode prog-minor-modes)
      (add-hook 'prog-mode-hook mode)))

  (defun kd-prog-mode-hook-func ()
    (setq-local show-trailing-whitespace t)
    (setq-local indicate-empty-lines t)
    (use-package quickrun
      :ensure t))

  (add-hook 'prog-mode-hook #'kd-prog-mode-hook-func))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-session)
  :bind (:map lsp-mode-map
              ("<f8>" . treemacs))
  :init
  (defun kd/lsp-mode-hook-func ()
    (require 'lsp-clients)
    (require 'lsp-ui-flycheck)
    (lsp-ui-flycheck-enable t)
    (kd/local-push-company-backend #'company-lsp)
    (when (lsp--capability "documentSymbolProvider")
      (lsp-enable-imenu))
    (lsp-ui-mode 1)
    (highlight-symbol-mode -1))
  (add-hook 'lsp-mode-hook #'kd/lsp-mode-hook-func)
  :config
  (setq lsp-response-timeout 5)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-auto-configure nil)
  (setq lsp-lens-check-interval 1)
  (setq lsp-document-highlight-delay 0.5)
  (setq lsp-eldoc-prefer-signature-help t)
  (setq lsp-eldoc-enable-signature-help t)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t))

(use-package treemacs
  :ensure t
  :commands treemacs
  :bind (:map treemacs-mode-map
              ("<f8>" . treemacs))
  :init
  (defun kd/treemacs-mode-hook-func ()
    (treemacs-resize-icons 15))
  (add-hook 'treemacs-mode-hook #'kd/treemacs-mode-hook-func))

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-mode lsp-ui-flycheck-enable)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-delay 1)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-show-symbol nil))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package realgud
  :ensure t
  :commands realgud:trepan2)

(use-package subword
  :diminish subword-mode
  :commands subword-mode)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :commands (smartparens-global-strict-mode show-smartparens-global-mode)
  :bind (:map smartparens-mode-map
              ("<C-s-268632093>" . sp-unwrap-sexp)
              ("C-s-]" . sp-unwrap-sexp)
              ("C-)" . sp-slurp-hybrid-sexp))
  :init
  (add-hook 'after-init-hook #'smartparens-global-strict-mode)
  (add-hook 'after-init-hook #'show-smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (use-package paren
    :config (show-paren-mode -1)))

(use-package hideshow
  :commands (hs-minor-mode hs-toggle-hiding hs-hide-all kd/hs-toggle-hiding-all)
  :diminish hs-minor-mode
  :preface
  (defvar kd/hs-hided nil)
  (defun kd/hs-toggle-hiding-all ()
    (interactive)
    (if kd/hs-hided
        (hs-show-all)
      (hs-hide-all)))
  :init
  (add-hook 'hs-hide-hook (lambda ()
                            (setq kd/hs-hided t)))
  (add-hook 'hs-show-hook (lambda ()
                            (setq kd/hs-hided nil)))
  :bind (:map hs-minor-mode-map
              ("C-\\" . hs-toggle-hiding)
              ("C-|" . kd/hs-toggle-hiding-all)
              ("M-+" . hs-show-all)))

;;;; Flycheck

(use-package flycheck
  :ensure t
  :commands (flycheck-mode counsel-flycheck)
  :preface
  (defvar kd/counsel-flycheck-history nil
    "History for `counsel-flycheck'")

  (defun kd/counsel-flycheck ()
    (interactive)
    (if (not (bound-and-true-p flycheck-mode))
        (message "Flycheck mode is not available or enabled")
      (ivy-read "Error: "
                (let ((source-buffer (current-buffer)))
                  (with-current-buffer (or (get-buffer flycheck-error-list-buffer)
                                           (progn
                                             (with-current-buffer
                                                 (get-buffer-create flycheck-error-list-buffer)
                                               (flycheck-error-list-mode)
                                               (current-buffer))))
                    (flycheck-error-list-set-source source-buffer)
                    (flycheck-error-list-reset-filter)
                    (revert-buffer t t t)
                    (split-string (buffer-string) "\n" t " *")))
                :action (lambda (s &rest _)
                          (-when-let* ( (error (get-text-property 0 'tabulated-list-id s))
                                        (pos (flycheck-error-pos error)) )
                            (goto-char (flycheck-error-pos error))))
                :history 'counsel-flycheck-history)))

  :bind (:map kd/errors-map
              ("c" . flycheck-buffer)
              ("C" . flycheck-clear)
              ("x" . flycheck-disable-checker)
              ("l" . kd/counsel-flycheck))
  :config
  (setq flycheck-check-syntax-automatically '(save))
  (setq flycheck-mode-line-prefix "⚠"))

;;;; Tags

(use-package ggtags
  :ensure t
  :diminish ggtags-mode
  :bind ((:map ggtags-global-mode-map
               ("n" . next-error-no-select)
               ("p" . previous-error-no-select)))
  :commands (ggtags-mode ggtags-create-tags ggtags-update-tags)
  :config
  (setq ggtags-mode-sticky nil)
  (setq ggtags-use-sqlite3 t)
  (setq ggtags-sort-by-nearness nil)
  (setq ggtags-highlight-tag nil)
  (setq ggtags-enable-navigation-keys nil))

(use-package imenu-list
  :ensure t
  :bind (:map kd/pop-map
              ("'" . imenu-list-smart-toggle))
  :config
  (setq idle-update-delay 1)
  (setq imenu-list-focus-after-activation t))

;;;; Completion

(use-package company
  :ensure t
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :init
  (setq company-backends '(company-capf company-dabbrev-code company-keywords))
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 2)
  (setq tab-always-indent 'complete)
  (setq company-show-numbers t)
  (defun kd/local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (setq-local company-backends (add-to-list 'company-backends backend))))

(use-package company-flx
  :ensure t
  :after company
  :commands company-flx-mode
  :config (company-flx-mode 1))

(use-package company-quickhelp
  :disabled t
  :ensure t
  :after company
  :commands company-quickhelp-mode
  :bind (:map company-active-map
              ("M-d" . company-quickhelp-manual-begin))
  :init (add-hook 'company-mode-hook #'company-quickhelp-mode)
  :config
  (setq company-quickhelp-use-propertized-text t)
  (setq company-quickhelp-delay 1))

(use-package company-tern
  :ensure t
  :commands company-tern)

(use-package js2-refactor
  :ensure t
  :commands js2-refactor-mode)

(use-package indium
  :ensure t
  :commands indium-interaction-mode
  :config
  (setq indium-chrome-executable "/Applications/Chromium.app/Contents/MacOS/Chromium"))

(use-package tern
  :ensure t
  :commands tern-mode
  :config (unbind-key "M-." tern-mode-keymap))

(use-package xref-js2
  :ensure t
  :commands xref-js2-xref-backend)

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :init
  (defun kd-js2-mode-hook-func ()
    (js2-minor-mode 1)
    (js2-imenu-extras-mode 1)
    (js2-refactor-mode 1)
    (indium-interaction-mode 1)

    (setq-local flycheck-checker 'javascript-jshint)

    (tern-mode 1)
    (kd/local-push-company-backend #'company-tern)

    ;; (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
    )
  (add-hook 'js2-mode-hook #'kd-js2-mode-hook-func)
  ;; :config (unbind-key "M-." js2-mode-map)
  )

(use-package json-mode
  :ensure t
  :defer t)

(use-package swift-mode
  :ensure t
  :defer t)

(use-package csv-mode
  :ensure t
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile.*\\'" . dockerfile-mode))

(use-package groovy-mode
  :ensure t
  :commands groovy-mode)

(use-package tex-mode
  :defer t
  :functions LaTeX-narrow-to-environment)

(use-package mmm-mode
  :quelpa (mmm-mode :fetcher github :repo "emacsmirror/mmm-mode")
  :defer t)

(use-package salt-mode
  :quelpa (salt-mode :fetcher github :repo "emacsmirror/salt-mode")
  :mode ("\\.sls\\'" . salt-mode))

(use-package apib-mode
  :ensure t
  :commands apib-mode
  :mode ("\\.apib\\'" . apib-mode))

(use-package ansible
  :ensure t
  :commands (ansible ansible-doc)
  :mode (("\\(playbook\\|site\\|main\\|local\\)\\.ya?ml\\'" . ansible)
         ("/\\(tasks\\|roles\\|handlers\\)/.*\\.ya?ml\\'" . ansible)
         ("/\\(group\\|host\\)_vars/". ansible)))

(use-package company-ansible
  :ensure t
  :commands company-ansible
  :init
  (add-hook 'ansible::hook (lambda ()
                             (kd/local-push-company-backend #'company-ansible))))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :bind ((:map markdown-mode-map
               ("s-t m" . markdown-toggle-markup-hiding)))
  :commands (markdown-mode markdown-view-mode gfm-mode)
  :init
  (defun kd/markdown-mode-common-hook-func ()
    (setq-local fill-column 100)
    (visual-fill-column-mode 1))

  (defun kd/markdown-mode-hook-func ()
    (kd/markdown-mode-common-hook-func)
    (orgtbl-mode 1))

  (defun kd/markdown-view-mode-hook-func ()
    (kd/markdown-mode-common-hook-func)
    (setq buffer-face-mode-face '(:family "Verdana" :height 190))
    (buffer-face-mode 1)
    (markdown-toggle-markup-hiding 1))

  (add-hook 'markdown-mode-hook #'kd/markdown-mode-hook-func)
  (add-hook 'markdown-view-mode-hook #'kd/markdown-view-mode-hook-func)
  (add-hook 'gfm-view-mode-hook #'kd/markdown-view-mode-hook-func)
  :config (setq markdown-command "multimarkdown"))

(use-package conf-mode
  :mode ("rc$" . conf-mode))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode))

(use-package thrift-mode
  :mode ("\\.thrift\\'" . thrift-mode))

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package yasnippet
  :ensure t
  :commands (yas-global-mode yas-expand)
  :diminish yas-minor-mode
  :init (add-hook 'after-init-hook #'yas-global-mode)
  :config (setq yas-indent-line 'fixed))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package undo-tree
  :quelpa (undo-tree :fetcher github :repo "emacsmirror/undo-tree")
  :commands (global-undo-tree-mode undo-tree-mode)
  :bind ((:map kd/toggle-map
               ("u" . undo-tree-visualize)))
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook #'global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)

  ;; Keep region when undoing in region
  ;; http://whattheemacsd.com/my-misc.el-02.html
  (defadvice undo-tree-undo (around keep-region activate)
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          ad-do-it
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      ad-do-it)))

(use-package abbrev
  :diminish abbrev-mode
  :commands abbrev-mode)

(use-package autorevert
  :diminish auto-revert-mode
  :commands (global-auto-revert-mode turn-on-auto-revert-mode)
  :init (add-hook 'after-init-hook #'global-auto-revert-mode))

(use-package makefile-executor
  :ensure t
  :bind (:map kd/compile-map
              ("t" . makefile-executor-execute-target)
              ("p" . makefile-executor-execute-project-target)
              ("c" . makefile-executor-execute-last))
  :commands (makefile-executor-mode))

(use-package pdf-tools
  :if IS-GUI
  :disabled t
  :ensure t
  :pin manual
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind ((:map pdf-view-mode-map
               ("h" . pdf-annot-add-highlight-markup-annotation)
               ("t" . pdf-annot-add-text-annotation)
               ("D" . pdf-annot-delete)))
  :commands pdf-view-mode
  :init
  (defun kd/pdf-view-mode-hook-func ()
    (cua-mode 0))
  (add-hook 'pdf-view-mode-hook #'kd/pdf-view-mode-hook-func)
  ;; ~libffi~ is keg-only (maybe it wasn't before), therefore only
  ;; exists in ~/usr/local/Cellar~. There is no ~libffi.pc~ in
  ;; ~/usr/local/lib/pkgconfig/~, and ~pkgconfig~ cannot find it.
  ;;
  ;; ref: https://github.com/politza/pdf-tools/issues/480#issuecomment-473707355
  (setenv "PKG_CONFIG_PATH" "/usr/local/opt/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig")
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-resize-factor 1.1))


;;;; C/C++

(use-package company-c-headers
  :ensure t
  :commands company-c-headers)

(use-package google-c-style
  :ensure t
  :commands (google-set-c-style google-make-newline-indent))

(use-package semantic
  :disabled t
  :commands (semantic-mode)
  :init
  (setq semantic-inhibit-functions '((lambda ()
                                       (equal major-mode 'python-mode))))
  (setq semantic-default-submodes nil)
  (set-default 'semantic-case-fold t)
  :config
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))

(use-package stickyfunc-enhance
  :disabled t
  :ensure t
  :after semantic)

(use-package cc-mode
  :commands (c-mode c++-mode)
  :init
  (defun kd/c-mode-common-hook-func ()
    ;; style
    (google-set-c-style)
    (google-make-newline-indent)
    (setq-local c-basic-offset 4)
    (setq c-auto-newline nil)

    ;; whitespace
    (setq-local whitespace-style '(face indentation::tab)))

  (add-hook 'c-mode-common-hook #'kd/c-mode-common-hook-func)

  (defun kd/c-mode-hook-func ()
    ;; irony
    (irony-mode 1)

    ;; company
    (setq company-backends (delete 'company-semantic company-backends))
    (kd/local-push-company-backend #'company-c-headers))

  (add-hook 'c-mode-hook #'kd/c-mode-hook-func)
  (add-hook 'c++-mode-hook #'kd/c-mode-hook-func)
  :config (unbind-key "C-c C-a" c-mode-map))

;;;;; Irony

(use-package irony
  :ensure t
  :diminish irony-mode
  :commands irony-mode
  :init
  (defun kd/irony-mode-hook-func ()
    (irony-cdb-autosetup-compile-options)

    ;; eldoc
    (irony-eldoc 1)

    ;; company
    (company-irony-setup-begin-commands)
    (kd/local-push-company-backend #'company-irony)

    ;; flycheck
    (flycheck-irony-setup))
  (add-hook 'irony-mode-hook #'kd/irony-mode-hook-func))

(use-package irony-cdb
  :after irony
  :commands irony-cdb-autosetup-compile-options)

(use-package irony-eldoc
  :after irony
  :ensure t
  :commands irony-eldoc)

(use-package company-irony
  :after irony
  :ensure t
  :commands (company-irony company-irony-setup-begin-commands)
  :config (setq company-irony-ignore-case 'smart))

(use-package flycheck-irony
  :after irony
  :ensure t
  :commands flycheck-irony-setup)


;;;; Python

(use-package python
  :quelpa (python :fetcher url :url "https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/progmodes/python.el")
  :commands python-mode
  :init
  (setenv "PYTHONIOENCODING" "UTF-8")
  (defun kd/python-mode-hook-function ()
    ;; jedi
    (jedi:setup)
    (kd/local-push-company-backend #'company-jedi)

    ;; imenu
    (setq imenu-create-index-function #'python-imenu-create-index)

    ;; whitespace
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
    (setq-local whitespace-style '(face indentation::tab))

    (subword-mode 1)
    (hs-hide-level 2))

  (add-hook 'python-mode-hook #'kd/python-mode-hook-function)
  :config
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  (let ((cond '(python-mode . "Python file encoding")))
    (unless (assoc cond auto-insert-alist)
      (define-auto-insert cond '(nil "# coding: utf-8\n")))))

(use-package jedi-core
  :ensure t
  :commands jedi:setup
  :bind (:map jedi-mode-map
              ("<s-mouse-1>" . jedi:goto-definition))
  :config
  (setq jedi:use-shortcuts t)
  (setq jedi:tooltip-method nil)
  (setq jedi:complete-on-dot t))

(use-package company-jedi
  :ensure t
  :commands company-jedi)

(use-package python-pytest
  :ensure t
  :commands python-pytest-popup
  :bind (:map kd/testing-map
              ("p p" . python-pytest-popup)
              ("p r" . python-pytest-repeat)
              ("p f" . python-pytest-file)
              ("p l" . python-pytest-last-failed)
              ("p d" . python-pytest-function-dwim)))

;;;; Golang

(use-package go-mode
  :ensure t
  :commands go-mode
  :bind
  (:map go-mode-map
        ("M-." . godef-jump)
        ("M-C-." . godef-jump-other-window)
        ("M-k" . godoc-at-point))
  :init
  (defun kd/go-mode-hook-func ()
    (add-hook 'before-save-hook #'gofmt-before-save nil t)

    ;; company
    (kd/local-push-company-backend #'company-go)

    ;; flycheck
    (setq-local flycheck-checker 'go-vet)
    (setq-local flycheck-disabled-checkers '(go-unconvert
                                             go-megacheck
                                             go-errcheck))
    (setq flycheck-go-vet-shadow t)

    ;; highlight identifier
    (highlight-symbol-mode -1)
    (go-guru-hl-identifier-mode 1)

    ;; whitespace
    (setq-local indent-tabs-mode t)
    (setq-local whitespace-style '(face indentation::space))

    (subword-mode 1))
  (add-hook 'go-mode-hook #'kd/go-mode-hook-func)
  :config
  (setq gofmt-command "goimports")
  (setq go-packages-function 'go-packages-go-list)
  (when IS-MAC
    (exec-path-from-shell-copy-env "GOPATH")))

(use-package go-snippets
  :ensure t
  :after go-mode)

(use-package go-eldoc
  :ensure t
  :after go-mode
  :commands go-eldoc-setup)

(use-package company-go
  :ensure t
  :commands company-go
  :config (setq company-go-show-annotation t))

(use-package go-guru
  :ensure t
  :after go-mode)

(use-package gotest
  :ensure t
  :after go-mode
  :bind ((:map go-mode-map
               ("C-c C-c" . go-run))))

(use-package go-playground
  :ensure t
  :bind (:map go-playground-mode-map
              ("C-c C-c" . go-playground-exec)
              ("C-c C-k" . go-playground-rm))
  :commands go-playground)

(use-package go-direx
  :ensure t
  :after go-mode
  :bind (:map go-mode-map
              ("s-o '" . go-direx-pop-to-buffer)))


;;;; Java

(use-package cc-mode
  :commands java-mode
  :init
  (defun kd/java-mode-hook-func ()
    (lsp)
    (require 'lsp-java-boot)
    (lsp-java-boot-lens-mode 1))
  (add-hook 'java-mode-hook #'kd/java-mode-hook-func))

(use-package eglot
  :disabled t
  :ensure t)

(use-package lsp-java
  :ensure t
  :defer t
  :config
  (setq lsp-java-vmargs
        (list
         "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044"
         "-Declipse.application=org.eclipse.jdt.ls.core.id1"
         "-Dosgi.bundles.defaultStartLevel=4"
         "-Declipse.product=org.eclipse.jdt.ls.core.product"
         "-Dlog.level=ALL"
         ;; "-jar ./plugins/org.eclipse.equinox.launcher_1.5.200.v20180922-1751.jar"
         "--add-modules=ALL-SYSTEM"
         ;; "--add-opens java.base/java.util=ALL-UNNAMED"
         ;; "--add-opens java.base/java.lang=ALL-UNNAMED"
         "-noverify"
         "-Xmx1G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"))
  (setq lsp-java-java-path
        "/Users/kane/Library/Java/JavaVirtualMachines/jdk-9.0.4.jdk/Contents/Home/bin/java"))

(use-package autodisass-java-bytecode
  :ensure t
  :mode ("\\.class\\'" . ad-javap-mode))

(use-package gradle-mode
  :ensure t
  :diminish gradle-mode
  :commands gradle-mode)

(use-package meghanada
  :disabled t
  :ensure t
  :commands meghanada-mode
  :init
  (defun kd/java-mode-hook-func ()
    (meghanada-mode 1)
    (gradle-mode 1)
    (setq-local c-basic-offset 4)
    (add-hook 'before-save-hook #'meghanada-code-beautify-before-save))
  (add-hook 'java-mode-hook #'kd/java-mode-hook-func)
  :config
  (setq meghanada-gradle-version "4.2.1")
  (setq meghanada-java-path "java")
  (setq meghanada-maven-path "mvn"))


;;;; Scala

(use-package ensime
  :ensure t
  :defer t)


;;;; Clojure

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj\\'" . clojure-mode))


;;;; Elisp

(use-package elisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :preface
  (defun kd/emacs-lisp-mode-hook-func ()
    (aggressive-indent-mode 1)
    (kd/local-push-company-backend #'company-elisp)
    )
  :init
  (add-hook 'emacs-lisp-mode-hook #'kd/emacs-lisp-mode-hook-func)
  :config
  (let ((cond '(elisp-mode . "lexical binding")))
    (unless (assoc cond auto-insert-alist)
      (define-auto-insert cond '(nil ";;; -*- lexical-binding: t; -*-")))))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :commands turn-on-elisp-slime-nav-mode
  :init (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode))

;;;; Rust

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :commands rust-mode)



(use-package tldr
  :ensure t
  :commands tldr)

(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

(use-package deft
  :ensure t
  :commands (deft deft-find-file)
  :config
  (setq deft-default-extension "org")
  (setq deft-use-filter-string-for-filename nil)
  (setq deft-directory "~/Dropbox/nvALT")
  (setq deft-auto-save-interval 0)
  (setq deft-use-filename-as-title t)
  (setq deft-org-mode-title-prefix nil)
  (setq deft-incremental-search t)
  (setq deft-file-limit 500))

(use-package zetteldeft
  :quelpa (zetteldeft :fetcher github :repo "EFLS/zetteldeft"))

(use-package eww
  :commands eww
  :init
  (defun kd/eww-mode-hook-func ()
    (setq-local shr-use-colors nil)
    (turn-on-visual-line-mode))

  (add-hook 'eww-mode-hook #'kd/eww-mode-hook-func))

(use-package elfeed
  :ensure t
  :commands elfeed
  :init
  (defun kd-elfeed-show-mode-hook-func ()
    (face-remap-add-relative 'variable-pitch
                             '(:family "Input Sans" :height 150 :weight light))
    (setq-local line-spacing 2)
    (writeroom-mode 1)
    (setq visual-fill-column-width 80)
    (setq-local writeroom-width 80))
  (add-hook 'elfeed-show-mode-hook #'kd-elfeed-show-mode-hook-func))

(use-package elfeed-org
  :ensure t
  :commands elfeed-org
  :config (elfeed-org)
  :config (setq rmh-elfeed-org-files `(,(concat org-directory "/elfeed.org"))))

;; from: http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first.  Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if you
         ;; don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(bind-key "n" #'narrow-or-widen-dwim 'kd/toggle-map)

(defun prelude-open-with (arg)
  "Open visited file in default external program.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (shell-command (concat
                    (cond
                     ((and (not arg) (eq system-type 'darwin)) "open")
                     ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                     (t (read-shell-command "Open current file with: ")))
                    " "
                    (shell-quote-argument buffer-file-name)))))

(bind-key "e" #'prelude-open-with 'kd/pop-map)

;;; beancount

(use-package beancount
  :quelpa (beancount :fetcher url :url "https://raw.githubusercontent.com/beancount/beancount/master/editors/emacs/beancount.el")
  :mode ("\\.beancount\\'" . beancount-mode)
  :init
  (defun kd/beancount-mode-hook-func ()
    (add-to-list 'yas-extra-modes 'beancount-mode))
  (add-hook 'beancount-mode-hook #'kd/beancount-mode-hook-func))

;;; .emacs ends here
;;; Local Variables:
;;; no-byte-compile: t
;;; eval: (hs-hide-all)
;;; End:
