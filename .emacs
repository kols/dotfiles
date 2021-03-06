;;; .emacs --- Emacs init file -*- lexical-binding: t; -*-

;;; Commentary:
;;;   Emacs init file
;;;

;;; Code:

;;; Speed up init.
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
;; https://gitlab.com/ambrevar/dotfiles/blob/master/.emacs.d/init.el
(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.6)
(defun ambrevar/reset-gc-cons-threshold ()
  (setq gc-cons-threshold
        (car (get 'gc-cons-threshold 'standard-value))
        gc-cons-percentage 0.1))
(add-hook 'after-init-hook #'ambrevar/reset-gc-cons-threshold)
;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist
      setqfile-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist
        (append default-file-name-handler-alist
                file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))
(add-hook 'after-init-hook #'ambrevar/reset-file-name-handler-alist)

(setq load-prefer-newer t)

(defun kd/emacs-subdirectory (d)
  "Make dir path inside Emacs user dir for D."
  (expand-file-name d user-emacs-directory))

(defvar kd/cache-dir "~/.cache/emacs")
(unless (file-directory-p kd/cache-dir)
  (mkdir kd/cache-dir))

(defun kd/emacs-cache-dir (f &optional dirp)
  (let ((dir (expand-file-name f kd/cache-dir)))
    (unless (and dirp (file-exists-p dir))
      (mkdir dir))
    dir))

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
  (when (or (member "ClashX Pro"
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

;;; https://emacs.stackexchange.com/questions/51721/failed-to-download-gnu-archive
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(with-eval-after-load 'package
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("org" . "https://orgmode.org/elpa/"))))

(require 'package)
;; (call-interactively 'kd/turn-on-http-proxy)
(package-initialize)

(defun kd/install-pkgs (pkgs)
  (let ((refreshed nil))
    (dolist (pkg pkgs)
      (unless (package-installed-p pkg)
        (unless refreshed
          (package-refresh-contents)
          (setq refreshed t))
        (package-install pkg)))))

;;;; auto-compile
(kd/install-pkgs '(auto-compile))
(require 'auto-compile)
(auto-compile-on-save-mode 1)
(auto-compile-on-load-mode 1)

;;;; use-package
(kd/install-pkgs '(bind-key
                   diminish
                   use-package))

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

(use-package use-package-ensure-system-package
  :ensure t)

(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-now kd/update-git-packages)
  :config (setq auto-package-update-delete-old-versions nil))

(use-package paradox
  :ensure t
  :config
  (setq paradox-column-width-package 27)
  (setq paradox-column-width-version 13)
  (setq paradox-execute-asynchronously t)
  (setq paradox-hide-wiki-packages t)
  (paradox-enable)
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

;; (use-package rx
;;   :load-path "~/.emacs.d/quelpa/build/rx"
;;   :quelpa (rx :fetcher url :url "https://raw.githubusercontent.com/emacs-mirror/emacs/dbffbe08815644fd30404891ef81496277ed27da/lisp/emacs-lisp/rx.el"))

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

  (defvar kd/org-map nil)
  (kd/make-prefix-command (kbd "s-r") 'kd/org-map)
  (defvar kd/compile-map nil)
  (kd/make-prefix-command (kbd "s-c") 'kd/compile-map)
  (defvar kd/errors-map nil)
  (kd/make-prefix-command (kbd "s-e") 'kd/errors-map)
  (defvar kd/eshell-map nil)
  (kd/make-prefix-command (kbd "s-s") 'kd/eshell-map)
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

  :defines (kd/org-map
            kd/compile-map
            kd/errors-map
            kd/eshell-map)

  :commands kd/switch-to-previous-buffer
  :functions kd/make-prefix-command
  :chords ("JJ" . kd/switch-to-previous-buffer)
  :config (global-set-key (kbd "C-x K") #'kd/kill-buffer-no-select))


;;; Startup

;;;; envvar
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-shell-name (executable-find "zsh"))
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
  :commands (desktop-save desktop-save-in-desktop-dir desktop-read)
  :init
  (let ((desktop-dir (list (kd/emacs-cache-dir "desktop" t))))
    (setq desktop-dirname desktop-dir)
    (setq desktop-path desktop-dir)))

;;; User Interface (UI)

(use-package kd-UI
  :no-require t
  :config
  (menu-bar-mode 1)
  (column-number-mode 1)

  (defun kd/suppress-messages (old-fun &rest args)
    (cl-flet ((silence (&rest args1) (ignore)))
      (advice-add 'message :around #'silence)
      (unwind-protect
          (apply old-fun args)
        (advice-remove 'message #'silence))))

  (use-package face-remap
    :commands (buffer-face-mode
               text-scale-mode
               face-remap-add-relative))

  (use-package simple
    :diminish visual-line-mode
    :commands (global-visual-line-mode turn-on-visual-line-mode)
    :init (add-hook 'after-init-hook #'global-visual-line-mode)
    :config
    (setq set-mark-command-repeat-pop t)
    (setq next-error-recenter '(4))
    (advice-add 'toggle-truncate-lines :around #'kd/suppress-messages))

  (use-package shackle
    :ensure t
    :init (add-hook 'after-init-hook #'shackle-mode)
    :config
    (setq shackle-lighter "")
    (setq shackle-select-reused-windows nil)
    (setq shackle-default-alignment 'below)
    (setq shackle-default-size 0.45)

    ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-shackle.el
    (setq shackle-rules
          ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
          '(("*osx-dictionary*"            :select t                          :align below            :popup t)
            ("*info*"                      :select t                          :align right  :popup t)
            ("*Help*"                      :select t                          :align right            :popup t)
            ("\\`\\*[hH]elm.*?\\*\\'"      :regexp t                          :size 0.35 :align 'below)
            (helpful-mode                  :select t                          :align right            :popup t)
            (magit-status-mode             :select t                          :size 0.5 :align right  :popup t)
            (magit-log-mode                :select t                          :size 0.4 :align right  :popup t)
            ("*Flycheck errors*"           :select t                          :size 0.2 :align below  :popup t))))

  (use-package all-the-icons
    :config
    (setq all-the-icons-color-icons nil))

  (use-package doom-modeline
    :ensure t
    :preface
    (defun kd/setup-custom-doom-modeline ()
      (doom-modeline-set-modeline 'kd/doom-modeline-format 'default))
    :hook ((after-init . doom-modeline-mode)
           (doom-modeline-mode . kd/setup-custom-doom-modeline))
    :config
    (doom-modeline-def-modeline 'kd/doom-modeline-format
      '(bar persp-name workspace-name checker window-number modals matches buffer-info remote-host buffer-position vcs selection-info)
      '(misc-info debug lsp minor-modes indent-info buffer-encoding major-mode process))
    (setq doom-modeline-height 25)
    (setq doom-modeline-buffer-file-name-style 'buffer-name)
    (setq doom-modeline-icon nil)
    (setq doom-modeline-minor-modes nil)
    (setq doom-modeline-persp-name t)
    (set-face-attribute 'doom-modeline-persp-name nil
                        :inherit '(mode-line-emphasis italic))
    (set-face-attribute 'doom-modeline-persp-buffer-not-in-persp nil
                        :inherit '(mode-line-emphasis bold italic))))

(use-package writeroom-mode
  :ensure t
  :commands writeroom-mode
  :config
  (setq writeroom-mode-line-toggle-position 'mode-line-format)
  (setq writeroom-mode-line t)
  (setq writeroom-restore-window-config t)
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-global-effects '(writeroom-set-fullscreen
                                   writeroom-set-bottom-divider-width)))

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

;;;;; Font
  (set-frame-font (font-spec :family "Input Mono Narrow"
                             :size 17
                             :weight 'regular)
                  nil
                  t)

;;;;; Frame
  (setq default-frame-alist '((fullscreen . fit-frame-to-buffer-sizes)
                              (font . "Input Mono Narrow-17:regular")
                              (vertical-scroll-bars . nil)
                              (horizontal-scroll-bars . nil)))
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (setq initial-frame-alist '((fullscreen . maximized)))

;;;;; Theme
  (use-package default-black-theme
    :disabled t
    :config
    (load-theme 'default-black t))

  (use-package nord-theme
    :disabled t
    :ensure t
    :config
    (setq nord-region-highlight "frost")
    (load-theme 'nord t))

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
    (load-theme 'zenburn t))

  (use-package hc-zenburn-theme
    ;; :disabled t
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
    :disabled t
    :ensure t
    :config
    (setq doom-themes-padded-modeline t)
    (setq doom-opera-brighter-comments nil)
    (setq doom-opera-brighter-modeline nil)
    (setq doom-opera-comment-bg nil)
    (setq doom-opera-padded-modeline 1)
    (load-theme 'doom-opera t)
    (doom-themes-org-config)
    ;; (custom-theme-set-faces
    ;;  'doom-opera
    ;;  `(region
    ;;    ((t (:background ,(doom-lighten (doom-color 'base4) 0.1))) t))
    ;;  `(ivy-current-match
    ;;    ((t (:background ,(doom-lighten (doom-color 'base4) 0.1) :underline t)) t)))
    )

  (use-package tron-legacy-theme
    :disabled t
    :quelpa (tron-legacy-theme :fetcher github :repo "ianpan870102/tron-legacy-emacs-theme")
    :config
    ;; (add-to-list 'custom-theme-load-path
    ;;              "/Users/kane/.emacs.d/elpa/tron-legacy-theme-20200521.1649"
    ;;              )
    (load-theme 'tron-legacy t))

  (tool-bar-mode -1)
  (tooltip-mode -1))

;;;; macOS

(defconst IS-MAC (memq window-system '(mac ns)))

(use-package kd-macOS
  :if IS-MAC
  :no-require t
  :demand t
  :preface
  (defun kd/start-screen-saver ()
    (interactive)
    (osx-lib-run-osascript "do shell script \"
                 if [ -e /System/Library/Frameworks/ScreenSaver.framework/Versions/A/Resources/ScreenSaverEngine.app ]; then
	             open /System/Library/Frameworks/ScreenSaver.framework/Versions/A/Resources/ScreenSaverEngine.app
                 else
	             open -a ScreenSaverEngine
                 fi
                 \""))
  :config
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)

  (use-package osx-lib
    :commands osx-lib-run-osascript
    :ensure t)

  (use-package osx-dictionary
    :ensure t
    :functions kd/osx-dictionary-mode-hook-func
    :bind ("C-c f" . osx-dictionary-search-pointer)
    :init
    (when IS-GUI
      (defun kd/osx-dictionary-mode-hook-func ()
        (setq buffer-face-mode-face '(:family "Verdana" :height 190))
        (buffer-face-mode 1))
      (add-hook 'osx-dictionary-mode-hook #'kd/osx-dictionary-mode-hook-func))))

;;;; macOS GUI
(use-package kd-macOS-GUI
  :if (and IS-MAC IS-GUI)
  :demand t
  :no-require t
  :config
;;;;; Frame
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (when (fboundp 'mac-set-frame-tab-group-property)
    (mac-set-frame-tab-group-property nil :tab-bar-visible-p nil))
  (setq mac-frame-tabbing nil)

;;;;; Unicode font
  ;; Useful for https://github.com/dunn/company-emoji
  ;; https://www.reddit.com/r/emacs/comments/8ph0hq/i_have_converted_from_the_mac_port_to_the_ns_port/
  ;; not tested with emacs26 (requires a patched Emacs version for multi-color font support)
  (if (version< "27.0" emacs-version)
      (set-fontset-font
       "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
    (set-fontset-font
     t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

  (setq mac-mouse-wheel-smooth-scroll nil))

;;; ---

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq history-delete-duplicates t)
  (setq history-length t)
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (setq savehist-file (expand-file-name "history" kd/cache-dir))
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1))

(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-exclude (list "COMMIT_EDITMSG"
                              "~$"
                              "/scp:"
                              "/ssh:"
                              "/sudo:"
                              "/tmp/"))
  (setq recentf-max-menu-items 15)
  (setq recentf-max-saved-items 200)
  (setq recentf-save-file (expand-file-name "recentf" kd/cache-dir)))

(use-package whitespace
  :diminish whitespace-mode
  :commands whitespace-mode
  :config
  (setq whitespace-line-column nil)
  (setq whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [9166 10])
                                      (tab-mark 9 [9654 9] [92 9]))))

(use-package fill
  :diminish auto-fill-mode
  :commands auto-fill-mode)

(use-package visual-fill-column
  :ensure t
  :commands visual-fill-column-mode)

(use-package which-func
  :commands which-function-mode
  :config (add-to-list 'which-func-non-auto-modes 'nxml-mode))

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
  :diminish highlight-symbol-mode
  :config (set-face-attribute 'highlight-symbol-face nil :background "gray40"))

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

(use-package ace-isearch
  :disabled t
  :ensure t
  :bind (:map isearch-mode-map
              ("C-'" . ace-isearch-jump-during-isearch))
  :hook (after-init . global-ace-isearch-mode)
  :config
  (setq ace-isearch-function-from-isearch #'swiper-from-isearch)
  (setq ace-isearch-jump-delay 0.5))

(use-package amx
  :ensure t
  :hook (after-init . amx-mode)
  :config
  (setq amx-save-file (expand-file-name "amx-items" kd/cache-dir)))

(use-package scratch
  :ensure t
  :preface
  (defun kd/scratch-create-buffer-hook-func ()
    (emacs-lock-mode 'kill))
  :commands scratch
  :hook (scratch-create-buffer . kd/scratch-create-buffer-hook-func))

(use-package so-long
  :commands global-so-long-mode
  :hook (after-init . global-so-long-mode))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

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
  :demand t
  :bind (("C-c C--" . eyebrowse-next-window-config)
         ("C-c C-=" . eyebrowse-prev-window-config)
         ("s-w" . eyebrowse-close-window-config)
         ("s-'" . eyebrowse-last-window-config)
         ("s-1" . eyebrowse-switch-to-window-config-1)
         ("s-2" . eyebrowse-switch-to-window-config-2)
         ("s-3" . eyebrowse-switch-to-window-config-3)
         ("s-4" . eyebrowse-switch-to-window-config-4)
         ("s-5" . eyebrowse-switch-to-window-config-5))
  :config
  (setq eyebrowse-mode-line-separator " ")
  (setq eyebrowse-mode-line-style 'always)
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-wrap-around t)
  (eyebrowse-mode 1))

(use-package persp-mode
  :disabled t
  :ensure t
  :demand t
  :bind (:map persp-mode-map
              ("s-w" . persp-switch)
              ("s-x" . persp-switch-to-buffer))
  :config
  (setq wg-morph-on nil)
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (setq persp-kill-foreign-buffer-behaviour nil)
  (setq persp-nil-hidden t)
  (setq persp-auto-save-opt 1)
  (setq persp-remove-buffers-from-nil-persp-behaviour nil)
  (setq persp-add-buffer-on-after-change-major-mode 'free)
  (persp-mode 1))

;; Perspective-local eyebrwose window config
;; https://github.com/syl20bnr/spacemacs/pull/4068/files
(use-package persp-mode-local-eyebrowse
  :disabled t
  :no-require t
  :after (persp-mode eyebrowse)
  :config
  (defun kd/load-eyebrowse-for-perspective (&optional frame-or-window)
    "Load an eyebrowse workspace according to a perspective's parameters.
FRAME's perspective is the perspective that is considered, defaulting to
the current frame's perspective.
If the perspective doesn't have a workspace, create one."
    (let* ((frame (if (eq frame-or-window 'frame)
                      (selected-frame)
                    nil))
           (window (if (eq frame-or-window 'window)
                       (selected-window)
                     nil))
           (persp (get-current-persp frame window))
           (window-configs (persp-parameter 'eyebrowse-window-configs persp))
           (current-slot (persp-parameter 'eyebrowse-current-slot persp))
           (last-slot (persp-parameter 'eyebrowse-last-slot persp)))
      (if window-configs
          (progn
            (eyebrowse--set 'window-configs window-configs frame)
            (eyebrowse--set 'current-slot current-slot frame)
            (eyebrowse--set 'last-slot last-slot frame)
            (eyebrowse--load-window-config current-slot))
        (eyebrowse--set 'window-configs nil frame)
        (eyebrowse-init frame)
        (kd/save-eyebrowse-for-perspective frame))))

  (defun kd/update-eyebrowse-for-perspective (_new-persp-name &optional frame)
    "Update and save current frame's eyebrowse workspace to its perspective.
Parameter _NEW-PERSP-NAME is ignored, and exists only for compatibility with
`persp-before-switch-functions'."
    (eyebrowse--update-window-config-element
     (eyebrowse--current-window-config (eyebrowse--get 'current-slot)
                                       (eyebrowse--get 'current-tag)))
    (kd/save-eyebrowse-for-perspective))

  (defun kd/save-eyebrowse-for-perspective (&optional frame)
    "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
    (let ((persp (get-frame-persp frame)))
      (set-persp-parameter
       'eyebrowse-window-configs (eyebrowse--get 'window-configs frame) persp)
      (set-persp-parameter
       'eyebrowse-current-slot (eyebrowse--get 'current-slot frame) persp)
      (set-persp-parameter
       'eyebrowse-last-slot (eyebrowse--get 'last-slot frame) persp)))

  (add-hook 'persp-before-switch-functions #'kd/update-eyebrowse-for-perspective)
  (add-hook 'eyebrowse-post-window-switch-hook #'kd/save-eyebrowse-for-perspective)
  (add-hook 'persp-activated-functions #'kd/load-eyebrowse-for-perspective))

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

(use-package dired-single
  :ensure t
  :bind (:map dired-mode-map
              ("<RET>" . dired-single-buffer)
              ("<mouse-1>" . dired-single-buffer-mouse)
              ("^" . dired-single-up-directory)))

(use-package dired-x
  :commands dired-omit-mode
  :defines dired-omit-files
  :init (add-hook 'dired-mode-hook #'dired-omit-mode))

(use-package dired-du
  :commands dired-du-mode
  :ensure t)

;; (use-package dired+
;;   :quelpa (dired+ :fetcher url :url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/dired+.el")
;;   :after dired)

(use-package peep-dired
  :ensure t
  :commands peep-dired
  :bind ((:map dired-mode-map
               ("P" . peep-dired))))

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
  :demand t
  :diminish projectile-mode
  :commands projectile-mode
  :bind ("s-x" . projectile-switch-to-buffer)
  ;; :bind ((:map kd/projectile-map
  ;;              ("a" . projectile-add-known-project)
  ;;              ("b" . projectile-ibuffer)
  ;;              ("i" . projectile-invalidate-cache)))
  :config
  ;; (defadvice projectile-project-root (around exlude-tramp activate)
  ;;   "This should disable projectile when visiting a remote file"
  ;;   (unless (--any? (and it (file-remote-p it))
  ;;                   (list
  ;;                    (buffer-file-name)
  ;;                    list-buffers-directory
  ;;                    default-directory
  ;;                    dired-directory))
  ;;     ad-do-it))

  (setq projectile-cache-file (expand-file-name "projectile.cache" kd/cache-dir))
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
  (setq projectile-sort-order 'recently-active)
  (projectile-mode 1))

(use-package persp-mode-projectile-bridge
  :disabled t
  :ensure t
  :after (persp-mode projectile)
  :preface
  (defun kd/persp-mode-projectile-bridge-mode-hook-func ()
    (if persp-mode-projectile-bridge-mode
        (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
      (persp-mode-projectile-bridge-kill-perspectives)))
  :hook ((persp-mode-projectile-bridge-mode . kd/persp-mode-projectile-bridge-mode-hook-func)
         (after-init . persp-mode-projectile-bridge-mode)))

(use-package rg
  :ensure t
  :commands rg-define-search
  :bind ("C-c A" . rg-dwim)
  :hook (rg-mode . wgrep-rg-setup))

(use-package deadgrep
  :ensure t
  :bind ("C-c a" . deadgrep))


;;; Git

(use-package magit
  :ensure t
  :bind (("s-g" . nil)
         ("s-g s" . magit-status)
         ("s-g l" . magit-log-current)
         ("s-g b" . magit-blame))
  :preface
  (defun kd/magit-revision-mode-hook-func ()
    (text-scale-mode 1)
    (text-scale-set -0.3))
  :hook ((magit-process-mode . goto-address-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (magit-revision-mode . kd/magit-revision-mode-hook-func))
  :config
  (setq magit-git-executable (executable-find "git")))

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

(use-package duplicate-thing
  :ensure t
  :bind (:map ctl-x-map
              ("u" . duplicate-thing)))

(use-package hydra
  :ensure t
  :bind (("s-z" . hydra-goto/body)
         ("s-t" . hydra-toggle/body)
         ("s-o" . hydra-open/body)
         ("C-c w" . hydra-winner/body)
         ("C-M-o" . hydra-window-size/body))
  :config
  (use-package pretty-hydra
    :ensure t)

  (pretty-hydra-define hydra-goto
    (:hint nil :color blue :quit-key "q" :title "Goto")
    ("Coding"
     (("p" counsel-projectile-switch-project "Project")
      ("s" kd/jump-to-src "Repo")
      ("f" kd/jump-to-reference "Ref")
      ("e" (find-file "~/.dotfiles/.emacs") ".emacs"))
     "Org"
     (("c" kd/default-captured-org-note "cap.org: Default note")
      ("r" kd/counsel-org-find-file "Org files")
      ("t" (find-file "~/Dropbox/nvALT/snippets.org") "Snippets")
      ("w" (find-file (expand-file-name "work.org" org-directory)) "Work")
      ("l" helm-org-rifle-org-directory "Rifle"))
     "File"
     (("g" (find-file "~/Dropbox/ledger/ledger.beancount") "Ledger"))
     "Deft"
     (("zz" zetteldeft-new-file "New zettle")
      ("zf" zetteldeft-follow-link "Follow zettle link")
      ("zi" zetteldeft-find-file-full-title-insert "Insert zettle link")
      ("d" deft "Deft"))
     "Buffer"
     (("x" (switch-to-buffer "*scratch*") "*scratch*"))))

  (pretty-hydra-define hydra-toggle
    (:hint nil :color blue :quit-key "q" :title "Toggle")
    ("UI"
     (("l" display-line-numbers-mode "Show line number" :toggle t)
      ("w" whitespace-mode "Show white space" :toggle t)
      ("z" writeroom-mode "Writeroom" :toggle t)
      ("v" visual-fill-column-mode "Visual fill column mode" :toggle t)
      ("h" hl-line-mode "Show highlight line" :toggle t)
      ("m" markdown-toggle-markup-hiding "Hiding markdown markup")
      ("t" toggle-truncate-lines "Truncate long lines"))
     "Editing"
     (("f" auto-fill-mode "Auto fill mode"))
     "Desktop"
     (("s" desktop-save-in-desktop-dir "Save desktop")
      ("r" desktop-read "Read desktop"))
     "Debugging"
     (("D" dap-hydra)
      ("de" toggle-debug-on-error "Debug on error")
      ("dq" toggle-debug-on-quit "Debug on quit"))))

  (pretty-hydra-define hydra-open
    (:hint nil :color blue :quit-key "q" :title "Open")
    ("Editing"
     (("u" undo-tree-visualize "Show undo tree")
      ("U" undo-propose "Propose undo"))
     "Open"
     (("b" browse-url-at-point "Browse URL at point")
      ("r" browse-at-remote "Browse at repository")
      ("'" imenu-list-smart-toggle "Imenu list")
      ("e" prelude-open-with "Open externally")
      ("x" scratch "Scratch for current major mode"))
     "Package"
     (("p" list-packages "List packages"))))

  (defhydra hydra-winner ()
    "winner mode"
    ("h" winner-undo "undo")
    ("l" winner-redo "redo"))

  (defhydra hydra-zoom ()
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))

  (pretty-hydra-define hydra-window-size
    (:hint nil :color amaranth :quit-key "q" :title "Window size")
    ("Horizontal"
     (("h" shrink-window-horizontally "shrink horizontal")
      ("l" enlarge-window-horizontally "enlarge horizontal"))
     "Vertical"
     (("j" enlarge-window "enlarge vertical")
      ("k" shrink-window "shrink vertical"))
     "Balance"
     (("=" balance-windows "balance")))))

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
  :disabled t
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
  :bind (:map ivy-mode-map
              ("s-X" . ivy-switch-buffer)
              ("C-c C-r" . ivy-resume))
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

(use-package ivy-hydra
  :ensure t
  :after swiper)

(use-package ivy-prescient
  :disabled t
  :after ivy
  :ensure t
  :config
  (setq ivy-prescient-enable-filtering t)
  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode 1))

(use-package ivy-posframe
  :disabled t
  :ensure t
  :config
  (setq ivy-display-function #'ivy-posframe-display-at-point)
  (setq ivy-posframe-font "Input Mono")
  (setq ivy-posframe-border-width 1)
  (setq ivy-posframe-hide-minibuffer t)
  (setq ivy-posframe-width 90)
  (setq ivy-posframe-min-width 45)
  (setq ivy-posframe-parameters '((left-fringe . 8)
                                  (right-fringe . 8)))
  (set-face-attribute 'ivy-posframe-border nil :background "white" :inherit nil)
  (ivy-posframe-enable))

(use-package ivy-xref
  :ensure t
  :commands ivy-xref-show-xrefs
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  (setq xref-show-definitions-function #'ivy-xref-show-defs))

;;;; Counsel

(use-package counsel
  :ensure t
  :after ivy
  :bind
  (("C-S-s" . counsel-grep-or-swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-." . counsel-imenu)
   ("M-y" . counsel-yank-pop)
   ("C-h b" . counsel-descbinds)
   ("C-h y" . counsel-find-library)
   ("s-j" . counsel-mark-ring))
  :commands counsel-esh-history
  :config
  (add-to-list 'ivy-initial-inputs-alist '(counsel-rg . ivy-thing-at-point)))

(use-package xref)
(use-package gxref
  :ensure t
  :commands gxref-xref-backend
  :bind ((:map kd/tags-map
               ("c" . gxref-create-db)
               ("u" . gxref-single-update-db)
               ("U" . gxref-update-db)))
  :init (add-to-list 'xref-backend-functions 'gxref-xref-backend))

(use-package counsel-gtags
  :quelpa (counsel-gtags :fetcher github :repo "kols/emacs-counsel-gtags")
  :bind (("C-]" . counsel-gtags-find-definition)
         ("C-}" . counsel-gtags-find-reference)
         ("C-t" . counsel-gtags-go-backward)
         ("C-S-t" . counsel-gtags-go-forward))
  ;; :bind (:map counsel-gtags-mode-map
  ;;             ("C-]" . counsel-gtags-find-definition)
  ;;             ("C-}" . counsel-gtags-find-reference)
  ;;             ("C-t" . counsel-gtags-go-backward))
  ;; :chords (("gd" . counsel-gtags-find-definition)
  ;;          ("gr" . counsel-gtags-find-definition))
  :defer t
  :diminish counsel-gtags-mode
  :config
  (setq counsel-gtags-ignore-case t)
  (setq counsel-gtags-use-input-at-point t)
  (setq counsel-gtags-auto-select-only-candidate t)
  (setq counsel-gtags-auto-update t)
  (setq counsel-gtags-path-style 'root)
  (add-to-list 'ivy-more-chars-alist '(counsel-gtags--read-tag . 0)))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :bind ((:map projectile-mode-map
               ("s-P" . counsel-projectile-switch-project)
               ("s-p" . counsel-projectile-find-file)))
  :config (counsel-projectile-mode 1))

;;;; Swiper

(use-package swiper
  :ensure t
  :after ivy
  :commands (swiper)
  :bind (("C-s" . swiper-isearch)
         (:map isearch-mode-map
               ("M-i" . swiper-isearch-toggle))
         (:map swiper-map
               ("M-i" . swiper-isearch-toggle)
               ("C-r" . ivy-previous-line)))
  :config
  ;; (add-to-list 'ivy-re-builders-alist '(swiper . ivy--regex-plus))
  (add-to-list 'ivy-height-alist '(swiper-isearch . 10))
  (add-to-list 'ivy-height-alist '(swiper . 10))
  (setq swiper-include-line-number-in-search nil)
  (setq swiper-goto-start-of-match t)
  (set-face-attribute 'swiper-line-face nil :inherit 'swiper-match-face-3))


;; (use-package bookmark+
;;   :quelpa (bookmark+ :fetcher url :url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/bookmark+.el")
;;   :defer t)

(use-package isearch
  ;; :bind ("C-s" . isearch-forward)
  :config
  (use-package isearch+
    :quelpa (isearch+ :fetcher url :url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/isearch+.el")
    :defer 0.1))

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

(use-package help-fns+
  :quelpa (help-fns+ :fetcher url :url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/help-fns+.el")
  :after help-fns)

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

(use-package logview
  :ensure t
  :commands logview-mode
  :config
  (setq datetime-timezone 'Asia/Shanghai))


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

  ;; https://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode
  (defun endless/insert-key (key)
    "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
    (interactive "kType key sequence: ")
    (let* ((is-org-mode (derived-mode-p 'org-mode))
           (tag (if is-org-mode
                    "@@html:<kbd>%s</kbd>@@"
                  "<kbd>%s</kbd>")))
      (if (null (equal key "\r"))
          (insert
           (format tag (help-key-description key nil)))
        (insert (format tag ""))
        (forward-char (if is-org-mode -8 -6)))))
  :commands (org-end-of-line org-narrow-to-block org-mode orgtbl-mode kd/counsel-org-find-file kd/default-captured-org-note)
  :defines (org-directory
            org-imenu-depth
            org-default-notes-file)
  :bind ((:map org-mode-map
               ("C-M-<return>" . org-insert-subheading)
               ("C-c L" . org-insert-link-global)
               ("C-c R" . org-refile)
               ("C-c k" . endless/insert-key)
               ("C-t" . org-mark-ring-goto))
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
  (setq org-modules '(ox-md))

  (defun kd/org-mode-hook-func ()
    (visual-line-mode -1))

  (add-hook 'org-mode-hook #'kd/org-mode-hook-func)
  :config
  (setq org-adapt-indentation nil)
  (setq org-imenu-depth 3)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-cycle-separator-lines 0)
  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "+")))
  (setq org-use-sub-superscripts nil)
  (setq org-image-actual-width nil)

  ;; face
  (dolist (face org-level-faces)
    (custom-set-faces `(,face ((t (:height 1.0 :weight semi-bold))))))

  (unbind-key "C-'" org-mode-map)       ; used by `imenu-list'

  ;; link
  (org-link-set-parameters "devonthink"
                           :follow
                           (lambda (id)
                             (osx-open-url-at-point (concat "x-devonthink-item://" id))))
  (org-link-set-parameters "x-devonthink-item"
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

  (use-package ob-restclient
    :ensure t
    :defer t)

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)
                                 (shell . t)
                                 (emacs-lisp . t)
                                 (plantuml . t)
                                 ))
  (setq org-plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")
  (setq ob-async-no-async-languages-alist '("ipython"
                                            "restclient"))
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append))

(use-package ox-publish
  :disabled t
  :commands (org-publish org-publish-project)
  :config
  (setq org-publish-project-alist
        `(
          ("org-wiki-html"
           :base-directory ,(expand-file-name "wiki" org-directory)
           :base-extension "org"
           :publishing-directory ,(expand-file-name "wiki/html" org-directory)
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 3
           :auto-preamble t
           )
          ("org-wiki-static"
           :base-directory ,(expand-file-name "wiki" org-directory)
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory ,(expand-file-name "wiki/html" org-directory)
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("org-wiki" :components ("org-wiki-html" "org-wiki-static"))
          )))

(use-package org-wiki
  :disabled t
  :quelpa (org-wiki :fetcher github :repo "caiorss/org-wiki")
  :config
  (setq org-wiki-location-list `(,(expand-file-name "wiki" org-directory)))
  (setq org-wiki-location (car org-wiki-location-list))
  (setq org-wiki-server-host "127.0.0.1")
  (setq org-wiki-server-port "8000")
  (setq org-wiki-template
        (string-trim
         "
  #+TITLE: %n
  #+DESCRIPTION:
  #+KEYWORDS:
  #+STARTUP:  content
  #+DATE: %d
  #+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"css/org.css\"/>

  - [[wiki:index][Index]]

  - Related:

  * %n
  ")))

(use-package org-bullets
  :disabled t
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("●" "►" "▸")))

(use-package org-category-capture
  :ensure t
  :commands occ-capture-goto-marker)

(use-package org-projectile
  :ensure t
  :demand t
  :commands (org-projectile-project-todo-completing-read
             org-projectile-goto-location-for-project)
  :bind (:map projectile-mode-map
              ("s-N" . kd/org-projectile-goto-location-for-current-project))
  :init
  (defun kd/org-projectile-goto-location-for-current-project ()
    (interactive)
    (let ((project (projectile-project-name)))
      (if (projectile-project-p)
          (occ-capture-goto-marker
           (make-instance 'occ-context
                          :category project
                          :template org-projectile-capture-template
                          :strategy org-projectile-strategy
                          :options nil))
        (error (format "%s is not a recognized projectile project."
                       project-name)))))
  :config
  (org-projectile-single-file)
  (setq org-projectile-projects-file
        (expand-file-name "proj_notes.org" org-directory))
  (push (org-projectile-project-todo-entry) org-capture-templates))

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
          `(("n" "note" entry (file+olp+datetree ,default-capture-file) "* %?\n:LOGBOOK:\n:CREATED: %U\n:END:\n%i")
            ("m" "meeting record" entry (file+olp+datetree ,meeting-record-file) "* %?\n:LOGBOOK:\n:CREATED: %U\n:END:\n%i" :tree-type week)
            ("t" "tldr" entry (file+olp ,tldr-file "TL;DR") "* %?\n:LOGBOOK:\n:CREATED: %U\n:END:\n%i" :tree-type week)
            ("u" "url bookmark" entry (file+olp ,bookmark-file "Bookmarks") #'kd/org-bookmark-template)))))

(use-package org-clock
  ;; See: x-devonthink-item://F2D598BA-2D9D-4B7D-925C-64EB4CC68A9F
  :ensure org
  :preface
  (defun kd/org-clock-in ()
    (interactive)
    (org-clock-in '(4)))
  :commands org-clock-in
  :bind
  (("C-c I" . kd/org-clock-in)
   ("C-c O" . org-clock-out))
  :hook (org-clock-out . save-buffer)
  :config
  (setq org-clock-persist t)
  (setq org-clock-in-resume t)
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-into-drawer t)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-out-when-done t)
  (setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
  (setq org-clock-report-include-clocking-task t)
  (setq org-pretty-entities t)
  (setq org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 60 :score 0))
  (setq org-clock-idle-time 15)
  (setq org-global-properties
        '(("Effort_ALL" .
           "0:15 0:30 1:00 2:00 4:00 6:00 8:00 16:00 24:00 32:00 40:00")))
  (setq org-columns-default-format "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"))

(use-package htmlize
  :ensure t
  :after ox
  :commands (htmlize-file
             htmlize-buffer))

(use-package ox-reveal
  :ensure t
  :commands (org-reveal-export-to-html
             org-reveal-export-to-html-and-browse
             org-reveal-export-current-subtree)
  :config
  (setq org-reveal-root (kd/ghq-github-repo-path "hakimel/reveal.js"))
  (setq org-reveal-theme "white")
  (setq org-reveal-transition "none")
  (setq org-reveal-title-slide nil))

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

  (use-package ol-git-link
    :after org))

(use-package org-annotate
  :disabled t
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
    (goto-address-prog-mode 1)
    (toggle-truncate-lines 1)
    (text-scale-set -0.6)

    (add-to-list 'eshell-visual-commands "ssh")
    (add-to-list 'eshell-preoutput-filter-functions #'xterm-color-filter)
    (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

    ;; alias
    (eshell/alias "lm" "ls -lahF $1")
    (eshell/alias "ff" "find-file $1")
    (eshell/alias "fo" "find-file-other-window $1")
    (eshell/alias "d" "dired $1")
    ;; magit
    (eshell/alias "gst" #'kd/eshell-gst)
    (eshell/alias "gd" #'magit-diff-unstaged)
    (eshell/alias "gds" #'magit-diff-staged)
    ;; grep
    (eshell/alias "hrg" "helm-rg $1")
    ;; projectile
    (eshell/alias "cr" #'kd/eshell-cd-projectile-root))
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
    (let* ((parent (or (projectile-project-root)
                       (if (buffer-file-name)
                           (file-name-directory (buffer-file-name))
                         default-directory)))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t))))
           (eshell-buffer-name (concat "*eshell: " name "*"))
           (eshell-buffer-window (get-buffer-window eshell-buffer-name 'visible)))
      (if eshell-buffer-window
          (delete-window eshell-buffer-window)
        (split-window-vertically (- height))
        (other-window 1)
        (with-current-buffer (eshell)
          (unless eshell-buffer-window
            (kd/eshell-cd-projectile-root)
            (rename-buffer eshell-buffer-name))))))

  (defun kd/eshell-gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))

  (defun kd/eshell-cd-projectile-root ()
    (let ((root (projectile-project-root)))
      (eshell/cd root)))

  (add-hook 'eshell-before-prompt-hook (lambda ()
                                         (setq xterm-color-preserve-properties t)))
  :config
  (use-package em-hist
    :ensure eshell
    :config
    (bind-key "C-c C-l" #'counsel-esh-history eshell-hist-mode-map))

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
  :mode ("\\.zsh\\(env\\|rc\\)\\'" . sh-mode)
  :config
  (let ((cond '(sh-mode . "Strict bash options")))
    (unless (assoc cond auto-insert-alist)
      (define-auto-insert cond '(nil "#!/bin/bash\nset -euo pipefail\nIFS=$'\\n\\t'\n")))))

(use-package comint
  :preface
  (defun kd/comint-mode-hook-func ()
    (smartscan-mode -1))
  :bind (:map comint-mode-map
              ("C-c C-l" . counsel-shell-history))
  :hook
  ((comint-mode . kd/comint-mode-hook-func)
   (kill-buffer . comint-write-input-ring)
   (kill-emacs . comint-write-input-ring))
  :config
  (setq comint-buffer-maximum-size 99999)
  (setq comint-input-ring-size 10000)
  (setq comint-terminfo-terminal "xterm-256color")
  (setq comint-history-isearch 'dwim)
  (setq comint-insert-previous-argument-from-end t)
  (setq comint-input-autoexpand t))

;;; Term

(use-package vterm
  :ensure t
  :commands vterm)

(use-package xterm-color
  :ensure t
  :commands xterm-color-filter)

(use-package compile
  :commands compilation-mode
  :hook (compilation-mode . (lambda () (text-scale-set -0.6)))
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun kd/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'kd/advice-compilation-filter))

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
  :commands browse-url-generic browse-url-at-point
  :config (setq browse-url-generic-program "open"))

(use-package browse-at-remote
  :ensure t
  :commands browse-at-remote
  :config (dolist (elt '(("gitlab.alibaba-inc.com" . "gitlab")
                         ("gitlab.alipay-inc.com" . "gitlab")))
            (add-to-list 'browse-at-remote-remote-type-domains elt)))

(use-package goto-addr
  :diminish goto-address-mode
  :bind ((:map goto-address-highlight-keymap
               ("C-c C-o" . 'goto-address-at-point)))
  :commands (goto-address-mode goto-address-prog-mode))

(use-package ispell
  :defer t
  :ensure-system-package hunspell
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

(use-package flyspell
  :commands flyspell-mode
  :hook ((markdown-mode org-mode text-mode) . flyspell-mode)
  :config
  (define-key flyspell-mode-map [remap flyspell-auto-correct-word] nil)
  (unbind-key "C-." flyspell-mode-map))

(use-package langtool
  :ensure t
  :ensure-system-package languagetool
  :config
  (setq langtool-default-language "en-US")
  (setq langtool-disabled-rules '("COMMA_PARENTHESIS_WHITESPACE"
                                  "COPYRIGHT"
                                  "DASH_RULE"
                                  "EN_QUOTES"
                                  "EN_UNPAIRED_BRACKETS"
                                  "UPPERCASE_SENTENCE_START"
                                  "WHITESPACE_RULE"))
  (setq langtool-language-tool-jar "/usr/local/opt/languagetool/libexec/languagetool-commandline.jar")
  (setq langtool-language-tool-server-jar "/usr/local/opt/languagetool/libexec/languagetool-server.jar"))

(use-package graphql-mode
  :ensure t
  :defer t)

(use-package plantuml-mode
  :ensure t
  :ensure-system-package plantuml
  :commands plantuml-mode
  :preface
  (defun kd/plantuml-mode-hook-func ()
    (setq-local plantuml-exec-mode 'jar))
  :hook (plantuml-mode . kd/plantuml-mode-hook-func)
  :init (setq plantuml-default-exec-mode 'jar)
  :config
  (setq plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar"))

(use-package flycheck-plantuml
  :ensure t
  :commands flycheck-plantuml-setup
  :init (add-hook 'plantuml-mode-hook #'flycheck-plantuml-setup))


;;; Programming

(use-package prog-mode
  ;; :bind (:map prog-mode-map
  ;;             ("C-]" . xref-find-definitions)
  ;;             ("C-}" . xref-find-references)
  ;;             ("C-t" . xref-pop-marker-stack))
  :init
  (defun kd-prog-mode-hook-func ()
    (setq-local show-trailing-whitespace t)
    (setq-local indicate-empty-lines t)
    (let ((prog-minor-modes '(highlight-symbol-mode
                              diff-hl-mode
                              counsel-gtags-mode
                              goto-address-prog-mode
                              abbrev-mode
                              hs-minor-mode
                              flycheck-mode
                              which-function-mode)))
      (dolist (mode prog-minor-modes)
        (apply mode '(1)))))

  (add-hook 'prog-mode-hook #'kd-prog-mode-hook-func))

(use-package counsel-dash
  :ensure t
  :commands counsel-dash counsel-dash-install-docset)

(use-package dash-at-point
  :ensure t
  :bind ("C-c D" . dash-at-point))

(use-package format-all
  :ensure t
  :bind (:map ctl-x-map
              ("m" . format-all-buffer)))

(use-package lsp-mode
  :disabled t
  :ensure t
  :commands (lsp lsp-deferred lsp-flycheck-add-mode)
  :bind ((:map lsp-mode-map
               ("C-c d" . lsp-describe-thing-at-point)
               ("M-RET" . lsp-execute-code-action)
               ;; ("M--" . lsp-find-implementation)
               ("M--" . lsp-treemacs-implementations)
               ("M-?" . lsp-treemacs-references)
               ("<f2>" . lsp-rename)))
  :config
  (setq lsp-response-timeout 10)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-auto-configure nil)
  (setq lsp-lens-check-interval 1)
  (setq lsp-document-highlight-delay 0.5)
  (setq lsp-eldoc-prefer-signature-help t)
  (setq lsp-eldoc-enable-signature-help t)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-completion-at-point t)
  (setq lsp-enable-file-watchers t)
  (setq lsp-enable-imenu t)
  (setq lsp-enable-xref t)
  (setq lsp-enable-indentation t)
  (setq lsp-diagnostic-package :auto)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-headerline-breadcrumb-enable t)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t)
  (lsp-flycheck-add-mode 'java-mode)
  ;; (setf (lsp-session-folders-blacklist (lsp-session)) `(,(expand-file-name "~")))

  (use-package lsp-treemacs
    :ensure t
    :commands (lsp-treemacs-implementations lsp-treemacs-references)
    :bind (:map lsp-mode-map
                ("<f7>" . lsp-treemacs-symbols))))

(use-package lsp-ivy
  :ensure t
  :preface
  (defun kd/lsp-ivy-workspace-symbol-at-point (arg)
    (interactive "P")
    (lsp-ivy--workspace-symbol (lsp-workspaces)
                               "Symbol: "
                               (thing-at-point 'symbol)))
  :commands lsp-ivy--workspace-symbol
  :bind ("s-l g s" . kd/lsp-ivy-workspace-symbol-at-point))

(use-package dap-mode
  :ensure t
  :commands (dap-mode dap-register-debug-template dap-debug)
  :hook (dap-server-log-mode . (lambda () (text-scale-set -0.6)))
  :config
  (remove-hook 'dap-session-created-hook 'doom-modeline--debug-visual))

(use-package dap-java
  ;; see kols/homebrew-jdt-language-server, plugins/ folder for
  ;; installing required debugging plugins
  :ensure dap-mode
  :commands (dap-java-debug
             dap-java-run-test-method
             dap-java-run-test-class
             dap-java-debug-test-method
             dap-java-debug-test-class)
  :init
  (pretty-hydra-define kd/dap-java-hydra
    (:hint nil :color blue :quit-key "q" :title "Dap mode (Java)")
    ("Test"
     (("r" dap-java-run-test-method)
      ("R" dap-java-run-test-class)
      ("dr" (dap-java-debug-test-method 50001))
      ("dR" (dap-java-debug-test-class 50001)))))
  :config
  (bind-key "M-S-SPC" #'kd/dap-java-hydra/body java-mode-map)
  (dap-register-debug-template "talos.search(alta1)"
                               (list :type "java"
                                     :request "attach"
                                     :hostName "alta1-talos-search-1.vm.elenet.me"
                                     :port 4001
                                     :name "alta1-talos-search-1.vm.elenet.me(4001)"))
  (dap-register-debug-template "talos.core(local)"
                               (list :type "java"
                                     :request "launch"
                                     :args ""
                                     :vmArgs "-DAPPID=bpm.talos.core"
                                     :cwd (expand-file-name "~/work/repos/gitlab.alibaba-inc.com/eleme-trade/talos-core/talos-collector")
                                     :host "localhost"
                                     :request "launch"
                                     :name "talos.core(local)"
                                     :projectName "talos-collector"
                                     :mainClass "me.ele.talos.collector.App"))
  (dap-register-debug-template "biz.bos_new(local)"
                               (list :type "java"
                                     :request "launch"
                                     :args ""
                                     :vmArgs "-DAPPID=biz.bos_new"
                                     :cwd (expand-file-name "~/work/repos/gitlab.alibaba-inc.com/eleme-trade/bos/bos-biz")
                                     :host "localhost"
                                     :request "launch"
                                     :name "biz.bos_new(local)"
                                     :projectName "bos-biz"
                                     :mainClass "me.ele.trade.bos.biz.main.BizOrderServiceApplication")))

(use-package dap-hydra
  :ensure dap-mode
  :commands dap-hydra)

(use-package treemacs
  :ensure t
  :preface
  (defun kd/treemacs-mode-hook-func ()
    (treemacs-resize-icons 15)
    (treemacs-tag-follow-mode 0)
    (treemacs-filewatch-mode 1)
    (treemacs-git-mode 'deferred)
    (treemacs-fringe-indicator-mode 1)
    ;; (treemacs-load-theme "Default")
    (text-scale-mode 1)
    (text-scale-set -0.5))
  :commands treemacs
  :bind ("<f8>" . treemacs)
  :hook (treemacs-mode . kd/treemacs-mode-hook-func)
  :config
  (setq treemacs-python-executable (string-trim (shell-command-to-string "PYENV_VERSION='3.7.2' pyenv which python"))
        treemacs-eldoc-display nil
        treemacs-follow-after-init          t
        treemacs-width                      40
        treemacs-indentation                2
        treemacs-collapse-dirs              99
        treemacs-silent-refresh             t
        treemacs-sorting                    'alphabetic-case-insensitive-asc
        treemacs-show-hidden-files          t
        treemacs-is-never-other-window      nil
        treemacs-goto-tag-strategy          'call-xref))

(use-package treemacs-projectile
  :ensure t
  :after treemacs)

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-delay 1)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-show-symbol nil))

(use-package company-lsp
  :disabled t
  :ensure t
  :commands company-lsp
  :hook (java-mode . (lambda ()
                       (kd/local-push-company-backend #'company-lsp))))

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
  :hook (after-init . smartparens-global-strict-mode)
  :config
  (require 'smartparens-config))

(use-package paren
  :hook (after-init . show-paren-mode))

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

(use-package emmet-mode
  :ensure t
  :commands emmet-mode)

(use-package nxml-mode
  :init
  (defun kd/nxml-where ()
    "Display the hierarchy of XML elements the point is on as a path."
    (interactive)
    (let ((path nil))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                      (condition-case nil
                          (progn
                            (nxml-backward-up-element) ; always returns nil
                            t)
                        (error nil)))
            (setq path (cons (xmltok-start-tag-local-name) path)))
          (if (called-interactively-p t)
              (message "/%s" (mapconcat 'identity path "/"))
            (format "/%s" (mapconcat 'identity path "/")))))))

  (defun kd/nxml-mode-hook-func ()
    (which-function-mode 1)
    (setq-local which-func-mode t)
    (add-hook 'which-func-functions #'kd/nxml-where)
    (hs-minor-mode 1)
    (emmet-mode 1)
    (smartparens-strict-mode -1))
  :hook (nxml-mode . kd/nxml-mode-hook-func)
  :config
  (setq nxml-child-indent 4)
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>" ;; regexp for start block
                 "-->\\|</[^/>]*[^/]>" ;; regexp for end block
                 "<!--"
                 nxml-forward-element
                 nil)))

;;;; Flycheck

(use-package flycheck
  :ensure t
  :commands (flycheck-mode counsel-flycheck flycheck-add-next-checker)
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
                :history 'kd/counsel-flycheck-history)))

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
  :commands imenu-list-smart-toggle
  :config
  (setq idle-update-delay 1)
  (setq imenu-list-focus-after-activation t))

;;;; Completion

(use-package company
  :ensure t
  :diminish company-mode
  :preface
  (defun kd/local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (setq-local company-backends (add-to-list 'company-backends backend)))
  :commands (company-mode global-company-mode)
  :init
  (setq company-backends '(company-capf company-dabbrev-code company-keywords))
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 2)
  (setq tab-always-indent 'complete)
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  (setq company-echo-truncate-lines nil))

(use-package company-flx
  :disabled t
  :ensure t
  :after company
  :commands company-flx-mode
  :config (company-flx-mode 1))

(use-package company-prescient
  :after company
  :ensure t
  :config (company-prescient-mode 1))

(use-package company-quickhelp
  :if (and IS-GUI (not IS-MAC))
  :ensure t
  :after company
  :commands company-quickhelp-mode
  :bind (:map company-active-map
              ("M-d" . company-quickhelp-manual-begin))
  :init (add-hook 'company-mode-hook #'company-quickhelp-mode)
  :config
  (setq company-quickhelp-use-propertized-text nil)
  (setq company-quickhelp-delay 1))

(use-package js2-refactor
  :ensure t
  :commands js2-refactor-mode)

(use-package indium
  :ensure t
  :commands indium-interaction-mode
  :config
  (setq indium-chrome-executable "/Applications/Chromium.app/Contents/MacOS/Chromium"))

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
  :mode ("\\.json\\'" . js2-mode))

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
    (face-remap-add-relative 'variable-pitch
                             '(:family "Avenir Next" :height 200 :weight regular))
    (face-remap-add-relative 'markdown-pre-face
                             '(:family "Input Mono Condensed" :height 180 :weight light))
    (buffer-face-mode 1)
    (markdown-toggle-markup-hiding 1))

  (defun kd/read-markdown ()
    (interactive)
    (markdown-view-mode)
    (writeroom-mode 1))

  (add-hook 'markdown-mode-hook #'kd/markdown-mode-hook-func)
  (add-hook 'markdown-view-mode-hook #'kd/markdown-view-mode-hook-func)
  (add-hook 'gfm-view-mode-hook #'kd/markdown-view-mode-hook-func)
  :config
  (setq markdown-command "multimarkdown")
  (advice-add 'markdown-toggle-markup-hiding :around #'kd/suppress-messages))

(use-package sql-indent
  :ensure t
  :commands sqlind-minor-mode
  :hook (sql-mode . sqlind-minor-mode))

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

(use-package subed
  :quelpa (subed :fetcher file :path "~/.ghq/github.com/rndusr/subed/subed")
  :mode ("\\.srt\\'" . subed-mode)
  :config
  (subed-disable-sync-point-to-player))


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
  :commands (global-undo-tree-mode undo-tree-mode undo-tree-visualize)
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode)
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
      ad-do-it))
  (unbind-key "C-x u" undo-tree-map))

(use-package undo-propose
  :ensure t
  :bind (:map undo-propose-mode-map
              ("C-/" . undo-only))
  :commands undo-propose)

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

(use-package vlf
  :ensure t
  :commands vlf
  :config
  (use-package vlf-setup
    :ensure vlf
    :hook (after-init . vlf-setup)))


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
  :load-path "~/.emacs.d/quelpa/build/python"
  :quelpa (python :fetcher url :url "https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/progmodes/python.el")
  :preface
  (defun kd/python-mode-hook-function ()
    (pyenv-mode 1)

    ;; jedi
    (jedi:setup)
    (kd/local-push-company-backend #'company-jedi)

    ;; imenu
    (setq imenu-create-index-function #'python-imenu-create-index)

    ;; whitespace
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
    (setq-local whitespace-style '(face indentation::tab))

    (subword-mode 1)

    (which-function-mode -1)
    (setq-local flycheck-checker 'python-flake8))

  (defun kd/inferior-python-mode-hook-func ()
    (kd/turn-on-comint-history (expand-file-name "infpy_hist" kd/cache-dir)))
  :commands python-mode
  :hook ((python-mode . kd/python-mode-hook-function)
         (inferior-python-mode . kd/inferior-python-mode-hook-func))
  :init
  (setenv "PYTHONIOENCODING" "UTF-8")
  :config
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-python-pylint-executable "pylint")
  (flycheck-add-next-checker 'python-flake8 'python-pylint t)
  (let ((cond '(python-mode . "Python file encoding")))
    (unless (assoc cond auto-insert-alist)
      (define-auto-insert cond '(nil "# coding: utf-8\n")))))

(use-package python-docstring
  :ensure t)

(use-package pyenv-mode
  :ensure t
  :preface
  (defun kd/pyenv-mode-set-auto ()
    "Automatically activates pyenv version if .python-version file exists."
    (f-traverse-upwards
     (lambda (path)
       (let ((pyenv-version-path (f-expand ".python-version" path)))
         (if (f-exists? pyenv-version-path)
             (progn
               (pyenv-mode-set (car (s-lines (s-trim (f-read-text pyenv-version-path 'utf-8)))))
               t))))))

  (defun kd/pyenv-mode-set-auto-projectile ()
    (let* ((root projectile-project-root)
           (pyenv-version-path (f-expand ".python-version" root)))
      (if (f-exists? pyenv-version-path)
          (progn
            (pyenv-mode-set (car (s-lines (s-trim (f-read-text pyenv-version-path 'utf-8)))))
            t))))
  :commands (pyenv-mode pyenv-mode-set pyenv-mode-unset)
  :hook ((find-file . kd/pyenv-mode-set-auto)
         (projectile-find-file . kd/pyenv-mode-set-auto-projectile)
         (projectile-after-switch-project . kd/pyenv-mode-set-auto-projectile))
  :init
  (defun kd/post-pyenv-mode-set (orig-func version)
    (let ((current-version (pyenv-mode-version)))
      (apply orig-func (list version))
      (unless (equal current-version version)
        (let ((venv-root (pyenv-mode-full-path version)))
          (setenv "VIRTUAL_ENV" venv-root)
          (setq jedi:server-args `("--virtual-env" ,venv-root))
          (setq inhibit-message t)
          (jedi:stop-server)
          (setq inhibit-message nil)))))
  (advice-add 'pyenv-mode-set :around #'kd/post-pyenv-mode-set)
  :config
  (setq pyenv-mode-mode-line-format
        '(:eval
          (when (and (derived-mode-p 'python-mode)
                     (pyenv-mode-version))
            (concat "py:" (pyenv-mode-version) " ")))))

(use-package jedi-core
  :ensure t
  :commands (jedi:setup jedi:stop-server)
  :bind (:map jedi-mode-map
              ("<s-mouse-1>" . jedi:goto-definition)
              ("C-c d" . jedi:show-doc))
  :config
  (setq jedi:use-shortcuts t)
  (setq jedi:tooltip-method nil)
  (setq jedi:complete-on-dot t)
  (setq jedi:import-python-el-settings nil))

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

(use-package cython-mode
  :ensure t
  ;; https://github.com/bbatsov/prelude/issues/940
  :hook (cython-mode . (lambda () (which-function-mode -1))))

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

(use-package java-mode
  :disabled t
  :no-require t
  :ensure cc-mode
  :preface
  (defun kd/java-mode-hook-func ()
    (require 'lsp-java)
    (lsp-deferred)
    (lsp-ui-mode 1)
    (lsp-lens-mode 1)
    (lsp-enable-which-key-integration)

    ;; company
    ;; (kd/local-push-company-backend #'company-lsp)

    ;; flycheck
    (setq-local flycheck-checker 'lsp)
    (setq-local flycheck-check-syntax-automatically '(save idle-change))
    ;; (flycheck-pmd-setup)
    ;; (setq-local flycheck-enabled-checkers '(lsp-ui java-pmd))
    ;; (flycheck-add-next-checker 'lsp-ui 'java-pmd)

    (require 'dap-java)
    (dap-mode 1)
    ;; (dap-ui-mode 1)
    (subword-mode 1)

    (visual-line-mode -1)
    (setq truncate-lines t)
    (which-func-mode -1)                ; use `lsp-headerline-breadcrumb-mode' instead

    (unbind-key "{" c-mode-base-map)
    (unbind-key "}" c-mode-base-map)
    (unbind-key "(" c-mode-base-map)
    (unbind-key ")" c-mode-base-map))
  :commands java-mode
  :bind ("<f5>" . dap-debug)
  :hook (java-mode . kd/java-mode-hook-func)
  :config
  (rg-define-search kd/search-jar
    ;; grep --binary-files=binary -r --include="*.jar" "org.springframework.transaction.annotation" .
    :query ask
    :files "all"
    :dir current
    :flags ("--binary"
            "--glob '**/*.jar'"
            "--files-with-matches")))

(use-package flycheck-pmd
  :ensure-system-package pmd
  :quelpa (flycheck-pmd :fetcher github :repo "kols/flycheck-pmd")
  :commands flycheck-pmd-setup
  :config
  ;; ali rulesets: https://github.com/alibaba/p3c/tree/master/p3c-pmd
  (setq flycheck-pmd-rulesets '("rulesets/java/quickstart.xml"
                                "rulesets/java/ali-comment.xml"
                                "rulesets/java/ali-concurrent.xml"
                                "rulesets/java/ali-constant.xml"
                                "rulesets/java/ali-exception.xml"
                                "rulesets/java/ali-flowcontrol.xml"
                                "rulesets/java/ali-naming.xml"
                                "rulesets/java/ali-oop.xml"
                                "rulesets/java/ali-orm.xml"
                                "rulesets/java/ali-other.xml"
                                "rulesets/java/ali-set.xml")))

(use-package lsp-java-boot
  :ensure lsp-java
  :commands lsp-java-boot-lens-mode)

(use-package lsp-java
  :ensure t
  :init
  (defvar kd/eclipse-jdt-server-dir (expand-file-name "/usr/local/opt/jdt-language-server/libexec"))
  (defvar kd/eclipse-jdt-home (expand-file-name "/usr/local/opt/jdt-language-server/libexec/plugins/org.eclipse.equinox.launcher_1.5.600.v20191014-2022.jar"))
  :config
  (setq lsp-java-workspace-dir (expand-file-name "~/lsp_java_workspace/"))
  (setq lsp-java-workspace-cache-dir (expand-file-name ".cache/" lsp-java-workspace-dir))
  (let ((cp (getenv "CLASSPATH")))
    (setenv "CLASSPATH" (concat cp ":" kd/eclipse-jdt-home)))
  (setq lsp-java-server-install-dir kd/eclipse-jdt-server-dir)
  (setq lsp-java-java-path "/usr/local/opt/jenv/versions/1.8/bin/java")
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx2G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-javaagent:/Users/kane/.m2/repository/org/projectlombok/lombok/1.16.20/lombok-1.16.20.jar"
         "-Xbootclasspath/a:/Users/kane/.m2/repository/org/projectlombok/lombok/1.16.20/lombok-1.16.20.jar"
         ;; "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044"
         ;; "-Declipse.application=org.eclipse.jdt.ls.core.id1"
         ;; "-Dosgi.bundles.defaultStaprtLevel=4"
         ;; "-Declipse.product=org.eclipse.jdt.ls.core.product"
         ;; "-javaagent:/Users/kane/.m2/repository/org/projectlombok/lombok/1.16.20/lombok-1.16.20.jar"
         ;; "-Xbootclasspath/a:/Users/kane/.m2/repository/org/projectlombok/lombok/1.16.20/lombok-1.16.20.jar"
         ;; "-Dlog.level=ALL"
         ;; "-jar"
         ;; kd/eclipse-jdt-home
         ;; "--add-modules=ALL-SYSTEM"
         ;; "--add-opens java.base/java.util=ALL-UNNAMED"
         ;; "--add-opens java.base/java.lang=ALL-UNNAMED"
         ;; "-noverify"
         ;; "-Xmx1G"
         ;; "-XX:+UseG1GC"
         ;; "-XX:+UseStringDeduplication"
         ))
  (setq lsp-file-watch-ignored
        '(".idea" ".ensime_cache" ".eunit" "node_modules" ".git" ".hg" ".fslckout" "_FOSSIL_"
          ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "build")
        ;; Formatter profile
        ;; lsp-java-format-settings-url (concat "file://" jmi/java-format-settings-file)
        lsp-enable-on-type-formatting t
        lsp-enable-indentation t)
  (setq lsp-java-progress-reports-enabled nil)
  (setq lsp-java-references-code-lens-enabled nil)
  (setq lsp-java-content-provider-preferred "fernflower"))

(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

(use-package ad-javap-mode
  :ensure autodisass-java-bytecode
  :mode ("\\.class\\'" . ad-javap-mode)
  :config (require 'autodisass-java-bytecode))

(use-package gradle-mode
  :ensure t
  :diminish gradle-mode
  :commands gradle-mode)

(use-package javadoc-lookup
  :after java-mode
  :ensure t)

(use-package eglot
  :disabled t
  :ensure t
  :commands eglot-ensure
  :bind (:map java-mode-map
              ("C-c d" . eglot-help-at-point)
              ("M--" . eglot-find-implementation))
  :config
  (defvar kd/eclipse-jdt-home (expand-file-name "/usr/local/opt/jdt-language-server/libexec/plugins/org.eclipse.equinox.launcher_1.5.600.v20191014-2022.jar"))
  (let ((cp (getenv "CLASSPATH")))
    (setenv "CLASSPATH" (concat cp ":" kd/eclipse-jdt-home)))
  (setq eglot-autoshutdown t))

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
    (kd/local-push-company-backend #'company-elisp))

  (defun kd/ielm-here ()
    (interactive)
    (let* ((height (/ (window-total-height) 3))
           (ielm-buffer-window (get-buffer-window "*ielm*" 'visible)))
      (if ielm-buffer-window
          (delete-window ielm-buffer-window)
        (split-window-vertically (- height))
        (other-window 1)
        (ielm)
        (rename-buffer "*ielm*"))))
  :bind (:map emacs-lisp-mode-map
              ("C-c C-p" . kd/ielm-here))
  :init
  (add-hook 'emacs-lisp-mode-hook #'kd/emacs-lisp-mode-hook-func)
  :config
  (let ((cond '(elisp-mode . "lexical binding")))
    (unless (assoc cond auto-insert-alist)
      (define-auto-insert cond '(nil ";;; -*- lexical-binding: t; -*-")))))

(use-package ielm
  :commands ielm
  :preface
  (defun kd/turn-on-comint-history (history-file)
    (setq-local comint-input-ring-file-name history-file)
    (comint-read-input-ring 'silent))
  :init
  (defun kd/ielm-mode-hook-func ()
    (kd/turn-on-comint-history (expand-file-name "ielm_hist" kd/cache-dir)))
  (add-hook 'ielm-mode-hook #'kd/ielm-mode-hook-func))


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


;;; ---

(use-package command-log-mode
  :ensure t
  :commands command-log-mode)

(use-package eg
  :quelpa (eg :fetcher github :repo "mnewt/eg.el")
  :commands (eg eg-at-point))

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
  :ensure t)

(use-package eww
  :commands eww
  :preface
  (defun kd/eww-mode-hook-func ()
    (setq-local shr-use-colors nil)
    (turn-on-visual-line-mode))
  :hook (eww-mode . kd/eww-mode-hook-func))

(use-package elpher
  :ensure t)

(use-package elfeed
  :disabled t
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
  :disabled t
  :ensure t
  :commands elfeed-org
  :config (elfeed-org)
  :config (setq rmh-elfeed-org-files `(,(expand-file-name "elfeed.org" org-directory))))

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

(bind-key "t" #'narrow-or-widen-dwim ctl-x-map)

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

;;; beancount

(use-package beancount
  :quelpa (beancount :fetcher url :url "https://raw.githubusercontent.com/beancount/beancount/master/editors/emacs/beancount.el")
  :mode ("\\.beancount\\'" . beancount-mode)
  :init
  (defun kd/beancount-mode-hook-func ()
    (add-to-list 'yas-extra-modes 'beancount-mode))
  (add-hook 'beancount-mode-hook #'kd/beancount-mode-hook-func))


;;; aliyun log
(use-package aliyun-log
  :no-require t
  :config
  (setq aliyun-query-log-time-ragne 604800) ; 1 week
  (setq aliyun-query-log-aliyunlog-executable (expand-file-name "~/.pyenv/versions/3.7.2/bin/aliyunlog"))

  (defun kd/aliyun-query-log (project logstore query-string &optional limit offset)
    (require 'json)

    (with-current-buffer (get-buffer-create "*aliyun-query-log*")
      (let* ((log-buffer (current-buffer))
             (end-time (current-time))
             (from-time (seconds-to-time (- (time-to-seconds end-time) aliyun-query-log-time-ragne)))
             (req-args (json-encode-alist
                        `(("topic" . "")
                          ("logstore" . ,logstore)
                          ("project" . ,project)
                          ("toTime" . ,(format-time-string "%Y-%m-%d %T" end-time))
                          ("offset" . ,(number-to-string (or offset 0)))
                          ("query" . ,query-string)
                          ("line" . ,(number-to-string (min (or limit 20) 50)))
                          ("fromTime" . ,(format-time-string "%Y-%m-%d %T" from-time))
                          ("reverse" . "true")))))
        ;; (emacs-lock-mode 'kill)
        (goto-char (point-max))
        (insert "* Query_"
                (string-join (list project logstore) "_")
                (concat "<" (format-time-string "%Y-%m-%d %T" end-time) ">" "\n")
                "** Args\n#+begin_src json\n"
                req-args
                "\n#+end_src\n** Response\n#+begin_src json\n")
        (start-process "aliyun-query-log"
                       log-buffer
                       aliyun-query-log-aliyunlog-executable
                       "log"
                       "get_logs"
                       (concat "--request=" req-args)
                       (concat "--jmes-filter=" "join('\\n', map(&to_string(@), @))"))
        (insert "#+end_src\n")
        (switch-to-buffer-other-window log-buffer)
        (org-mode)
        (goto-char (point-max))
        (previous-line)
        (org-show-subtree)
        (flyspell-mode -1)))))

;;; .emacs ends here
;;; Local Variables:
;;; no-byte-compile: t
;;; eval: (progn (outshine-mode 1))
;;; End:
