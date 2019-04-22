;;; .emacs --- Emacs init file

;;; Commentary:
;;;   Emacs init file
;;;
;;;   - Clone all the `~/.ghq' prefixed repos first

;;; Code:

(defun kd/emacs-subdirectory (d)
  "Make dir path inside Emacs user dir for D."
  (expand-file-name d user-emacs-directory))

(defvar kd/ghq-dir "~/.ghq")
(defvar kd/ghq-managed-repos nil)
(defun kd/ghq-repo-path (repo)
  "Return a path of repo managed by ghq.
REPO's pattern: `<domain>/<user>/<repo>'."
  (let* ((repo-path (expand-file-name repo kd/ghq-dir)))
    (unless (file-directory-p repo-path)
      (call-process-shell-command (concat "ghq get " repo)))
    (add-to-list 'kd/ghq-managed-repos repo-path)
    repo-path))
(defun kd/ghq-github-repo-path (repo)
  "Return a path of github repo mamaged by ghq.
REPO's pattern: `<user>/<repo>'"
  (expand-file-name (kd/ghq-repo-path (concat "github.com/" repo))))

(add-to-list 'load-path (kd/emacs-subdirectory "elisp"))
(add-to-list 'load-path (expand-file-name "lisp/progmodes" (kd/ghq-github-repo-path "emacs-mirror/emacs")))

;;; Package

(eval-after-load 'package
  (progn
    (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                             ("org" . "https://orgmode.org/elpa/")))
    (setq load-prefer-newer t)))

(require 'package)
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
(eval-after-load 'use-package
  (progn
    (setq use-package-enable-imenu-support t)
    (setq use-package-compute-statistics t)
    (setq use-package-verbose t)
    (setq use-package-expand-minimally t)))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)

(use-package auto-package-update
  :ensure t
  :commands auto-package-update-now
  :init
  (defun kd/update-git-packages ()
    "Update git managed packages."
    (interactive)
    (let ((repos kd/ghq-managed-repos))
      (dolist (repo repos)
        (let* ((default-directory repo)
               (proc (start-process "git-update-repo" (concat "*update-git-packages*") "git" "pull")))
          (message default-directory)
          (display-buffer (process-buffer proc))))))
  (add-hook 'auto-package-update-before-hook #'kd/update-git-packages))

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

;;; Defaults
(use-package kd-defaults
  :no-require t
  :config
  (use-package better-defaults
    :ensure t)
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq gc-cons-threshold 50000000)
  (setq scroll-conservatively 10000)
  (setq scroll-preserve-screen-position t)
  (setq vc-follow-symlinks t)
  (setq custom-file (kd/emacs-subdirectory "custom.el"))
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
          inhibit-startup-echo-area-message "Dou Qilong"
          inhibit-default-init t
          initial-major-mode 'fundamental-mode
          initial-scratch-message nil
          ring-bell-function 'ignore))

  (when (file-exists-p custom-file)
    (load custom-file)))


;;; Keybinding

(use-package kd-keybinding
  :no-require t
  :init
  (defun kd/make-prefix-command (key command)
    "Bind KEY for a prefix COMMAND."
    (define-prefix-command command)
    (global-set-key key command))
  :config
  (defvar kd/toggle-map nil)
  (kd/make-prefix-command (kbd "s-t") 'kd/toggle-map)
  (bind-key "l" #'display-line-numbers-mode 'kd/toggle-map)
  (defvar kd/pop-map nil)
  (kd/make-prefix-command (kbd "s-o") 'kd/pop-map)
  (defvar kd/org-map nil)
  (kd/make-prefix-command (kbd "s-r") 'kd/org-map)
  (defvar kd/compile-map nil)
  (kd/make-prefix-command (kbd "s-c") 'kd/compile-map)

  (defun kd/switch-to-previous-buffer ()
    "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
  (key-chord-define-global "JJ" #'kd/switch-to-previous-buffer))


;;; Startup

;;;; envvar
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-shell-name "/usr/local/bin/zsh")
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "HOMEBREW_AUTO_UPDATE_SECS"))
  (exec-path-from-shell-initialize))

;;;; daemon
(use-package server
  :init
  (defun kd/server-start ()
    (unless (server-running-p)
      (server-start)))
  (add-hook 'after-init-hook #'kd/server-start))

(use-package desktop
  :config
  (setq desktop-dirname user-emacs-directory)
  (desktop-save-mode 1))


;;; User Interface (UI)

(use-package kd-UI
  :no-require t
  :config
  (menu-bar-mode 1)
  (horizontal-scroll-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)

  (use-package remap-face
    :defines buffer-face-mode-face
    :commands buffer-face-mode)

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
    (setq shackle-default-size 0.45)

    ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-shackle.el
    (setq shackle-rules
          ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
          '(("*osx-dictionary*"            :select t                          :size 0.3 :align below  :popup t)
            ("*info*"                      :select t                          :align right            :popup t)
            ("*Help*"                      :select t                          :align right            :popup t)
            ("\\`\\*[hH]elm.*?\\*\\'"      :regexp t                          :size 0.35 :align above :popup t)
            (helpful-mode                  :select t                          :align right            :popup t)
            (magit-status-mode             :select t                          :align below            :popup t)
            (magit-log-mode                :select t                          :align below            :popup t)
            ("^\\*go-direx:"               :regexp t                          :size 0.3 :align right  :popup t)))))

;;;; Graphic

(defconst IS-GUI (display-graphic-p))

(use-package kd-GUI
  :no-require t
  :if IS-GUI
  :config
  ;;;;; Typeface
  (set-frame-font (font-spec :family "Fira Code Retina" :size 18))
  ;; 单独设置 CJK 字体，在 orgtbl 中英文混排时可对齐
  ;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
  ;;   (set-fontset-font (frame-parameter nil 'font)
  ;;                     charset (font-spec :family "Noto Sans Mono" :size 18)))

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
    :ensure t
    :config
    (load-theme 'zenburn t)
    (custom-theme-set-faces
     'zenburn
     '(highlight-symbol-face
       ((t (:foreground "#2B2B2B" :background "#8FB28F"))) t)
     '(region
       ((t (:foreground "#DCDCCC" :background "#2B2B2B"))) t)))

  (use-package prez-theme
    :disabled t
    :config
    (load-theme 'prez t))

  (tool-bar-mode -1)
  (tooltip-mode -1))

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
  (setq default-frame-alist '((fullscreen . maximized)
                              (ns-transparent-titlebar . t)
                              (ns-appearance . dark)))
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
  :commands wgrep-change-to-wgrep-mode)

(use-package autoinsert
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
  :defer t
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

(use-package dired+
  :load-path (lambda () (kd/ghq-github-repo-path "emacsmirror/dired-plus"))
  :after dired
  :defer t)

(use-package dired-x
  :commands dired-omit-mode
  :init (add-hook 'dired-mode-hook #'dired-omit-mode))

(use-package peep-dired
  :ensure t
  :commands peep-dired
  :bind ((:map dired-mode-map
               ("P" . peep-dired))))

;;; Tramp

(use-package tramp
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
  :init
  (add-hook 'after-init-hook #'projectile-mode)
  (defun kd-projectile-mode-hook-func ()
    (remove-hook 'find-file-hook #'projectile-find-file-hook-function)

    (bind-key "s-p" #'helm-projectile-find-file projectile-mode-map)
    (bind-key "C-c C-a" #'helm-projectile-rg projectile-mode-map)
    (bind-key "p" #'helm-projectile-find-file projectile-command-map)
    (bind-key "r" #'helm-projectile-rg projectile-command-map))
  (add-hook 'projectile-mode-hook #'kd-projectile-mode-hook-func)
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
  (setq projectile-completion-system 'helm)
  (setq projectile-tags-backend 'ggtags)
  (setq projectile-mode-line "P"))

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
    "
    ("e" (find-file "~/.dotfiles/.emacs"))
    ("d" deft)
    ("c" kd/default-captured-org-note)
    ("r" kd/find-org-file)
    ("s" kd/jump-to-src)
    ("t" (find-file "~/Dropbox/nvALT/snippets.org"))
    ("p" (helm-projectile-switch-project t))
    ("f" kd/jump-to-reference)
    ("x" (switch-to-buffer "*scratch*"))
    ("l" (hydra-helm-org-rifle/body))
    ("g" (find-file "~/Dropbox/ledger/ledger.beancount")))

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
  :bind (("s-x" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ("C-." . helm-imenu)
         ("C-x C-f" . helm-find-files)
         ("C-S-s" . helm-occur)
         ("C-S-j" . helm-all-mark-rings)
         (:map isearch-mode-map
               ("M-s o" . helm-occur-from-isearch))
         (:map kd/toggle-map
               ("h" . helm-resume)))
  :init
  (defun kd/jump-to-src (&optional initial-input)
    (interactive)
    (helm :sources (helm-build-sync-source "Jump to repo"
                     :candidates
                     (split-string
                      (shell-command-to-string "ghq list -p") "\n" t)
                     :action (lambda (d) (dired d)))))

  (defun kd/jump-to-reference (&optional initial-input)
    (interactive)
    (helm :sources (helm-build-sync-source "Jump to reference"
                     :candidates
                     '("~/.ghq/git.kernel.org/pub/scm/docs/man-pages/man-pages"
                       "~/Documents/rfc")
                     :action (lambda (d) (dired d)))))

  (defun kd/toggle-http-proxy ()
    (interactive)
    (helm :sources (helm-build-sync-source "Select proxy"
                     :candidates
                     '("127.0.0.1:7890")
                     :action (lambda (x)
                               (if url-proxy-services
                                   (progn
                                     (setq url-proxy-services nil)
                                     (message "Proxy turned off"))
                                 (kd/turn-on-http-proxy x))))))

  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  :config
  (setq helm-idle-delay 0.0)
  (setq helm-input-idle-delay 0.01)
  (setq helm-ff-skip-boring-files t)

  (setq helm-echo-input-in-header-line t)
  (setq helm-display-header-line nil)

  (setq helm-follow-mode-persistent t)
  (require 'helm-config)
  (helm-mode 1))

(use-package flx
  :ensure t)

(use-package helm-flx
  :ensure t
  :after helm
  :config (helm-flx-mode 1))

(use-package helm-fuzzier
  :disabled t
  :ensure t
  :after helm
  :config (helm-fuzzier-mode 1))

(use-package helm-swoop
  :disabled t
  :ensure t
  :after helm
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

(use-package helm-rg
  :ensure t
  :bind ("C-c a" . helm-rg)
  :config (setq helm-rg-default-directory 'git-root))

(use-package helm-xref
  :ensure t
  :commands helm-xref-show-xrefs
  :config (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package helm-gtags
  :ensure t
  :diminish helm-gtags-mode
  :bind (("C-]" . helm-gtags-find-tag)
         ("C-}" . helm-gtags-find-rtag)
         ("C-t" . helm-gtags-pop-stack)
         ("C-S-t" . helm-gtags-show-stack))
  :chords (("gd" . helm-gtags-find-tag)
           ("gr" . helm-gtags-find-rtag))
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


(use-package bookmark+
  :load-path (lambda () (kd/ghq-github-repo-path "emacsmirror/bookmark-plus"))
  :demand t)

(use-package isearch+
  :load-path (lambda () (kd/ghq-github-repo-path "emacsmirror/isearch-plus"))
  :demand t)

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)))

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
  :load-path (lambda () (kd/ghq-github-repo-path "emacsmirror/irfc"))
  :commands irfc-mode
  :mode ("/rfc[0-9]+\\.txt\\'" . irfc-mode)
  :config
  (setq irfc-directory "~/Documents/rfc/rfc")
  (setq irfc-assoc-mode 1))


;;; Org-mode

(use-package org
  :ensure t
  :functions (org-narrow-to-block
              org-end-of-line)
  :commands (org-mode orgtbl-mode)
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

  (defun kd/find-org-file (&optional directory)
    (interactive)
    (let ((default-directory (or directory org-directory)))
      (helm :sources (helm-build-sync-source "Org file"
                       :candidates
                       (split-string
                        (shell-command-to-string "fd -e org") "\n" t)
                       :action (lambda (x)
                                 (find-file
                                  (expand-file-name x default-directory)))))))

  (defun kd/default-captured-org-note ()
    "Move to the end of penultimate line of the last org capture note."
    (interactive)
    (find-file org-default-notes-file)
    (goto-char (point-max))
    (forward-line -2)
    (org-end-of-line))

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

(use-package ox-latex
  :after org
  :init
  (setq org-latex-compiler "xelatex"))

(use-package org-agenda
  :after org
  :bind (:map kd/org-map
              ("a" . org-agenda))
  :init
  (setq org-agenda-files (concat org-directory "/agenda_files.txt")))

(use-package org-capture
  :after org
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

(use-package org-bullets
  :disabled t
  :ensure t
  :after org
  :commands org-bullets-mode)

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
    :bind ("s-r n" . org-annotate-file)
    :after org
    :config
    (setq org-annotate-file-add-search t)
    (setq org-annotate-file-storage-file (concat org-directory "/annotation.org")))

  (use-package org-git-link
    :after org))

(use-package org-annotate
  :load-path (lambda () (kd/ghq-github-repo-path "girzel/org-annotate"))
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

(defvar kd/eshell-map nil)
(kd/make-prefix-command (kbd "s-e") 'kd/eshell-map)
(use-package eshell
  :bind
  (:map kd/eshell-map
        ("e" . eshell)
        ("v" . kd/eshell-new-other-window)
        ("h" . kd/eshell-new-other-window-horizontally)
        ("." . kd/eshell-here))
  :init
  (defun kd/eshell-mode-hook-func ()
    (smartscan-mode -1)
    (add-to-list 'eshell-visual-commands "ssh"))
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
           (eshell-buffer-name (concat "*eshell: " name "*")))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell)
      (rename-buffer eshell-buffer-name)))

  (defun kd/eshell-gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))

  :config
  (setenv "PAGER" "cat")
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)
  (eshell/alias "lm" "ls -lahF")
  (eshell/alias "ff" "find-file $1")
  (eshell/alias "d" "dired $1")
  ;; magit
  (eshell/alias "gst" #'kd/eshell-gst)
  (eshell/alias "gd" #'magit-diff-unstaged)
  (eshell/alias "gds" #'magit-diff-staged)
  ;; grep
  (eshell/alias "hrg" "helm-rg $1"))

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
  :config (define-auto-insert 'sh-mode '(nil "#!/bin/bash\nset -euo pipefail\nIFS=$'\\n\\t'\n")))


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
  :bind (:map goto-address-highlight-keymap
              ("C-c C-o" . goto-address-at-point))
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
                            helm-gtags-mode
                            goto-address-prog-mode
                            abbrev-mode
                            flycheck-mode
                            eshell-mode)))
    (dolist (mode prog-minor-modes)
      (add-hook 'prog-mode-hook mode)))

  (defun kd-prog-mode-hook-func ()
    (setq-local show-trailing-whitespace t)
    (setq-local indicate-empty-lines t))

  (add-hook 'prog-mode-hook #'kd-prog-mode-hook-func))

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

;;;; Flycheck

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :config
  (setq flycheck-check-syntax-automatically '(save))
  (setq flycheck-mode-line-prefix "⚠"))

(use-package flycheck-pos-tip
  :disabled t
  :ensure t
  :commands flycheck-pos-tip-mode
  :init (add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode))

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
              ("'" . imenu-list-smart-toggle)))

;;;; Completion

(use-package company
  :ensure t
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :init (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 2)
  (setq tab-always-indent 'complete)
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
  :load-path (lambda () (kd/ghq-github-repo-path "emacsmirror/csv-mode"))
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile.*\\'" . dockerfile-mode))

(use-package groovy-mode
  :ensure t)

(use-package tex-mode
  :defer t
  :functions LaTeX-narrow-to-environment)

(use-package mmm-mode
  :load-path (lambda () (kd/ghq-github-repo-path "emacsmirror/mmm-mode"))
  :defer t)

(use-package salt-mode
  :load-path (lambda () (kd/ghq-github-repo-path "emacsmirror/salt-mode"))
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
  :init (add-hook 'after-init-hook #'yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package undo-tree
  :load-path (lambda () (kd/ghq-github-repo-path "emacsmirror/undo-tree"))
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

(use-package python-mode
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

    (subword-mode 1))

  (add-hook 'python-mode-hook #'kd/python-mode-hook-function)
  :config (define-auto-insert 'python-mode '(nil "# coding: utf-8\n")))

(use-package jedi-core
  :ensure t
  :commands jedi:setup
  :config
  (setq jedi:use-shortcuts t)
  (setq jedi:tooltip-method nil)
  (setq jedi:complete-on-dot t))

(use-package company-jedi
  :ensure t
  :commands company-jedi)

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

(use-package autodisass-java-bytecode
  :ensure t
  :mode ("\\.class\\'" . ad-javap-mode))

(use-package eclim
  :disabled t
  :ensure t
  :init
  (defun kd/java-mode-hook-func ()
    (setq-local c-basic-offset 4)
    (eclim-mode t)
    (company-emacs-eclim-setup))
  (add-hook 'java-mode-hook #'kd/java-mode-hook-func)
  :config
  (setq eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/plugins/org.eclim_2.8.0/bin/eclim")
  (setq eclimd-default-workspace "/Users/kane/devel/eclipse-workspace")
  (setq eclimd-autostart t))

(use-package company-emacs-eclim
  :disabled t
  :ensure t
  :commands company-emacs-eclim-setup)

(use-package gradle-mode
  :ensure t
  :diminish gradle-mode
  :commands gradle-mode)

(use-package meghanada
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

(use-package queue
  :load-path (lambda () (kd/ghq-github-repo-path "emacsmirror/queue"))
  :defer t)

(use-package cider
  :load-path (lambda () (kd/ghq-github-repo-path "emacsmirror/cider"))
  :commands cider-mode)


;;;; Elisp

(use-package elisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :init (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

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

(use-package multiple-cursors
  :ensure t
  :commands (mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this
             mc/edit-lines))

(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

(use-package deft
  :ensure t
  :commands (deft deft-find-file)
  :config
  (setq deft-default-extension "md")
  (setq deft-org-mode-title-prefix t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-directory (expand-file-name "~/Dropbox/nvALT")))

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
    (setq buffer-face-mode-face '(:family "Verdana" :height 190))
    (buffer-face-mode 1)
    (setq-local fill-column 90)
    (visual-fill-column-mode 1))
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

;;; .emacs ends here
;;; Local Variables:
;;; no-byte-compile: t
;;; eval: (outshine-mode 1)
;;; End:
