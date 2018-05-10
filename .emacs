;;; .emacs --- Emacs init file

;;; Commentary:
;;;   Emacs init file

;;; Code:

(defun kd/emacs-subdirectory (d)
  "Make dir path inside Emacs user dir for D."
  (expand-file-name d user-emacs-directory))

(add-to-list 'load-path (kd/emacs-subdirectory "elisp"))

;;; Package

;; straight.el bootstrap
(require 'package)
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'use-package)
;; You can turn this on to see when exactly a package get's configured
(setq use-package-verbose t
      use-package-expand-minimally t)

(use-package use-package-chords
    :config (key-chord-mode 1))

;;;; auto-compile
(use-package auto-compile
  :disabled t
  :init
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; Settings
(use-package kd-settings
  :straight nil
  :init
  (use-package better-defaults)

  (fset 'yes-or-no-p 'y-or-n-p)

  (setq-default
   default-tab-width 4
   tab-width 4
   indent-tabs-mode nil)

  (setq
   confirm-kill-emacs 'y-or-n-p
   load-prefer-newer t
   gc-cons-threshold 50000000
   scroll-conservatively 10000
   scroll-preserve-screen-position t
   vc-follow-symlinks t)

;;;; customization
  (setq custom-file (kd/emacs-subdirectory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

;;;; ui
  (unless noninteractive
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    (setq
     inhibit-startup-message t
     inhibit-startup-echo-area-message user-login-name
     inhibit-default-init t
     initial-major-mode 'fundamental-mode
     initial-scratch-message nil
     mode-line-format nil))

  (setq visible-bell t)

  (provide 'kd-settings))

;;; Keybinding
(use-package bind-key
  :init
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
  (kd/make-prefix-command (kbd "s-c") 'kd/compile-map))

;;; Startup
;;;; daemon
(use-package server
  :init
  (add-hook 'after-init-hook (lambda ()
                               (unless (server-running-p)
                                 (server-start)))))

(use-package desktop
  :commands desktop-save-mode
  :init (add-hook 'after-init-hook #'desktop-save-mode)
  :config (setq desktop-dirname user-emacs-directory))


;;; Interface

;; GUI
(when (window-system)
  (use-package default-black-theme
    :disabled t
    :config (load-theme 'default-black t))

  (use-package cyberpunk-theme
        :config (load-theme 'cyberpunk t))

  (use-package zenburn-theme
    :disabled t
        :config (load-theme 'zenburn t))

  ;; ui
  (tool-bar-mode 0)
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)

  ;; font
  (custom-set-faces '(default ((t (:font "-apple-fira mono-regular-*-*-*-16-*-*-*-m-*-iso10646-1")))))

  ;; mouse
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse 't)

  ;; maximize
  (add-hook 'after-init-hook #'toggle-frame-maximized))

(use-package simple
  :straight nil
  :diminish visual-line-mode
  :commands global-visual-line-mode
  :init (add-hook 'after-init-hook #'global-visual-line-mode))


;;; macOS
(use-package kd-macOS
  :straight nil
  :defines IS-MAC
  :functions kd/lock-screen
  :init
  (defconst IS-MAC (eq system-type 'darwin))
  (provide 'kd-macOS)
  :config
  (defun kd/lock-screen ()
    (interactive)
    (cond (IS-MAC
           (start-process "lock-screen" nil "/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession" "-suspend"))))
  (key-chord-define-global "lk" #'kd/lock-screen)

  (when IS-MAC
    (setq mac-option-modifier 'meta)
    (setq mac-command-modifier 'super)
    (when (memq window-system '(mac ns))
      (when (fboundp 'mac-set-frame-tab-group-property)
        (mac-set-frame-tab-group-property nil :tab-bar-visible-p nil))
      (setq mac-mouse-wheel-smooth-scroll nil)))

  (use-package osx-lib
    :if IS-MAC)

  (use-package osx-dictionary
    :if IS-MAC
        :bind ("C-c f" . osx-dictionary-search-word-at-point)))


(use-package whitespace
  :straight nil
  :diminish whitespace-mode
  :commands whitespace-mode
  :bind (:map kd/toggle-map
              ("w" . whitespace-mode))
  :config
  (setq whitespace-line-column nil
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [9166 10])
                                      (tab-mark 9 [9654 9] [92 9])))
  (set-face-attribute 'whitespace-space       nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-newline     nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-indentation nil :foreground "#666666" :background nil))

(use-package fill
  :straight nil
  :diminish auto-fill-mode
  :bind (:map kd/toggle-map
              ("f" . auto-fill-mode)))

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :bind (:map kd/toggle-map
              ("v" . visual-fill-column-mode)))

(use-package which-key
  :straight nil
  :diminish which-key-mode
  :init (add-hook 'after-init-hook #'which-key-mode))

(use-package wgrep
  :straight nil
  :commands wgrep-change-to-wgrep-mode)

(use-package autoinsert
  :straight nil
  :commands auto-insert-mode
  :init (add-hook 'after-init-hook #'auto-insert-mode)
  :config
  (define-auto-insert 'python-mode '(nil
                                     "# coding: utf-8\n")))

(use-package highlight-symbol
  :commands (highlight-symbol-mode)
  :diminish highlight-symbol-mode
  :init (add-hook 'prog-mode-hook 'highlight-symbol-mode))

;; Window

(use-package windmove
  :straight nil
  :bind (("C-s-h" . windmove-left)
         ("C-s-j" . windmove-down)
         ("C-s-k" . windmove-up)
         ("C-s-l" . windmove-right)))

(use-package winner
  :straight nil
  :commands winner-mode
  :init
  (setq winner-dont-bind-my-keys t)
  (add-hook 'after-init-hook #'winner-mode))

(use-package ace-window
  :bind ("s-j" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame)
  (setq aw-ignore-current t)
  (global-unset-key (kbd "C-x o")))

(use-package zygospore
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

(use-package eyebrowse
  :bind (("C-c C--" . eyebrowse-next-window-config)
         ("C-c C-=" . eyebrowse-prev-window-config)
         ("s-w" . eyebrowse-close-window-config)
         ("s-'" . eyebrowse-last-window-config)
         ("s-1" . eyebrowse-switch-to-window-config-1)
         ("s-2" . eyebrowse-switch-to-window-config-2)
         ("s-3" . eyebrowse-switch-to-window-config-3)
         ("s-4" . eyebrowse-switch-to-window-config-4)
         ("s-5" . eyebrowse-switch-to-window-config-5))
  :init
  (add-hook 'after-init-hook #'eyebrowse-mode)
  :config
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-mode-line-style 'always
        eyebrowse-new-workspace t
        eyebrowse-wrap-around t))

;;; Dired

(use-package dired
  :straight nil
  :defer t
  :commands dired
  :diminish dired-mode
  :defines dired-isearch-filenames
  :init
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  :config
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-lahF")
  (setq dired-isearch-filenames t)
  (setq dired-ls-F-marks-symlinks t))

(use-package dired-x
  :straight nil
  :commands dired-omit-mode
  :init (add-hook 'dired-mode-hook #'dired-omit-mode))


;;; neotree

(use-package neotree
  :bind (:map kd/pop-map
              ("n" . neotree-toggle)
              ("N" . neotree-find))
  :commands (neotree neotree-toggle neotree-find)
  :config
  (setq neo-window-width 35)
  (setq neo-confirm-change-root 'off-p))


;;; ido

(use-package ido
  :commands ido-mode
  :init
  (defun kd/ido-after-init-hook-function ()
    ;; disable ido faces to see flx highlights.
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    (ido-vertical-mode 1)
    (ido-at-point-mode 1))
  (add-hook 'after-init-hook #'kd/ido-after-init-hook-function)
  :config
  (setq ido-use-faces nil))

(use-package flx-ido
  :after ido
  :commands flx-ido-mode)

(use-package ido-vertical-mode
  :after ido
  :commands ido-vertical-mode
  :config (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-at-point
  :after ido
  :commands ido-at-point-mode)


(use-package find-file-in-project
  :bind ("C-c o" . find-file-in-project))

(use-package projectile
  :diminish projectile-mode
  :commands projectile-mode
  :bind ("s-p" . projectile-find-file)
  :init
  (add-hook 'after-init-hook #'projectile-mode)
  (add-hook 'projectile-mode-hook (lambda ()
                                    (remove-hook 'find-file-hook #'projectile-find-file-hook-function)))
  :config
  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-generic-command "fd --type f --print0")
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-tags-backend 'ggtags)
  (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))))

(use-package counsel-projectile
  :bind (:map projectile-command-map
              ("p" . counsel-projectile-switch-project)
              ("r" . counsel-projectile-rg)))

(use-package avy
  :bind (:map kd/org-map
              ("w" . avy-org-refile-as-child))
  :chords (("jw" . avy-goto-word-1)
           ("jc" . avy-goto-char-timer)
           ("jl" . avy-goto-line))
  :config (setq avy-background t))

(use-package rg
  :bind (("C-c a" . rg-dwim)
         ("C-c C-a" . rg-project)))


;;; Git

(use-package magit
  :bind (("s-g s" . magit-status)
         ("s-g l" . magit-log-current)
         ("s-g b" . magit-blame))
  :defines magit-status-expand-stashes
  :config
  (setq magit-git-executable "/usr/local/bin/git")
  (setq magit-status-expand-stashes nil))

(use-package diff-hl
  :commands turn-on-diff-hl-mode
  :init (add-hook 'prog-mode-hook #'turn-on-diff-hl-mode))


(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package smartscan
  :commands global-smartscan-mode
  :init (add-hook 'after-init-hook #'global-smartscan-mode)
  :config (setq smartscan-symbol-selector "symbol"))

(use-package hydra
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
         _D_eft
         _c_ap.org: default note
         o_r_g files
         _p_rojectile
         _s_rc
         code snippe_t_s
         re_f_erence
    "
    ("e" (find-file "~/.dotfiles/.emacs"))
    ("D" deft)
    ("c" kd/default-captured-org-note)
    ("r" kd/find-org-file)
    ("s" kd/jump-to-src)
    ("t" (find-file "~/Dropbox/nvALT/snippets.org"))
    ("p" (projectile-switch-project t))
    ("f" kd/jump-to-reference))

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
    :bind ("C-c '" . edit-indirect-region))


;;; Ivy, Swiper & Counsel

(use-package flx)

(use-package ivy
  :commands (ivy-switch-buffer ivy-read)
  :bind (:map ivy-mode-map
              ("s-x" . ivy-switch-buffer)
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
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d ")
  (define-key ivy-mode-map [remap ivy-switch-buffer] nil)
  (global-unset-key (kbd "C-x b")))

(use-package swiper
  :after ivy
  :commands swiper)

;; cache for M-x
(use-package smex
  :defer t)

(use-package counsel
  :bind
  (("C-S-s" . counsel-grep-or-swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-." . counsel-imenu)))

(use-package bookmark+
  :defer t)

(use-package isearch+
  :disabled t)

(use-package dired+
  :defer t)

(use-package info
  :straight nil
  :commands info)

(use-package info+
  :after info)

(use-package help+
  :defer t)

(use-package help-fns+
  :defer t)

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :commands (exec-path-from-shell-initialize)
    :init (add-hook 'after-init-hook #'exec-path-from-shell-initialize)
  :config (setq exec-path-from-shell-check-startup-files nil))

(use-package saveplace
  :straight nil
  :commands save-place-mode
  :init (add-hook 'after-init-hook #'save-place-mode))

(use-package restclient
  :commands restclient-mode)

(use-package company-restclient
  :after company
  :commands company-restclient
  :init
  (add-hook 'restclient-mode-hook (lambda ()
                                    (kd/local-push-company-backend #'company-restclient))))

(use-package outshine
  :after outline
  :commands outshine-hook-function
  :init (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  :config (setq outshine-use-speed-commands t))

(use-package outline
  :straight nil
  :commands outline-minor-mode
  :init (add-hook 'prog-mode-hook 'outline-minor-mode))

(use-package irfc
  :commands irfc-mode
  :mode ("/rfc[0-9]+\\.txt\\'" . irfc-mode)
  :init
  (setq irfc-assoc-mode 1)
  :config
  (setq irfc-directory "~/Documents/rfc/rfc"))


;;; Org-mode

(use-package org
  :straight (:host github :repo "emacsmirror/org" :branch "release_9.1.12")
  :commands (orgtbl-mode)
  :defines (org-directory
            org-capture-templates
            org-imenu-depth
            org-default-notes-file)
  :bind (("C-M-<return>" . org-insert-subheading)
         (:map kd/org-map
               ("l" . org-store-link)
               ("a" . org-agenda)
               ("c" . org-capture)))
  :custom-face
  (org-document-title ((t (:height 1.1 :wight semi-bold))))
  :init
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file (concat org-directory "/cap.org"))

  (defun kd/find-org-file (&optional directory)
    (interactive)
    (let ((default-directory (or directory org-directory)))
      (ivy-read "org file: "
                (split-string (shell-command-to-string "fd -e org") "\n" t)
                :action (lambda (x)
                          (with-ivy-window
                            (find-file (expand-file-name x ivy--directory))))
                :require-match 'confirm-after-completion
                :history 'file-name-history
                :caller 'kd/find-org-file)))

  (defun kd/default-captured-org-note ()
    "Move to the end of penultimate line of the last org capture note."
    (interactive)
    (find-file org-default-notes-file)
    (goto-char (point-max))
    (forward-line -2)
    (org-end-of-line))

  (add-hook 'org-mode-hook #'org-bullets-mode)
  :config
  ;; face
  (dolist (face org-level-faces)
    (custom-set-faces `(,face ((t (:height 1.0 :weight semi-bold))))))

  (unbind-key "C-'" org-mode-map)       ; used by `imenu-list'
  (setq org-capture-templates
        '(("n" "note" entry (file+datetree "") "* %?\n  %U\n  %i")))
  (setq org-src-fontify-natively t)
  (setq org-imenu-depth 3)

  ;; link
  (org-link-set-parameters "devonthink"
                           :follow
                           (lambda (id)
                             (osx-open-url-at-point (concat "x-devonthink-item://" id))))
  (org-link-set-parameters "gmail"
                           :follow
                           (lambda (id)
                             (browse-url
                              ;; or "https://mail.google.com/mail/h/?&v=c&s=l&th="
                              ;; for html-browser
                              (concat "https://mail.google.com/mail/?shva=1#all/" id))))

  ;; babel
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ipython . t)
                                 (shell . t)
                                 (emacs-lisp . t)))
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append))

(use-package ox-reveal
  :config
  (setq org-reveal-root "file:///Users/kane/src/reveal.js"))

(use-package org-noter
  :config
  (setq org-noter-auto-save-last-location t))

(use-package org-bullets
  :commands org-bullets-mode)

(use-package org-mac-link
  :if (eq system-type 'darwin)
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
  :after org
  :bind (:map kd/org-map
              ("d" . org-download-yank)))

(use-package ob-ipython
  :after org)

(use-package ob-async
  :after org)


;;; Term

(use-package xterm-color
  :commands xterm-color-filter
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))

(use-package shell
  :straight nil
  :commands shell
  :init
  (defun kd/shell-mode-hook-func ()
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)
    (make-local-variable 'process-environment)
    (setenv "TERM" "xterm-256color"))
  ;; (add-hook 'shell-mode-hook #'kd/shell-mode-hook-func))

(use-package eterm-256color
  :commands eterm-256color-mode)

(use-package term
  :straight nil
  :commands (term ansi-term)
  :init
  (defun kd/term-mode-hook-func ()
    (make-local-variable 'process-environment)
    (setenv "TERM" "eterm-256color")
    (eterm-256color-mode 1))
  ;; (add-hook 'term-mode-hook #'kd/term-mode-hook-func))

(use-package multi-term
  :commands (multi-term multi-term-dedicated-open multi-term-dedicated-toggle)
  :config (setq multi-term-program "/usr/local/bin/zsh"))


;;; Integration

(use-package browse-url
  :straight nil
  :bind (:map kd/pop-map
              ("u" . browse-url-at-point)))

(use-package browse-at-remote
  :bind (:map kd/pop-map
              ("r" . browse-at-remote))
  :config (dolist (elt '(("gitlab.xiaohongshu.com" . "gitlab")
                         ("code.devops.xiaohongshu.com" . "gitlab")))
            (add-to-list 'browse-at-remote-remote-type-domains elt)))

(use-package ispell
  :straight nil
  :defer t
  :config (setq ispell-program-name "aspell"))

(use-package flyspell
  :straight nil
  :commands flyspell-mode
  :init (add-hook 'org-mode-hook #'flyspell-mode)
  :config (define-key flyspell-mode-map [remap flyspell-auto-correct-word] nil))

(use-package graphql-mode
  :commands graphql-mode)

(use-package plantuml-mode
  :commands plantuml-mode
  :config (setq plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar"))

(use-package flycheck-plantuml
  :commands flycheck-plantuml-setup
  :init (add-hook 'plantuml-mode-hook #'flycheck-plantuml-setup))


;;; Tags

(use-package ggtags
  :diminish ggtags-mode
  :commands (ggtags-mode ggtags-create-tags ggtags-update-tags)
  :bind (("M-[" . ggtags-find-definition)
         ("M-]" . ggtags-find-reference))
  :init
  (add-hook 'prog-mode-hook #'ggtags-mode)
  (add-hook 'ggtags-global-mode-hook (lambda ()
                                       (local-set-key (kbd "n") #'next-error-no-select)
                                       (local-set-key (kbd "p") #'previous-error-no-select)))
  :config
  (setq ggtags-mode-sticky nil)
  (setq ggtags-use-sqlite3 t)
  (setq ggtags-sort-by-nearness nil)
  (setq ggtags-highlight-tag nil)
  (setq ggtags-enable-navigation-keys nil))

(use-package imenu-list
  :bind (:map kd/pop-map
              ("'" . imenu-list-smart-toggle)))


;;; Completion

(use-package company
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :init (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-minimum-prefix-length 2)
  (setq tab-always-indent 'complete)
  (defun kd/local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (setq-local company-backends (add-to-list 'company-backends backend))))

(use-package company-flx
  :after company
  :commands company-flx-mode
  :init (add-hook 'company-mode-hook #'company-flx-mode))

(use-package company-quickhelp
  :after company
  :bind (:map company-active-map
              ("M-d" . company-quickhelp-manual-begin))
  :init (add-hook 'company-mode-hook #'company-quickhelp-mode)
  :config (setq company-quickhelp-delay nil))


(use-package smartparens
  :diminish smartparens-mode
  :commands (smartparens-global-strict-mode show-smartparens-global-mode)
  :bind (:map smartparens-mode-map
              ("C-s-]" . sp-unwrap-sexp)
              ("C-)" . sp-slurp-hybrid-sexp))
  :init (add-hook 'after-init-hook #'smartparens-global-strict-mode)
  :config (use-package smartparens-config))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode))

(use-package json-mode
  :defer t)

(use-package swift-mode
  :defer t)

(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(use-package dockerfile-mode
    :mode ("Dockerfile.*\\'" . dockerfile-mode))

(use-package salt-mode
    :mode ("\\.sls\\'" . salt-mode))

(use-package apib-mode
    :commands apib-mode
  :mode ("\\.apib\\'" . apib-mode))

(use-package ansible
    :commands (ansible ansible-doc)
  :mode (("\\(playbook\\|site\\|main\\|local\\)\\.ya?ml\\'" . ansible)
         ("/\\(tasks\\|roles\\|handlers\\)/.*\\.ya?ml\\'" . ansible)
         ("/\\(group\\|host\\)_vars/". ansible)))

(use-package company-ansible
    :commands company-ansible
  :init
  (add-hook 'ansible::hook (lambda ()
                             (kd/local-push-company-backend #'company-ansible))))

(use-package markdown-mode
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
    (setq buffer-face-mode-face '(:family "Verdana" :height 180))
    (buffer-face-mode 1))

  (add-hook 'markdown-mode-hook #'kd/markdown-mode-hook-func)
  (add-hook 'markdown-view-mode-hook #'kd/markdown-view-mode-hook-func)
  :config (setq markdown-command "multimarkdown"))

(use-package conf-mode
  :mode ("rc$" . conf-mode))

(use-package lua-mode
    :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode))

(use-package thrift-mode
  :mode ("\\.thrift\\'" . thrift-mode))

(use-package yasnippet
    :commands (yas-global-mode yas-expand)
  :diminish yas-minor-mode
  :init (add-hook 'after-init-hook #'yas-global-mode))

(use-package yasnippet-snippets
    :after yasnippet)

(use-package undo-tree
    :commands (global-undo-tree-mode undo-tree-mode)
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook #'global-undo-tree-mode))

(use-package abbrev
  :diminish abbrev-mode
  :commands abbrev-mode
  :init (add-hook 'prog-mode-hook #'abbrev-mode))

(use-package autorevert
  :diminish auto-revert-mode
  :commands (global-auto-revert-mode turn-on-auto-revert-mode)
  :init (add-hook 'after-init-hook #'global-auto-revert-mode))

(use-package flycheck
    :commands flycheck-mode
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  :config (setq flycheck-check-syntax-automatically '(save)))

(use-package realgud
    :commands (realgud:trepan2))

(use-package makefile-executor
    :bind (:map kd/compile-map
              ("t" . makefile-executor-execute-target)
              ("p" . makefile-executor-execute-project-target)
              ("c" . makefile-executor-execute-last))
  :commands (makefile-executor-mode))

(use-package pdf-tools
    :mode ("\\.pdf\\'" . pdf-view-mode)
  :commands pdf-view-mode
  :config
  (pdf-tools-install))


;;; C/C++

(use-package company-c-headers
    :commands company-c-headers)

(use-package google-c-style
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
    :after semantic)

(use-package rtags
    :commands rtags-start-process-unless-running
  :init
  (setq rtags-completions-enabled t)
  (setq rtags-autostart-diagnostics t))

(use-package cc-mode
  :commands (c-mode c++-mode)
  :init
  (defun kd/c-mode-common-hook-func ()
    ;; style
    (google-set-c-style)
    (google-make-newline-indent))

  (add-hook 'c-mode-common-hook #'kd/c-mode-common-hook-func)

  (defun kd/c-mode-hook-func ()
    ;; rtags
    (rtags-start-process-unless-running)

    ;; irony
    (irony-mode 1)

    ;; company
    (setq company-backends (delete 'company-semantic company-backends))
    (kd/local-push-company-backend #'company-c-headers)
    )

  (add-hook 'c-mode-hook #'kd/c-mode-hook-func)
  (add-hook 'c++-mode-hook #'kd/c-mode-hook-func)
  :config
  (setq c-basic-offset 4)
  (setq c-auto-newline nil))

;; Irony

(use-package irony
    :commands irony-mode
  :init
  (defun kd/irony-mode-hook-func ()
    (irony-cdb-autosetup-compile-options)
    (irony-eldoc)

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
    :commands irony-eldoc)

(use-package company-irony
  :after irony
    :commands (company-irony company-irony-setup-begin-commands)
  :config (setq company-irony-ignore-case 'smart))

(use-package flycheck-irony
  :after irony
    :commands flycheck-irony-setup)


;;; Python

(use-package python
  :commands python-mode
  :init
  (defun kd/python-mode-hook-function ()
    ; jedi
    (jedi:setup)

    ; imenu
    (setq imenu-create-index-function #'python-imenu-create-index)

    ; trim whitespace
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

  (add-hook 'python-mode-hook #'kd/python-mode-hook-function))

(use-package subword
  :diminish subword-mode
  :commands subword-mode
  :init (add-hook 'python-mode-hook #'subword-mode))

(use-package pyenv-mode
    :commands pyenv-mode
  :init (add-hook 'python-mode-hook #'pyenv-mode))

(use-package pyenv-mode-auto
    :commands pyenv-mode-auto-hook)

(use-package jedi-core
    :commands jedi:setup
  :config
  (setq jedi:use-shortcuts t)
  (setq jedi:tooltip-method nil)
  (setq jedi:complete-on-dot t))

(use-package company-jedi
    :commands company-jedi
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (kd/local-push-company-backend #'company-jedi))))

(use-package ein
    :defer t)


;;; Golang

(use-package go-mode
    :commands go-mode
  :bind
  (:map go-mode-map
        ("M-." . godef-jump)
        ("M-C-." . godef-jump-other-window)
        ("M-k" . godoc-at-point))
  :init
  (defun kd/go-mode-hook-func ()
    (flycheck-gometalinter-setup)
    (setq-local flycheck-checker 'go-build)
    (add-hook 'before-save-hook #'gofmt-before-save nil t)
    (when (bound-and-true-p ggtags-mode)
      (ggtags-mode -1))
    (go-eldoc-setup))
  (add-hook 'go-mode-hook #'kd/go-mode-hook-func)
  :config
  (setq gofmt-command "goimports"))

(use-package flycheck-gometalinter
    :commands (flycheck-gometalinter-setup)
  :after go-mode
  :config
  (setq flycheck-gometalinter-vendor t)
  (setq flycheck-gometalinter-errors-only t)
  (setq flycheck-gometalinter-fast t))

(use-package go-eldoc
    :after go-mode
  :commands go-eldoc-setup)

(use-package company-go
    :commands company-go
  :init
  (add-hook 'go-mode-hook (lambda ()
                            (kd/local-push-company-backend #'company-go))))

(use-package go-guru
    :commands go-guru-hl-identifier-mode
  :init (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(use-package gotest
  :after go-mode)

(use-package go-playground
    :bind (:map go-playground-mode-map
              ("C-c C-c" . go-playground-exec)
              ("C-c C-k" . go-playground-rm))
  :commands go-playground)


;;; Java

(use-package autodisass-java-bytecode
    :mode ("\\.class\\'" . ad-javap-mode))

(use-package meghanada
    :commands meghanada-mode
  :init
  (defun kd/java-mode-hook-func ()
    (meghanada-mode t)
    (setq c-basic-offset 4)
    (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))
  (add-hook 'java-mode-hook #'kd/java-mode-hook-func)
  :config
  (setq meghanada-java-path "java")
  (setq meghanada-maven-path "mvn"))


;;; Elisp

(use-package elisp-slime-nav
    :diminish elisp-slime-nav-mode
  :commands turn-on-elisp-slime-nav-mode
  :init (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode))


(use-package tldr
    :commands tldr)

(use-package multiple-cursors
    :commands (mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this
             mc/edit-lines))

(use-package iedit
    :bind ("C-;" . iedit-mode))

(use-package deft
    :commands (deft deft-find-file)
  :config
  (setq deft-org-mode-title-prefix t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-directory (expand-file-name "~/Dropbox/nvALT")))

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

(define-key kd/toggle-map "n" #'narrow-or-widen-dwim)

;;; .emacs ends here
