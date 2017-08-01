(defconst kd/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun kd/emacs-subdirectory (d)
  (expand-file-name d kd/emacs-directory))
(add-to-list 'load-path (kd/emacs-subdirectory "elisp"))

(defvar kd/toggle-map nil)
(define-prefix-command 'kd/toggle-map)
(define-key ctl-x-map "t" 'kd/toggle-map)


;;; Customization

(setq custom-file (kd/emacs-subdirectory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;;; Package

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; You can turn this on to see when exactly a package get's configured
(setq use-package-verbose t)

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))


;;; Daemon

(use-package server
  :init
  (add-hook 'after-init-hook (lambda ()
                               (unless (server-running-p)
                                 (server-start)))))


;;; General

(use-package better-defaults
  :ensure t)

(setq gc-cons-threshold 50000000)
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)
(setq vc-follow-symlinks t)
(setq-default default-tab-width 4)


;;; Interface

(setq initial-scratch-message "")
(setq visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p)

;; GUI
(when (window-system)
  (use-package default-black-theme)
  (tool-bar-mode 0)
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (set-face-attribute 'default nil :font "-apple-luculent 14-regular-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  ;; mouse
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse 't))

(use-package simple
  :diminish visual-line-mode
  :commands global-visual-line-mode
  :init (add-hook 'after-init-hook #'global-visual-line-mode))

;; Theme
(use-package cyberpunk-theme
  :disabled t
  :ensure t)

(use-package popwin
  :ensure t
  :commands popwin-mode
  :init (add-hook 'after-init-hook #'popwin-mode))


;;; macOS

(when (eq system-type 'darwin)
    (defun kd/osx-lock-screen ()
      (interactive)
      (start-process "lock-screen" nil "/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession" "-suspend"))
    (key-chord-define-global "lk" #'kd/osx-lock-screen)
    (setq mac-option-modifier 'meta)
    (setq mac-command-modifier 'super)
    (when (memq window-system '(mac ns))
      (setq mac-mouse-wheel-smooth-scroll nil)))

(use-package osx-lib
  :if '(eq system-type 'darwin)
  :defer t
  :ensure t)

(use-package osx-dictionary
  :if '(eq system-type 'darwin)
  :ensure t
  :bind ("C-c f" . osx-dictionary-search-word-at-point))


(use-package whitespace
  :commands whitespace-mode
  :bind (:map kd/toggle-map
              ("w" . whitespace-mode))
  :init
  (setq whitespace-line-column nil
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [9166 10])
                                      (tab-mark 9 [9654 9] [92 9])))
  :config
  (set-face-attribute 'whitespace-space       nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-newline     nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-indentation nil :foreground "#666666" :background nil)
  :diminish whitespace-mode)

(use-package fill
  :diminish auto-fill-mode
  :bind (:map kd/toggle-map
              ("f" . auto-fill-mode)))

(use-package visual-fill-column
  :ensure t
  :commands visual-fill-column-mode)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (add-hook 'after-init-hook #'which-key-mode))

(use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode)

;; Window

(use-package winner
  :commands winner-mode
  :init
  (setq winner-dont-bind-my-keys t)
  (add-hook 'after-init-hook #'winner-mode))

(use-package ace-window
  :ensure t
  :bind ("s-o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame)
  (setq aw-ignore-current t)
  :config (global-unset-key (kbd "C-x o")))

(use-package zoom-window
  :ensure t
  :bind (:map kd/toggle-map
              ("z" . zoom-window-zoom)))


;;; Dired

(use-package dired
  :defer t
  :commands dired
  :diminish dired-mode
  :init
  (setq dired-listing-switches "-lahF")
  (setq dired-isearch-filenames t)
  (setq dired-ls-F-marks-symlinks t)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package dired-x
  :commands dired-omit-mode
  :init (add-hook 'dired-mode-hook #'dired-omit-mode))


;;; ido

(use-package ido
  :ensure t
  :commands ido-mode
  :init
  (defun kd/ido-defaults ()
    "ido defaults"
    ;; disable ido faces to see flx highlights.
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    (ido-vertical-mode 1)
    (ido-at-point-mode 1))
  (setq ido-use-faces nil)
  (add-hook 'after-init-hook #'kd/ido-defaults))

(use-package flx-ido
  :ensure t
  :after ido
  :commands flx-ido-mode)

(use-package ido-vertical-mode
  :ensure t
  :after ido
  :commands ido-vertical-mode
  :init (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-at-point
  :ensure t
  :after ido
  :commands ido-at-point-mode)


(use-package find-file-in-project
  :ensure t
  :bind ("C-c o" . find-file-in-project))

(use-package projectile
  :ensure t
  :commands projectile-mode
  :bind ("s-p" . projectile-find-file)
  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-tags-backend 'ggtags)
  (setq projectile-tags-file-name ""))

(use-package avy
  :ensure t
  :chords (("jw" . avy-goto-word-1)
           ("jc" . avy-goto-char-timer)
           ("js" . avy-goto-symbol-1)
           ("jl" . avy-goto-line))
  :init (setq avy-background t))

(use-package rg
  :ensure t
  :bind (("C-c a" . rg-dwim)
         ("C-c C-a" . rg-project)))


;;; Git

(use-package magit
  :ensure t
  :bind (("s-g s" . magit-status)
         ("s-g l" . magit-log-current)
         ("s-g b" . magit-blame))
  :init
  (setq magit-git-executable "/usr/local/bin/git")
  (setq magit-status-expand-stashes nil))

(use-package git-gutter
  :ensure t
  :commands global-git-gutter-mode
  :diminish git-gutter-mode
  :init (add-hook 'after-init-hook #'global-git-gutter-mode))


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package smartscan
  :ensure t
  :commands global-smartscan-mode
  :init
  (setq smartscan-symbol-selector "symbol")
  (add-hook 'after-init-hook #'global-smartscan-mode))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-winner (global-map "C-c w")
    "winner mode"
    ("h" winner-undo "undo")
    ("l" winner-redo "redo"))
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))
  (defhydra hydra-window-size (global-map "C-M-o")
    "window size"
    ("h" shrink-window-horizontally "shrink horizontal")
    ("j" enlarge-window "enlarge vertical")
    ("k" shrink-window "shrink vertical")
    ("l" enlarge-window-horizontally "enlarge horizontal")
    ("=" balance-windows "balance")))

(use-package edit-indirect
  :ensure t
  :bind ("C-c '" . edit-indirect-region))


;;; Ivy, Swiper & Counsel

(use-package ivy
  :ensure t
  :commands ivy-switch-buffer
  :bind (:map ivy-mode-map
              ("s-x" . ivy-switch-buffer)
              ("C-c C-r" . ivy-resume))
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d ")
  (add-hook 'after-init-hook 'ivy-mode)
  :config
  (define-key ivy-mode-map [remap ivy-switch-buffer] nil)
  (global-unset-key (kbd "C-x b")))

(use-package swiper
  :ensure t
  :after ivy
  :commands swiper)

(use-package counsel
  :ensure t
  :after (ivy swiper)
  :bind
  (("C-s" . counsel-grep-or-swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-." . counsel-imenu)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  (add-hook 'after-init-hook #'exec-path-from-shell-initialize))

(use-package saveplace
  :init (setq-default save-place t))

(use-package w3m
  :ensure t
  :commands w3m)

(use-package restclient
  :ensure t
  :commands restclient-mode)

(use-package company-restclient
  :ensure t
  :after company
  :commands company-restclient
  :init
  (add-hook 'restclient-mode-hook (lambda ()
                                    (kd/local-push-company-backend #'company-restclient))))

(use-package outshine
  :ensure t
  :commands outshine-hook-function
  :init
  (setq outshine-use-speed-commands t)
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function))

(use-package outline
  :disabled t
  :commands outline-minor-mode
  :init (add-hook 'prog-mode-hook 'outline-minor-mode))

(use-package irfc
  :ensure t
  :commands irfc-mode
  :init
  (setq irfc-directory "~/Documents/rfc/rfc")
  (setq irfc-assoc-mode t))


;;; Org-mode

(use-package org
  :bind ("C-c l" . org-store-link)
  :init
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file (concat org-directory "/cap.org"))
  (setq org-capture-templates
        '(("c" "cap" entry (file "") "* %?\n  %U")))
  (setq org-src-fontify-natively t)
  (setq org-imenu-depth 3)
  ;; babel
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append)
  ;; avy
  (defun kd/avy-goto-org-heading ()
    (interactive)
    (avy--generic-jump org-heading-regexp nil 'at))
  (add-hook 'org-mode-hook (lambda ()
                             (key-chord-define-local "jh" #'kd/avy-goto-org-heading)))
  :config
  (unbind-key "C-'" org-mode-map)       ; used by `imenu-list'
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ipython . t)
                                 (sh . t)
                                 (emacs-lisp . t))))

(use-package org-download
  :ensure t
  :after org
  :commands org-download-yank)

(use-package ob-ipython
  :ensure t
  :defer t
  :after org)

(use-package ob-async
  :ensure t
  :after org
  :commands ob-async-org-babel-execute-src-block
  :init (add-hook 'org-ctrl-c-ctrl-c-hook #'ob-async-org-babel-execute-src-block))


;;; Integration

(use-package browse-at-remote
  :ensure t
  :commands browse-at-remote
  :config (add-to-list 'browse-at-remote-remote-type-domains '("gitlab.xiaohongshu.com" . "gitlab")))


;;; Tags

(use-package ggtags
  :ensure t
  :commands (ggtags-after-save-function)
  :bind (("M-[" . ggtags-find-definition)
         ("M-]" . ggtags-find-reference))
  :init
  (setq ggtags-mode-sticky nil)
  (setq ggtags-use-sqlite3 t)
  (setq ggtags-sort-by-nearness t)
  (setq ggtags-highlight-tag nil)
  (setq ggtags-enable-navigation-keys nil)
  (add-hook 'after-save-hook #'ggtags-after-save-function nil t))

(use-package counsel-gtags
  :ensure t
  :after ggtags
  :bind (("C-," . counsel-gtags-find-definition)
         ("C-<" . counsel-gtags-go-backward)))

(use-package imenu-list
  :ensure t
  :bind ("C-'" . imenu-list-smart-toggle))


;;; Completion

(use-package company
  :ensure t
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :init
  (setq company-minimum-prefix-length 2)
  (setq tab-always-indent 'complete)
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (defun kd/local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (set (make-local-variable 'company-backends)
         (add-to-list 'company-backends backend))))

(use-package company-flx
  :ensure t
  :after company
  :commands company-flx-mode
  :init (add-hook 'company-mode-hook #'company-flx-mode))

(use-package company-quickhelp
  :ensure t
  :after company
  :bind (:map company-active-map
              ("M-d" . company-quickhelp-manual-begin))
  :init
  (setq company-quickhelp-delay nil)
  (add-hook 'company-mode-hook #'company-quickhelp-mode))


(use-package paredit
  :ensure t
  :commands paredit-mode
  :diminish paredit-mode
  :init (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :commands smartparens-global-mode
  :init
  (add-hook 'after-init-hook #'smartparens-global-mode)
  (add-hook 'python-mode-hook (lambda () (require 'smartparens-python))))

(use-package csv-mode
  :ensure t
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile.*\\'" . dockerfile-mode))

(use-package salt-mode
  :ensure t
  :mode ("\\.sls\\'" . salt-mode))

(use-package apib-mode
  :ensure t
  :commands apib-mode
  :mode ("\\.apib\\'" . apib-mode))

(use-package ansible
  :ensure t
  :commands ansible)

(use-package company-ansible
  :ensure t
  :after (company ansible)
  :commands company-ansible
  :init
  (add-hook 'ansible::hook (lambda ()
                             (kd/local-push-company-backend #'company-ansible))))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :commands (markdown-mode gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package conf-mode
  :mode ("rc$" . conf-mode))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode))

(use-package thrift-mode
  :mode ("\\.thrift\\'" . thrift-mode))

(use-package yasnippet
  :ensure t
  :commands yas-global-mode
  :diminish yas-minor-mode
  :init (add-hook 'after-init-hook #'yas-global-mode))

(use-package undo-tree
  :ensure t
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
  :ensure t
  :commands flycheck-mode
  :diminish flycheck-mode
  :init
  (setq flycheck-check-syntax-automatically '(save))
  (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package which-func
  :commands which-function-mode
  :init (add-hook 'prog-mode-hook #'which-function-mode))

(use-package realgud
  :ensure t
  :commands (realgud:trepan2))


;;; Python

(use-package python
  :commands python-mode
  :init
  (defun kd/python-mode-defaults ()
    (defun kd/avy-goto-py-declaration ()
      (interactive)
      (avy--generic-jump python-nav-beginning-of-defun-regexp nil 'pre))
    (key-chord-define-local "jd" #'kd/avy-goto-py-declaration))
  (add-hook 'python-mode-hook #'kd/python-mode-defaults))

(use-package subword
  :diminish subword-mode
  :commands subword-mode
  :init (add-hook 'python-mode-hook #'subword-mode))

(use-package pyenv-mode
  :ensure t
  :commands pyenv-mode
  :init (add-hook 'python-mode-hook #'pyenv-mode))

(use-package pyenv-mode-auto
  :ensure t
  :commands pyenv-mode-auto-hook
  :after pyenv-mode)

(use-package jedi-core
  :ensure t
  :commands jedi:setup
  :init
  (setq jedi:use-shortcuts t)
  (setq jedi:tooltip-method nil)
  (setq jedi:complete-on-dot t)
  (add-hook 'python-mode-hook #'jedi:setup))

(use-package company-jedi
  :ensure t
  :after (company jedi-core)
  :commands company-jedi
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (kd/local-push-company-backend #'company-jedi))))


;;; Golang

(use-package go-mode
  :ensure t
  :commands go-mode
  :bind
  (:map go-mode-map
        ("M-." . godef-jump)
        ("M-C-." . godef-jump-other-window)
        ("M-k" . godoc-at-point))
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  (setq-local flycheck-disabled-checkers '(go-golint)))

(use-package go-eldoc
  :ensure t
  :after (go-mode eldoc)
  :commands go-eldoc-setup
  :init (add-hook 'go-mode-hook #'go-eldoc-setup))

(use-package company-go
  :ensure t
  :after company
  :commands company-go
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (kd/local-push-company-backend #'company-go))))

(use-package go-guru
  :ensure t
  :commands go-guru-hl-identifier-mode
  :init (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))


;;; Elisp

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :commands turn-on-elisp-slime-nav-mode
  :init (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode))


(use-package tldr
  :ensure t
  :commands tldr)

(use-package multi-term
  :ensure t
  :commands multi-term)

(use-package multiple-cursors
  :ensure t
  :commands mc/edit-lines)

(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))


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
