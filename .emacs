(defconst kd/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun kd/emacs-subdirectory (d) (expand-file-name d kd/emacs-directory))

(define-prefix-command 'kd/toggle-map)
(define-key ctl-x-map "t" 'kd/toggle-map)

;;; customization file
(setq custom-file (kd/emacs-subdirectory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; package initialize
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;; general
(use-package better-defaults
  :ensure t)

(setq scroll-conservatively 1)
(setq vc-follow-symlinks t)

;;; tab
(setq-default default-tab-width 4)

;;; display
(setq initial-scratch-message "")
(setq visible-bell t)

(when (window-system)
  (load-theme 'default-black t)
  (tool-bar-mode 0)
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (set-face-attribute 'default nil :font "-apple-hack-regular-normal-normal-*-16-*-*-*-m-0-iso10646-1"))

(when (eq system-type 'darwin)
    (defun kd/osx-lock-screen ()
      (interactive)
      (start-process "lock-screen" nil "/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession" "-suspend"))
    (define-key kd/toggle-map "l" #'kd/osx-lock-screen)
    (setq mac-option-modifier 'meta))

; show whitespace
(use-package whitespace
  :commands whitespace-mode
  :init
  (define-key kd/toggle-map "w" 'whitespace-mode)
  (setq whitespace-line-column nil
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [9166 10])
                                      (tab-mark 9 [9654 9] [92 9])))
  :config
  (set-face-attribute 'whitespace-space       nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-newline     nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-indentation nil :foreground "#666666" :background nil)
  :diminish whitespace-mode)

; auto wrap
(use-package fill
  :commands auto-fill-mode
  :init
  (define-key kd/toggle-map "f" 'auto-fill-mode)
  :diminish auto-fill-mode)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c a" . counsel-ag)))

(use-package wgrep
  :ensure t)

(use-package winner
  :commands winner-mode
  :init
  (add-hook 'after-init-hook #'winner-mode))

;;; dired
(use-package dired
  :init
  (setq dired-listing-switches "-lahF")
  (setq dired-isearch-filenames t)
  (setq dired-ls-F-marks-symlinks t))

(use-package dired-x
  :commands dired-omit-mode
  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode))

;;; ido
(use-package ido
  :ensure t
  :commands ido-mode
  :init
  (add-hook 'after-init-hook (lambda ()
                               (ido-mode 1)
                               (ido-everywhere 1)
                               (flx-ido-mode 1)
                               (ido-vertical-mode 1)
                               (ido-at-point-mode 1)))
  :config
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package flx-ido
  :ensure t
  :commands flx-ido-mode)

(use-package ido-vertical-mode
  :ensure t
  :commands ido-vertical-mode
  :config
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-at-point
  :ensure t
  :commands ido-at-point-mode)

(use-package find-file-in-project
  :ensure t
  :bind ("C-c o" . find-file-in-project)
  :config
  (setq ffip-prefer-ido-mode t))

(use-package speedbar
  :defer t
  :init
  (setq speedbar-tag-hierarchy-method nil))

(use-package sr-speedbar
  :ensure t
  :commands sr-speedbar-toggle
  :init
  (define-key kd/toggle-map "s" #'sr-speedbar-toggle))

(use-package avy
  :ensure t
  :bind
  (("C-c SPC" . avy-goto-char)
   ("C-c l" . avy-goto-line)))

(use-package ag
  :ensure t
  :bind ("C-c C-a" . ag-project-regexp)
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

(use-package magit
  :ensure t
  :init
  (setq magit-git-executable "/usr/local/bin/git")
  :bind
  (("C-c g s" . magit-status)
   ("C-c g l" . magit-log-current)))

(use-package mo-git-blame
  :ensure t
  :bind ("C-c g b" . mo-git-blame-current))

(use-package git-gutter
  :ensure t
  :commands global-git-gutter-mode
  :diminish git-gutter-mode
  :init
  (add-hook 'after-init-hook #'global-git-gutter-mode))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package smartscan
  :ensure t
  :bind
  (("M-n" . smartscan-symbol-go-forward)
   ("M-p" . smartscan-symbol-go-backward)))

(use-package ivy
  :ensure t
  :commands ivy-switch-buffer
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (add-hook 'after-init-hook 'ivy-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package saveplace
  :init
  (setq-default save-place t))

(use-package osx-lib
  :if '(eq system-type 'darwin)
  :ensure t)

(use-package restclient
  :ensure t
  :commands restclient-mode)

(use-package company-restclient
  :ensure t
  :commands company-restclient
  :init
  (add-hook 'restclient-mode-hook (lambda ()
                                    (company-mode 1)
                                    (kd/local-push-company-backend 'company-restclient))))

;;; org-mode
(use-package org
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-iswitchb))
  :init
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file (concat org-directory "/cap.org"))
  (setq org-capture-templates
        '(("c" "cap" entry (file "") "* %?\n  %U")))
  (setq org-src-fontify-natively t))

;;; tags
(use-package etags
  :bind ("C-c ." . kd/ivy-find-tag)
  :config
  (defun kd/ivy-find-tag ()
    "find a tag using ivy"
    (interactive)
    (tags-completion-table)
    (let ((ivy-sort-functions-alist)
          (tag-names))
      (mapatoms (lambda (x)
                  (push (prin1-to-string x t) tag-names))
                tags-completion-table)
      (find-tag (ivy-completing-read "tag: " tag-names)))))

(use-package ctags-update
  :ensure t
  :commands turn-on-ctags-auto-update-mode
  :diminish ctags-auto-update-mode
  :init
  (add-hook 'prog-mode-hook 'turn-on-ctags-auto-update-mode)
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (setq-local ctags-update-other-options '("--fields=+l"
                                                                         "--languages=python"
                                                                         "--python-kinds=-iv")))))

(use-package imenu-anywhere
  :ensure t
  :bind ("C-." . ivy-imenu-anywhere)
  :config
  ;; only show tag for current buffer
  (setq-default imenu-anywhere-buffer-list-function (lambda () (list (current-buffer))))
  (setq imenu-anywhere-buffer-filter-functions '((lambda (current other) t))))

(use-package company
  :ensure t
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq tab-always-indent 'complete)
  (defun kd/local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (set (make-local-variable 'company-backends)
         (append (list backend) company-backends))))

(use-package company-flx
  :ensure t
  :commands company-flx-mode
  :init
  (add-hook 'company-mode-hook #'company-flx-mode))

(use-package paredit
  :ensure t
  :commands paredit-mode
  :diminish paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(use-package csv-mode
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :commands dockerfile-mode)

(use-package salt-mode
  :ensure t
  :commands salt-mode)

(use-package fish-mode
  :ensure t
  :commands fish-mode)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :commands markdown-mode)

(use-package conf-mode
  :mode ("rc$" . conf-mode))

(use-package yasnippet
  :ensure t
  :commands yas-global-mode
  :diminish yas-minor-mode
  :init
  (add-hook 'after-init-hook #'yas-global-mode))

(use-package undo-tree
  :ensure t
  :commands undo-tree-mode
  :diminish undo-tree-mode
  :init
  (add-hook 'after-init-hook #'global-undo-tree-mode))

(use-package abbrev
  :defer t
  :diminish abbrev-mode
  :init
  (add-hook 'prog-mode-hook #'abbrev-mode))

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :diminish flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically nil))

(use-package which-func
  :commands which-function-mode
  :init
  (add-hook 'prog-mode-hook #'which-function-mode))

;;; python
(use-package pyvenv
  :ensure t
  :commands pyenv-mode
  :init
  (add-hook 'python-mode-hook #'pyvenv-tracking-mode))

(use-package pip-requirements
  :ensure t
  :defer t)

(use-package anaconda-mode
  :ensure t
  :commands (anaconda-mode anaconda-eldoc-mode)
  :diminish anaconda-mode
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (anaconda-mode 1)
                                (anaconda-eldoc-mode 1))))

(use-package company-anaconda
  :ensure t
  :after (company anaconda-mode)
  :commands company-anaconda
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (company-mode 1)
                                (kd/local-push-company-backend 'company-anaconda))))

(use-package ycmd
  :disabled t
  :ensure t
  :commands (ycmd-mode ycmd-open)
  :diminish ycmd-mode
  :bind
  (("M-." . ycmd-goto)
   ("M-," . ycmd-goto-declaration))
  :init
  (set-variable 'ycmd-server-command `("python" ,(expand-file-name "~/.ghq/github.com/Valloric/ycmd/ycmd/__main__.py")))
  (add-hook 'python-mode-hook (lambda ()
                                (ycmd-mode)
                                (local-set-key (kbd "M-.") #'ycmd-goto)
                                (local-set-key (kbd "M-,") #'ycmd-goto-declaration)))
  (advice-add 'pyvenv-activate :after (lambda (&rest r) (ycmd-open))))

(use-package company-ycmd
  :disabled t
  :ensure t
  :after (company ycmd)
  :commands company-ycmd-setup
  :init
  (add-hook 'python-mode-hook #'company-ycmd-setup))

;;; golang
(use-package go-mode
  :ensure t
  :commands go-mode
  :bind
  (:map go-mode-map
        ("M-." . godef-jump)
        ("M-C-." . godef-jump-other-window)
        ("M-k" . godoc-at-point))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  (setq-local flycheck-disabled-checkers '(go-golint)))

(use-package go-eldoc
  :ensure t
  :commands go-eldoc-setup
  :init
  (add-hook 'go-mode-hook #'go-eldoc-setup))

(use-package company-go
  :ensure t
  :commands company-go
  :after company
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (company-mode 1)
                            (kd/local-push-company-backend 'company-go))))

(use-package go-guru
  :ensure t
  :defer t
  :init
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

;;; lisp
(use-package elisp-slime-nav
  :ensure t
  :commands turn-on-elisp-slime-nav-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode))

(defun init--package-install ()
  (let ((packages '(cyberpunk-theme
                    multi-term
                    multiple-cursors
                    realgud
                    tldr)))
    (dolist (pkg packages)
      (unless (package-installed-p pkg)
        (package-install pkg)))))

(condition-case nil
    (init--package-install)
  (error
   (package-refresh-contents)
   (init--package-install)))

;; from: http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
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
