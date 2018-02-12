;;; .emacs --- Emacs init file

;;; Commentary:
;;;   Emacs init file

;;; Code:

;;; Package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;;;; quelpa
(unless (require 'quelpa nil t)
  (load "quelpa-bootstrap.el"))
(quelpa '(quelpa-use-package
          :fetcher github
          :repo "quelpa/quelpa-use-package"))

;;;; use-package
(setq use-package-expand-minimally t)
(require 'use-package)
(require 'quelpa-use-package)
(use-package use-package-chords
  :quelpa
  :config (key-chord-mode 1))

;;;; auto-compile
(use-package auto-compile
  :disabled t
  :quelpa
  :init
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; Settings
(use-package kd-settings
  :init
  (defun kd/emacs-subdirectory (d)
    "Make dir path inside Emacs user dir for D."
    (expand-file-name d user-emacs-directory))

  (add-to-list 'load-path (kd/emacs-subdirectory "elisp"))

  (fset 'yes-or-no-p 'y-or-n-p)

  (setq-default default-tab-width 4)

  (setq confirm-kill-emacs 'y-or-n-p
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
    (setq inhibit-startup-message t
          inhibit-startup-echo-area-message user-login-name
          inhibit-default-init t
          initial-major-mode 'fundamental-mode
          initial-scratch-message nil
          mode-line-format nil))

  (setq visible-bell t)

  (provide 'kd-settings))

;;; Keybinding
(use-package bind-key
  :quelpa
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

;;; Daemon
(use-package server
  :init
  (add-hook 'after-init-hook (lambda ()
                               (unless (server-running-p)
                                 (server-start)))))

(use-package better-defaults
  :quelpa)

(use-package desktop
  :init (add-hook 'after-init-hook #'desktop-save-mode))


;;; Interface

;; GUI
(when (window-system)
  (use-package default-black-theme
    :disabled t
    :config (load-theme 'default-black t))

  (use-package cyberpunk-theme
    :disabled t
    :quelpa
    :config (load-theme 'cyberpunk t))

  (use-package zenburn-theme
    :quelpa
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
  (custom-set-faces '(default ((t (:font "-apple-hack-regular-normal-normal-*-17-*-*-*-m-0-iso10646-1")))))

  ;; mouse
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse 't)

  ;; maximize
  (add-hook 'after-init-hook #'toggle-frame-maximized))

(use-package simple
  :diminish visual-line-mode
  :commands global-visual-line-mode
  :init (add-hook 'after-init-hook #'global-visual-line-mode))


;;; macOS
(use-package kd-macOS
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
    :if IS-MAC
    :ensure t)

  (use-package osx-dictionary
    :if IS-MAC
    :ensure t
    :bind ("C-c f" . osx-dictionary-search-word-at-point)))


(use-package whitespace
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
  :init (add-hook 'after-init-hook #'auto-insert-mode)
  :config
  (define-auto-insert 'python-mode '(nil
                                     "# coding: utf-8\n")))

(use-package highlight-symbol
  :ensure t
  :commands (highlight-symbol-mode)
  :diminish highlight-symbol-mode
  :init (add-hook 'prog-mode-hook 'highlight-symbol-mode))

;; Window

(use-package windmove
  :bind (("C-s-h" . windmove-left)
         ("C-s-j" . windmove-down)
         ("C-s-k" . windmove-up)
         ("C-s-l" . windmove-right)))

(use-package winner
  :commands winner-mode
  :init
  (setq winner-dont-bind-my-keys t)
  (add-hook 'after-init-hook #'winner-mode))

(use-package ace-window
  :ensure t
  :bind ("s-j" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame)
  (setq aw-ignore-current t)
  (global-unset-key (kbd "C-x o")))

(use-package zygospore
  :ensure t
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

(use-package eyebrowse
  :ensure t
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
  :commands dired-omit-mode
  :init (add-hook 'dired-mode-hook #'dired-omit-mode))


;;; neotree

(use-package neotree
  :ensure t
  :bind (:map kd/pop-map
              ("n" . neotree-toggle)
              ("N" . neotree-find))
  :commands (neotree neotree-toggle neotree-find)
  :config
  (setq neo-window-width 35)
  (setq neo-confirm-change-root 'off-p))


;;; ido

(use-package ido
  :ensure t
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
  :ensure t
  :after ido
  :commands flx-ido-mode)

(use-package ido-vertical-mode
  :ensure t
  :after ido
  :commands ido-vertical-mode
  :config (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-at-point
  :ensure t
  :after ido
  :commands ido-at-point-mode)


(use-package find-file-in-project
  :ensure t
  :bind ("C-c o" . find-file-in-project))

(use-package projectile
  :ensure t
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
  :ensure t
  :bind (:map projectile-command-map
              ("p" . counsel-projectile-switch-project)
              ("r" . counsel-projectile-rg)))

(use-package avy
  :ensure t
  :bind (:map kd/org-map
              ("w" . avy-org-refile-as-child))
  :chords (("jw" . avy-goto-word-1)
           ("jc" . avy-goto-char-timer)
           ("js" . avy-goto-symbol-1)
           ("jl" . avy-goto-line))
  :config (setq avy-background t))

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
  :defines magit-status-expand-stashes
  :config
  (setq magit-git-executable "/usr/local/bin/git")
  (setq magit-status-expand-stashes nil))

(use-package diff-hl
  :ensure t
  :commands turn-on-diff-hl-mode
  :init (add-hook 'prog-mode-hook #'turn-on-diff-hl-mode))


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
         ("C-M-o" . hydra-window-size/body)
         ("s-m" . hydra-mc/body))
  :config
  (defun kd/default-captured-org-note ()
    "Move to the end of penultimate line of the last org capture note."
    (interactive)
    (find-file org-default-notes-file)
    (end-of-buffer)
    (forward-line -2)
    (org-end-of-line))
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
              :initial-input initial-input
              :action (lambda (d) (dired d))
              :caller 'kd/jump-to-reference))
  (defhydra hydra-goto (:color blue :hint nil)
    "
  goto   file
         -----------------------
         ._e_macs
         _d_eft files
         _D_eft
         _c_ap.org: default note
         _j_apan_trip.org
         o_r_g files
         _p_rojectile
         _s_rc
         re_f_erence
    "
    ("e" (find-file "~/.dotfiles/.emacs"))
    ("d" deft-find-file)
    ("D" deft)
    ("c" kd/default-captured-org-note)
    ("j" (find-file (concat org-directory "/japan_trip.org")))
    ("r" (counsel-file-jump nil org-directory))
    ("s" (kd/jump-to-src))
    ("p" (projectile-switch-project t))
    ("f" (kd/jump-to-reference)))
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
    ("=" balance-windows "balance"))
  (defhydra hydra-mc ()
    "multiple cursor"
    ("n" mc/mark-next-like-this "mark next")
    ("p" mc/mark-previous-like-this "mark previous")
    ("A" mc/mark-all-like-this "mark all")
    ("e" mc/edit-lines "edit lines")))

(use-package edit-indirect
  :ensure t
  :bind ("C-c '" . edit-indirect-region))


;;; Ivy, Swiper & Counsel

(use-package flx
  :ensure t)

(use-package ivy
  :ensure t
  :commands ivy-switch-buffer
  :bind (:map ivy-mode-map
              ("s-x" . ivy-switch-buffer)
              ("C-c C-r" . ivy-resume))
  :diminish ivy-mode
  :init (add-hook 'after-init-hook 'ivy-mode)
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
  :ensure t
  :after ivy
  :commands swiper)

;; cache for M-x
(use-package smex
  :ensure t
  :defer t)

(use-package counsel
  :ensure t
  :bind
  (("C-S-s" . counsel-grep-or-swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-." . counsel-imenu)))

(use-package bookmark+
  :defer t
  :ensure t)

(use-package isearch+
  :ensure t)

(use-package dired+
  :defer t
  :ensure t)

(use-package info
  :commands info)

(use-package info+
  :ensure t
  :after info)

(use-package help+
  :defer t
  :ensure t)

(use-package help-fns+
  :defer t
  :ensure t)

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :commands (exec-path-from-shell-initialize)
  :ensure t
  :init (add-hook 'after-init-hook #'exec-path-from-shell-initialize)
  :config (setq exec-path-from-shell-check-startup-files nil))

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
  (add-hook 'restclient-mode-hook (lambda ()
                                    (kd/local-push-company-backend #'company-restclient))))

(use-package outshine
  :ensure t
  :after outline
  :commands outshine-hook-function
  :init (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  :config (setq outshine-use-speed-commands t))

(use-package outline
  :commands outline-minor-mode
  :init (add-hook 'prog-mode-hook 'outline-minor-mode))

(use-package irfc
  :ensure t
  :commands irfc-mode
  :mode ("/rfc[0-9]+\\.txt\\'" . irfc-mode)
  :init
  (setq irfc-assoc-mode 1)
  :config
  (setq irfc-directory "~/Documents/rfc/rfc"))


;;; Org-mode

(use-package org
  :commands (orgtbl-mode)
  :defines (org-capture-templates org-imenu-depth org-default-notes-file)
  :bind (("C-M-<return>" . org-insert-subheading)
         (:map kd/org-map
               ("l" . org-store-link)
               ("a" . org-agenda)
               ("c" . org-capture)))
  :custom-face
  (org-document-title ((t (:height 1.1 :wight semi-bold))))
  :init
  (setq org-directory "~/Dropbox/org")
  (add-hook 'org-mode-hook #'org-bullets-mode)
  :config
  ;; face
  (dolist (face org-level-faces)
    (custom-set-faces `(,face ((t (:height 1.0 :weight semi-bold))))))

  (unbind-key "C-'" org-mode-map)       ; used by `imenu-list'
  (setq org-default-notes-file (concat org-directory "/cap.org"))
  (setq org-capture-templates
        '(("n" "note" entry (file+datetree "") "* %?\n  %U\n  %i")))
  (setq org-src-fontify-natively t)
  (setq org-imenu-depth 3)

  ;; link type
  (org-add-link-type
   "gmail"
   (lambda (link)
     (browse-url
      ;; or "https://mail.google.com/mail/h/?&v=c&s=l&th="
      ;; for html-browser
      (concat "https://mail.google.com/mail/?shva=1#all/" link))))

  ;; babel
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ipython . t)
                                 (sh . t)
                                 (emacs-lisp . t)))
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append))

(use-package org-bullets
  :ensure t
  :commands org-bullets-mode)

(use-package org-mac-link
  :ensure t
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
  :ensure t
  :after org
  :bind (:map kd/org-map
              ("d" . org-download-yank)))

(use-package ob-ipython
  :ensure t
  :after org)

(use-package ob-async
  :ensure t
  :after org)


;;; Integration

(use-package browse-url
  :bind (:map kd/pop-map
              ("u" . browse-url-at-point)))

(use-package browse-at-remote
  :ensure t
  :bind (:map kd/pop-map
              ("r" . browse-at-remote))
  :config (dolist (elt '(("gitlab.xiaohongshu.com" . "gitlab")
                         ("code.devops.xiaohongshu.com" . "gitlab")))
            (add-to-list 'browse-at-remote-remote-type-domains elt)))

(use-package ispell
  :defer t
  :config (setq ispell-program-name "aspell"))

(use-package flyspell
  :commands flyspell-mode
  :init (add-hook 'org-mode-hook #'flyspell-mode))

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


;;; Tags

(use-package ggtags
  :ensure t
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
  :ensure t
  :bind (:map kd/pop-map
              ("'" . imenu-list-smart-toggle)))


;;; Completion

(use-package company
  :ensure t
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
  :ensure t
  :after company
  :commands company-flx-mode
  :init (add-hook 'company-mode-hook #'company-flx-mode))

(use-package company-quickhelp
  :ensure t
  :after company
  :bind (:map company-active-map
              ("M-d" . company-quickhelp-manual-begin))
  :init (add-hook 'company-mode-hook #'company-quickhelp-mode)
  :config (setq company-quickhelp-delay nil))


(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :commands (smartparens-global-strict-mode show-smartparens-global-mode)
  :bind (:map smartparens-mode-map
              ("C-s-]" . sp-unwrap-sexp)
              ("C-)" . sp-slurp-hybrid-sexp))
  :init
  (add-hook 'after-init-hook #'smartparens-global-strict-mode)
  (add-hook 'after-init-hook #'show-smartparens-global-mode)
  :config
  (use-package smartparens-config))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode))

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

(use-package salt-mode
  :ensure t
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
  :commands (markdown-mode gfm-mode)
  :init (add-hook 'markdown-mode-hook #'orgtbl-mode)
  :config (setq markdown-command "multimarkdown"))

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
  :commands (yas-global-mode yas-expand)
  :diminish yas-minor-mode
  :init (add-hook 'after-init-hook #'yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

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
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  :config (setq flycheck-check-syntax-automatically '(save)))

(use-package realgud
  :ensure t
  :commands (realgud:trepan2))

(use-package makefile-executor
  :ensure t
  :bind (:map kd/compile-map
              ("t" . makefile-executor-execute-target)
              ("p" . makefile-executor-execute-project-target)
              ("c" . makefile-executor-execute-last))
  :commands (makefile-executor-mode))


;;; C/C++

(use-package company-c-headers
  :ensure t
  :commands company-c-headers)

(use-package irony
  :ensure t
  :commands irony-mode
  :init
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package irony-eldoc
  :ensure t
  :commands irony-eldoc)

(use-package company-irony
  :ensure t
  :commands (company-irony company-irony-setup-begin-commands))

(use-package flycheck-irony
  :ensure t
  :commands flycheck-irony-setup
  :init
  (add-hook 'irony-mode-hook #'flycheck-irony-setup))

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
  :ensure t
  :after semantic)

(use-package rtags
  :ensure t
  :commands rtags-start-process-unless-running
  :init
  (setq rtags-completions-enabled t)
  (setq rtags-autostart-diagnostics t))

(use-package cc-mode
  :defer t
  :init
  (setq c-basic-offset 4)
  (defun kd/cc-mode-hook-func ()
    ; rtags
    (rtags-start-process-unless-running)

    ; irony
    (irony-mode 1)

    ; company
    (setq company-backends (delete 'company-semantic company-backends))
    (kd/local-push-company-backend #'company-c-headers)
    (kd/local-push-company-backend #'company-irony)
    (company-irony-setup-begin-commands)

    ; flycheck
    (flycheck-select-checker 'irony))

  (add-hook 'c-mode-hook #'kd/cc-mode-hook-func)
  (add-hook 'c++-mode-hook #'kd/cc-mode-hook-func)
  :config
  (setq c-auto-newline nil))


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
  :ensure t
  :commands pyenv-mode
  :init (add-hook 'python-mode-hook #'pyenv-mode))

(use-package pyenv-mode-auto
  :ensure t
  :commands pyenv-mode-auto-hook)

(use-package jedi-core
  :ensure t
  :commands jedi:setup
  :config
  (setq jedi:use-shortcuts t)
  (setq jedi:tooltip-method nil)
  (setq jedi:complete-on-dot t))

(use-package company-jedi
  :ensure t
  :commands company-jedi
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (kd/local-push-company-backend #'company-jedi))))

(use-package ein
  :ensure t
  :defer t)


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
  (defun kd/go-mode-hook-function ()
    (flycheck-gometalinter-setup)
    (setq-local flycheck-checker 'go-build)
    (add-hook 'before-save-hook #'gofmt-before-save nil t)
    (when (bound-and-true-p ggtags-mode)
      (ggtags-mode -1))
    (go-eldoc-setup))
  (add-hook 'go-mode-hook #'kd/go-mode-hook-function)
  :config
  (setq gofmt-command "goimports"))

(use-package flycheck-gometalinter
  :ensure t
  :commands (flycheck-gometalinter-setup)
  :after go-mode
  :config
  (setq flycheck-gometalinter-vendor t)
  (setq flycheck-gometalinter-errors-only t)
  (setq flycheck-gometalinter-fast t))

(use-package go-eldoc
  :ensure t
  :after (go-mode eldoc)
  :commands go-eldoc-setup)

(use-package company-go
  :ensure t
  :commands company-go
  :init
  (add-hook 'go-mode-hook (lambda ()
                            (kd/local-push-company-backend #'company-go))))

(use-package go-guru
  :ensure t
  :commands go-guru-hl-identifier-mode
  :init (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(use-package gotest
  :after go-mode
  :ensure t)

(use-package go-playground
  :ensure t
  :bind (:map go-playground-mode-map
              ("C-c C-c" . go-playground-exec)
              ("C-c C-k" . go-playground-rm))
  :commands go-playground)


;;; Java

(use-package jdee
  :ensure t
  :defer t
  :init
  (setq jdee-server-dir "/Users/kane/.ghq/github.com/jdee-emacs/jdee-server/target"))


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
