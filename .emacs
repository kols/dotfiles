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

;;; tab
(setq-default default-tab-width 4)
(setq-default tab-always-indent 'complete)

;;; display
(setq initial-scratch-message "")
(setq visible-bell t)

(when (window-system)
  (tool-bar-mode 0)
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (set-face-attribute 'default nil :font "-apple-monaco-regular-normal-normal-*-15-*-*-*-m-0-iso10646-1"))

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
  :commands 'auto-fill-mode
  :init
  (define-key kd/toggle-map "f" 'auto-fill-mode)
  :diminish auto-fill-mode)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package find-file-in-project
  :ensure t
  :bind ("C-c o" . find-file-in-project))

(use-package avy
  :ensure t
  :bind
  ("C-c SPC" . avy-goto-char)
  ("C-c l" . avy-goto-line))

(use-package ag
  :ensure t
  :bind ("C-c a" . ag-project-regexp))

(use-package magit
  :ensure t
  :init
  (setq magit-git-executable "/usr/local/bin/git")
  :bind
  ("C-c g s" . magit-status)
  ("C-c g l" . magit-log-current)
  ("C-c g b" . magit-blame))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package smartscan
  :ensure t
  :bind
  ("M-n" . smartscan-symbol-go-forward)
  ("M-p" . smartscan-symbol-go-backward))

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (add-hook 'after-init-hook 'ivy-mode))
(if (eq system-type 'darwin)
    (setq mac-option-modifier 'meta))
(setq scroll-conservatively 1)
(setq vc-follow-symlinks t)

(defun init--package-install ()
  (let ((packages '(better-defaults
                    company
                    company-go
                    company-ycmd
                    cyberpunk-theme
                    exec-path-from-shell
                    fish-mode
                    flx-ido
                    flycheck
                    go-eldoc
                    go-mode
                    ido-at-point
                    ido-vertical-mode
                    markdown-mode
                    multi-term
                    multiple-cursors
                    paredit
                    pyvenv
                    realgud
                    salt-mode
                    sr-speedbar
                    tldr
                    undo-tree
                    yasnippet
                    ycmd)))
    (dolist (pkg packages)
      (unless (package-installed-p pkg)
        (package-install pkg)))))

(condition-case nil
    (init--package-install)
  (error
   (package-refresh-contents)
   (init--package-install)))

(load-theme 'cyberpunk)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(desktop-save-mode 1)

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
(define-key kd/toggle-map "s" #'sr-speedbar-toggle)

(setq-default save-place t)
(require 'saveplace)

(setq flycheck-check-syntax-automatically nil)
(set-variable 'ycmd-server-command `("python" ,(expand-file-name "~/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd/__main__.py")))

(add-hook 'after-init-hook
          (lambda ()
            (setq speedbar-tag-hierarchy-method nil)
            (global-undo-tree-mode 1)
            (yas-global-mode 1)
            (ido-mode 1)
            (ido-everywhere 1)
            (flx-ido-mode 1)
            ;; disable ido faces to see flx highlights.
            (setq ido-enable-flex-matching t)
            (setq ido-use-faces nil)

            (ido-vertical-mode 1)
            (setq ido-vertical-define-keys 'C-n-and-C-p-only)
            (ido-at-point-mode 1)
            (setq ffip-prefer-ido-mode t)
            (global-company-mode 1)))

(add-hook 'prog-mode-hook
          (lambda ()
            (abbrev-mode 1)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode 1)))

(add-hook 'python-mode-hook
          (lambda ()
            (set-fill-column 79)
            (which-function-mode 1)
            (company-mode 1)
            (ycmd-mode 1)
            (eval-after-load "ycmd"
              '(progn
                 (local-set-key (kbd "M-.") 'ycmd-goto)
                 (local-set-key (kbd "M-,") 'ycmd-goto-declaration)))
            (flycheck-mode 1)
            (company-ycmd-setup)
            (pyvenv-mode 1)))
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq gofmt-command "goimports")
            (go-eldoc-setup)
            (which-function-mode 1)
            (company-mode 1)
            (flycheck-mode 1)
            (eval-after-load "company"
              '(progn
                 (add-to-list 'company-backends 'company-go)))
            (local-set-key (kbd "M-.") 'godef-jump)
            (local-set-key (kbd "M-C-.") 'godef-jump-other-window)
            (local-set-key (kbd "M-k") 'godoc-at-point)))
