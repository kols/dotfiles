(setq custom-file "~/.emacs-custom.el")
(load custom-file)
(custom-set-variables
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))))

(load-theme 'default-black)
(set-face-attribute 'default nil :font "-apple-iosevka-regular-normal-normal-*-16-*-*-*-m-0-iso10646-1")
(setq mac-option-modifier 'meta)
(setq scroll-conservatively 1)
(column-number-mode 1)
(setq default-tab-width 4)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1))

(require 'package)
(package-initialize)
(defun init--package-install ()
  (let ((packages '(ag
                    avy
                    better-defaults
                    company
                    company-go
                    company-ycmd
                    exec-path-from-shell
                    expand-region
                    find-file-in-project
                    fish-mode
                    flx-ido
                    flycheck
                    go-eldoc
                    go-mode
                    ido-at-point
                    ido-vertical-mode
                    magit
                    markdown-mode
                    multi-term
                    multiple-cursors
                    paredit
                    pyvenv
                    realgud
                    salt-mode
                    smex
                    sr-speedbar
                    swiper
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

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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

(define-prefix-command 'my/toggle-map)
(define-key ctl-x-map "t" 'my/toggle-map)
(define-key my/toggle-map "n" #'narrow-or-widen-dwim)
(define-key my/toggle-map "s" #'sr-speedbar-toggle)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c o") 'find-file-in-project)
(global-set-key (kbd "C-c SPC") 'avy-goto-char)
(global-set-key (kbd "C-c l") 'avy-goto-line)
(global-set-key (kbd "C-c a") 'ag-project-regexp)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log-current)

(setq-default save-place t)
(require 'saveplace)

(setq flycheck-check-syntax-automatically nil)
(setq magit-git-executable "/usr/local/bin/git")
(set-variable 'ycmd-server-command `("python" ,(expand-file-name "~/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd/__main__.py")))

(add-hook 'after-init-hook
          (lambda ()
            (setq speedbar-tag-hierarchy-method nil)
            (progn
              (ivy-mode 1)
              (setq ivy-use-virtual-buffers t))
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
            (go-eldoc-setup)
            (which-function-mode 1)
            (company-mode 1)
            (eval-after-load "company"
              '(progn
                 (add-to-list 'company-backends 'company-go)))
            (local-set-key (kbd "M-.") 'godef-jump)))
