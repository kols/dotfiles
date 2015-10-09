(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920" default)))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'default-black)
(set-face-attribute 'default nil :font "-apple-Hack-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")
(setq mac-option-modifier 'meta)

(column-number-mode 1)

(require 'package)
(package-initialize)
(defun init--package-install ()
  (let ((packages '(ag
                    anaconda-mode
                    avy
                    better-defaults
                    company
                    company-anaconda
                    company-go
                    expand-region
                    fiplr
                    fill-column-indicator
                    flx-ido
                    flycheck
                    ggtags
                    go-eldoc
                    go-mode
                    ido-vertical-mode
                    magit
                    multi-term
                    paredit
                    pyvenv
                    smex
                    swiper
                    undo-tree
                    yasnippet)))
    (dolist (pkg packages)
      (unless (package-installed-p pkg)
        (package-install pkg)))))

(condition-case nil
    (init--package-install)
  (error
   (package-refresh-contents)
   (init--package-install)))

(setq flycheck-check-syntax-automatically nil)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c o") 'fiplr-find-file)
(global-set-key (kbd "C-c SPC") 'avy-goto-char)
(global-set-key (kbd "C-c l") 'avy-goto-line)
(global-set-key (kbd "C-c a") 'ag-project-regexp)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log-current)

(add-hook 'after-init-hook
          (lambda ()
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
            (global-company-mode 1)))

(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
(add-hook 'python-mode-hook
          (lambda ()
            (set-fill-column 79)
            (fci-mode 1)
            (company-mode 1)
            (flycheck-mode 1)
            (ggtags-mode 1)
            (anaconda-mode 1)
            (eval-after-load "company"
              '(progn
                 (add-to-list 'company-backends 'company-anaconda)))
            (eldoc-mode 1)
            (pyvenv-mode 1)
            (let ((projname (file-name-nondirectory (directory-file-name (fiplr-root)))))
              (if (member projname (pyvenv-virtualenv-list))
                  (pyvenv-workon projname)))))
(add-hook 'go-mode-hook
          (lambda ()
            (go-eldoc-setup)
            (company-mode)
            (set (make-local-variable 'company-backends) '(company-go))
            (local-set-key (kbd "M-.") 'godef-jump)))
