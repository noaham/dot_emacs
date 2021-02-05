;; (setq gc-cons-threshold 1000000000)
;; (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(setq straight-recipes-gnu-elpa-use-mirror t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package esup
    :straight t
    )

(setq default-frame-alist '((top + 0) 
                            (left + 0) 
                            (height . 100) 
                            (width . 120)
                            (font . "Roboto Mono:style=Light:size=12")
                            (internal-border-width . 0)
                            (left-fringe    . 18)
                            (right-fringe   . 18)
                            ))
(set-face-attribute 'fringe nil
                      :background "white"
                      )

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-message t)

(set-face-attribute 'mode-line nil
                    :weight 'light
                    :foreground "grey20"
                    :background "grey90"
                    :box '(:line-width -1 :color "grey75" :style nil)
                    )

;; (use-package modus-themes
;;   :straight t
;;   :config
;;   (load-theme 'modus-operandi t)
;;   )

;; (use-package nord-theme
;;   :straight t
;;   :init
;;   :config
;;   (load-theme 'nord t)
;;   )

;; (use-package solo-jazz-theme
;;   :straight t
;;   :init
;;   :config
;;   (load-theme 'solo-jazz t)
;;   )

(delete-selection-mode)

;; (savehist-mode 1)

(use-package saveplace
  :init
  (save-place-mode t)
  )

(use-package recentf
    :init
    (setq recentf-max-saved-items 300)
    :config
    (recentf-mode t)
    )

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)

(use-package undo-tree
    :straight t
    :config
    (global-undo-tree-mode)
    )

(global-visual-line-mode 1)

(use-package selectrum
  :straight t
  :config (selectrum-mode t)
  )

(use-package selectrum-prescient
  :straight t
  :config
  (selectrum-prescient-mode t)
  (prescient-persist-mode t)
  )

(use-package consult
  :straight t
  :bind (("C-c o" . consult-outline)
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-s" . consult-line)
         )
  )

(use-package marginalia
  :straight t
  :config (marginalia-mode)
  )

(use-package company
  :straight t
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort))
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

(electric-pair-mode t)
(show-paren-mode t)

(use-package tex
  :straight auctex
  :defer t
  :config
  (setq LaTeX-electric-left-right-brace t)
  )
