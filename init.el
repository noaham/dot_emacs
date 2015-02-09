(require 'cask)
(cask-initialize)
(require 'pallet)
(require 'use-package)
(setq use-package-verbose t
      use-package-idle-interval 10)
(use-package paradox
;;  :defer t
  :config
  (setq paradox-github-token t))
(setq default-frame-alist '((top + 100) 
			    (left + 100) 
			    (height . 62) 
			    (width . 120)))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)
(use-package linum
  :init
  (global-linum-mode -1))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'base16-eighties-dark :no-confirm)
(add-to-list 'default-frame-alist '(font . "Menlo-12"))
(use-package smart-mode-line
  :config
  (progn
    (load-theme 'smart-mode-line-respectful :no-confirm)
    (setq sml/theme nil
          rm-blacklist "\\([A-z]\\|[-]\\)*")
    (sml/setup)))
(setq ring-bell-function 'ignore)
(delete-selection-mode)
(setq backup-by-copying t 
      backup-directory-alist
      '((".*" . "~/.emacs.d/backups")) 
      auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosaves/" t)) 
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(use-package savehist
  :config
  (progn
    (savehist-mode 1)
    (setq history-length 100
          history-delete-duplicates t
          savehist-additional-variables '(search-ring
                                          regexp-search-ring)))
)
(use-package saveplace
  :init
  (progn
    (setq-default save-place t)
    (setq save-place-file "~/.emacs.d/places")))
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)
(use-package windmove
  :init
  (windmove-default-keybindings))
(use-package recentf
  :init
  (progn
    (setq recentf-save-file "~/.emacs.d/.recentf")
    (recentf-mode t)
    (setq recentf-max-menu-items 50)
    (add-to-list 'recentf-exclude "\\.emacs.d/.cask/")
    ))
(use-package undo-tree
  :init
  (global-undo-tree-mode))
(global-visual-line-mode 1)
(use-package browse-kill-ring
  :bind ("C-c C-y" . browse-kill-ring)
  )
(use-package flyspell
  :bind
  ("C-'" . ispell-word)
  :config
  (progn
    (setq ispell-dictionary "british")
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'markdown-mode-hook 'flyspell-mode)))
(use-package popwin
  :init
  (popwin-mode 1))
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-r" . helm-recentf))
  :init
  (progn
    (require 'helm-config)
    (setq helm-mode-reverse-history nil)
    (helm-mode 1)
    (setq helm-locate-command "mdfind -onlyin $HOME -name %s %s | grep -v \"$HOME/Library\" "))
  )
(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))
(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (progn
            (bind-key "C-n" 'company-select-next company-active-map)
            (bind-key "C-p" 'company-select-previous company-active-map)
            ))
(use-package smartparens-config
  ; :bind (("C-M-f" . 'sp-forward-sexp)
  ; ("C-M-b" . 'sp-backward-sexp)
  ; ("C-M-d" . 'sp-down-sexp)
  ; ("C-M-a" . 'sp-backward-down-sexp)
  ; ("C-S-a" . 'sp-beginning-of-sexp)
  ; ("C-S-d" . 'sp-end-of-sexp)
  ; ("C-M-e" . 'sp-up-sexp)
  ; ("C-M-u" . 'sp-backward-up-sexp)
  ; ("C-M-t" . 'sp-transpose-sexp)
  ; ("C-M-n" . 'sp-next-sexp)
  ; ("C-M-p" . 'sp-previous-sexp)
  ; ("C-M-k" . 'sp-kill-sexp)
  ; ("C-M-w" . 'sp-copy-sexp))
  :init
  (progn
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)
    (sp-use-smartparens-bindings)
    (sp-pair "\\(" nil :actions :rem)
    (sp-pair "\\( " " \\)" :trigger "\\(")
    (sp-local-pair 'latex-mode "\\left| " " \\right|" :trigger "\\l|")
    (sp-local-pair 'latex-mode "\\left( " " \\right)" :trigger "\\l(")
    (sp-local-pair 'latex-mode "\\left{ " " \\right}" :trigger "\\l{")
    )
)
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-+" . er/contract-region)))
(use-package yasnippet
  :init
  (yas-reload-all))
(use-package projectile
  :init
  (projectile-global-mode)
  :config
  (setq projectile-completion-system 'helm))

(use-package helm-projectile)
(use-package markdown-mode
			 :mode "\\.md\\'")
(use-package tex-site
  :defer t
  :config
  (progn
    (setq TeX-engine 'xetex
          exec-path (append exec-path '("/usr/texbin")))
    (setenv "TEXINPUTS" ".:~/latex:")
    (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin")))
  :init
  (progn
    (add-hook 'LaTeX-mode-hook 
              (lambda() 
                (add-to-list 
                 'TeX-command-list 
                 '("XeLaTeX" "%`xelatex%(mode) --shell-escape%' %t" TeX-run-TeX nil t))))
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (setq TeX-command-default "LaTexMk"
                      TeX-save-query nil 
                      TeX-show-compilation nil)))
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (LaTeX-add-environments
                 '("Theorem" LaTeX-env-label)
                 '("Lemma" LaTeX-env-label)
                 '("proof" LaTeX-env-label)
                 '("Proposition" LaTeX-env-label)
                 '("Definition" LaTeX-env-label)
                 '("Example" LaTeX-env-label)
                 '("Exercise" LaTeX-env-label)
                 '("Conjecture" LaTeX-env-label)
                 '("Corollary" LaTeX-env-label)
                 '("Remark" LaTeX-env-label)
                 '("Problem" LaTeX-env-label)
                 )))
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                  (add-to-list 'LaTeX-label-alist '("Theorem" . "thm:"))
                  (add-to-list 'LaTeX-label-alist '("Lemma" . "lem:"))
                  (add-to-list 'LaTeX-label-alist '("Proposition" . "prp"))
                  (add-to-list 'LaTeX-label-alist '("Definition" . "def:"))
                  (add-to-list 'LaTeX-label-alist '("Example" . "exm:"))
                  (add-to-list 'LaTeX-label-alist '("Exercise" . "exr:"))
                  (add-to-list 'LaTeX-label-alist '("Conjecture" . "coj:"))
                  (add-to-list 'LaTeX-label-alist '("Corollary" . "cor:"))
                  (add-to-list 'LaTeX-label-alist '("Remark" . "rem:"))
                  (add-to-list 'LaTeX-label-alist '("Problem" . "prb:"))))
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (yas-minor-mode)))
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (setq reftex-plug-into-AUCTeX t)
    (company-auctex-init)
    (flyspell-mode)
    ))

(use-package auctex-latexmk
  :init (auctex-latexmk-setup)
  )
(use-package reftex
  :defer t
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-ref-macro-prompt nil
        reftex-bibliography-commands '("bibliography"
                                       "nobibliography"
                                       "addbibresource")
        reftex-insert-label-flags '(t t)
        reftex-label-alist
        '(("Theorem" ?h "thm:" "~\\ref{%s}" nil ("Theorem" "thm."))
          ("Lemma" ?l "lem:" "~\\ref{%s}" t ("Lemma" "lem."))
          ("Proposition" ?p "prp:" "~\\ref{%s}" t ("Proposition" "prp."))
          ("Definition" ?d "def:" "~\\ref{%s}" t ("Definition" "def."))
          ("Example" ?x "exm:" "~\\ref{%s}" t ("Example" "exm."))
          ("Exercise" ?E "exr:" "~\\ref{%s}" t ("Exercise" "exr."))
          ("Conjecture" ?C "coj:" "~\\ref{%s}" t ("Conjecture" "coj."))
          ("Corollary" ?c "cor:" "~\\ref{%s}" t ("Corollary" "cor."))
          ("Remark" ?r "rem:" "~\\ref{%s}" t ("Remark" "rem."))
          ("Problem" ?o "prb:" "~\\ref{%s}" t ("Remark" "prb.")))
        ))
(use-package magma-mode
  :mode "\\.m\\'"
  :pre-load
  (add-to-list 'load-path "~/.emacs.d/site-lisp/magma-mode"))
(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    ))
(use-package python
  :mode "\\.sage\\'"
  )
(use-package mmm-auto
			  :init
			  (progn
			    (setq mmm-global-mode 'maybe)
			    (defvar mmm-markdown-mode-alist '())
			    (defun mmm-markdown-get-mode (string)
			      (string-match "[a-zA-Z_-]+" string)
			      (setq string (match-string 0 string))
			      (or (mmm-ensure-modename
			     	  ;; First try the user override variable.
			     	  (some #'(lambda (pair)
			     				(if (string-match (car pair) string) (cdr pair) nil))
			     			mmm-markdown-mode-alist))
			     	 (let ((words (split-string (downcase string) "[_-]+")))
			     	   (or (mmm-ensure-modename
			     			;; Try the whole name, stopping at "mode" if present.
			     			(intern
			     			 (mapconcat #'identity
			     						(nconc (ldiff words (member "mode" words))
			     							   (list "mode"))
			     						"-")))
			     		   ;; Try each word by itself (preference list)
			     		   (some #'(lambda (word)
			     					 (mmm-ensure-modename (intern word)))
			     				 words)
			     		   ;; Try each word with -mode tacked on
			     		   (some #'(lambda (word)
			     					 (mmm-ensure-modename
			     					  (intern (concat word "-mode"))))
			     				 words)
			     		   ;; Try each pair of words with -mode tacked on
			     		   (loop for (one two) on words
			     				 if (mmm-ensure-modename
			     					 (intern (concat one two "-mode")))
			     				 return it)
			     		   ;; I'm unaware of any modes whose names, minus `-mode',
			     		   ;; are more than two words long, and if the entire mode
			     		   ;; name (perhaps minus `-mode') doesn't occur in the
			     		   ;; markdownument name, we can give up.
			     		   (signal 'mmm-no-matching-submode nil)))))
			    (mmm-add-classes
			     '((markdown
			        :front "```+\\([a-zA-Z0-9_-]+\\)"
			        :front-offset (end-of-line 1)
			        :back "```+[ ]*$"
			        :save-matches 1
			        :delimiter-mode nil
			        :match-submode mmm-markdown-get-mode
			        :end-not-begin t
			        )))
			    (mmm-add-mode-ext-class 'markdown-mode "\\.md\\'" 'markdown)
			    ))
(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
