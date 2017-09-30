
(package-initialize)
(require 'cask)
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'use-package)
(setq use-package-verbose t
      use-package-idle-interval 10)

; (use-package paradox
;   :config
;   (setq paradox-github-token t))

(setq default-frame-alist '((top + 0) 
                (left + 0) 
                (height . 100) 
                (width . 120)))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-message t)

(use-package linum
  :init
  (global-linum-mode -1))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'mccarthy :no-confirm)
; (load-theme 'base16-eighties-dark :no-confirm)
;(add-to-list 'load-path "~/.emacs.d/themes/emacs-doom-theme")
;(load-theme 'dracula :no-confirm)

(add-to-list 'default-frame-alist '(font . "Menlo-12"))

(use-package smart-mode-line
  :config
;  (load-theme 'smart-mode-line-respectful :no-confirm)
  (setq sml/theme nil
        rm-blacklist "\\([A-z]\\|[-]\\)*")
  (sml/setup))

(setq ring-bell-function 'ignore)

(delete-selection-mode)

;; (setq backup-by-copying t 
;;       backup-directory-alist
;;       '((".*" . "~/.emacs.d/backups")) 
;;       auto-save-file-name-transforms
;;       '((".*" "~/.emacs.d/autosaves/" t)) 
;;       delete-old-versions t
;;       kept-new-versions 6
;;       kept-old-versions 2
;;       version-control t)

(use-package savehist
  :config
  (savehist-mode 1)
  (setq history-length 100
        history-delete-duplicates t
        savehist-additional-variables '(search-ring
                                        regexp-search-ring))
)

(use-package saveplace
  :init
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/places"))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)

(use-package windmove
  :config
  (windmove-default-keybindings)
  )

(use-package recentf
  :config
  (setq recentf-save-file "~/.emacs.d/.recentf")
  (recentf-mode t)
  (setq recentf-max-menu-items 200)
  (add-to-list 'recentf-exclude "\\.emacs.d/.cask/")
  )

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

(global-visual-line-mode 1)

(use-package flyspell
  :bind
  ("C-'" . ispell-word)
  :config
    (setq ispell-program-name "aspell")
    (setq ispell-dictionary "british")
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'markdown-mode-hook 'flyspell-mode)
    )

; (use-package helm
;   :bind (("M-x" . helm-M-x)
;          ("M-y" . helm-show-kill-ring)
;          ("C-x b" . helm-mini)
;          ("C-x C-f" . helm-find-files)
;          ("C-x C-b" . helm-buffers-list)
;          ("C-x C-r" . helm-recentf))
;   :config
;   (require 'helm-config)
;   (setq helm-mode-reverse-history nil)
;   (helm-mode 1)
;   (setq helm-locate-command "mdfind -onlyin $HOME -name %s %s | grep -v \"$HOME/Library\" ")
;   (setq helm-truncate-lines t)
;   )

(use-package ivy
  :init (ivy-mode 1)
  :bind ("C-x b" . ivy-switch-buffer)
  :config (setq ivy-height 15)
  )

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("M-y" . counsel-yank-pop)
         ("C-x C-r" . counsel-recentf))
  )

(use-package avy
  :bind (("C-c SPC" . avy-goto-char)
         ("C-c b" . avy-goto-char-2)
         ("C-c v" . avy-goto-line))
  )

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper))
  )

(use-package hydra
  )

(defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue)
  )

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)


(defhydra hydra-move (:color pink :hint nil)
  "
_n_: next line     _f_: forward char  _F_: forward word  _a_: beginning line _v_: scroll up 
_p_: previous line _b_: backward char _B_: backward word _e_: end line       _V_: scroll down
_l_: re-center

"
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)
  ("b" backward-char)
  ("F" forward-word)
  ("B" backward-word)
  ("a" beginning-of-line)
  ("e" move-end-of-line)
  ("v" scroll-up-command)
  ;; Converting M-v to V here by analogy.
  ("V" scroll-down-command)
  ("l" recenter-top-bottom)
  ("q" nil "quit" :color blue))

(global-set-key (kbd "M-n") 'hydra-move/body)

(defhydra hydra-org-move (:color pink :hint nil)
  "
_n_ext heading     _f_orward same level  _u_p level
_p_revious heading _b_: back same level  _j_:ump

"
  ("n" outline-next-visible-heading)
  ("p" outline-previous-visible-heading)
  ("f" org-forward-heading-same-level)
  ("b" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("j" org-goto :color blue)
  ("q" nil "quit" :color blue))

(global-set-key (kbd "C-c M-n") 'hydra-org-move/body)

(global-set-key (kbd "M-p") 'hydra-sp/body)

(defhydra hydra-sp (:hint nil)
  "
  _B_ backward-sexp            -----
  _F_ forward-sexp               _s_ splice-sexp
  _L_ backward-down-sexp         _df_ splice-sexp-killing-forward
  _H_ backward-up-sexp           _db_ splice-sexp-killing-backward
^^------                         _da_ splice-sexp-killing-around
  _k_ down-sexp                -----
  _j_ up-sexp                    _C-s_ select-next-thing-exchange
-^^-----                         _C-p_ select-previous-thing
  _n_ next-sexp                  _C-n_ select-next-thing
  _p_ previous-sexp            -----
  _a_ beginning-of-sexp          _C-f_ forward-symbol
  _z_ end-of-sexp                _C-b_ backward-symbol
--^^-                          -----
  _t_ transpose-sexp             _c_ convolute-sexp
-^^--                            _g_ absorb-sexp
  _x_ delete-char                _q_ emit-sexp
  _dw_ kill-word               -----
  _dd_ kill-sexp                 _,b_ extract-before-sexp
-^^--                            _,a_ extract-after-sexp
  _S_ unwrap-sexp              -----
-^^--                            _AP_ add-to-previous-sexp
  _C-h_ forward-slurp-sexp       _AN_ add-to-next-sexp
  _C-l_ forward-barf-sexp      -----
  _C-S-h_ backward-slurp-sexp    _ join-sexp
  _C-S-l_ backward-barf-sexp     _|_ split-sexp
"
  ;; TODO: Use () and [] - + * | <space>
  ("B" sp-backward-sexp );; similiar to VIM b
  ("F" sp-forward-sexp );; similar to VIM f
  ;;
  ("L" sp-backward-down-sexp )
  ("H" sp-backward-up-sexp )
  ;;
  ("k" sp-down-sexp ) ; root - towards the root
  ("j" sp-up-sexp )
  ;;
  ("n" sp-next-sexp )
  ("p" sp-previous-sexp )
  ;; a..z
  ("a" sp-beginning-of-sexp )
  ("z" sp-end-of-sexp )
  ;;
  ("t" sp-transpose-sexp )
  ;;
  ("x" sp-delete-char )
  ("dw" sp-kill-word )
  ;;("ds" sp-kill-symbol ) ;; Prefer kill-sexp
  ("dd" sp-kill-sexp )
  ;;("yy" sp-copy-sexp ) ;; Don't like it. Pref visual selection
  ;;
  ("S" sp-unwrap-sexp ) ;; Strip!
  ;;("wh" sp-backward-unwrap-sexp ) ;; Too similar to above
  ;;
  ("C-h" sp-forward-slurp-sexp )
  ("C-l" sp-forward-barf-sexp )
  ("C-S-h" sp-backward-slurp-sexp )
  ("C-S-l" sp-backward-barf-sexp )
  ;;
  ;;("C-[" (bind (sp-wrap-with-pair "[")) ) ;;FIXME
  ;;("C-(" (bind (sp-wrap-with-pair "(")) )
  ;;
  ("s" sp-splice-sexp )
  ("df" sp-splice-sexp-killing-forward )
  ("db" sp-splice-sexp-killing-backward )
  ("da" sp-splice-sexp-killing-around )
  ;;
  ("C-s" sp-select-next-thing-exchange )
  ("C-p" sp-select-previous-thing)
  ("C-n" sp-select-next-thing)
  ;;
  ("C-f" sp-forward-symbol )
  ("C-b" sp-backward-symbol )
  ;;
  ;;("C-t" sp-prefix-tag-object)
  ;;("H-p" sp-prefix-pair-object)
  ("c" sp-convolute-sexp )
  ("g" sp-absorb-sexp )
  ("q" sp-emit-sexp )
  ;;
  (",b" sp-extract-before-sexp )
  (",a" sp-extract-after-sexp )
  ;;
  ("AP" sp-add-to-previous-sexp );; Difference to slurp?
  ("AN" sp-add-to-next-sexp )
  ;;
  ("_" sp-join-sexp ) ;;Good
  ("|" sp-split-sexp ))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (bind-key "C-n" 'company-select-next company-active-map)
  (bind-key "C-p" 'company-select-previous company-active-map)
  )

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
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (sp-use-smartparens-bindings)
  (sp-pair "\\(" nil :actions :rem)
  (sp-pair "\\( " " \\)" :trigger "\\(")
  (sp-pair "\\[ " " \\]" :trigger "\\[")
  (sp-pair "\\\\( " " \\\\)" :trigger "\\\\(")
  (sp-pair "\\\\[ " " \\\\]" :trigger "\\\\[")
  (sp-local-pair 'latex-mode "\\left| " " \\right|" :trigger "\\l|")
  (sp-local-pair 'latex-mode "\\left( " " \\right)" :trigger "\\l(")
  (sp-local-pair 'latex-mode "\\left{ " " \\right}" :trigger "\\l{")
  )

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region))
  )

(use-package yasnippet)

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  )

; (use-package helm-projectile)

(use-package tiny
  )

(use-package org
  :defer t
  :init
  (setq org-src-fontify-natively t)
  )

(use-package markdown-mode
  :mode "\\.md\\'"
  )

(use-package auctex
  :defer t
  :config
  ;; (setq TeX-engine 'xetex
  ;;       exec-path (append exec-path '("/usr/texbin")))
  (setenv "TEXINPUTS" ".:~/latex:")
  (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
  :init
  (add-hook 'LaTeX-mode-hook 
            (lambda() 
              (add-to-list 
               'TeX-command-list 
               '("XeLaTeX" "%`xelatex%(mode) --shell-escape%' %t" TeX-run-TeX nil t))
              (setq
               ;; Set the list of viewers for Mac OS X.
               TeX-view-program-list
               '(("Preview.app" "open -a Preview.app %o")
                 ("Skim" "displayline -b -g %n %o %b")
                 ("displayline" "displayline %n %o %b")
                 ("open" "open %o"))
               ;; Select the viewers for each file type.
               TeX-view-program-selection
               '((output-dvi "open")
                 (output-pdf "Skim")
                 (output-html "open")))
                ))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
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
              (add-to-list 'LaTeX-label-alist '("Problem" . "prb:"))
              )
            )
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (yas-minor-mode)
              (company-auctex-init)
              (flyspell-mode)
              (TeX-PDF-mode 1)
              ;; (setq TeX-source-correlate-mode t)
              ;; (setq TeX-source-correlate-start-server t)
              ;; (setq TeX-source-correlate-method 'synctex)
              )
            )
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-label-regexps '("\\\\label{\\(?1:[^}]*\\)}"))
  )

(use-package auctex-latexmk
  :init
  (auctex-latexmk-setup)
  )

(use-package reftex
;  :defer t
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
  ; :init
  ; (add-to-list 'load-path "~/.emacs.d/site-lisp/magma-mode")
  )

(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    )

(use-package elpy
  :mode "\\.sage\\'"
  :config
  (elpy-enable)
  (elpy-use-ipython)
  )

;; (use-package mmm-auto
;;   :config
;;   (setq mmm-global-mode 'maybe)
;;   (defvar mmm-markdown-mode-alist '())
;;   (defun mmm-markdown-get-mode (string)
;;     (string-match "[a-zA-Z_-]+" string)
;;     (setq string (match-string 0 string))
;;     (or (mmm-ensure-modename
;;          ;; First try the user override variable.
;;          (some #'(lambda (pair)
;;                    (if (string-match (car pair) string) (cdr pair) nil))
;;                mmm-markdown-mode-alist))
;;         (let ((words (split-string (downcase string) "[_-]+")))
;;           (or (mmm-ensure-modename
;;                ;; Try the whole name, stopping at "mode" if present.
;;                (intern
;;                 (mapconcat #'identity
;;                            (nconc (ldiff words (member "mode" words))
;;                                   (list "mode"))
;;                            "-")))
;;               ;; Try each word by itself (preference list)
;;               (some #'(lambda (word)
;;                         (mmm-ensure-modename (intern word)))
;;                     words)
;;               ;; Try each word with -mode tacked on
;;               (some #'(lambda (word)
;;                         (mmm-ensure-modename
;;                          (intern (concat word "-mode"))))
;;                     words)
;;               ;; Try each pair of words with -mode tacked on
;;               (loop for (one two) on words
;;                     if (mmm-ensure-modename
;;                         (intern (concat one two "-mode")))
;;                     return it)
;;               ;; I'm unaware of any modes whose names, minus `-mode',
;;               ;; are more than two words long, and if the entire mode
;;               ;; name (perhaps minus `-mode') doesn't occur in the
;;               ;; markdownument name, we can give up.
;;               (signal 'mmm-no-matching-submode nil)))))
;;   (mmm-add-classes
;;    '((markdown
;;       :front "```+\\([a-zA-Z0-9_-]+\\)"
;;       :front-offset (end-of-line 1)
;;       :back "```+[ ]*$"
;;       :save-matches 1
;;       :delimiter-mode nil
;;       :match-submode mmm-markdown-get-mode
;;       :end-not-begin t
;;       )))
;;   (mmm-add-mode-ext-class 'markdown-mode "\\.md\\'" 'markdown)
;;   )

(server-start)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flyspell-duplicate ((t (:underline "DarkOrange"))))
 '(flyspell-incorrect ((t (:background "#FFCCCC" :underline "Red1"))))
 '(font-latex-math-face ((t (:foreground "#6E66B6"))))
 '(highlight ((t (:background "#b5ffd1"))))
 '(hl-line ((t (:background "#b5ffd1" :underline t))))
 ;; '(helm-ff-dotted-directory ((t (:foreground "DarkRed"))))
 '(isearch-fail ((t (:background "#ffcccc"))))
 '(show-paren-match ((t (:background "#ff4500" :foreground "#e1e1e1" :weight bold)))) 
 '(sp-pair-overlay-face ((t (:inherit highlight :background "#d1f5ea"))))
 )
