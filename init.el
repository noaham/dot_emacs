;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                        ;;;;
;;;; dot emacs                                                              ;;;;
;;;;                                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Appearance


;; These should feature early so that emacs starts snappy!

;; Adjust window size
(add-to-list 'default-frame-alist '(height . 82))
(add-to-list 'default-frame-alist '(width . 179))

;; Turn off menu bar, tool bar and scroll bar.
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Turn on line numbering
(if (fboundp 'global-linum-mode) (global-linum-mode 1))

;; color theme
(load-theme 'wombat t)

;; theme the mode line a bit better and colors for auctex
(custom-set-faces
 '(font-latex-bold-face ((t (:inherit nil 
			     :foreground "#FFA000"
			     :weight bold
			     ))))
 '(font-latex-math-face ((t (:inherit nil 
			     :foreground "#DCCEFF"
			     ))))
 '(font-latex-italic-face ((t (:inherit nil 
			       :foreground "#FFA000"
			       :slant italic
			       ))))
 '(font-latex-sectioning-0-face ((t (:inherit nil 
				     :foreground "#FFE00D"
				     :height 1.5
				     ))))
 '(font-latex-sectioning-1-face ((t (:inherit nil 
				     :foreground "#FFE00D"
				     :height 1.4
				     ))))
 '(font-latex-sectioning-2-face ((t (:inherit nil 
				     :foreground "#FFE00D"
				     :height 1.3
				     ))))
 '(font-latex-sectioning-3-face ((t (:inherit nil 
				     :foreground "#FFE00D"
				     :height 1.2
				     ))))
 '(font-latex-sectioning-4-face ((t (:inherit nil 
				     :foreground "#FFE00D"
				     :height 1.1
				     ))))
 '(font-latex-sectioning-5-face ((t (:inherit nil 
				     :foreground "#FFE00D"
				     :height 1
				     ))))
 '(font-latex-slide-title-face ((t (:inherit nil 
				    :foreground "#FFA000"
				    :underline t
				    ))))
 '(font-latex-string-face ((t (:inherit nil 
			       :foreground "#E5786D"
			       ))))
 '(font-latex-warning-face ((t (:inherit nil 
			       :foreground "#FF3300"
			       ))))
 '(font-lock-type-face ((t (:inherit nil 
			    :foreground "#FFA000"
			    :weight bold
			    ))))
 '(mode-line ((t (
		  :background "#444444" 
		  :foreground "#D9D9D9" 
		  :box (:line-width 1 :color "#999999")
		  ))))
 '(mode-line-inactive ((t (
			   :inherit mode-line 
			   :background "#444444" 
			   :foreground "#848484" 
			   :box (:line-width 1 :color "#848484")
			   ))))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; emacs behaviour

;; add .emacs.d/elpa directory and everything under to load path
(let* ((my-lisp-dir "~/.emacs.d/elpa/") (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path)
  )

;; Change the behaviour of backups and autosaves
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '((".*" . "~/.emacs.d/backups"))    ; save all backups in one place
   auto-save-file-name-transforms
    '((".*" "~/.emacs.d/autosaves" t)) ; also autosaves
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Remember point in file (stored in .emacs.d/places)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

;; The bell is super annoying, turn it off
(setq ring-bell-function 'ignore)

;; If multiple buffers with the same name exist create beeter unique names
(require 'uniquify)
(setq 
  uniquify-buffer-name-style 'reverse
  uniquify-separator "::")

;; Keep a list of recently visited files, available with C-c C-r
(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Set undo tree mode globally
(require 'undo-tree)
(global-undo-tree-mode)

;; Use diminish to hide minor modes
(require 'diminish)
(diminish 'undo-tree-mode)

;; require po tip to create nice tool tip menus
(require 'pos-tip)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package management

;; Define repositories
(setq package-archives
      '(("original"    . "http://tromey.com/elpa/")
        ("gnu"         . "http://elpa.gnu.org/packages/")
	("melpa"       . "http://melpa.milkbox.net/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")))
(package-initialize)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Flyspell

;; Emacs path variable does not contain /usr/local/bin, lets add it as
;; that is where homebrew installs aspell
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; keybinding for ispell on word is below C-1

;; start flyspell with latex
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; defaul dictionary should be british english
(setq ispell-dictionary "british")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auctex and reftex modes

;; load reftex and set it up so it plays nicely with Auctex
(require 'reftex)
(setq reftex-plug-into-AUCTeX t)

;;Set Auctex to use xetex
(setq TeX-engine 'xetex)

;; set XeTeX mode in TeX/LaTeX
(add-hook 'LaTeX-mode-hook 
	  (lambda() 
	    (add-to-list 
	     'TeX-command-list 
	     '("XeLaTeX" "%`xelatex%(mode) --shell-escape%' %t" 
	       TeX-run-TeX nil t)) 
	    (setq TeX-command-default "XeLaTeX") 
	    (setq TeX-save-query nil) 
	    (setq TeX-show-compilation nil)))

;; Set up environments for Latex
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
	      )
))

;; set defaut prefixes for reftex to use on labels
(setq reftex-label-alist
      '(("Theorem" ?h "thm:" "~\\ref{%s}" t ("Theorem" "thm."))
	("Lemma" ?l "lem:" "~\\ref{%s}" t ("Lemma" "lem."))
	("Proposition" ?p "prp:" "~\\ref{%s}" t ("Proposition" "prp."))
	("Definition" ?d "def:" "~\\ref{%s}" t ("Definition" "def."))
	("Example" ?x "exm:" "~\\ref{%s}" t ("Example" "exm."))
	("Exercise" ?s "ecs:" "~\\ref{%s}" t ("Exercise" "ecs."))
	("Conjecture" ?C "coj:" "~\\ref{%s}" t ("Conjecture" "coj."))
	("Corollary" ?c "cor:" "~\\ref{%s}" t ("Corollary" "cor."))
	("Remark" ?r "rem:" "~\\ref{%s}" t ("Remark" "rem."))
	("Problem" ?o "prb:" "~\\ref{%s}" t ("Remark" "prb."))
	))


;; So that RefTeX also recognizes \addbibresource. Note that you
(setq reftex-bibliography-commands 
      '("bibliography" "nobibliography" "addbibresource"))
(setq TeX-view-program-list '(("Preview" "open %o"))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Yas-snippet

(require 'yasnippet)
;; Dont load yas on init, it is very slow
(yas/global-mode 1)

;; Get rid of default x-prompt when multiple snippets
(setq yas-prompt-functions '(
			     yas-dropdown-prompt 
			     yas-completing-prompt 
			     yas-ido-prompt
			     yas-no-prompt
			     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Autocomplete mode

;; set up ac and default config (loads default sources)
(require 'auto-complete-config)
(ac-config-default)

;; use fuzzy matching
(setq ac-use-fuzzy t)

;; make sure that autocomplete knows about latex mode
(add-to-list 'ac-modes 'LaTeX-mode)

;; add sources from ac-math to sources in latex file
(require 'ac-math)
(require 'auto-complete-auctex)
(defun ac-latex-mode-setup ()
  (setq ac-sources
     (append '(
	       ac-source-math-latex 
	       ac-source-latex-commands
	       ac-source-math-unicode
	       ac-source-yasnippet
	       )
	     ac-sources)))
;; make this happen when latex mode is activated
(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

;; Set trigger key for ac
;(ac-set-trigger-key "TAB")
;(ac-set-trigger-key "<tab>")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start t)
 '(ac-trigger-key "TAB")
 '(ac-use-menu-map t))

;; For some reason flyspell doesn't work with ac so we use:
(ac-flyspell-workaround)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Keybindings and special functions

;; Join current line with next line
(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
		  (join-line -1)))

;; To run ispell on word C-'
(global-set-key (kbd "C-'") 'ispell-word)

