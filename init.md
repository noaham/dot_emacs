# Emacs init #

This is my emacs init file. It written in markdown and then use `sed` to strip out the elisp in fenced code blocks. This is so that I can heavily comment on how certain things work. This file and the files it depends on are kept up-to-date in a [github repository][]. Hopefully someone will find this helpful.

At the moment I am using Yamamoto Mitsuharu's [emacs mac port][]. This port has much better integration with OSX including full screen mode and pixel scrolling. You can obtain this version of emacs using [homebrew][].

This init file assumes that some directories exist such as `~/.emacs.d/snippets` and `~/.emacs.d/themes` as well as some theme files being available so it is best to obtain the entire [github repository][].

[emacs mac port]: https://github.com/railwaycat/emacs-mac-port

[homebrew]: http://brew.sh

[github repository]: https://github.com/noaham/dot_emacs


TODO:

+ Learn and implement keybindings for smartparens movement commands
+ tone down the greys in the base16-brewer-light theme
+ configure mmm-mode so that I can simply write elisp instead of emacs-lisp next to fenced code blocks. This is desirable as github recognised applies syntax highlighting to blocks with the keyword elisp but not emacs-lisp. 


## Package management ##

The very first thing we do is set up [cask][] and [pallet][]. These two utilities work in tandem to keep track of packages which I use and to make sure they are installed when emacs starts. The [Cask](./Cask) file defines which repositories package should use and which packages should be installed.

[cask]: http://cask.github.io

[pallet]: https://github.com/rdallasgray/pallet

```emacs-lisp
(require 'cask)
(cask-initialize)
(require 'pallet)
```
    
### use-package ###

[Use-package][] is a useful macro for organising how packages are loaded in my init file. In particular it will only load a package if it is available.

Very roughly (see the website for more details) the basic syntax is `(use package PACKAGE :KEYWORD EFFECT)`, where `PACKAGE` is the name of the package and `:KEYWORD EFFECT` pair is a space/newline seperated list of commands that have special actions.

For example the keyword `:bind` which takes as its argument a keybinding and a command `(KEYS . COMMAND)`. This is extremely useful because you can access these keybindings with the command `M-x describe-personal-keybindings`.

Another example is the `:init` which should be followed by a space seperated list of elisp code to be evaluated when the package loads.

The variable `use-package-verbose` controls whether messages are sen on package load. I find this useful as I like to know how long it taks for a package to load. The variable `use-package-idle-interval` controls how long emacs waits before loading `:idle` items. I make sure this is nice and long as I generally don't care about the things I let idle.

It is probably best to see the use-package website or below for examples.

[Use-package]: https://github.com/jwiegley/use-package

```emacs-lisp
(require 'use-package)
(setq use-package-verbose t
      use-package-idle-interval 10)
```

### Paradox ###

[Paradox][] is a nice package which cleans up the package-list buffer. It lists github stars and can also integrate with my github if I supply it wit a provate github token so that I can star repositories from the package men, but I don't want this so I set `paradox-github-token` to `t`.

[Paradox]: https://github.com/Bruce-Connor/paradox

```emacs-lisp
(use-package paradox
  :config
  (setq paradox-github-token t))
```




## Emacs appearance ##

The settings here are general settings effecting emacs behaviour and appearance.

### Window appearance ###

Most of these settings are for an uncluttered emacs experience.

Emacs by default starts quite small. I used to change this so that it started reasonably large (left two thirds of the screen) however i usually use it in full screen mode now so I have commented it out.

```emacs-lisp
; (setq default-frame-alist '((top + 0) 
; 			    (left + 0) 
; 			    (height . 82) 
; 			    (width . 179)))
```

We do not need tool bars or scroll bars so we turn them off.

```emacs-lisp
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
```

We also want to disable the splash screen when we start emacs as it doesn't serve much of a purpose.

```emacs-lisp
(setq inhibit-startup-message t)
```

### Line numbering ###

Line numbering is also sometimes useful. At the moment it is off by default. I go through phases of wanting it and not wanting it. Hence at the moment it is commented out but it can be started on a per buffer basis with `M-x linum-mode` or globally with `M-x global-linum-mode`.

```emacs-lisp
(use-package linum
  :init
  (global-linum-mode -1))
```

### Theme ###

Here we load the color theme for emacs. I really enjoy the [base16 themes][]. 

First we add the `themes` directory to the load-path and then set the theme. The keyword `:no-confirm` means that emacs wont constantly ask if it is safe to run the lisp code in the theme.

[base16 themes]: https://github.com/chriskempson/base16

```emacs-lisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'base16-brewer-light :no-confirm)
```

### Font ###

Change the default font to Menlo. Ideally I should check that this is actually installed but I haven't got around to doing this.

```emacs-lisp
(add-to-list 'default-frame-alist '(font . "Menlo-12"))
```

### Mode line ###

The default mode line is ugly and cluttered. [Smart-mode-line][] is a nice solution which make the mode line a bit more readable. There are light and dark themes but I like to use respectful which respects my current choice of color theme. The variable `sml/hidden-modes` takes a regex argument and hides all matching minor modes, since I don't want to see any minor modes I hide them all.

[Smart-mode-line]: https://github.com/Bruce-Connor/smart-mode-line

```emacs-lisp
(use-package smart-mode-line
  :config
  (progn
    (load-theme 'smart-mode-line-respectful :no-confirm)
    (setq sml/theme nil
          sml/hidden-modes "\\([A-z]\\|[-]\\)*")
    (sml/setup)))
```

## Emacs behaviour ##

The following are just some settings affecting the general behaviou of emacs.

The bell is annoying so we turn it off.

```emacs-lisp
(setq ring-bell-function 'ignore)
```


### Backups and autosaves ###

The way emacs handles backup file is annoying also. It saves a file ending in `"~"`. We could just turn this off but Almost certaily I will regret this at some point when I loose some data. Hence we just stick all backups in a directory `~/.emacs.d/backups` and the same with autosave files in `~/.emacs.d/autosaves`. We also turn on version control so we keep a number of backup files.

```emacs-lisp
(setq backup-by-copying t 
      backup-directory-alist
      '((".*" . "~/.emacs.d/backups")) 
      auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosaves/" t)) 
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
```

### Command history ###

Saving command history across emacs sessions is really useful. History is saved to the `~/.emacs.d/savehist` file

```emacs-lisp
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/savehist"
      history-length 100
      history-delete-duplicates t
      savehist-additional-variables '(search-ring
                                      regexp-search-ring))
```

### Save place in file ###

It is very useful for emacs to save the place of the cursor in the file so that when we open it back up again we return to the last position we were editing. To to this we use [saveplace][]. The buffer-local variable `save-place` can be set globally using `setq-default` so this is what we do. The list of places is kept in a file of the same name.

[saveplace]: http://www.emacswiki.org/emacs/SavePlace

```emacs-lisp
(use-package saveplace
  :init
  (progn
    (setq-default save-place t)
    (setq save-place-file "~/.emacs.d/places")))
```



### Indentation and tabs ###

Tab characters are annoying so we turn them off and make sure the default indent is 4 spaces. We also bind the return key to newline and indent.

```emacs-lisp
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)
```

### Windmove ###

[Windmove][] is a mode that lets you move buffers with `Shift-<arrow>` which is much easier than `C-x o`. This conflicts with `markdown-promote` but I don't use this often enough to car. Requiring windmove gives access to the functions `windmove-up` etc but the command `windmove-default-keybindings` sets the `Shift-<arrow>` bindings.

I also bind `C-x C-b` to `buffer-menu`instead of `buffer-list`.

[Windmove]: http://www.emacswiki.org/emacs/WindMove

```emacs-lisp
(use-package windmove
  :bind
  ("C-x C-b" . buffer-menu)
  :init
  (windmove-default-keybindings))
```

### Recent files ###

Maintain a list of recent files using [recentf-mode][]. This is fairly self explanitory. We access the list using `C-x C-r`. This conflicts with open read only but I have no use for this.

[recentf-mode]: http://www.emacswiki.org/emacs/RecentFiles

```emacs-lisp
(use-package recentf
  :bind ("C-x C-r" . recentf-open-files)
  :init
  (progn
    (recentf-mode t)
    (setq recentf-max-menu-items 25)))
```

### Undo-tree ###

Emacs' undo function isn't the most intuitive or easy to use. I like using [undo-tree-mode][] which replaces the keybinding `C-x u` and calls a graphical interface to navigating undo's and redo's in a tree structure.

[undo-tree-mode]: http://www.emacswiki.org/emacs/UndoTree

```emacs-lisp
(use-package undo-tree
  :init
  (global-undo-tree-mode))
```

### Line breaking  ###

Almost always I want lines to break at words rather than half way through a word. [Visual-line-mode][] acheives this nicely.

[Visual-line-mode]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Visual-Line-Mode.html

```emacs-lisp
(global-visual-line-mode 1)
```

### Spelling ###

The package [flyspell][] enables on-the-fly spell checking. It is fairly intelligent and ignores latex commands etc. my default keybinding to correct the word at point is `C-'`.

We also make sure flyspell starts by default in LaTeX and markdown modes.

[flyspell]: http://www.emacswiki.org/emacs/FlySpell

```emacs-lisp
(use-package flyspell
  :bind
  ("C-'" . ispell-word)
  :config
  (progn
    (setq ispell-dictionary "british")
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'markdown-mode-hook 'flyspell-mode)))
```



### Popwin ###

[Popwin][] manages popup buffers such as `*Completions*`.

[Popwin]: https://github.com/m2ym/popwin-el

```emacs-lisp
(use-package popwin
  :init
  (popwin-mode 1))
```


## Editing ##

In this section I load packages useful for general editing

### Auto-complete ###

For global auto-completion I use [auto-complete-mode][]. The function `ac-config-default` sets sane default setting for auto-complete, inclusing starting automatically for certain modes such as emacs-lisp-mode.

The variable `ac-use-fuzzy` allows fuzzy matching for the completeing list. The variable `ac-auto-start` takes an integer and only allows completion once th string is longer than tis value. The `ac-trigger-key` is the key bound to `auto-complete`. Setting `ac-use-menu-map` allows me to use `C-n` etc to navigate the menu.

The file `ac-ignores` which is loaded defines words which auto-complete should not try and complete. This is an autogenerated list of yasnippet keys. This solves the issue of the clash for the tab key between auto-complete and yasnippet.

The modes for which auto-complete will start automatically for are defined in the variable `ac-modes`. We can add to this list using the `add-to-list` function. Below we add markdown-mode and latex-mode to the list.

[auto-complete-mode]: http://cx4a.org/software/auto-complete/

```emacs-lisp
(use-package auto-complete-config
  :config
  (progn
    (ac-config-default)
    (setq ac-use-fuzzy t
          ac-auto-start 2
          ac-trigger-key "TAB"
          ac-use-menu-map t)
    (ac-flyspell-workaround)
    (add-to-list 'ac-modes 'markdown-mode)
    (add-to-list 'ac-modes 'LaTeX-mode))
  :idle
  (load "~/.emacs.d/ac-ignores"))
```

### Smartparens ###

[Smartparens-mode][] is a mode for intelligent parenthesis (and other pairs) matching. It is ver extensible and you can define your own pairs. It has some nifty navigation commands which I should learn at some point and make key bindings for.

To define custom pairs the syntax at its most basic is `(sp-local-pair MODE "LEFT" "RIGHT")` we can add `:actions :rem` and substitute `nil` for `"RIGHT"` to delete the definition of a pair locally. 

[Smartparens-mode]: https://github.com/Fuco1/smartparens

```emacs-lisp
(use-package smartparens
  :init
  (smartparens-global-mode t))
```


### Expand region ###

Selecting regions intelligently is very useful, [Expand region][] allows to to incrementally increas and decrease the region selected in a smart way. Because this is so useful I have bound `er/expand-region` to `C-=` and `er/contract-region` to `C-+`. This is not intuitive.

[Expand region]: https://github.com/magnars/expand-region.el

```emacs-lisp
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-+" . er/contract-region)))
```

### Yasnippet ###

[Yasnippet][] is a template system. I use it mostly with LaTeX. Personal snippets are saved in `~/.emacs.d/snippets`, this is the default place.

[Yasnippet]: https://github.com/capitaomorte/yasnippet

```emacs-lisp
(use-package yasnippet
  :idle
  (yas-global-mode 1))
```

## Markdown ##

I use  [markdown-mode+][], which is an extension of [markdown-mode][]. 

[markdown-mode+]: https://github.com/milkypostman/markdown-mode-plus

[markdown-mode]: http://jblevins.org/projects/markdown-mode/

```emacs-lisp
(use-package markdown-mode
			 :mode "\\.md\\'")
```


## LaTeX ##

Since I am a mathematician I use latex a lot hence lots of configuration to do.

### AUCTeX ###

[AUCTeX][] is the major mode for editing LaTeX files. Here I first make sure that emacs recognises XeLaTeX and has latex in its load path. Then I load various sources for auto-complete. I also set up some default environments which I use a lot and have it load the `ac-math` and `auto-complete-auctex` packages.

The packages [ac-math][] and [auto-complete-auctex][] add auto-complet sources for common math symbols and auctex commands.

[AUCTeX]: http://www.gnu.org/software/auctex/

[ac-math]: https://github.com/vitoshka/ac-math

[auto-complete-auctex]: https://github.com/monsanto/auto-complete-auctex



```emacs-lisp
(use-package tex-site
  :config
  (progn
    (setq TeX-engine 'xetex
          TeX-view-program-list '(("Preview" "open %o"))
          exec-path (append exec-path '("/usr/texbin"))
          ac-source (append '( ac-source-math-latex 
                               ac-source-latex-commands
                               ac-source-math-unicode
                               ac-source-yasnippet)
                            ac-sources))
    (setenv "TEXINPUTS" ".:~/latex:")
    (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin")))
  :idle
  (progn
    (add-hook 'LaTeX-mode-hook 
              (lambda() 
                (add-to-list 
                 'TeX-command-list 
                 '("XeLaTeX" "%`xelatex%(mode) --shell-escape%' %t" 
                   TeX-run-TeX nil t)) 
                (setq TeX-command-default "XeLaTeX"
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
    (use-package ac-math)
    (use-package auto-complete-auctex)
    ))
```





### RefTeX ###


[RefTeX][] is a reference and citation manager for AUCTeX. I set `reftex-plug-into-AUCTeX` so that it behaves well with AUCTeX, setting `reftex-ref-macro-prompt` to `nil` gets rid of the annoying prompt when seaching for references and setting `reftex-bibliography-commands` allows me to use the `\addbibresource` command in my LaTeX documents. `reftex-label-alist` gives me quick access to looking for specific evironments to reference.

[RefTeX]: http://www.gnu.org/software/auctex/reftex.html


```emacs-lisp
(use-package reftex
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-ref-macro-prompt nil
        reftex-bibliography-commands '("bibliography"
                                       "nobibliography"
                                       "addbibresource")
        reftex-label-alist
        '(("Theorem" ?h "thm:" "~\\ref{%s}" t ("Theorem" "thm."))
          ("Lemma" ?l "lem:" "~\\ref{%s}" t ("Lemma" "lem."))
          ("Proposition" ?p "prp:" "~\\ref{%s}" t ("Proposition" "prp."))
          ("Definition" ?d "def:" "~\\ref{%s}" t ("Definition" "def."))
          ("Example" ?x "exm:" "~\\ref{%s}" t ("Example" "exm."))
          ("Exercise" ?s "ecs:" "~\\ref{%s}" t ("Exercise" "ecs."))
          ("Conjecture" ?C "coj:" "~\\ref{%s}" t ("Conjecture" "coj."))
          ("Corollary" ?c "cor:" "~\\ref{%s}" t ("Corollary" "cor."))
          ("Remark" ?r "rem:" "~\\ref{%s}" t ("Remark" "rem."))
          ("Problem" ?o "prb:" "~\\ref{%s}" t ("Remark" "prb.")))
        ))
```





## Programming ##

Mode specific to programming languages.

### Magma ###

Magma is a computer algebra package, the package [magma-mode][] provides syntax highlighting and indentation as well as the ability to interact with a magma process.

[magma-mode]: https://github.com/ThibautVerron/magma-mode

```emacs-lisp
(use-package magma-mode
  :mode "\\.m\\'"
  :pre-load
  (add-to-list 'load-path "~/.emacs.d/site-lisp/magma-mode"))
```

### Haskell ###

Here I load Haskell mode. At the moment there is no fancy configuration.

```emacs-lisp
(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    ))
```


### Python ###

I want to load python when I am editting sage files

```emacs-lisp
(use-package python-mode
  :mode "\\.sage\\'"
  )
```



## Multiple major modes ##

Often it is useful to have mutliple major modes in a single buffer. This markdown file is an example. I want to be able to edit the markdown in markdown-mode and the elisp in emacs-lisp-mode. To achieve this I use [mmm-mode][].

We don't have both major modes activated at once. Instead mmm-mode seperates the major modes into different regions of the buffer. A submode defines a set of major modes which can be activated in regions of a buffer which can be described unsing regex.

For example, below I use `mmm-add-class` to add such a submode called "markdown". It recognised regions of fenced code blocks (the regex defining this is set by `:front` and `:back`), the `:front` regex is then fed into the function `mmm-markdown-get-mode` detirmines which major mode to use. For example

    ```python
    ...
    ```

would be detected as a region which should use python-mode. To have mmm-mode scan the buffer for regions to mmm-ify, use the function `mmm-parse-buffer` which is bound to `C-c % C-b`.

The code used to do this is taken straight from the definition of the here-document submode which is built in. It is only very slightly changed.

The only problem I have experecned with this is that indentation does not seem to work so well in the submode regions. As a work-around I usually ahve another buffer open where I do the coding and then copy and past it into the markdown file.

[mmm-mode]: https://github.com/purcell/mmm-mode

```emacs-lisp
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
```

## Key bindings ##

```emacs-lisp

```
