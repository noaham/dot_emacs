# emacs.d

These are my personal emacs configuration files. Feel free to use any of it as you see fit.

Many of the configuration snippets have been stolen from various sources including:  
[Emacs wiki](http://www.emacswiki.org)  
[Emacs-fu](http://emacs-fu.blogspot.co.uk)  
[What the emacs.d?](http://whattheemacsd.com)  
and many other sources that I cannot remember.

## Packages loaded

The following packages are loaded

+ **[saveplace](http://www.emacswiki.org/emacs/SavePlace)** when opeing a file returns the cursor to the last position.
+ **[uniquify](https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html)** better buffer names.
+ **[recentf](https://www.gnu.org/software/emacs/manual/html_node/emacs/File-Conveniences.html)** maintains a list of recently visited files.
+ **[undo-tree](http://www.dr-qubit.org/emacs.php#undo-tree-docs)** visual undo.
+ **[diminish](http://www.emacswiki.org/emacs/DiminishedModes)** hide minor modes in mode line.
+ **[visual-line-mode](http://www.gnu.org/software/emacs/manual/html_node/emacs/Visual-Line-Mode.html)** wrap lines intelligently, don't break words when possible.
+ **[AUCTeX](https://www.gnu.org/software/auctex/)** excellent LaTeX editing mode.
+ **[RefTex](http://www.gnu.org/software/auctex/reftex.html)** support for citations and references in LaTeX.
+ **[yasnippet](https://github.com/capitaomorte/yasnippet)** insert and complete snippets.
+ **[auto-complete](http://cx4a.org/software/auto-complete/)** does what it says on the tin.
+ **[smartparens](https://github.com/Fuco1/smartparens)** automatic parenthesis matching.
+ **[markdown-mode+](https://github.com/milkypostman/markdown-mode-plus)** mode for editing markdown, extension of [markdown-mode](http://jblevins.org/projects/markdown-mode/).

## Issues

There are two main issues that need to bee sorted out.

### TAB key

The biggest problem I encountered was that the `TAB` key is bound to three different
functions:

+ yasnippet expand
+ auto-complete
+ indent

I would like to	 use `TAB` for all of these in the above order of precedence. However 
it seems that automcomplete for some reason takes priority no matter what I do. The 
way I solved this was to add `ac-source-yasnippet` to my `ac-sources` list and to add 
my yas keys to `ac-ignores`. This works quite well.


### Slow load

Emacs is slow to load. Since I want to load so many packages this is just a fact of life. However loading `auto-complete-auctex` takes this from 2 seconds to 6. The solution is to run `emacs --daemon` on login and then run `emacsclient -n -c` to produce a new gui emacs window. However I am running [Yamamoto Matsumura's OSX port](https://github.com/railwaycat/emacs-mac-port) (which is excellent) and this is not supported. Guess I just wont ever shut emacs down!


