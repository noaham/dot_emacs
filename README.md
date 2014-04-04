These are my personal emacs configuration files. Feel free to use any of it as you see fit.

Many of the configuration snippets have been stolen from various sources including:  
[Emacs wiki](http://www.emacswiki.org)  
[Emacs-fu](http://emacs-fu.blogspot.co.uk)  
[What the emacs.d?](http://whattheemacsd.com)  
and many other sources that I cannot remember.


The biggest problem I encountered was that the `TAB` key is bound to three different
functions:

+ yasnippet expand
+ auto-complete
+ indent

I would like to	 use `TAB` for all of these in the above order of precedence. However 
it seems that automcomplete for some reason takes priority no matter what I do. The 
way I solved this was to add `ac-source-yasnippet` to my `ac-sources` list and to add 
my yas keys to `ac-ignores`. This works quite well.