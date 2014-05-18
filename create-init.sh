#!/bin/bash
sed -n '/^```emacs-lisp/,/^```/ p' < ~/.emacs.d/init.md | sed '/^```/ d' > ~/.emacs.d/init.el
