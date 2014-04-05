#!/bin/bash
# Short script to collect the yas keys and format them into a list
# to be set as ac-ignores in init.el

# make sure we are in .emacs.d
cd ~/.emacs.d/

touch keys

#create list of keys
grep -hr '# key: ' ./elpa/yasnippet-20140314.255/snippets/ > keys
grep -hr '# key: ' ./snippets/ >> keys

# Use this list of keys to produce ac-ignores.el
echo "(setq ac-ignores '(" > ac-ignores.el

# loop though keys file, extract keys and write to ac.ignore
while read x
do
    echo -n "\"" >> ac-ignores.el
    echo -n $(echo $x | cut -c 8-) >> ac-ignores.el
    echo "\"" >> ac-ignores.el
done < keys

# finish file
echo "))" >> ac-ignores.el

# clean up
rm keys
