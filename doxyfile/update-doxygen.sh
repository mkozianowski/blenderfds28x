#!/bin/bash

#generation of new doxygen
doxygen doxyfile

#moving to branch gh-pages
git checkout gh-pages
git pull

#replacing current doxygen
rm -rf  ../docs/html
mv html ../docs/

#commit
git add ../docs/html
COMMITMESSAGE="Doxygen update $(date)"
git commit -m "$COMMITMESSAGE"
git push

#restoring master
git checkout master
