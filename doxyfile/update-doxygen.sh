#!/bin/bash

#generation of new doxygen
echo "Doxygen generation ..."
doxygen doxyfile > doxygen.log 2>&1

#moving to branch gh-pages
echo "Pulling gh-pages branch ..."
git checkout gh-pages
git pull

#replacing current doxygen
echo "Deploying doxygen ..."
rm -rf  ../docs/html
mv html ../docs/

#commit
echo "Commit doxygen ..."
git add ../docs/html
COMMITMESSAGE="Doxygen update $(date)"
git commit -m "$COMMITMESSAGE"
git push

#restoring master
git checkout master
