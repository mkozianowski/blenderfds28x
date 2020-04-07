#!/bin/bash

# Generation of new doxygen
echo "Doxygen generation ..."
doxygen doxyfile > doxygen.log 2>&1

# Moving to branch gh-pages
echo "Pulling gh-pages branch ..."
git checkout gh-pages
git pull

# Replacing current doxygen
echo "Deploying doxygen ..."
rm -rf  ../docs/html
mv html ../docs/

# Commit
echo "Commit doxygen ..."
git add ../docs/html
COMMITMESSAGE="Doxygen update $(date)"
git commit -m "$COMMITMESSAGE"
git push

# Restoring master
git checkout master
