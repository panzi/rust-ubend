#!/bin/bash

set -e

cargo doc --no-deps
git checkout gh-pages
rm -rv *.woff *.svg *.js *.txt *.css src/ubend implementors ubend
cp -r target/doc/* .
git commit -a -m "updated documentation"
git push
