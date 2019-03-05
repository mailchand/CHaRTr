#!/bin/bash

git status
git add $1
git commit -m '[Chand] Updating results'
git pull
git push
