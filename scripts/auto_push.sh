#!/bin/sh
# File "auto_push.sh" by Nathaniel Bock for CSE250, Spring 2022
# When called will grab any new examples/changes and push changes to github

DATE=$(date)

git pull

./scripts/grab_files.sh

git add .
git commit -m "Updated at ${DATE}"
git push
