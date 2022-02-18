#!/bin/sh
# File "grab_files.sh" by Nathaniel Bock for CSE250, Spring 2022
# Grabs the example scala files from Dr. Kenneth Regan's website
# And keeps the ones that have a header

EXAMPLES="https://cse.buffalo.edu/~regan/cse250/ScalaSamples/"

wget -q -r -np -nH --cut-dirs=1 -P download $EXAMPLES

if [ ! -d ./ScalaSamples ]
then
    mkdir ./ScalaSamples
fi
mv download/cse250/ScalaSamples/*.scala ./ScalaSamples/
rm -rf download

# As per Dr. Regan's request/suggestion, any file without a header is probably
# not finished. Therefore this will remove all files without a header.
# Using regex... Hopefully it does not vary much.

# Reference: https://www.digitalocean.com/community/tutorials/workflow-loop-through-files-in-a-directory

for FILE in ./ScalaSamples/*
do
    grep -E "/[*][*].*File.*[.]scala.*KWR" $FILE &>/dev/null
    EXIT=$?
    if [ $((EXIT)) -eq 1 ]
    then
        rm $FILE
    fi
done