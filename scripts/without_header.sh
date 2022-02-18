#!/bin/sh
# File "without_header.sh" by Nathaniel Bock for CSE250, Spring 2022
# Used to check which files at ~regan/cse250/ScalaSamples/ do not have a header
# Based on "grab_files.sh"

EXAMPLES="https://cse.buffalo.edu/~regan/cse250/ScalaSamples/"

wget -q -r -np -nH --cut-dirs=1 -P download $EXAMPLES

echo "Files without a header:"
for FILE in ./download/cse250/ScalaSamples/*.scala
do
    grep -E "/[*][*].*File.*[.]scala.*KWR" $FILE &>/dev/null
    EXIT=$?
    if [ $((EXIT)) -eq 1 ]
    then
        echo -e "\t$FILE"
    fi
done

rm -rf download