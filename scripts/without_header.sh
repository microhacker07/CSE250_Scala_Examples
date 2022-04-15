#!/bin/sh
# File "without_header.sh" by Nathaniel Bock for CSE250, Spring 2022
# Used to check which files at ~regan/cse250/ScalaSamples/ do not have a header
# Based on "grab_files.sh"

URL="https://cse.buffalo.edu/~regan/cse250"

FOLDERS=("ScalaSamples" "MaxWords" "DataStructures")

WORKDIR=$(pwd)
echo "Files without a header:"
for i in "${FOLDERS[@]}"
do
    wget -q -r -np -nH --cut-dirs=1 -P download "$URL/$i/"

    cd "./download/cse250/$i/"

    for FILE in *.scala
    do
        grep -E "/[*][*].*File.*[.]scala.*KWR" $FILE &>/dev/null
        EXIT=$?
        if [ $((EXIT)) -eq 1 ]
        then
            echo "$i/$FILE"
        fi
    done

    cd $WORKDIR
done

# rm -rf download
