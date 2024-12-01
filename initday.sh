#!/bin/sh
# creates a folder and template files for the input day

if (($# < 1)); then
    echo "Please provide an ID"
    exit 1
fi


day=$1
dir=day_$day
mkdir $dir
touch $dir/main.hs
touch $dir/input.txt


