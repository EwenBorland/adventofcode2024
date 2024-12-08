#!/bin/sh
# creates a folder and template files for the input day

if (($# < 1)); then
    echo "Please provide an ID"
    exit 1
fi


day=$(printf %02d $1)
dir=Day$day

mkdir data/$dir
touch src/$dir.hs
touch data/$dir/input.txt
touch data/$dir/sample.txt

cat > src/$dir.hs << EOF
module $dir where

import Tools.Parsing

run ::  IO()
run = do
    let filePath = "data/$dir/sample.txt"
    fileData <- parseFile filePath
    putStrLn "Day $day solution has not been implemented yet"
EOF

t="    "
cabalKey="--key"
mainKey1="--endofimports"
mainKey2="| otherwise"
sed -i -e "s/$cabalKey/$dir\n$t$t$cabalKey/g" "adventofcode2024.cabal"
sed -i -e "s/$mainKey1/import $dir\n$mainKey1/g" "app/Main.hs"
sed -i -e "s/$mainKey2/| n == $1 = $dir.run\n  $mainKey2/g" "app/Main.hs"
