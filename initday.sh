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

run ::  IO()
run = do
    putStrLn "Day $day solution has not been implemented yet"
EOF

t="    "
cabalKey="--key"
sed -i -e "s/$cabalKey/$dir\n$t$t$cabalKey/g" "adventofcode2024.cabal"
