#!/bin/sh

day=$1
dir=day_${day}

#compile
ghc $dir/code.hs -o build/$dir

#run

./build/$dir

# cleanup
rm $dir/*.hi
rm $dir/*.o
