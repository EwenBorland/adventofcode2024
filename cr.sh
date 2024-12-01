#!/bin/sh

day=$1
dir=day_${day}

#compile
ghc $dir/main.hs -o build/$dir || echo "failed to compile" ; exit

#run
. /build/$dir.exe ${@:2}

# cleanup
rm $dir/*.hi
rm $dir/*.o
