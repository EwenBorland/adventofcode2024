#!/bin/sh

day=$1
dir=day_${day}

#compile
echo "building $dir/$dir.hs"
ghc $dir/$dir.hs -o build/$dir || { echo "failed to compile"; exit; }

#run
echo "running /build/$dir.exe ${@:2} "
./build/$dir.exe ${@:2} || { echo "failed to run" ; exit; }

# cleanup
rm $dir/*.hi
rm $dir/*.o