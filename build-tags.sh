#!/bin/bash -vx
mkdir -p libs
cd libs
for p in `stack list-dependencies --separator -`; do
    stack unpack $p
done
cd ..
stack install hasktags
hasktags --etags libs
