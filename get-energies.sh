#!/bin/sh
mkdir ./data ;
grep -r  'free  energy' . | sort | awk '{gsub(/\.\//,"",$1); gsub(/\/.*/,"",$1); print $1 " " $6}' > ./data/energies.dat ;
