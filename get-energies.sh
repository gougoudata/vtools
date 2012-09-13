#!/bin/sh

# backup old data file
date=`date +"%Y-%m-%d-%H-%M-%S"`
mkdir ./data/
mkdir ./data/old/
mv ./data/energies.dat ./data/old/energies-$date.dat

# create new data file
for dir in $( ls ); do
    if (tail $dir/OUTCAR | grep "Voluntary"); then 
	grep 'free  energy' $dir/OUTCAR | tail -1 | awk -v dir=${dir} '{print dir " " $5}' >> ./data/energies.dat ;
    fi
done

