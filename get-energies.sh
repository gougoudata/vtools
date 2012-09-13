#!/bin/sh

date=`date +"%Y-%m-%d-%H-%M-%S"`
mkdir ./data ;

for dir in $( ls ); do
    if (tail $dir/OUTCAR | grep "Voluntary"); then 
	grep 'free  energy' $dir/OUTCAR | awk -v dir=${dir} '{print dir " " $5}' >> ./data/energies-$date.dat ;
    fi
done

