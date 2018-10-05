#!/bin/bash
dir="guisdap9"
excludeobj="lib mex7"
excludelib="mexsources models"
excludeP="exps/*/*P*.mat*"

xclude=''
for x in $excludeobj $excludelib $excludeP
do 
 xclude="$xclude --exclude=$dir/$x"
done
tar --exclude-vcs --exclude-backups $xclude -cf - $dir | bzip2 -c9 > $dir.tar.bz2
