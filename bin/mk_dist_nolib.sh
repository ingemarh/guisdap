#!/bin/bash
dir="guisdap9"
excludeobj="lib mex7"
excludelib="mexsources models"

xclude=''
for x in $excludeobj $excludelib
do 
 xclude="$xclude --exclude='$dir/$x'"
done
echo $xclude
#tar jcf guisdap9.tar.bz2 --exclude-vcs --exclude-backups $xclude guisdap9
echo tar --exclude-vcs --exclude-backups $xclude jcfv /dev/null guisdap9
#tar --exclude-vcs --exclude-backups $xclude jcfv /dev/null guisdap9
tar $xclude jcvf /dev/null guisdap9
