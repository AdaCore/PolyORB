#!/bin/sh
TMP=.tmp_rep/
LOG=log.test
mkdir $TMP 
cd $TMP
idlac ../tin.idl > /dev/null 2> /dev/null
idlac -i ../tin.idl > /dev/null 2> /dev/null 
cp ../../Makefile.ada ./Makefile > /dev/null
iac -ada -h ../tin.idl > iac.ada
gnatchop -w iac.ada > /dev/null

# Effacer tous les fichiers .adb qui n'ont pas un fichier .ads associe. 
for f in `ls *.adb`
do 
  ff=`basename $f .adb`.ads
  ls $ff 2> /dev/null >&2
  a=$?
  if [ $a = 1 ]
  then 
    rm $f
  fi
done

make > /dev/null 2>$LOG
CODE=$?
if [ $CODE != 0 ]; then
    cat $LOG
fi; 
cd ..
rm -fr $TMP
exit $CODE