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
rm -f *idl_file*
rm -f m1-skel*
make > /dev/null 2>$LOG
CODE=$?
if [ $CODE != 0 ]; then
    cat $LOG
fi; 
cd ..
rm -fr $TMP
exit $CODE