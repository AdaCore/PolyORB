#!/bin/sh
TMP=.tmp_rep/
LOG=log.test
mkdir $TMP 
cd $TMP
cp ../../Makefile.ada ./Makefile > /dev/null
iac -I../ -ada -i ../int1.idl
iac -I../ -ada -i ../int2.idl
iac -I../ -ada -i ../int3.idl

make > /dev/null 2>$LOG
CODE=$?
if [ $CODE != 0 ]; then
    cat $LOG
fi; 
cd ..
rm -fr $TMP
exit $CODE
