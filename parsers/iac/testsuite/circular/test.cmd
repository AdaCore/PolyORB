#!/bin/sh
TMP=.tmp_rep/
LOG=log.test
mkdir $TMP 
cd $TMP
cp ../../Makefile.ada ./Makefile > /dev/null
for i in `/bin/ls ../*.idl`; do
iac -ada -i $i > iac.ada
gnatchop -w iac.ada > /dev/null
rm -f iac.ada
done

make > /dev/null 2>$LOG
CODE=$?
if [ $CODE != 0 ]; then
    cat $LOG
fi; 
cd ..
rm -fr $TMP
exit $CODE
