# !/bin/sh
# This script test the Ada code generation of IAC. After executing
# IAC on a file (or on many files), it compiles the generated code
# The test is considered successful if the Ada code is generates and 
# if it compiles correctly

if [ -d $1 ]; then
  DIR=$1;
  FILES=`ls $DIR/*.idl| awk 'BEGIN{FS="/"}{print $NF;exit}'`
else
  DIR=`dirname $1`;
  FILES=`basename $1`
fi

cd $DIR

# Generates code in a temporary directory

TMP=.tmp_rep/
LOG=log.test
mkdir $TMP 
cd $TMP

# compile generated code

cp ../../Makefile.ada ./Makefile > /dev/null
for i in $FILES; do
   iac -ada -i ../$i
done

make > /dev/null 2>$LOG
CODE=$?
if [ $CODE != 0 ]; then
   cat $LOG
fi;
 
cd ..
rm -rf $TMP
exit $CODE

