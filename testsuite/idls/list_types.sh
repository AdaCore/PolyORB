# !/bin/sh
# This script tests the Types backend of IAC

DIR=`dirname $1`
FILE=`basename $1`
RESULT="`basename $1 .idl`.typ"

cd $DIR
iac-types $FILE
cat $RESULT
rm $RESULT

