# !/bin/sh
# This script runs the test given to the command line and displays the
# result

# There are 3 test cathegories :
# 1 - Frontend tests    : IDL tree tests and error messages tests
# 2 - Ada Backend tests : Single File tests and multi files tests
# 3 - Types Backend tests

# The test cathegory is given in the command line :
# run_test.sh <test>:<cathegory>

# The test name
TEST_NAME=`echo $1 | awk 'BEGIN { FS=":"}{print $1} '`

# The test cathegory
TEST_CATHEGORY=`echo $1 | awk 'BEGIN { FS=":"}{print $2} '`

# Setting environment variables
DIR=`dirname $0`
PATH=$PWD:$PWD/$DIR:$PATH

if [ -d $TEST_NAME ]; then
  DIR=$TEST_NAME;
  FILE=`ls $DIR/*.idl| awk 'BEGIN{IFS="/"}{print $NF;exit}'`
else
  DIR=`dirname $TEST_NAME`;
  FILE=`basename $TEST_NAME`
fi

# Copy the test script corresponding to the test cathegory

if [ x$TEST_CATHEGORY = xada_backend ]; then
  echo "$TEST_NAME : Aad backend test !";
elif [ x$TEST_CATHEGORY = xidl_frontend ]; then
  echo "$TEST_NAME : IDL frontend test !";
elif [ x$TEST_CATHEGORY = xtypes_backend ]; then
  echo "$TEST_NAME : Types Backend test !";
else
  echo "$1 : Invalid test cathegory !";
  exit 1;
fi

exit 0;



TEST=`basename $DIR`
LOG=/tmp/$TEST.log
cd $DIR
if [ -f test.cmd ]; then
  sh test.cmd >$LOG 2>&1
else
  iac -idl $FILE >$LOG 2>&1
fi
if [ -f test.out ]; then
  diff test.out $LOG
else
  test ! -s $LOG
fi
CODE=$?
if [ $CODE != 0 ]; then
  echo "$DIR FAILED" | awk '{printf ("%-20s%20s\n", $1, $2)}'
  echo "--------------- expected output ------------------"
  if [ -f test.out ]; then
     cat test.out
  fi
  echo "---------------- actual output -------------------"
  cat $LOG
  echo "--------------------------------------------------"
fi;
rm $LOG 
exit $CODE
