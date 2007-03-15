# !/bin/sh
# This script runs the test given to the command line and displays the
# result

# There are 3 test categories :
# 1 - Frontend tests    : IDL tree tests
# 2 - IDL errors tests
# 2 - Ada Backend tests : Single File tests and multi files tests
# 3 - Types Backend tests

# The test category is given in the command line :
# run_test.sh <test>:<category>

# The test name
TEST_NAME=`echo $1 | awk 'BEGIN { FS=":"}{print $1} '`

# The test category
TEST_CATEGORY=`echo $1 | awk 'BEGIN { FS=":"}{print $2} '`

# Setting environment variables
DIR=`dirname $0`
PATH=$PWD:$PWD/$DIR:$PATH

if [ -d $TEST_NAME ]; then
  DIR=$TEST_NAME;
else
  DIR=`dirname $TEST_NAME`;
fi

# Copy the test script corresponding to the test category

TEST_SCRIPT=
TEST_MSG=
if [ x$TEST_CATEGORY = xada_backend ]; then
  TEST_SCRIPT=compile_files.sh;
elif [ x$TEST_CATEGORY = xidl_frontend ]; then
  TEST_SCRIPT=parse_file.sh;
elif [ x$TEST_CATEGORY = xidl_errors ]; then
  TEST_SCRIPT=test_errors.sh;
elif [ x$TEST_CATEGORY = xtypes_backend ]; then
  TEST_SCRIPT=list_types.sh;
else
  echo "$1 : Invalid test category !";
  exit 1;
fi

# Execute the script

LOG=/tmp/$TEST.log

./$TEST_SCRIPT $TEST_NAME  >$LOG 2>&1

cd $DIR;
if [ -f "test.out" ]; then
  diff test.out $LOG #> /dev/null
else
  test ! -s $LOG
fi
CODE=$?

if [ $CODE != 0 ]; then
  echo "$TEST_NAME FAILED" | awk '{printf ("%-30s%20s\n", $1, $2)}'
  echo "--------------- expected output ------------------"
  if [ -f "test.out" ]; then
     cat test.out
  fi
  echo "---------------- actual output -------------------"
  cat $LOG
  echo "--------------------------------------------------"
else
  echo "$TEST_NAME : PASSED";
fi;
rm $LOG
exit $CODE

