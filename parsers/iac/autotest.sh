#!/bin/sh

make clean
if [ $? != 0 ]; then exit $?; fi
make
if [ $? != 0 ]; then exit $?; fi

n=0
for i in `cat testsuite/MANIFEST | grep -v '^#'`; do
  testsuite/run-test testsuite/$i
  if [ $? != 0 ]; then
    n=`expr $n + 1`
  fi
done
exit $n

