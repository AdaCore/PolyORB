#!/bin/sh

make

n=0
for i in `cat testsuite/MANIFEST | grep -v '^#'`; do
  testsuite/run-test testsuite/$i
  if [ $? != 0 ]; then
    n=`expr $n + 1`
  fi
done
exit $n

