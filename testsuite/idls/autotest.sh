#!/bin/sh

n=0
for i in `grep -v '^#' MANIFEST`; do
  ./run-test.sh $i
  if [ $? != 0 ]; then
    n=`expr $n + 1`
  fi
done

exit $n
