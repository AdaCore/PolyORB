#! /bin/sh
LOG=log~

make clean >/dev/null

make > /dev/null 2> $LOG

#./server  1> IOR.txt &

#sleep 1; 

#./client `cat IOR.txt` 

CODE=$?
if [ $CODE != 0 ]; then
    cat $LOG
fi; 

make clean >/dev/null
exit $CODE


