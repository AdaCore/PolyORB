#! /bin/sh
#
# $Id: //depot/adabroker/main/broca/Utils/mangle.sh#1 $
#
# Usage: mangle.sh [-d]
#

base=mangle$$
temp=/tmp/$base

if [ x$1 = "x-d" ]; then
  (echo "begin 600 $base.gz"; cat; echo "end") | \
    (cd /tmp && uudecode && gunzip -f < $base.gz > $base && \
       cat $base && rm $base)
elif [ x$1 = "x-x" ]; then
  sed -e '1,/BEGIN\]/d' -e '/END\]/,$d' | $0 -d
else
  cat > $temp && gzip -9 $temp
  uuencode $temp.gz $temp.gz | sed -e '1d' -e '$d'
  rm $temp.gz
fi
