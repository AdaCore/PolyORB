#!/bin/sh

SED=$1; shift
RM=$1; shift

for file in "$@"; do
  ${SED} -e '1i\
pragma Warnings (Off);' \
         < $file > $file.new
  mv $file.new $file
done
