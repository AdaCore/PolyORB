#!/bin/sh

SED=$1; shift
RM=$1; shift

for file in "$@"; do
  ${SED} -e '1i\
pragma Warnings (Off);' \
         -e 's/\/CORBA\/Repository_Root\//\/CORBA\//g' \
         -e 's/IDL_SEQUENCE_CORBA_Repository_Root_/IDL_Sequence_CORBA_/g' < $file > $file.new
  mv $file.new $file
done
