#!/bin/sh

SED=$1
RM=$2

for file in *.ad?; do
  ${SED} -e '/with CORBA.TypeCode;/ D' \
         -e '/use CORBA.TypeCode;/ D' \
         -e 's/with CORBA.TypeCode.Stream; use CORBA.TypeCode.Stream;/with Broca.CDR; use Broca.CDR;/' \
         -e 's/CORBA.TypeCode.Ref/CORBA.TypeCode.Object/g' \
         -e '/with CORBA.Stream; use CORBA.Stream;/ D' \
         -e '/with CORBA.TypeCode.Helper;/ D' \
         -e 's/TypeCode.Helper.//g' < $file > $file.new
  mv $file.new $file
done
${RM} -f corba.ads corba-typecode*
${RM} corba-stream.ad?

