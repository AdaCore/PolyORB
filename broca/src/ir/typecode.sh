#!/bin/sh

for file in `ls *.ad?`; do
sed '/with CORBA.TypeCode;/ D' $file > ${file}.old
sed '/use CORBA.TypeCode;/ D' ${file}.old > $file
sed 's/with CORBA.TypeCode.Stream; use CORBA.TypeCode.Stream;/with Broca.CDR; use Broca.CDR;/g'  $file > ${file}.old
sed 's/CORBA.TypeCode.Ref/CORBA.TypeCode.Object/g'  ${file}.old > $file
sed '/with CORBA.Stream; use CORBA.Stream;/ D' $file > ${file}.old
sed '/with CORBA.TypeCode.Helper;/ D' ${file}.old > $file
sed 's/TypeCode.Helper.//g'  $file > ${file}.old
mv  ${file}.old $file 
done
rm corba.ads corba-typecode*
rm corba-stream.ad?

