#!/bin/sh
# This script parse an IDL file

DIR=`dirname $1`
FILE=`basename $1`

cd $DIR
iac -idl $FILE

