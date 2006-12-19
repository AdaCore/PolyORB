# !/bin/sh
# 
# This script builds a test for SII use
# The command line options are :
# sii : build test using SII
# dii : build test using DII
# clean : cleanup

MAKEFILE_SII=Makefile.SII
MAKEFILE_DII=Makefile.DII

if [ $# -ne 1 ]; then
   echo "Usage: $0 request_hadling_mode" ;
   echo "    request_hadling_mode :" ;
   echo "       sii : build test using SII" ;
   echo "       dii : build test using DII" ;
   echo "       clean : cleanup" ;
   exit 1 ;
else
   case "$1" in
   "sii")
      make -f $MAKEFILE_SII clean ;
      make -f $MAKEFILE_SII ;;
   "dii")
      make -f $MAKEFILE_DII clean ;
      make -f $MAKEFILE_DII ;;
   "clean")
      make -f $MAKEFILE_SII clean ;
      make -f $MAKEFILE_DII clean ;;
   *)
      echo "You must give a valid optimization mode";
      exit 1;;
   esac
fi;

