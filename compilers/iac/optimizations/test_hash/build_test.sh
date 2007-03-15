# !/bin/sh
# 
# This script builds a test for the minimal perfect hash function optimisation.
# The command line options are :
# none : generate a test without optimization
# cpu  : generate a test with CPU Time optimization
# mem  : generate a test with memory space optimization

MAKEFILE_NOP=Makefile.no_opt
MAKEFILE_CPU=Makefile.opt_cpu
MAKEFILE_MEM=Makefile.opt_mem
MAKEFILE=Makefile

CP=/bin/cp

if [ $# -ne 1 ]; then
   echo "Usage: $0 optimization_mode" ;
   echo "    optimization_mode :" ;
   echo "       none : generate a test without optimization" ;
   echo "       cpu  : generate a test with CPU Time optimization" ;
   echo "       mem  : generate a test with memory space optimization" ;
   exit 1 ;
else
   case "$1" in
   "none")
      $CP -f $MAKEFILE_NOP $MAKEFILE ;;
   "cpu")
      $CP -f $MAKEFILE_CPU $MAKEFILE ;;
   "mem")
      $CP -f $MAKEFILE_MEM $MAKEFILE ;;
   *)
      echo "You must give a valid optimization mode";
      exit 1;;
   esac
fi;

make clean ;
make ;
$CP -f "client" "client.$1"
$CP -f "server" "server.$1"

exit 0 ;

