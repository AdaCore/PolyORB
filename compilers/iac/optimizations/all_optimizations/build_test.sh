# !/bin/sh
# 
# This script builds a test for the IAC optimisations.

# The command line options are :
# none : generate a test without optimization
# all  : generate a test with all optimizations
# mem  : generate a test with memory space optimization

MAKEFILE_NOP=Makefile.no_opt
MAKEFILE_ALL=Makefile.all_opts
MAKEFILE=Makefile

CP=/bin/cp

if [ $# -ne 1 ]; then
   echo "Usage: $0 optimization_mode" ;
   echo "    optimization_mode :" ;
   echo "       none : generate a test without optimization" ;
   echo "       all  : generate a test with all optimizations" ;
   exit 1 ;
else
   case "$1" in
   "none")
      $CP -f $MAKEFILE_NOP $MAKEFILE ;;
   "all")
      $CP -f $MAKEFILE_ALL $MAKEFILE ;;
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

