#!/bin/sh
#
# This file generates any necessary constant for a given platform
#
# Usage: constants.sh {pkgname srcdir | filename srcdir platform headerfile}
#
# Example: constants.sh System.Garlic.Constants /opt/s/glade/Garlic
#       or constants.sh 5lgarcon.ads /opt/s/glade/Garlic "Linux 2.x" copyright

# Name of package

if [ $# = 4 ]; then
  name="System.Garlic.Constants"
  fname=$1
  srcdir=$2
  platform="$3"
  header=$4
else
  name=$1
  srcdir=$2
  fname=`gnatkr ${name}`.ads
  platform=x
fi

# List of constants we need to know

constants="TCP_NODELAY AF_INET PF_INET SOCK_STREAM SOCK_DGRAM EINTR EAGAIN"
constants="${constants} EWOULDBLOCK EINPROGRESS EALREADY EISCONN"
constants="${constants} ECONNREFUSED FNDELAY FASYNC-FIOASYNC F_GETFL F_SETFL"
constants="${constants} F_SETOWN-FIOSSAIOOWN SO_RCVBUF SO_REUSEADDR"
constants="${constants} SOL_SOCKET SIGTERM SIGKILL O_RDONLY O_WRONLY"
constants="${constants} O_RDWR HOST_NOT_FOUND TRY_AGAIN NO_RECOVERY"
constants="${constants} NO_DATA NO_ADDRESS POLLIN POLLPRI POLLOUT POLLERR"
constants="${constants} POLLHUP POLLNVAL I_SETSIG S_RDNORM S_WRNORM"

# Debug

debug=$1

# Look for any header file found

tmpe=./tmpe$$
trap "rm -f ${tmpe}" 0 1 2 3 15

# Header of generated file

if [ "$platform" = x ]; then
   cat > ${fname} << EOF
--  This package has been generated automatically on:
EOF
   ./split "`uname -a`" >> ${fname}
echo "--  Generation date: `date`" >> ${fname}
cat >> ${fname} << EOF
--  Any change you make here is likely to be lost !
package ${name} is
EOF
else
   cat ${srcdir}/${header} > ${fname}
   echo >> ${fname}
   echo "--  This is the version for $platform" >> ${fname}
   echo >> ${fname}
   echo "package ${name} is" >> ${fname}
fi

# For each constant, try to output its value or -1 if undefined

for c in ${constants}; do
    case ${c} in
	*-*)
            OIFS=${IFS}; IFS="-"; set ${c}; IFS=${OIFS}; \
	    echo "Checking value of $1 (or $2 as a substitute)"; \
            (gcc -DCONSTANT_NAME=$1 -I. -o ${tmpe} ${srcdir}/constants.c \
	            2>/dev/null &&
	            ${tmpe} $1 >> ${fname}) ||
            (gcc -DCONSTANT_NAME=$2 -I. -o ${tmpe} ${srcdir}/constants.c \
	            2>/dev/null &&
	            ${tmpe} $1 >> ${fname}) ||
            ./constants_nodef $1 >> ${fname}
    ;;
	*)
	    echo "Checking value of $c"; \
            (gcc -DCONSTANT_NAME=${c} -I. -o ${tmpe} ${srcdir}/constants.c \
	            2>/dev/null && \
	            ${tmpe} ${c} >> ${fname}) || \
             ./constants_nodef ${c} >> ${fname}
    ;;
    esac
done

# Trailer of generated file

cat >> ${fname} << EOF
end ${name};
EOF
