#!/bin/sh
#
# 1.7
#
# This file generates any necessary constant for a given platform
#

# Name of package

name=$1
fname=`gnatkr ${name}`.ads

# List of include files to look for (from /usr/include)

incfiles="stdio.h sys/types.h sys/socket.h errno.h netdb.h netinet/in.h signal.h fcntl.h termio.h termios.h sys/file.h linux/fcntl.h sys/ioctl.h netinet/tcp.h sys/systeminfo.h poll.h"

# List of constants we need to know

constants="TCP_NODELAY AF_INET SOCK_STREAM SOCK_DGRAM EINTR EAGAIN EWOULDBLOCK EINPROGRESS EALREADY EISCONN ECONNREFUSED FNDELAY FASYNC-FIOASYNC F_GETFL F_SETFL F_SETOWN-FIOSSAIOOWN SO_RCVBUF SO_REUSEADDR SOL_SOCKET SIGTERM SIGKILL O_RDONLY O_WRONLY O_RDWR HOST_NOT_FOUND TRY_AGAIN NO_RECOVERY NO_DATA NO_ADDRESS POLLIN POLLPRI POLLOUT POLLERR POLLHUP POLLNVAL"

# Debug

debug=$1

# Look for any header file found

tmph=./tmph$$.c
tmpe=./tmpe$$
trap "rm -f ${tmpe} ${tmph}" 0 1 2 3 15
for i in ${incfiles}; do
  if [ -f "/usr/include/$i" ]; then
    echo "#include <$i>" >> ${tmph}
  fi
done

# Generate a list of constants

cat >> ${tmph} << EOF
struct cons {
  char *name;
  int value;
} list[] = {
EOF
for i in ${constants}; do
  case $i in
  *-*)
      OFS=${IFS}
      IFS='-'
      set $i
      j=$1
      k=$2
      IFS=${OFS}
      echo "#ifdef $j" >> ${tmph}
      echo "{\"$j\",$j}," >> ${tmph}
      echo "#else" >> ${tmph}
      echo "#ifdef $k" >> ${tmph}
      echo "{\"$j\",$k}," >> ${tmph}
      echo "#else" >> ${tmph}
      echo "{\"$j\",-1}," >> ${tmph}
      echo "#endif" >> ${tmph}
      echo "#endif" >> ${tmph}
      ;;
  *)
      echo "#ifdef $i" >> ${tmph}
      echo "{\"$i\",$i}," >> ${tmph}
      echo "#else" >> ${tmph}
      echo "{\"$i\",-1}," >> ${tmph}
      echo "#endif" >> ${tmph}
      ;;
  esac
done
cat >> ${tmph} << EOF
{0,0}
};
void loosecase(char *s)
{
  int f;
  for (f=1;*s;s++) {
    if (*s=='_') {
      f=1;
    } else if (f) {
      f=0;
    } else {
      *s+='a'-'A';
    }
  }
}
main()
{
  struct cons *i; for (i=list;i->name;i++) {loosecase(i->name);
  printf("   %-20s : constant := 16#%04X#;\n", i->name, i->value);}
}
EOF
if [ "${debug}" = "-d" ]; then
  cat ${tmph} >& 2
fi
gcc -o ${tmpe} -fwritable-strings ${tmph}
cat > ${fname} << EOF
--  This package has been generated automatically on:
EOF
echo "--  `uname -a`" >> ${fname}
echo "--  Generation date: `date`" >> ${fname}
cat >> ${fname} << EOF
--  Any change you make here is likely to be lost !
package ${name} is
EOF
${tmpe} >> ${fname}
cat >> ${fname} << EOF
end ${name};
EOF
