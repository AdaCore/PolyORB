/*
 * $Id$
 *
 * This file provides the necessary stuff to connect two partitions
 * using two sockets.
 *
 */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#include <netdb.h>
#include <signal.h>
#include <stdio.h>

#define PORT 8000
#define HOST "esmeralda.enst.fr"

#define bcopy(a,b,c) memcpy(b,a,c)

#ifdef DEBUG
#define trace(x,y) fprintf(stderr,x,y)
#else
#define trace(x,y)
#endif

static int fd;

int packet_open (void)
{
  int one = 1, i;
  struct hostent *hp;
  struct sockaddr_in sin;

  if (!(hp = gethostbyname (HOST))) {
    return 0;
  }
  sin.sin_family = AF_INET;
  sin.sin_port   = htons (PORT);
  bcopy (hp->h_addr, &sin.sin_addr, hp->h_length);
  
  for (i = 0; i < 8; i++) {
    sin.sin_zero [i] = 0;
  }

  fd = socket (AF_INET, SOCK_STREAM, 0);
  setsockopt (fd, SOL_SOCKET, SO_REUSEADDR, (char *)&one, sizeof(one));
  
  if (connect (fd, (struct sockaddr *)&sin, sizeof (sin)) < 0) {
    return 0;
  }

  return 1;
}

int packet_write (const char *buffer, int len)
{
  int n;
  trace ("Writing packet of length %d\n",len);
  while (len) {
    trace ("(%d left to write)\n",len);
    n = write (fd, buffer, len);
    if (n <= 0) {
      return 0;
    }
    len    -= n;
    buffer += n;
  }
  trace ("Write done\n",0);
  return 1;
}

int packet_read (char *buffer, int len)
{
  int n;
  trace ("Waiting for packet of length %d\n",len);
  while (len) {
    trace ("(%d left to read)\n",len);
    n = read (fd, buffer, len);
    if (n <= 0) {
      return 0;
    }
    len    -= n;
    buffer += n;
  }
  trace ("Read done\n",0);
  return 1;
}
