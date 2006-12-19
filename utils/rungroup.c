/* rungroup -- Run a command, kill whole process group if a signal
/*   is received.
/********************************************************************/

/* $Id: //depot/adabroker/main/broca/utils/rungroup.c#1 $ */

#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>

int sig = 0;
int status;

void hnd (int s) {
  sig = s;
}

void hndcld (int s) {
  int st;

  while (waitpid (-1, &st, WNOHANG) > 0) {
    if (!sig && WIFEXITED (st))
      status = st;
      sig = SIGCHLD;
  }
}

int main (int argc, char **argv, char **envp) {
  int child;

  if (setpgid (0, 0)) {
    perror ("setpgid");
    exit (1);
  }

  ++argv;
  if ((child = fork ()) < 0) {
    perror ("fork");
    exit (1);
  }
  if (!child) {
    execve (*argv, argv, envp);
  }
  
  signal (SIGTERM, hnd);
  signal (SIGINT, hnd);
  signal (SIGCHLD, hndcld);

  while (!(sig == SIGTERM || sig == SIGINT)) {
    pause ();
    if (sig == SIGCHLD) {
      printf ("Child terminated with status %u.\n", WEXITSTATUS (status));
      exit (WEXITSTATUS (status));
    }
  }

  printf ("Build aborted on signal %u.\n", sig);

  signal (SIGTERM, SIG_IGN);
  signal (SIGINT, SIG_IGN);
  signal (SIGCHLD, SIG_IGN);

  if (killpg (0, SIGTERM))
    perror ("killpg-TERM");
  sleep (2);

  while (waitpid (-1, NULL, WNOHANG) > 0);

  killpg (0, SIGKILL);
  exit (128 + sig);
}
