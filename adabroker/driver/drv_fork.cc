/*

COPYRIGHT

Copyright 1992, 1993, 1994 Sun Microsystems, Inc.  Printed in the United
States of America.  All Rights Reserved.

This product is protected by copyright and distributed under the following
license restricting its use.

The Interface Definition Language Compiler Front End (CFE) is made
available for your use provided that you include this license and copyright
notice on all media and documentation and the software program in which
this product is incorporated in whole or part. You may copy and extend
functionality (but may not remove functionality) of the Interface
Definition Language CFE without charge, but you are not authorized to
license or distribute it to anyone else except as part of a product or
program developed by you or with the express written consent of Sun
Microsystems, Inc. ("Sun").

The names of Sun Microsystems, Inc. and any of its subsidiaries or
affiliates may not be used in advertising or publicity pertaining to
distribution of Interface Definition Language CFE as permitted herein.

This license is effective until terminated by Sun for failure to comply
with this license.  Upon termination, you shall destroy or return all code
and documentation for the Interface Definition Language CFE.

INTERFACE DEFINITION LANGUAGE CFE IS PROVIDED AS IS WITH NO WARRANTIES OF
ANY KIND INCLUDING THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS
FOR A PARTICULAR PURPOSE, NONINFRINGEMENT, OR ARISING FROM A COURSE OF
DEALING, USAGE OR TRADE PRACTICE.

INTERFACE DEFINITION LANGUAGE CFE IS PROVIDED WITH NO SUPPORT AND WITHOUT
ANY OBLIGATION ON THE PART OF Sun OR ANY OF ITS SUBSIDIARIES OR AFFILIATES
TO ASSIST IN ITS USE, CORRECTION, MODIFICATION OR ENHANCEMENT.

SUN OR ANY OF ITS SUBSIDIARIES OR AFFILIATES SHALL HAVE NO LIABILITY WITH
RESPECT TO THE INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY
INTERFACE DEFINITION LANGUAGE CFE OR ANY PART THEREOF.

IN NO EVENT WILL SUN OR ANY OF ITS SUBSIDIARIES OR AFFILIATES BE LIABLE FOR
ANY LOST REVENUE OR PROFITS OR OTHER SPECIAL, INDIRECT AND CONSEQUENTIAL
DAMAGES, EVEN IF SUN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

Use, duplication, or disclosure by the government is subject to
restrictions as set forth in subparagraph (c)(1)(ii) of the Rights in
Technical Data and Computer Software clause at DFARS 252.227-7013 and FAR
52.227-19.

Sun, Sun Microsystems and the Sun logo are trademarks or registered
trademarks of Sun Microsystems, Inc.

SunSoft, Inc.  
2550 Garcia Avenue 
Mountain View, California  94043

NOTE:

SunOS, SunSoft, Sun, Solaris, Sun Microsystems or the Sun logo are
trademarks or registered trademarks of Sun Microsystems, Inc.

 */

/*
 * DRV_fork.cc - Fork a process for each file to be processed, wait for
 *		 status from the child process
 */

#include	<idl.hh>
#include	<idl_extern.hh>
#include	<drv_private.hh>
#include	<drv_link.hh>

#ifdef HAS_pch
#pragma hdrstop
#endif

#if defined(__sunos__) 
#ifdef SVR4
#include	<unistd.h>		// POSIX standard types
#include	<wait.h>		// POSIX definition of wait()
#else
#include	<sys/unistd.h>		// POSIX standard types
#include	<sys/wait.h>		// POSIX definition of wait()
#ifndef SUNOS4
#define SUNOS4
#endif
#endif
#endif

#if defined(__osf1__) || defined(__VMS)
#include	<unistd.h>		// POSIX standard types
#include	<wait.h>		// POSIX definition of wait()
#endif

#if defined(__hpux__)
#include	<sys/unistd.h>		// POSIX standard types
#include	<sys/wait.h>		// POSIX definition of wait()
#endif

#if defined(__aix__) || defined(__SINIX__)
#include        <unistd.h>              // POSIX standard types
#include        <sys/wait.h>            // POSIX definition of wait()
#endif

#if defined(__GLIBC__) && __GLIBC__ >= 2
#include <unistd.h>                     // POSIX standard types
#include <sys/wait.h>
#endif

#if defined(__nextstep__)
#include <sys/types.h>
typedef int pid_t;
#endif

#if defined(__irix__) 
#include      <unistd.h>              // POSIX standard types
#include      <wait.h>                // POSIX definition of wait()
#endif

#if !defined(__WIN32__) && !defined(__VMS)

// Note that Windows NT version can only handle one file on command line.


/*
 * Fork off a process, wait for it to die
 */
void
DRV_fork()
{
  pid_t child_pid;
  pid_t wait_pid;
#if defined(apollo) || defined(SUNOS4)
  union wait wait_status;
#else
  int wait_status;
#endif	// defined(apollo) || defined(SUNOS4)

  /*
   * The parent loops over all files to be processed, forking off a child
   * for each one. When they terminate, the exit status is checked. If not
   * zero, the parent does not process any more files.
   */
  for (DRV_file_index = 0; DRV_file_index < DRV_nfiles; DRV_file_index++) {
    if ((child_pid = fork()) != 0) {
      if (child_pid == -1) {
	std::cerr << GTDEVEL("IDL: fork failed\n");
	exit(99);
      }

      while ((wait_pid = wait(&wait_status)) != child_pid);
#if defined(apollo) || defined(SUNOS4)
      if (wait_status.w_status != 0)
	exit(wait_status.w_status);
#else
      if (wait_status != 0)
	exit(wait_status);
#endif	// defined(apollo) || defined(SUNOS4)
    } else {
      /*
       * OK, do it to this file (in the child)
       */
      DRV_drive(DRV_files[DRV_file_index]);
      exit(0);
    }
  }
  /*
   * Now the parent process can exit
   */
  exit(0);
}

#endif
