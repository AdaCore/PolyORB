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
 * DRV_pre_proc.cc - pass an IDL file through the C preprocessor
 */

#include	<idl.hh>
#include	<idl_extern.hh>
#include	<drv_private.hh>
#include	<drv_link.hh>

#ifdef HAS_pch
#pragma hdrstop
#endif

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<fcntl.h>

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
extern "C" char * mktemp(char *);

#if defined(__VMS) && __VMS_VER < 70000000
#include <omniVms/unlink.hxx>
#endif

#endif


#ifdef		apollo
#include	<sysent.h>
#endif		// apollo

#if defined(__hpux__)
#include	<unistd.h>		// POSIX definitions
#include	<sys/wait.h>		// POSIX definition of wait()
#endif		// defined(__hpux__)

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

#ifdef __WIN32__
#include <io.h>
#include <process.h>
#include <sys/stat.h>
#endif

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

#undef	MAX_ARGLIST
#define	MAX_ARGLIST	128

static	char	*arglist[MAX_ARGLIST];
static	long	argcount = 0;
static  idl_bool  copy_src = I_TRUE;

/*
 * Push the new CPP location if we got a -Yp argument
 */
void
DRV_cpp_new_location(char *new_loc)
{
#ifdef __irix__
  char *help = strchr (new_loc, ' ');
  if (help) *help = '\0';
#endif
  arglist[0] = new_loc;
}

/*
 * Push an argument into the arglist
 */
void
DRV_cpp_putarg(char *str)
{
#ifdef __irix__
  char *help = strchr (str, ' ');
  if (help) *help = '\0';
#endif
  if (argcount >= MAX_ARGLIST) {
    std::cerr << idl_global->prog_name()
         << GTDEVEL(": More than ")
	 << MAX_ARGLIST
	 << GTDEVEL(" arguments to preprocessor\n");
    exit (99);
  }
  arglist[argcount++] = str;
}

/*
 * Initialize the cpp argument list
 */
void
DRV_cpp_init()
{
  DRV_cpp_putarg(idl_global->cpp_location());
  // If this is gcc, we need to put in a -x c flag to stop it from
  // guessing from the file extension what language this file is
  char *p = strrchr(idl_global->cpp_location(),'g');
  if (p != NULL && (strcmp(p,"gcc") == 0 || strcmp(p,"g++") == 0)) {
    DRV_cpp_putarg("-xc++");
    copy_src = I_FALSE;
  }
  else if(strcmp(idl_global->cpp_location(),"CL") == 0 || strcmp(idl_global->cpp_location(),"cl") == 0) {
	  // MSVC++ 4.2 ignores the file extension for preprocessing.
	  copy_src = I_FALSE;
	  DRV_cpp_putarg("-nologo");
  }

  DRV_cpp_putarg("-E");
  DRV_cpp_putarg("-I.");
}

/*
 * lines can be 1024 chars long
 */
#define	LINEBUF_SIZE	1024
static	char	drv_line[LINEBUF_SIZE + 1];

/*
 * Get a line from stdin
 */
static long
DRV_get_line(FILE *f)
{
    char	*l = fgets(drv_line, LINEBUF_SIZE, f);
    long	i;

    if (l == NULL)
	return I_FALSE;
    if (*l == '\0' && feof(f))
	return I_FALSE;
    if (*l == '\0')
	return I_TRUE;
    i = strlen(l) - 1;
    if (l[i] == '\n')
	l[i] = '\0';
    return I_TRUE;
}

/*
 * Copy from stdin to a file
 */
static void
DRV_copy_input(FILE *fin, char *fn)
{
  FILE	*f = fopen(fn, "w");

  if (f == NULL) {
    std::cerr << idl_global->prog_name()
	 << GTDEVEL(": cannot open temp file ")
	 << fn
	 << GTDEVEL(" for writing\n");
    exit(EXIT_FAILURE);
  }
  if (fin == NULL) {
      std::cerr << idl_global->prog_name()
           << GTDEVEL(": cannot open input file\n");
      exit(EXIT_FAILURE);
  }
  while (DRV_get_line(fin))
    fprintf(f, "%s\n", drv_line);
  fclose(f);
}

/*
 * Strip down a name to the last component, i.e. everything after the last
 * '/' character
 */
static char *
DRV_stripped_name(char *fn)
{
    char	*n = fn;
    long	l;

    if (n == NULL)
	return NULL;
    l = strlen(n);
#if defined(__WIN32__)
    for (n += l; l > 0 && *n != '\\'; l--, n--);
    if (*n == '\\') n++;
#elif defined(__VMS)
    for (n += l; l > 0 && *n != ']' && *n != ':'; l--, n--);
    if (*n == ']' || *n == ':') n++;
#else
    for (n += l; l > 0 && *n != '/'; l--, n--);
    if (*n == '/') n++;
#endif
    return n;
}

/*
 * File names
 */

static char	tmp_file[128];
static char	tmp_ifile[128];


#if !defined(__WIN32__) && !defined(__VMS)
/*
 * Pass input through preprocessor
 */
void
DRV_pre_proc(char *myfile)
{
#if defined(apollo) || defined(SUNOS4)
  union wait wait_status;
#else
  int	wait_status;
#endif	// defined(apollo) || defined(SUNOS4)
  long	readfromstdin = I_FALSE;
  pid_t	child_pid;
  char	catbuf[512];
  
  strcpy(tmp_file, "/tmp/idlf_XXXXXX");
  strcpy(tmp_ifile, "/tmp/idli_XXXXXX");

  (void) mktemp(tmp_file); strcat(tmp_file, ".cc");
  (void) mktemp(tmp_ifile); strcat(tmp_ifile, ".cc");
  if (strcmp(myfile, "standard input") == 0) {
    idl_global->set_filename((*DRV_FE_new_UTL_String)(tmp_ifile));
    idl_global->set_main_filename((*DRV_FE_new_UTL_String)(tmp_ifile));
    idl_global->
        set_stripped_filename(
            (*DRV_FE_new_UTL_String)(DRV_stripped_name(tmp_ifile))
        );
    idl_global->set_real_filename((*DRV_FE_new_UTL_String)(tmp_ifile));
    DRV_copy_input(stdin, tmp_ifile);
    idl_global->set_read_from_stdin(I_TRUE);
  } else {
    if (copy_src) {
      FILE *fd = fopen(myfile, "r");
      DRV_copy_input(fd, tmp_ifile);
      fclose(fd);
    }
    idl_global->set_read_from_stdin(I_FALSE);
    idl_global->set_filename((*DRV_FE_new_UTL_String)(myfile));
    idl_global->set_main_filename((*DRV_FE_new_UTL_String)(myfile));
    idl_global->
        set_stripped_filename(
            (*DRV_FE_new_UTL_String)(DRV_stripped_name(myfile))
        );
    if (copy_src) {
      idl_global->set_real_filename((*DRV_FE_new_UTL_String)(tmp_ifile));
    }
    else {
      idl_global->set_real_filename((*DRV_FE_new_UTL_String)(myfile));
    }
  }
  switch (child_pid = fork()) {
  case 0:	/* Child - call cpp */
    if (copy_src)
      DRV_cpp_putarg(tmp_ifile);
    else 
      DRV_cpp_putarg(myfile);
    {
      int fd = open(tmp_file, O_WRONLY | O_CREAT | O_TRUNC, 0777);
      if (fd < 0) {
        std::cerr << idl_global->prog_name()
    	  << GTDEVEL(": cannot open temp file ")
 	  << tmp_file << " for writing\n";
        exit(EXIT_FAILURE);
      }
      int result = dup2(fd, 1);
      if (result < 0) {
        std::cerr << idl_global->prog_name()
    	  << GTDEVEL(": temp file ")
  	  << tmp_file << " dup error\n";
        exit(EXIT_FAILURE);
      }
      close(fd);
    }
    execvp(arglist[0], arglist);
    std::cerr << idl_global->prog_name() 
         << GTDEVEL(": execvp of ")
	 << arglist[0]
	 << GTDEVEL(" failed\n");
    exit(EXIT_FAILURE);
  case -1:
    std::cerr << idl_global->prog_name() << GTDEVEL(": fork failed\n");
    exit(EXIT_FAILURE);
  default:	/* Parent - wait */
#if defined(hpux) || defined(__hpux)
    sleep(1);  // try to get around libc_r defect
#endif
    while (child_pid != wait(&wait_status));
#if defined(WIFEXITED) && defined(WEXITSTATUS)
    if (WIFEXITED(wait_status)) {
      if (WEXITSTATUS(wait_status) != 0) {
        std::cerr << idl_global->prog_name()
	     << GTDEVEL(": Preprocessor returned non-zero status ")
	     << (int) WEXITSTATUS(wait_status)
	     << "\n";
	if (copy_src) unlink(tmp_ifile);
	unlink(tmp_file);
        exit(WEXITSTATUS(wait_status));
      }
    } else {
      // child terminated abnormally - wait_status is meaningless
      std::cerr << idl_global->prog_name()
	   << GTDEVEL(": Preprocessor terminated abnormally")
	   << "\n";
      if (copy_src) unlink(tmp_ifile);
      unlink(tmp_file);
      exit(1);
    }
#else
#if defined(apollo) || defined(SUNOS4)
    if (wait_status.w_status != 0) {
#else
    if (wait_status != 0) {
#endif	// defined(apollo) || defined(SUNOS4)
      std::cerr << idl_global->prog_name()
	   << GTDEVEL(": Preprocessor returned non-zero status ")
#if defined(apollo) || defined(SUNOS4)
	   << wait_status.w_status
#else
	   << wait_status
#endif	// defined(apollo) || defined(SUNOS4)
	   << "\n";
      if (copy_src) unlink(tmp_ifile);
      unlink(tmp_file);
#if defined(apollo) || defined(SUNOS4)
      exit(wait_status.w_status);
#else
      exit((int) wait_status);
#endif	// defined(apollo) || defined(SUNOS4)
    }
#endif  // defined(WIFEXITED) && defined(WEXITSTATUS)
  }
  FILE * yyin = fopen(tmp_file, "r");
  if (yyin == NULL) {
    std::cerr << idl_global->prog_name()
	 << GTDEVEL(": Could not open cpp output file ")
	 << tmp_file
	 << "\n";
    exit(EXIT_FAILURE);
  }
  (*DRV_FE_set_yyin)((File *) yyin);
  if (idl_global->compile_flags() & IDL_CF_ONLY_PREPROC) {
    sprintf(catbuf, "cat < %s", tmp_file);
    system(catbuf);
  }
  if (copy_src) {
    if (unlink(tmp_ifile) != 0) {
      std::cerr << idl_global->prog_name()
	   << GTDEVEL(": Could not remove cpp input file ")
	   << tmp_ifile
	   << "\n";
      exit(EXIT_FAILURE);
    }
  }
  if (unlink(tmp_file) != 0) {
    std::cerr << idl_global->prog_name()
	 << GTDEVEL(": Could not remove cpp output file ")
	 << tmp_file
	 << "\n";
    exit(EXIT_FAILURE);
  }
  if (idl_global->compile_flags() & IDL_CF_ONLY_PREPROC)
    exit(0);
}

#elif defined(__WIN32__)

// WIN 32 Version

/*
 * Pass input through preprocessor
 */
void
DRV_pre_proc(char *myfile)
{
  int	wait_status;

  long	readfromstdin = I_FALSE;

  char	catbuf[512];
  

  char* tmpfn;	
  tmpfn = _tempnam(NULL,"idl_");
  if (tmpfn == NULL)
    {
      std::cerr << "Error creating temporary filename." << std::endl;
      exit(-1);
    }
  else if (strlen(tmpfn) > 127)
    {
      std::cerr << "Temporary filename too large." << std::endl;
      exit(-1);
    }
  
  strcpy(tmp_file,tmpfn);
  free(tmpfn);

  tmpfn = _tempnam(NULL,"idl_");

  if (tmpfn == NULL)
    {
      std::cerr << "Error creating temporary filename." << std::endl;
      exit(-1);
    }
  else if (strlen(tmpfn) > 127)
    {
      std::cerr << "Temporary filename too large." << std::endl;
      exit(-1);
    }
  strcpy(tmp_ifile,tmpfn);
  free(tmpfn);

  if (strcmp(myfile, "standard input") == 0) {
    idl_global->set_filename((*DRV_FE_new_UTL_String)(tmp_ifile));
    idl_global->set_main_filename((*DRV_FE_new_UTL_String)(tmp_ifile));
    idl_global->
        set_stripped_filename(
            (*DRV_FE_new_UTL_String)(DRV_stripped_name(tmp_ifile))
        );
    idl_global->set_real_filename((*DRV_FE_new_UTL_String)(tmp_ifile));
    DRV_copy_input(stdin, tmp_ifile);
    idl_global->set_read_from_stdin(I_TRUE);
  } else {
    if (copy_src) {
      FILE *fd = fopen(myfile, "r");
      DRV_copy_input(fd, tmp_ifile);
      fclose(fd);
    }
    idl_global->set_read_from_stdin(I_FALSE);
    idl_global->set_filename((*DRV_FE_new_UTL_String)(myfile));
    idl_global->set_main_filename((*DRV_FE_new_UTL_String)(myfile));
    idl_global->
        set_stripped_filename(
            (*DRV_FE_new_UTL_String)(DRV_stripped_name(myfile))
        );
    if (copy_src) {
      idl_global->set_real_filename((*DRV_FE_new_UTL_String)(tmp_ifile));
    }
    else {
      idl_global->set_real_filename((*DRV_FE_new_UTL_String)(myfile));
    }
  }


  {
    if (copy_src)
      DRV_cpp_putarg(tmp_ifile);
    else 
      DRV_cpp_putarg(myfile);
    {
      int fd = _open(tmp_file, _O_RDWR | _O_CREAT | _O_TRUNC, 
			                                        _S_IWRITE);
      if (fd < 0) {
        std::cerr << idl_global->prog_name()
    	  << GTDEVEL(": cannot open temp file ")
 	  << tmp_file << " for writing\n";
        exit(EXIT_FAILURE);
      }
      int result = _dup2(fd, _fileno(stdout));
      if (result < 0) {
        std::cerr << idl_global->prog_name()
    	  << GTDEVEL(": temp file ")
  	  << tmp_file << " dup error\n";
        exit(EXIT_FAILURE);
      }
      _close(fd);
    }
    int spawn_rc = _spawnvp(_P_WAIT,arglist[0], 
			                 (const char* const*) arglist);
	if (spawn_rc != 0)
		{
			std::cerr << idl_global->prog_name() 
				 << GTDEVEL(": spawnvp of ")
				 << arglist[0]
				 << GTDEVEL(" failed\n");
			std::cerr << "Preprocessor returned non-zero status " << spawn_rc << std::endl;
			
			_close(1);
			if (copy_src) {
				 if (_unlink(tmp_ifile) == -1) {
					std::cerr << idl_global->prog_name()
						 << GTDEVEL(": Could not remove cpp input file ")
						 << tmp_ifile
						 << "\n";
					exit(EXIT_FAILURE);	
					}
			 }

			if (_unlink(tmp_file) == -1) {
				 std::cerr << idl_global->prog_name()
					  << GTDEVEL(": Could not remove cpp output file ")
					  << tmp_file
					  << "\n";
			     exit(EXIT_FAILURE);
			}


		    exit(EXIT_FAILURE);
		}
  }
  FILE * yyin = fopen(tmp_file, "r+");
  if (yyin == NULL) {
    std::cerr << idl_global->prog_name()
	 << GTDEVEL(": Could not open cpp output file ")
	 << tmp_file
	 << "\n";
    exit(EXIT_FAILURE);
  }

  (*DRV_FE_set_yyin)((File *) yyin);
  if (idl_global->compile_flags() & IDL_CF_ONLY_PREPROC) {
    _close(1);
	_dup2(_fileno(stderr),1); // Redirected to stderr, in order to write to the screen!
    sprintf(catbuf, "type %s", tmp_file);
    system(catbuf);
  }
  if (copy_src) {
    if (_unlink(tmp_ifile) == -1) {
      std::cerr << idl_global->prog_name()
	   << GTDEVEL(": Could not remove cpp input file ")
	   << tmp_ifile
	   << "\n";
      exit(EXIT_FAILURE);
    }
  }


  idl_global->set_temp_filename((*DRV_FE_new_UTL_String)(tmp_file));

  if (idl_global->compile_flags() & IDL_CF_ONLY_PREPROC)
  {
	/* Remove the temporary file [containing the preprocessor output. */
  fclose(yyin);
  if (_unlink((idl_global->temp_filename())->get_string()) == -1)
	{
	 std::cerr << idl_global->prog_name()
	 << GTDEVEL(": Could not remove cpp output file ")
	 << (idl_global->temp_filename())->get_string()
	 << "\n";
     exit(EXIT_FAILURE);
	 }
  
    exit(0);
  }
}
#elif defined(__VMS)

// OpenVMS Version

#include <string>

#if defined(__DECCXX) && __DECCXX_VER < 60000000 && !defined(std)
// pretend we have an std namespace
#define std
#endif

/*
 * Pass input through preprocessor
 */
void
DRV_pre_proc(char *myfile)
{
    int	wait_status;

    long	readfromstdin = I_FALSE;

    char	catbuf[512];


    strcpy(tmp_file, "idlf_XXXXXX");
    strcpy(tmp_ifile, "idli_XXXXXX");

    (void) mktemp(tmp_file); strcat(tmp_file, ".cc");
    (void) mktemp(tmp_ifile); strcat(tmp_ifile, ".cc");

    if (strcmp(myfile, "standard input") == 0) {
	idl_global->set_filename((*DRV_FE_new_UTL_String)(tmp_ifile));
	idl_global->set_main_filename((*DRV_FE_new_UTL_String)(tmp_ifile));
	idl_global->
	    set_stripped_filename(
		(*DRV_FE_new_UTL_String)(DRV_stripped_name(tmp_ifile))
	    );
	idl_global->set_real_filename((*DRV_FE_new_UTL_String)(tmp_ifile));
	DRV_copy_input(stdin, tmp_ifile);
	idl_global->set_read_from_stdin(I_TRUE);
    } else {
	if (copy_src) {
	  FILE *fd = fopen(myfile, "r");
	  DRV_copy_input(fd, tmp_ifile);
	  fclose(fd);
	}
	idl_global->set_read_from_stdin(I_FALSE);
	idl_global->set_filename((*DRV_FE_new_UTL_String)(myfile));
	idl_global->set_main_filename((*DRV_FE_new_UTL_String)(myfile));
	idl_global->
	    set_stripped_filename(
		(*DRV_FE_new_UTL_String)(DRV_stripped_name(myfile))
	    );
	if (copy_src) {
	  idl_global->set_real_filename((*DRV_FE_new_UTL_String)(tmp_ifile));
	}
	else {
	  idl_global->set_real_filename((*DRV_FE_new_UTL_String)(myfile));
	}
    }


    {
      //  Modification: 1998-12-17
      //  Enhancement:  Accumulate unix style switches that have values so
      //  that (e.g.) -Dfoo -Dbar becomes /Defi=("foo","bar").  Here, I make
      //  the assumption that the first four characters of a qualifier are
      //  unique (this was once guaranteed...alas, it is no longer...)
      if (copy_src)
	DRV_cpp_putarg(tmp_ifile);
      else 
	DRV_cpp_putarg(myfile);
      std::string preprocessOnlySwitch;
      std::string includePath;
      std::string macroDefs;
      std::string inputFiles;
      for (int i(1); arglist[i]; ++i) {
	std::string unixArg(arglist[i]);

	if (unixArg[0]=='-') {
	  switch(unixArg[1]) {
	  case 'E': {
	    preprocessOnlySwitch = std::string("/Prep=") + tmp_file;
	  } break;
	  case 'I': {
	    if (!includePath.empty())
	      includePath += ',';
	    includePath += '"' + unixArg.substr(2) + '"';
	  } break;
	  case 'D': {
	    if (!macroDefs.empty())
	      macroDefs += ',';
	    macroDefs += '"' + unixArg.substr(2) + '"';
	  } break;
	  default: {
	    std::cerr << "Unix preprocessor switch: "
		      << unixArg
		      << " does not have corresponding VMS qualifier."
		      << std::endl;
	  }
	  }
	} else {
	  if (!inputFiles.empty())
	    inputFiles += ',';
	  inputFiles += unixArg;
	}
      }

      std::string commandLine = std::string(arglist[0]) + ' ' + inputFiles;
      if (!preprocessOnlySwitch.empty()) {
	commandLine += preprocessOnlySwitch;
      }
      if (!includePath.empty()) {
	commandLine += "/Incl=(" + includePath + ')';
      }
      if (!macroDefs.empty()) {
	commandLine += "/Defi=(" + macroDefs + ')';
      }

      int spawn_rc = system(commandLine.c_str());
      if (spawn_rc != 0 && spawn_rc & 1==0) {
	std::cerr << idl_global->prog_name() 
		  << GTDEVEL(": command ")
		  << commandLine
		  << GTDEVEL(" failed\n");
	std::cerr << "Preprocessor returned non-zero status " << spawn_rc
		  << std::endl;

	if (copy_src) {
	  if (unlink(tmp_ifile) == -1) {
	    std::cerr << idl_global->prog_name()
		      << GTDEVEL(": Could not remove cpp input file ")
		      << tmp_ifile
		      << "\n";
	    exit(EXIT_FAILURE);	
	  }
	}

	if (unlink(tmp_file) == -1) {
	  std::cerr << idl_global->prog_name()
		    << GTDEVEL(": Could not remove cpp output file ")
		    << tmp_file
		    << "\n";
	  exit(EXIT_FAILURE);
	}

	exit(EXIT_FAILURE);
      }   // system(...) failed
    }	// scope for no apparent reason.
    FILE * yyin = fopen(tmp_file, "r+");
    if (yyin == NULL) {
      std::cerr << idl_global->prog_name()
		<< GTDEVEL(": Could not open cpp output file ")
		<< tmp_file
		<< "\n";
      exit(EXIT_FAILURE);
    }

    (*DRV_FE_set_yyin)((File *) yyin);
    if (idl_global->compile_flags() & IDL_CF_ONLY_PREPROC) {
      std::string command(std::string("type ") + tmp_file);
      system(command.c_str());
    }
    if (copy_src) {
      if (unlink(tmp_ifile) == -1) {
	std::cerr << idl_global->prog_name()
		  << GTDEVEL(": Could not remove cpp input file ")
		  << tmp_ifile
		  << "\n";
	exit(EXIT_FAILURE);
      }
    }


    idl_global->set_temp_filename((*DRV_FE_new_UTL_String)(tmp_file));

    if (idl_global->compile_flags() & IDL_CF_ONLY_PREPROC) {
      /* Remove the temporary file [containing the preprocessor output. */
      fclose(yyin);
      if (unlink((idl_global->temp_filename())->get_string()) == -1) {
	std::cerr << idl_global->prog_name()
		  << GTDEVEL(": Could not remove cpp output file ")
		  << (idl_global->temp_filename())->get_string()
		  << "\n";
	exit(EXIT_FAILURE);
      }

      exit(0);
    }
}
#endif
