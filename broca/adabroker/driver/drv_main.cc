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
** drv_main.cc - Main program for IDL compiler driver
**
** LOGIC:
**
** 1. Initialize compiler driver
** 2. Parse command line args
** 3. If more than one file to parse, fork
** 4. Otherwise, for the single file, invoke DRV_drive
*/

#include	<idl.hh>
#include	<idl_extern.hh>
#include	<drv_private.hh>
#include    <drv_link.hh>

#ifdef HAS_pch
#pragma hdrstop
#endif

#include <stdlib.h>
#include <stdio.h>

#ifdef __VMS
#if __VMS_VER < 70000000
#include <omniVms/unlink.hxx>
#else
#include <unistd.h>
#endif
#endif

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

static void
DRV_version()
{
  std::cerr <<
    "Omniidl 2.7.0\n"
    "Copyright (C) 1996-1999 Olivetti & Oracle Research Laborartory,"
    " Cambridge, UK.\n"
    "Omniidl comes with ABSOLUTELY NO WARRANTY.\n" << std::endl;
}

/*
** Drive the compilation
**
** LOGIC:
**
** 1. Initialize the CFE, stage 1. This builds the scope stack
** 2. Initialize the BE. This builds an instance of the generator
** 3. Initialize the CFE, stage 2. This builds the global scope
**    and populates it with the predefined types
** 4. Invoke FE_yyparse
** 5. Check for errors from FE_yyparse. If any, exit now
** 6. Check for undefined forward declared interfaces. If any, exit now
** 7. Check if asked to dump AST. If so, do.
** 8. Invoke BE.
*/
void
DRV_drive(char *s)
{
  char	*fn;

  /*
   * Pass through CPP
   */
  if (idl_global->compile_flags() & IDL_CF_INFORMATIVE)
    std::cerr << idl_global->prog_name()
	 << GTDEVEL(": preprocessing ")
	 << s
	 << "\n";
  DRV_pre_proc(s);
  /*
   * Initialize FE stage 1
   */
  (*DRV_FE_init_stage1)();
  /*
   * Initialize BE
   */
  idl_global->set_gen((*DRV_BE_init)());
  /*
   * Initialize FE stage 2
   */
  (*DRV_FE_init_stage2)();
  /*
   * Parse
   */
  if (idl_global->compile_flags() & IDL_CF_INFORMATIVE)
    std::cerr << idl_global->prog_name()
	 << GTDEVEL(": parsing ")
	 << s
	 << "\n";
  (*DRV_FE_yyparse)();


#ifdef __WIN32__

  /* Remove the temporary file [containing the preprocessor output. */
 
  if (_unlink((idl_global->temp_filename())->get_string()) == -1)
	{
	 std::cerr << idl_global->prog_name()
	 << GTDEVEL(": Could not remove cpp output file ")
	 << (idl_global->temp_filename())->get_string()
	 << "\n";
     exit(EXIT_FAILURE);
	 }
#endif

#ifdef __VMS

    // Remove the temporary file (containing the preprocessor output).
 
  if (unlink((idl_global->temp_filename())->get_string()) == -1) {
    std::cerr << idl_global->prog_name()
         << GTDEVEL(": Could not remove cpp output file ")
         << (idl_global->temp_filename())->get_string()
         << "\n";
    exit(EXIT_FAILURE);
  }

#endif


  /*
   * If there were any errors, stop
   */
  if (idl_global->err_count() > 0) {
    std::cerr << idl_global->prog_name()
	 << ": "
	 << s 
	 << GTDEVEL(": found ");
    std::cerr << idl_global->err_count()
	 << GTDEVEL(" error");
    std::cerr << (idl_global->err_count() > 1
	    ? GTDEVEL("s") : "")
    	 << "\n";
    /*
     * Call BE_abort to allow a BE to clean up after itself
     */
    (*DRV_BE_abort)();
    exit(EXIT_FAILURE);
  }
  /*
   * Dump the code
   */
  if ((idl_global->compile_flags() & IDL_CF_INFORMATIVE)
      && (idl_global->compile_flags() & IDL_CF_DUMP_AST))
    std::cerr << idl_global->prog_name()
	 << GTDEVEL(": dump ")
	 << s
	 << "\n";
  if (idl_global->compile_flags() & IDL_CF_DUMP_AST) {
    std::cerr << GTDEVEL("Dump of AST:\n");
    idl_global->root()->dump(std::cerr);
  }
  /*
   * Call the main entry point for the BE
   */
  if (idl_global->compile_flags() & IDL_CF_INFORMATIVE)
    std::cerr << idl_global->prog_name()
	 << GTDEVEL(": BE processing on ")
	 << s
	 << "\n";
  (*DRV_BE_produce)();

  /*
   * Exit cleanly
   */
  if (idl_global->err_count() > 0) {
    std::cerr << idl_global->prog_name()
	 << ": "
	 << s 
	 << GTDEVEL(": found ");
    std::cerr << idl_global->err_count()
	 << GTDEVEL(" error");
    std::cerr << (idl_global->err_count() > 1
	    ? GTDEVEL("s") : "")
    	 << "\n";
    /*
     * Call BE_abort to allow a BE to clean up after itself
     */
    (*DRV_BE_abort)();
    exit(EXIT_FAILURE);
  }

  exit(0);
}

/*
 * IDL compiler main program. Logic as explained in comment at head
 * of file.
 */
int
main(int argc, char **argv)
{
  /*
   * Open front-end library
   */
  DRV_FE_open();
  /*
   * Initialize driver and global variables
   */
  DRV_init();
  /*
   * Open back-end library
   */
  DRV_BE_open();
  /*
   * Parse arguments
   */
  DRV_parse_args(argc, argv);
  /*
   * If a version message is requested, print it and exit
   */
  if (idl_global->compile_flags() & IDL_CF_VERSION) {
    DRV_version();
    exit(0);
  }
  /*
   * If a usage message is requested, give it and exit
   */
  if (idl_global->compile_flags() & IDL_CF_ONLY_USAGE) {
    DRV_usage();
    exit(0);
  }
  /*
   * Fork off a process for each file to process. Fork only if
   * there is more than one file to process -- [ except under NT - raise error ]
   */
  if (DRV_nfiles > 1) {

#if defined(__WIN32__) || defined(__VMS)

	  std::cerr << idl_global->prog_name()
		   << ": Only one IDL file may be specified at the command line."
	 	   << std::endl;
	  exit(EXIT_FAILURE);
#else

	 
    /*
     * DRV_fork never returns
     */
    DRV_fork();
#endif

  } else {
    /*
     * Do the one file we have to parse
     *
     * Check if stdin and handle file name appropriately
     */
    if (DRV_nfiles == 0) {
      DRV_files[0] = "standard input";
    }
    DRV_file_index = 0;
    DRV_drive(DRV_files[DRV_file_index]);
  }

  exit(0);
  /* NOTREACHED */
  return 0;
}
