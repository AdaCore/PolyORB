// -*- Mode: C++; -*-
//                          Package   : omniidl2
// obe_cfe_interface.cc     Created on: 8/8/1996
//			    Author    : Sai-Lai Lo (sll)
//
//    Copyright (C) 1996, 1997 Olivetti & Oracle Research Laboratory
//
//  This file is part of omniidl2.
//
//  Omniidl2 is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
//  USA.
//
// Description:
//    API entry points to interface with CFE

/*
  $Log: cfe_interface.cc,v $
  Revision 1.6  1999/03/10 18:50:04  niebel
  addition of Laurent's work
  addition of the modified makefile to compile the project

  Revision 1.5  1999/03/02 17:28:12  niebel
  The produce_adb is added to this class

  Revision 1.3  1999/03/01 23:27:54  niebel
  modification to allow different BE (the adabroker and the omniidl2 back-end)


  Revision 1.2  1999/03/01 19:48:01  niebel
  modification of the mapping of expressions in the union

  Revision 1.1  1999/02/14 17:45:00  niebel
  Ajout des sources d'omniidl2 en vue de l'ajout de notre back end.

  Revision 1.19  1999/01/07 09:48:23  djr
  Changes to support new output file ...DynSK.cc

  Revision 1.18  1998/10/26 12:19:58  sll
  Added catch for o2be_fe_error exception.

  Revision 1.17  1998/08/13 22:41:24  sll
  Added pragma hdrstop to control pre-compile header if the compiler feature
  is available.

  Revision 1.16  1998/08/10 15:33:52  sll
  Now catch all internal exceptions and print an error message instead
  of causing a core dump.

  Revision 1.15  1998/08/06 16:27:03  sll
  Re-indent getopt(). Previously getopt() failed to check for null buf_left
  before it is de-referenced.

  Revision 1.14  1998/08/06 16:23:27  sll
  *** empty log message ***

  Revision 1.13  1998/05/20 18:24:13  sll
  New option (-t) enable the generation of tie implementation template.

  Revision 1.12  1998/04/08 16:08:57  sll
  *** empty log message ***

  Revision 1.11  1998/04/07 18:41:11  sll
  Use std::cerr instead of cerr.
  Added compiler flag -m.

// Revision 1.10  1998/01/27  16:34:55  ewc
//  Added support for type any and TypeCode
//
// Revision 1.9  1998/01/20  19:13:38  sll
// Added support for OpenVMS.
//
  Revision 1.8  1997/12/09 19:55:22  sll
  *** empty log message ***

// Revision 1.7  1997/09/20  16:37:24  dpg1
// Added new -l flag for LifeCycle code generation.
//
// Revision 1.6  1997/05/07  10:12:52  ewc
// Changed win32 usage() message.
//
// Revision 1.5  1997/05/06  17:28:38  sll
// Public release.
//
  */

#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>
#include <adabe.h>   /////////////////////////

#ifdef HAS_pch
#pragma hdrstop
#endif

#include <drv_private.hh>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifndef __WIN32__
#include <unistd.h>
#endif



#if defined(__WIN32__) || defined(__VMS) && __VMS_VER < 60200000

// Win32 and VMS don't have an implementation of getopt() - supply a getopt() for this program:

char* optarg;
int optind = 1;


int
getopt(int num_args, char* const* args, const char* optstring)
{
  if (optind == num_args)
    return EOF;

  char* buf_left = *(args+optind);

  if (buf_left == NULL || (*buf_left != '-' && *buf_left != '/')) 
    return EOF;
  else if ((optind < (num_args-1)) && strcmp(buf_left,"-") == 0 
	   && strcmp(*(args+optind+1),"-") == 0) {
    optind+=2;
    return EOF;
  }
  else if (strcmp(buf_left,"-") == 0) {
    optind++;
    return '?';
  }

  for(int count = 0; count < strlen(optstring); count++) {
    if (optstring[count] == ':') 
      continue;
    if (buf_left[1] == optstring[count]) {
      if(optstring[count+1] == ':') {
	if (strlen(buf_left) > 2) {
	  optarg = (buf_left+2);
	  optind++;
	}
	else if (optind < (num_args-1)) {
	  optarg = *(args+optind+1);
	  optind+=2;
	}
	else {
	  optind++;
	  return '?';
	}
      }
      else
	optind++;
      return buf_left[1];
    }
  }
  optind++;
  return '?';
}

#endif


o2be_root* o2be_global::myself = NULL;
adabe_root* adabe_global::myself = NULL; /////////////////
char* o2be_global::pd_hdrsuffix = DEFAULT_IDL_HDR_SUFFIX;
char* o2be_global::pd_skelsuffix = DEFAULT_IDL_SKEL_SUFFIX;
char* o2be_global::pd_dynskelsuffix = DEFAULT_IDL_DYNSKEL_SUFFIX;
size_t o2be_global::pd_suffixlen = DEFAULT_IDL_SUFFIXLEN;
int o2be_global::pd_fflag = 0;
int o2be_global::pd_aflag = 0;
int o2be_global::pd_qflag = 0;
int o2be_global::pd_mflag = 1;

//
// Initialize the BE. The protocol requires only that this routine
// return an instance of AST_Generator (or a subclass thereof).
//
// Remember that none of the FE initialization has been done, when you
// add stuff here.
//
AST_Generator*
BE_init()
{
  AST_Generator *g;
  if (strcmp(idl_global->be(),"c")==0) g = new o2be_generator();
  else if (strcmp(idl_global->be(),"ada")==0) g = new adabe_generator();
  return g;/////////////////////////////////////
}

//
// Print out a version string for the BE
//
void
BE_version()
{
}

//
// Do the work of this BE.
//
void
BE_produce() //////////////////// revoir les catchs a cause des o2be 
{
  try {
    if (strcmp(idl_global->be(),"c")==0) o2be_global::root()->produce(); 
    else if (strcmp(idl_global->be(),"ada")==0) adabe_global::root()->produce();

  }
  catch (o2be_fe_error &ex) {
    std::cerr << "Error: " << ex.errmsg() << std::endl;
  }
  catch (o2be_fileio_error &ex) {
    std::cerr << "Error: " << ex.errmsg() << std::endl;
    idl_global->err_count();
  }
  catch (o2be_unsupported &ex) {
    std::cerr << "Error: " << ex.file() << "-" << ex.line()
         << " unsupported IDL syntax. " << ex.msg() << std::endl;
    idl_global->err_count();
  }
  catch (o2be_internal_error &ex) {
    std::cerr << "omniORB2 back end internal error: " 
	 << ex.file() << ":" << ex.line() << "-" << ex.errmsg() << std::endl;
    idl_global->err_count();
  };
  return;
}

//
// Abort this run of the BE
//
void
BE_abort()
{
  return;
}

static
void
usage()
{
  std::cerr << GTDEVEL("usage: ")
       << idl_global->prog_name()
#ifdef __WIN32__
       << GTDEVEL(" [flag]* file\n");
#else
       << GTDEVEL(" [flag]* file [file]*\n");
#endif
  std::cerr << GTDEVEL("Legal flags:\n");

  std::cerr << GTDEVEL(" -Dname[=value]\t\tdefines name for preprocessor\n");
  std::cerr << GTDEVEL(" -E\t\t\truns preprocessor only, prints on stdout\n");
  std::cerr << GTDEVEL(" -Idir\t\t\tincludes dir in search path for preprocessor\n");
  std::cerr << GTDEVEL(" -Uname\t\t\tundefines name for preprocessor\n");
  std::cerr << GTDEVEL(" -V\t\t\tprints version info then exits\n");
  std::cerr << GTDEVEL(" -a\t\t\tgenerates code required by type any\n");
  std::cerr << GTDEVEL(" -h suffix\t\tspecify suffix for the generated header file(s)\n");
  std::cerr << GTDEVEL(" -l\t\t\tgenerates code required by LifeCycle service\n");
  std::cerr << GTDEVEL(" -m\t\t\tallow modules to be reopened\n");
  std::cerr << GTDEVEL(" -s suffix\t\tspecify suffix for the generated stub file(s)\n");
  std::cerr << GTDEVEL(" -t\t\t\tgenerate 'tie' implementation skeleton\n");
  std::cerr << GTDEVEL(" -u\t\t\tprints usage message and exits\n");
  std::cerr << GTDEVEL(" -v\t\t\ttraces compilation stages\n");
  std::cerr << GTDEVEL(" -w\t\t\tsuppresses IDL compiler warning messages\n");
  std::cerr << GTDEVEL(" -bback_end\t\tcauses specified back end to be used\n");

  return;
}

void
BE_prep_arg(char *arg, idl_bool unknown_flag)
{
  return;
}

#ifndef __WIN32__
extern char *optarg;
extern int optind;
#endif

void
BE_parse_args(int argc, char **argv)
{
  int c;
  int be_defined = 0;
  char *buffer;


#ifdef __WIN32__
  o2be_global::set_skelsuffix("SK.cpp");
  o2be_global::set_dynskelsuffix("DynSK.cpp");
#endif

#ifdef HAS_Cplusplus_Namespace
  // Enable reopen module by default
  idl_global->set_compile_flags(idl_global->compile_flags() |
				IDL_CF_REOPENMODULE);
#endif

  DRV_cpp_init();
  idl_global->set_prog_name(argv[0]);
  while ((c = getopt(argc,argv,"D:EI:U:Vuvwh:s:lamtb:")) != EOF) //////////////
    {
      switch (c) 
	{
	case 'D':
	case 'I':
	case 'U':
	  buffer = new char[strlen(optarg) + 3];
	  sprintf(buffer, "-%c%s", c, optarg);
	  DRV_cpp_putarg(buffer);
	  break;
	case 'E':
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_CF_ONLY_PREPROC);
	  break;
	case 'V':
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_CF_VERSION);
	  return;
	case 'h':
	  o2be_global::set_hdrsuffix(optarg);
	  break;
	case 's':
	  {
	    o2be_global::set_skelsuffix(optarg);
	    char* s = new char[strlen(optarg) + strlen("Dyn") + 1];
	    strcpy(s, "Dyn");
	    strcat(s, optarg);
	    o2be_global::set_dynskelsuffix(s);
	    delete[] s;
	  }
	  break;
	case 'u':
	  usage();
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_CF_ONLY_USAGE);
	  return;
	case 'v':
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_CF_INFORMATIVE);
	  break;
	case 'w':
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_CF_NOWARNINGS);
	  break;
	case 'l':
	  // XXX -Life cycle compiler flag
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_CF_LIFECYCLE);
	  break;

	case 'a':
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_CF_ANY);
	  break;


	case 'm':
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_CF_REOPENMODULE);
	  break;

	case 't':
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_BE_GENERATE_TIE);
	  break;

	case 'b':                    ///////////////////////////////
       	  if ((strcmp(optarg,"ada")==0)||(strcmp(optarg,"c")==0))  idl_global->set_be(optarg);
	  else exit(99) ;
	  be_defined = 1;   
	  break;
	  
	case '?':
	  usage();
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_CF_ONLY_USAGE);
	  return;
	}
    }
  if (be_defined==0)  idl_global->set_be("c");  //////////////////////
  for (; optind < argc; optind++)
    {
      DRV_files[DRV_nfiles++] = argv[optind];
    }


  if (DRV_nfiles == 0)
    {
      std::cerr << "No file specified.\n";
      usage();
      idl_global->set_compile_flags(idl_global->compile_flags() |
				    IDL_CF_ONLY_USAGE);
    }
  return;
}
















