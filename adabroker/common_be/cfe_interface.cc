/**********************************************************************************
**                          Package   : omniidl2                                 ** 
** obe_cfe_interface.cc     Created on: 8/8/1996                                 ** 
**			    Author    : Sai-Lai Lo (sll)                         ** 
**                                                                               ** 
**    Copyright (C) 1996, 1997 Olivetti & Oracle Research Laboratory             ** 
**                                                                               ** 
**  This file is part of omniidl2.                                               ** 
**                                                                               ** 
**  Omniidl2 is free software; you can redistribute it and*or modify             ** 
**  it under the terms of the GNU General Public License as published by         **  
**  the Free Software Foundation; either version 2 of the License, or            ** 
**  (at your option) any later version.                                          ** 
**                                                                               ** 
**  This program is distributed in the hope that it will be useful,              ** 
**  but WITHOUT ANY WARRANTY; without even the implied warranty of               ** 
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                ** 
**  GNU General Public License for more details.                                 ** 
**                                                                               ** 
**  You should have received a copy of the GNU General Public License            ** 
**  along with this program; if not, write to the Free Software                  ** 
**  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,       ** 
**  USA.                                                                         ** 
**                                                                               **
**********************************************************************************/

#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>
#include <adabe.h> 

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
adabe_root* adabe_global::myself = NULL; 
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
  return g;
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
BE_produce() 
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

	case 'b':                   
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
  if (be_defined==0)  idl_global->set_be("c"); 
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
















