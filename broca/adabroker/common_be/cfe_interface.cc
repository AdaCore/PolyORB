#include <idl.hh>
#include <idl_extern.hh>
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

// Win32 and VMS don't have an implementation of getopt() - supply a
// getopt() for this program:

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


adabe_name* adabe_global::pd_adabe_current_file = NULL;
adabe_root* adabe_global::myself = NULL; 
bool adabe_global::pd_impl_flag = false;
unsigned long adabe_global::pd_debug_flag = 0;


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
  //  if (strcmp(idl_global->be(),"c")==0) g = new o2be_generator();
  /* else  if (strcmp(idl_global->be(),"ada")==0) */ g = new adabe_generator();
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
  //  try {
  //    if (strcmp(idl_global->be(),"c")==0) o2be_global::root()->produce(); 
  //    else if (strcmp(idl_global->be(),"ada")==0) 
  //      {
  //#ifdef DEBUG_CFE
  //	cout << "produce is launched on the root" << endl;
  //#endif
  adabe_global::root()->produce();	
  //      }
  //}
  /*  catch (o2be_fe_error &ex) {
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
      */
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
// std::cerr << GTDEVEL(" -h suffix\t\tspecify suffix for the
// generated header file(s)\n");
  std::cerr << GTDEVEL(" -i\t\t\tgenerates empty stubs for the server side (*-impl files)\n");
// std::cerr << GTDEVEL(" -l\t\t\tgenerates code required by LifeCycle
// service\n");
  std::cerr << GTDEVEL(" -m\t\t\tallow modules to be reopened\n");
// std::cerr << GTDEVEL(" -s suffix\t\tspecify suffix for the
// generated stub file(s)\n");
// std::cerr << GTDEVEL(" -t\t\t\tgenerate 'tie' implementation skeleton\n");
  std::cerr << GTDEVEL(" -u\t\t\tprints usage message and exits\n");
  std::cerr << GTDEVEL(" -vname\t\t\ttraces compilation of entity name\n");
  std::cerr << GTDEVEL(" -w\t\t\tsuppresses IDL compiler warning messages\n");
// std::cerr << GTDEVEL(" -bback_end\t\tcauses specified back end to
// be used\n");

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
  //int be_defined = 0;
  char *buffer;


#ifdef HAS_Cplusplus_Namespace
  // Enable reopen module by default
  idl_global->set_compile_flags(idl_global->compile_flags() |
				IDL_CF_REOPENMODULE);
#endif

  DRV_cpp_init();
  idl_global->set_prog_name(argv[0]);
  while ((c = getopt(argc,argv,"D:EI:U:Vuv:wmi" /*atlb:*/)) != EOF)
  // add "b:" if you want to select your back-end
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
	/*
	  case 'h':
	  o2be_global::set_hdrsuffix(optarg);
	  break;
	*/
	/*
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
	*/
	case 'u':
	  usage();
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_CF_ONLY_USAGE);
	  return;
	case 'v':
          if (strcmp(optarg,"argument")==0) 
	    adabe_global::set_debug_flag (D_ARGUMENT);
	  else if (strcmp(optarg,"attribute")==0)
	    adabe_global::set_debug_flag (D_ATTRIBUTE);
	  else if (strcmp(optarg,"constant")==0) 
	    adabe_global::set_debug_flag (D_CONSTANT);
          else if (strcmp(optarg,"enum")==0) 
	    adabe_global::set_debug_flag (D_ENUM);
          else if (strcmp(optarg,"exception")==0) 
	    adabe_global::set_debug_flag (D_EXCEPTION);
          else if (strcmp(optarg,"field")==0) 
	    adabe_global::set_debug_flag (D_FIELD);
          else if (strcmp(optarg,"interface")==0) 
	    adabe_global::set_debug_flag (D_INTERFACE);
          else if (strcmp(optarg,"forward")==0) 
	    adabe_global::set_debug_flag (D_FORWARD);
          else if (strcmp(optarg,"module")==0) 
	    adabe_global::set_debug_flag (D_MODULE);
          else if (strcmp(optarg,"operation")==0) 
	    adabe_global::set_debug_flag (D_OPERATION);
          else if (strcmp(optarg,"sequence")==0) 
	    adabe_global::set_debug_flag (D_SEQUENCE);
          else if (strcmp(optarg,"struct")==0) 
	    adabe_global::set_debug_flag (D_STRUCT);
          else if (strcmp(optarg,"typedef")==0) 
	    adabe_global::set_debug_flag (D_TYPEDEF);
	  else if (strcmp(optarg,"union")==0) 
	    adabe_global::set_debug_flag (D_UNION);
	  break;

	case 'w':
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_CF_NOWARNINGS);
	  break;
	/*
	  case 'l':
	  // XXX -Life cycle compiler flag
	  idl_global->set_compile_flags(idl_global->compile_flags() |
	  IDL_CF_LIFECYCLE);
	  break;
	  
	  case 'a':
	  idl_global->set_compile_flags(idl_global->compile_flags() |
	  IDL_CF_ANY);
	  break;
	*/
	  
	case 'm':
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_CF_REOPENMODULE);
	  break;
	/*
	  case 't':
	  idl_global->set_compile_flags(idl_global->compile_flags() |
	  IDL_BE_GENERATE_TIE);
	  break;
	*/
	/*
        case 'b':                   
          if ((strcmp(optarg,"ada")==0)||(strcmp(optarg,"c")==0))
	  idl_global->set_be(optarg);
	  else exit(99) ;
	  be_defined = 1;   
	  break;
	*/
	case '?':
	  usage();
	  idl_global->set_compile_flags(idl_global->compile_flags() |
					IDL_CF_ONLY_USAGE);
	  return;
	case 'i':
	  adabe_global::set_impl_flag (true);
	  break;
	}
    }
  //  if (be_defined==0)  idl_global->set_be("c"); 
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
















