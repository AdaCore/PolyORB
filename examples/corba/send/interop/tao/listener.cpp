#include "Printer.h"
#include "ace/Get_Opt.h"

ACE_RCSID (Printer, listener, "listener.cpp")
  
static char* conf[] = {"", "-ORBsvcconf", "server.conf"};
static int nb_conf_param = 3;
static const char* groupURL_file = "groupURL";

bool print_ior = false;

int
main (int argc, char *argv[])
{
  ACE_TRY_NEW_ENV
    {
      conf[0] = argv[0];

      CORBA::ORB_var orb =
        CORBA::ORB_init (nb_conf_param, conf, "" ACE_ENV_ARG_PARAMETER);
      ACE_TRY_CHECK;

      CORBA::Object_var poa_object =
        orb->resolve_initial_references("RootPOA" ACE_ENV_ARG_PARAMETER);
      ACE_TRY_CHECK;

      PortableServer::POA_var root_poa =
        PortableServer::POA::_narrow (poa_object.in () ACE_ENV_ARG_PARAMETER);
      ACE_TRY_CHECK;

      if (CORBA::is_nil (root_poa.in ()))
        ACE_ERROR_RETURN ((LM_ERROR,
                           " (%P|%t) Panic: nil RootPOA\n"),
                          1);

      PortableServer::POAManager_var poa_manager =
        root_poa->the_POAManager (ACE_ENV_SINGLE_ARG_PARAMETER);
      ACE_TRY_CHECK;

      switch (argc) {
      case 1: break;
      case 2: if (strcmp (argv[1], "-v") == 0) {
	print_ior = true;
	break;
      }
      default:
	cout << "usage:  " << argv[0] << " [-v]" << "\n";
	return 1;
      }

      // Get the group IOR.
      FILE *input_file= ACE_OS::fopen (groupURL_file, "r");
      if (input_file == 0)
        ACE_ERROR_RETURN ((LM_ERROR,
                           "Cannot open intput file for reading IOR: %s",
                           groupURL_file),
			  1);
      char groupURL[1000];
      int i = 0;
      int k;
      while ((k = ACE_OS::fgetc (input_file)) > 32) {
	groupURL[i++] = k;
	  }
      groupURL[i] = 0;
      ACE_OS::fclose (input_file);

      CORBA::String_var ior = CORBA::string_dup (groupURL);
      CORBA::Object_var group1 =
        orb->string_to_object (ior.in () ACE_ENV_ARG_PARAMETER);
      ACE_TRY_CHECK;

      cout << "Group IOR : '" << ior.in () << "'\n";
      PortableServer::ObjectId_var id =
        root_poa->create_id_for_reference (group1.in ()
                                           ACE_ENV_ARG_PARAMETER);
      ACE_TRY_CHECK;

      // Create and activate an instance of our servant.
      Printer server_impl (orb.in (), 0);

      root_poa->activate_object_with_id (id.in (),
                                         &server_impl
                                         ACE_ENV_ARG_PARAMETER);
      ACE_TRY_CHECK;
      
      if (print_ior) {
	Test::Printer_var server =
	  server_impl._this (ACE_ENV_SINGLE_ARG_PARAMETER);
	ACE_TRY_CHECK;
	
	cout << "Object IOR : '" << orb->object_to_string (server.in () ACE_ENV_ARG_PARAMETER) << "'\n";
      }

      poa_manager->activate (ACE_ENV_SINGLE_ARG_PARAMETER);
      ACE_TRY_CHECK;

      orb->run (ACE_ENV_SINGLE_ARG_PARAMETER);
      ACE_TRY_CHECK;

      ACE_DEBUG ((LM_DEBUG, "(%P|%t) server - event loop finished\n"));

      root_poa->destroy (1, 1 ACE_ENV_ARG_PARAMETER);
      ACE_TRY_CHECK;

      orb->destroy (ACE_ENV_SINGLE_ARG_PARAMETER);
      ACE_TRY_CHECK;
    }
  ACE_CATCHANY
    {
      ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION,
                           "Exception caught:");
      return 1;
    }
  ACE_ENDTRY;

  return 0;
}
