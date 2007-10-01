#include "printC.h"
#include "ace/Get_Opt.h"

ACE_RCSID(Printer, send, "send.cpp")

static char* conf[] = {"", "-ORBsvcconf", "client.conf"};
static int nb_conf_param = 3;

enum mode {S, L, TWS, TWL};

int
main (int argc, char *argv[])
{
  mode curr_mode = S;

  conf[0] = argv[0];

  ACE_TRY_NEW_ENV
    {
      CORBA::ORB_var orb =
        CORBA::ORB_init (nb_conf_param, conf, "" ACE_ENV_ARG_PARAMETER);
      ACE_TRY_CHECK;

      switch (argc) {
      case 2: break;
      case 3: 
	if (strcmp(argv[2], "s") == 0) {
	  curr_mode = S;
	  break;
	}
	else if (strcmp(argv[2], "l") == 0) {
	  curr_mode = L;
	  break;
	}
	else if (strcmp(argv[2], "tws") == 0) {
	  curr_mode = TWS;
	  break;
	}
	else if (strcmp(argv[2], "twl") == 0) {
	  curr_mode = TWL;
	  break;
	}
      default:
	cout << "usage:  " << argv[0] << " ior [s|l|tws|twl]" << "\n";
	return 1;
      }

      CORBA::Object_var tmp =
        orb->string_to_object(argv[1] ACE_ENV_ARG_PARAMETER);
      ACE_TRY_CHECK;

      /* Do an unchecked narrow since there's no way to do an is_a on
       * a multicast reference (yet...).
       */
      Test::Printer_var p =
        Test::Printer::_unchecked_narrow (tmp.in () ACE_ENV_ARG_PARAMETER);
      ACE_TRY_CHECK;

      if (CORBA::is_nil (p.in ()))
        {
          ACE_ERROR_RETURN ((LM_DEBUG,
                             "Nil Test::Printer reference <%s>\n",
                             argv[1]),
                            1);
        }

      switch (curr_mode) {
      case S:
	p->printString ("Hello multicast world !" ACE_ENV_ARG_PARAMETER);
	ACE_TRY_CHECK;
	break;
      case L:
	p->printLong (42 ACE_ENV_ARG_PARAMETER);
	ACE_TRY_CHECK;
	break;
      case TWS:
	{
#define STR_TEST "Hello multicast world !"
	  char* str = p->echoString (STR_TEST ACE_ENV_ARG_PARAMETER);
	  if (strcmp (str, STR_TEST) != 0) cout << "Echo string failed" << "\n";
	  ACE_TRY_CHECK;
	  break;
	}
      case TWL:
	{
#define K 42
	  CORBA::Long l = p->echoLong (K ACE_ENV_ARG_PARAMETER);
	  if (l != K) cout << "Echo long failed" << "\n";
	  ACE_TRY_CHECK;
	  break;
	}
      }

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
