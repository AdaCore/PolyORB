#include "Printer.h"

ACE_RCSID(Printer, Printer, "Printer.cpp, Bertrand Paquet")

Printer::Printer (CORBA::ORB_ptr orb,
		  int instance)
  : orb_ (CORBA::ORB::_duplicate (orb)),
    instance_ (instance)
{}

void
Printer::printLong (CORBA::Long K ACE_ENV_ARG_DECL_NOT_USED)
  ACE_THROW_SPEC ((CORBA::SystemException))
{
  cout << "Received Long : " << K << "\n";
}

void
Printer::printString (const char * Msg ACE_ENV_ARG_DECL_NOT_USED)
  ACE_THROW_SPEC ((CORBA::SystemException))
{
  cout << "Received String : " << Msg << "\n"; 
}

CORBA::Long 
Printer::echoLong (CORBA::Long K ACE_ENV_ARG_DECL_NOT_USED)
  ACE_THROW_SPEC ((CORBA::SystemException))
{
  cout << "Echo Long : " << K << "\n";
  return K;
}

char*
Printer::echoString (const char * Msg ACE_ENV_ARG_DECL_NOT_USED)
  ACE_THROW_SPEC ((CORBA::SystemException))
{
  char * z = new char[strlen(Msg)];
  cout << "Echo String : " << Msg << "\n"; 
  strcpy (z, Msg);
  return z;
}
