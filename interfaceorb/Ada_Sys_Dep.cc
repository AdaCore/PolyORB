////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is system dependant. It completes the sys_dep       ////
////     Ada class into making some tests.                              ////
////     Most of these test send error if the suppositions made to      ////
////     interface C and Ada on the internal representation of C        ////
////     types are false                                                ////
////                                                                    ////
////                Date : 02/17/99                                     ////
////                                                                    ////
////                authors : Sebastien Ponce                           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////

#include <omniORB2/CORBA_sysdep.h>
#include "iostream"

class myException {} ;

int main ()
{
  if (sizeof(bool) != 1)
    {
      cerr << "La taille du type C++ Bool est non standard.\n AdaBroker ne peut compiler.\n";
      throw new myException ;
    } ;

# ifdef NO_FLOAT
      cerr << "Le compilateur C ne fournit pas de type Float.\n AdaBroker ne peut compiler.\n";
      throw new myException ;  
# endif

  return 0 ;
};
