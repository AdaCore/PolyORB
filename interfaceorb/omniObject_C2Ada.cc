////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class provides is a descendant of the omniObject          ////
////     class. It provides the sames functions plus a pointer          ////
////     on a Ada_OmniObject.                                           ////
////                                                                    ////
////     Furthermore, the function dipatch is implemented and           ////
////     simply calls the Ada one. It allows the C code of              ////
////     omniORB to call the Ada objects.                               ////
////                                                                    ////
////                                                                    ////
////                Date : 02/16/99                                     ////
////                                                                    ////
////                authors : Sebastien Ponce                           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////


#include "omniObject_C2Ada.hh"

omniObject_C2Ada::omniObject_C2Ada (omniObjectManager *p,
				    Ada_OmniObject *Ada_Ptr) : omniObject (p)
{
  // Appel du constructeur de omniObject et initialisation
  // du pointeur sur l'objet Ada associe
  Ada_OmniObject_Ptr = Ada_Ptr;
};


omniObject_C2Ada::omniObject_C2Ada(const char *repoId,
				   Rope *r,
				   _CORBA_Octet *key,
				   size_t keysize,
				   IOP::TaggedProfileList *profiles,
				   _CORBA_Boolean release,
				   Ada_OmniObject *Ada_Ptr) : omniObject (repoId,
									  r,
									  key,
									  keysize,
									  profiles,
									  release)
{
  // Appel du constructeur de omniObject et initialisation
  // du pointeur sur l'objet Ada associe
  Ada_OmniObject_Ptr = Ada_Ptr;  
};


_CORBA_Boolean
omniObject_C2Ada::dispatch(GIOP_S &giop,
			   const char *operation,
			   _CORBA_Boolean response_expected)
{
  return Ada_OmniObject_Ptr->dispatch(giop,
				      operation,
				      response_expected);
};

