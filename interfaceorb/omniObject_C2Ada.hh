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


#include "omniORB2/omniInternal.h"

#ifndef __Ada_OmniObject__
#define __omniObject_C2Ada__
#include "Ada_OmniObject.hh"
#endif

class Ada_OmniObject;

class omniObject_C2Ada : public omniObject
{
protected:
  omniObject_C2Ada(omniObjectManager *p,
		   Ada_OmniObject* Ada_Ptr);
  // constructor for local objects
  
  
  omniObject_C2Ada(const char *repoId,
		   Rope *r,
		   _CORBA_Octet *key,
		   size_t keysize,
		   IOP::TaggedProfileList *profiles,
		   _CORBA_Boolean release,
		   Ada_OmniObject *Ada_Ptr);
  // constructor for proxy objects

  
  virtual _CORBA_Boolean dispatch(GIOP_S &,const char *operation,
				  _CORBA_Boolean response_expected);
  // overwrites omniObject::dispatch
  // calls dispatch on Ada_Omniobject

  friend Ada_OmniObject;
  // Ada_OmniObject must have full acces to this class
  
private:
  Ada_OmniObject* Ada_OmniObject_Ptr;
  // pointer on a Ada_Omniobject which contains the real dispatch function
};



