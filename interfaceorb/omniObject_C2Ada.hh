#include "omniORB2/omniInternal.h"

#include "Ada_Giop_s.hh"

#ifndef __Ada_OmniObject__
#define __omniObject_C2Ada__
#include "Ada_OmniObject.hh"
#endif

class Ada_OmniObject ;


class omniObject_C2Ada : public  omniObject {
  
public:
  
  omniObject_C2Ada(Ada_OmniObject* Ada_Ptr);
  // constructor for local objects
  // omniObjectManager is not needed
  
  omniObject_C2Ada(const char *repoId,
		   Rope *r,
		   _CORBA_Octet *key,
		   size_t keysize,
		   IOP::TaggedProfileList *profiles,
		   _CORBA_Boolean release);
  // constructor for proxy objects

  ~omniObject_C2Ada() {} ;
  // destructor of class
  
  virtual _CORBA_Boolean dispatch(GIOP_S &,const char *operation,
				  _CORBA_Boolean response_expected);
  // overwrites omniObject::dispatch
  // calls dispatch on Ada_Omniobject

  virtual void* _widenFromTheMostDerivedIntf(const char* repoId,
					     _CORBA_Boolean is_cxx_type_id=0) ;
  // surcharge CORBA::Object_ptr
  // see there for more details
  
  Ada_OmniObject * get_Ada_OmniObject ();
  // returns the underlying Ada_Omniobject object

  
private:

  Ada_OmniObject* Ada_OmniObject_Pointer;
  // pointer on a Ada_OmniObject which contains the real dispatch function
  
  friend class Ada_OmniObject;
  // Ada_OmniObject must have full acces to this class
};
