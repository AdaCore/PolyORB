#ifndef __OMNIOBJECT_C2ADA_H__
#define __OMNIOBJECT_C2ADA_H__

#include <omniORB2/omniInternal.h>
#include "Ada_Giop_s.hh"
#include "Ada_OmniObject.hh"

class Ada_OmniObject;

class omniObject_C2Ada : public  omniObject {
  
public:
  
  omniObject_C2Ada (Ada_OmniObject * to);
  // Constructor for local objects. omniObjectManager is not needed.
  
  omniObject_C2Ada (const char             * repoId,
		    Rope                   * r,
		    _CORBA_Octet           * key,
		    size_t                   keysize,
		    IOP::TaggedProfileList * profiles,
		    _CORBA_Boolean           release);
  // Constructor for proxy objects.

  ~omniObject_C2Ada () {};
  // Destructor.
  
  void set_Ada_OmniObject (Ada_OmniObject * to);
  // Set To_Ada_OmniObject when Ada_OmniObject is defined after
  // omniObject_C2Ada one.

  virtual _CORBA_Boolean dispatch (GIOP_S        &,
				   const char    * operation,
				  _CORBA_Boolean   response_expected);
  // Override omniObject::dispatch. Call dispatch on Ada_Omniobject.

  virtual void * _widenFromTheMostDerivedIntf
      (const char * repoId,
       _CORBA_Boolean is_cxx_type_id = 0);
  // Override CORBA::Object_ptr.
  
  Ada_OmniObject * get_Ada_OmniObject ();
  // Return underlying Ada_Omniobject object.
  
private:

  Ada_OmniObject * To_Ada_OmniObject;
  // Pointer on a Ada_OmniObject which contains the real dispatch function.
  
  friend class Ada_OmniObject;
  // Ada_OmniObject must have full access to this class.

};

#endif
