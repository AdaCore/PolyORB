#include "omniObject_C2Ada.hh"
#include "Ada_exceptions.hh"

//------------------------------------//
// omniObject_C2Ada::omniObject_C2Ada //
//------------------------------------//

omniObject_C2Ada::omniObject_C2Ada (Ada_OmniObject *Ada_Ptr) : omniObject ()
{
  // Call omniObject constructor and initialize the pointer on the
  // Ada_OmniObject Ada_OmniObject_Pointer.
  Ada_OmniObject_Pointer = Ada_Ptr;
}


//------------------------------------//
// omniObject_C2Ada::omniObject_C2Ada //
//------------------------------------//

omniObject_C2Ada::omniObject_C2Ada(const char *repoId,
				   Rope *r,
				   _CORBA_Octet *key,
				   size_t keysize,
				   IOP::TaggedProfileList *profiles,
				   _CORBA_Boolean release)
  : omniObject (repoId,
		r,
		key,
		keysize,
		profiles,
		release)
{
  // Call omniObject constructor.
}


//----------------------------//
// omniObject_C2Ada::dispatch //
//----------------------------//

_CORBA_Boolean
omniObject_C2Ada::dispatch(GIOP_S &giop_s,
			   const char *operation,
			   _CORBA_Boolean response_expected)
{
  ADABROKER_TRY

    if (omniORB::traceLevel > 5) 
      cerr << "omniObject_C2Ada::dispatch : begin" << endl;
  
    // Declare arguments for the Ada function.
    Ada_Giop_s ada_giop_s(&giop_s);
  
    if (omniORB::traceLevel > 5)
      cerr << "omniObject_C2Ada::dispatch : Ada_Giop_s created" << endl;
  
    _CORBA_Boolean success;
  
    if (omniORB::traceLevel > 5) {
      cerr << "omniObject_C2Ada::dispatch : call the Ada code" << endl;
  
      if (Ada_OmniObject_Pointer != 0) {
        cerr << "omniObject_C2Ada::dispatch : Ada_OmniObject_Pointer not null"
	     << endl;
      } else {
        cerr << "omniObject_C2Ada::dispatch : Ada_OmniObject_Pointer *IS* null"
	     << endl;
      }
      cerr << "omniObject_C2Ada::dispatch is_proxy ? " << is_proxy() << endl;
    }
  
    Ada_OmniObject_Pointer->dispatch(ada_giop_s,
				     operation,
				     response_expected,
				     success);
  
    if (omniORB::traceLevel > 5)
      cerr << "omniObject_C2Ada::dispatch : return from Ada code" << endl;
    
    return success;
    // Call dispatch on the Ada_OmniObject pointed by
    // Ada_OmniObject_Pointer. This function allows the C code to call
    // the Ada function dispatch.

  ADABROKER_CATCH

    // Never reach this code. Just a default return for dummy compilers.
    bool default_result = false;
    return default_result; 
}
  

//------------------------------------------------//
// omniObject_C2Ada::_widenFromTheMostDerivedIntf //
//------------------------------------------------//

void*
omniObject_C2Ada::_widenFromTheMostDerivedIntf(const char* repoId,
					       _CORBA_Boolean is_cxx_type_id)
{
  if (Ada_OmniObject_Pointer->Ada_Is_A(repoId) ) {
    return (void*) this;
  } else {
    return 0;
  }
}


//--------------------------------------//
// omniObject_C2Ada::get_Ada_OmniObject //
//--------------------------------------//

Ada_OmniObject *
omniObject_C2Ada::get_Ada_OmniObject ()
{
  return Ada_OmniObject_Pointer;
}
