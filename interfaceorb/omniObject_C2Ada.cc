////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class provides procedures and functions                   ////
////     for the server to call the Ada skeletons                       ////
////                                                                    ////
////                Date : 02/05/99                                     ////
////                                                                    ////
////                authors : Fabien Azavant, Sebastien Ponce           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////


#include "omniOject_C2Ada.hh"


CORBA::Boolean omniObject_C2Ada::dispatch(GIOP_S &giop_s,const char *operation,
					  _CORBA_Boolean response_expected) {
  void* result;
  result = dispatch_ada (AdaObject, giop_s, operation, response_expected);
  if (result == null) {
    return 0;
  } else {
    return 1;
  }
}

  void* _widenFromTheMostDerivedIntf(const char* type_id,
				     _CORBA_Boolean is_cxx_type_id=0);


