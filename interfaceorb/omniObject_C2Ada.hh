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


#include "omniORB2/omniInternal.h"

class omniObject_C2Ada : public omniObject {

  extern _CORBA_Boolean dispatch(GIOP_S &,const char *operation,
				 _CORBA_Boolean response_expected);
  
private:
  void* AdaObject;

};
