////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is both a C and an Ada Class (see omniObject.ads).  ////
////     It is wrapped around Rope in order to avoid the presence       ////
////     of non default constructors.                                   ////
////     So, it provides the same functions Rope except that cons-      ////
////     tructors are replaced by Init functions.                       ////
////     It has also a pointer on the underlining Rope Object           ////
////                                                                    ////
////                                                                    ////
////                Date : 02/17/99                                     ////
////                                                                    ////
////                authors : Sebastien Ponce                           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////

#include <omnithread.h>
#include <omniORB2/CORBA_sysdep.h>
#include <omniORB2/CORBA_basetypes.h>

#include <omniORB2/rope.h>

class Ada_Rope {

public:

  Ada_Rope ();
  // Default constructor
  
private:

  Rope *C_Rope;
  // Pointer on the underlining Rope object

  bool Init_Ok;
  // This flag tells whether an init function was called or not
  
  void raise_ada_exception (const char *msg);
  // this function allows C code to raise Ada exception
  // It is implemented in Ada and only raise a No_Initialisation
  // exception with the message msg. (see omniobject.ads)
};






