////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is both a C and an Ada Class (see omniObject.ads).  ////
////     It is wrapped around Rope in order to avoid the presence       ////
////     of non default constructors.                                   ////
////     So, it provides the same functions Rope except that cons-      ////
////     tructors are replaced by Init functions.                       ////
////     It has also a pointer on the underlying Rope Object            ////
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
  // Pointer on the underlying Rope object

  bool Init_Ok;
  // This flag tells whether an init function was called or not
  
};






