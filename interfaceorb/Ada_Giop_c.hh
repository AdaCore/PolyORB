////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is both a C class and an Ada Class (see             ////
////     giop_c.ads). It is wrapped around GIOP_C in order to           ////
////     avoid the presence of non default constructors.                ////
////     So, it provides the same functions as GIOP_C except that       ////
////     constructors are replaced by Init functions.                   ////
////     It has also a pointer on the underlining GIOP_C object.        ////
////                                                                    ////
////                                                                    ////
////                Date : 02/28/99                                     ////
////                                                                    ////
////                author : Sebastien Ponce                            ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////


#include "omniORB2/omniInternal.h"

class Ada_Giop_c {
  
public:

  Ada_Giop_c ();
  // Default Constructor
  
  void Init (Rope *r);
  // Initialisation of Ada_Giop_c, calls the
  // underlining GIOP_C constructor


  void InitialiseRequest(const void          *objkey,
			 const size_t         objkeysize,
			 const char          *opname,
			 const size_t         opnamesize,
			 const size_t         msgsize,
			 const _CORBA_Boolean oneway);
  // wrapper around void InitialiseRequest(const void          *objkey,
  //               			   const size_t         objkeysize,
  //			                   const char          *opname,
  //			                   const size_t         opnamesize,
  //			                   const size_t         msgsize,
  //			                   const _CORBA_Boolean oneway);

  GIOP::ReplyStatusType ReceiveReply();
  // wrapper around GIOP::ReplyStatusType ReceiveReply();

  void RequestCompleted(_CORBA_Boolean skip);
  // wrapper around void RequestCompleted(_CORBA_Boolean skip=0);

  
private:

  GIOP_C *C_Giop_c;
  // Pointer on the underlining Giop_s object

  bool Init_Ok;
  // This flag tells whether an init function was called or not

};

extern void raise_ada_exception (const char *msg);
// this function allows C code to raise Ada exception
// It is implemented in Ada and only raise a No_Initialisation
// exception with the message msg. (see corba.ads)

