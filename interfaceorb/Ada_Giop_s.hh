////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is both a C class and an Ada Class (see             ////
////     giop_s.ads). It is wrapped around GIOP_S in order to           ////
////     avoid the presence of non default constructors.                ////
////     So, it provides the same functions as GIOP_S except that       ////
////     constructors are replaced by Init functions.                   ////
////     It has also a pointer on the underlining GIOP_S object.        ////
////                                                                    ////
////                                                                    ////
////                Date : 02/28/99                                     ////
////                                                                    ////
////                author : Sebastien Ponce                            ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////


#include "omniORB2/omniInternal.h"

class Ada_Giop_s {
  
public:

  Ada_Giop_s ();
  // Default Constructor
  
  void Init (Strand *s);
  // Initialisation of Ada_Giop_s, calls the
  // underlining GIOP_S constructor

  void RequestReceived(_CORBA_Boolean skip);
  // wrapper around void RequestReceived(_CORBA_Boolean skip=0);

  void InitialiseReply(const int status,
        	       const size_t  msgsize);
  // wrapper around void InitialiseReply(const GIOP::ReplyStatusType status,
  //		                         const size_t  msgsize);

  void ReplyCompleted();
  // wrapper around void ReplyCompleted();
  
  
private:

  GIOP_S *C_Giop_s;
  // Pointer on the underlining Giop_s object

  bool Init_Ok;
  // This flag tells whether an init function was called or not

};

extern void raise_ada_exception (const char *msg);
// this function allows C code to raise Ada exception
// It is implemented in Ada and only raise a No_Initialisation
// exception with the message msg. (see corba.ads)

