////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is both a C class and an Ada Class (see             ////
////     giop_c.ads). It is wrapped around GIOP_C in order to           ////
////     avoid the presence of non default constructors.                ////
////     So, it provides the same functions as GIOP_C except that       ////
////     constructors are replaced by Init functions.                   ////
////     It has also a pointer on the underlying GIOP_C object.         ////
////                                                                    ////
////                                                                    ////
////                Date : 02/28/99                                     ////
////                                                                    ////
////                author : Sebastien Ponce                            ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////

#include "Ada_Giop_c.hh"


Ada_Giop_c::Ada_Giop_c ()
{
  Init_Ok = false;
  C_Giop_c = NULL;
};
// Default Constructor
  
void
Ada_Giop_c::Init (Rope *r)
{
  C_Giop_c = new GIOP_C (r);
  Init_Ok = true;
};


void
Ada_Giop_c::InitialiseRequest(const void          *objkey,
			      const size_t         objkeysize,
			      const char          *opname,
			      const size_t         opnamesize,
			      const size_t         msgsize,
			      const _CORBA_Boolean oneway)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Giop_c
    C_Giop_c->InitialiseRequest(objkey,
				objkeysize,
				opname,
				opnamesize,
				msgsize,
				oneway);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_Giop_c::InitialiseRequest without initialising object.");
  }
};


GIOP::ReplyStatusType
Ada_Giop_c::ReceiveReply()
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Giop_c
    return C_Giop_c->ReceiveReply();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_Giop_c::ReceiveReply without initialising object.");
  }
};


void
Ada_Giop_c::RequestCompleted(_CORBA_Boolean skip)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Giop_c
    C_Giop_c->RequestCompleted (skip);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_Giop_c::RequestCompleted without initialising object.");
  }
};
