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

#include "Ada_Giop_s.hh"

Ada_Giop_s::Ada_Giop_s ()
{
  Init_Ok = false;
  C_Giop_s = NULL;
};
// Default Constructor
  
void
Ada_Giop_s::Init (Strand *s)
{
  C_Giop_s = new GIOP_S (s);
  Init_Ok = true;
};

void
Ada_Giop_s::RequestReceived(_CORBA_Boolean skip)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Giop_s
    C_Giop_s->RequestReceived(skip);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_Giop_s::RequestReceived without initialising object.");
  }
};

void
Ada_Giop_s::InitialiseReply(const int status,
			    const size_t  msgsize)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Giop_s
    C_Giop_s->InitialiseReply((GIOP::ReplyStatusType) status,msgsize);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_Giop_s::InitialiseReply without initialising object.");
  }
};

void
Ada_Giop_s::ReplyCompleted()
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Giop_s
    C_Giop_s->ReplyCompleted();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_Giop_s::ReplyCompleted without initialising object.");
  }
};
