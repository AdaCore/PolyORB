#include "Ada_Giop_s.hh"
#include "Ada_exceptions.hh"

//------------------------//
// Ada_Giop_s::Ada_Giop_s //
//------------------------//

Ada_Giop_s::Ada_Giop_s () : Ada_netBufferedStream::Ada_netBufferedStream ()
{
  // everything is done in the parent constructor.
}

  
//------------------------//
// Ada_Giop_s::Ada_Giop_s //
//------------------------//

Ada_Giop_s::Ada_Giop_s (GIOP_S *c_obj) {
  Init_Ok = true;
  CPP_Object = c_obj; 
}


//------------------//
// Ada_Giop_s::Init //
//------------------//

void
Ada_Giop_s::Init (Strand *s)
{
  CPP_Object = new GIOP_S (s);
  Init_Ok = true;
}


//-----------------------------//
// Ada_Giop_s::RequestReceived //
//-----------------------------//

void
Ada_Giop_s::RequestReceived(_CORBA_Boolean skip)
{
  ADABROKER_TRY

    if (Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      ( (GIOP_S *) CPP_Object )->RequestReceived(skip);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_Giop_s::RequestReceived: uninitialized object.");
    }

  ADABROKER_CATCH
}

//-----------------------------//
// Ada_Giop_s::InitialiseReply //
//-----------------------------//

void
Ada_Giop_s::InitialiseReply(const int status,
			    const size_t  msgsize)
{
  ADABROKER_TRY

    if (Init_Ok) {
      // If already initialized then call corresponding function on
      // CPP_Object.
      ((GIOP_S *) CPP_Object)->InitialiseReply
	((GIOP::ReplyStatusType) status, msgsize);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_Giop_s::InitialiseReply: uninitialized object.");
  }

  ADABROKER_CATCH 
}


//----------------------------//
// Ada_Giop_s::ReplyCompleted //
//----------------------------//

void
Ada_Giop_s::ReplyCompleted()
{
  ADABROKER_TRY
    if (Init_Ok) {
      // If already initialized then call corresponding function on
      // CPP_Object.
      ((GIOP_S *) CPP_Object)->ReplyCompleted();

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_Giop_s::ReplyCompleted: uninitialized object.");
    }

  ADABROKER_CATCH 
}


//-----------------------------//
// Ada_Giop_s::ReplyHeaderSize //
//-----------------------------//

size_t
Ada_Giop_s::ReplyHeaderSize()
{
  ADABROKER_TRY

    size_t result = GIOP_S::ReplyHeaderSize ();

    if (omniORB::traceLevel > 5) 
      cerr << "Ada_Giop_s::ReplyHeaderSize: return " << result << endl;

    return result;

  ADABROKER_CATCH

    // Never reach this ceode. Just a default return for dummy
    // compilers.
    size_t default_result = 0;
    return default_result; 
}
