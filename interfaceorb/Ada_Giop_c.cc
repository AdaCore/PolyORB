//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.27 $
//                                                                          //
//         Copyright (C) 1999-2000 ENST Paris University, France.           //
//                                                                          //
// AdaBroker is free software; you  can  redistribute  it and/or modify it  //
// under terms of the  GNU General Public License as published by the  Free //
// Software Foundation;  either version 2,  or (at your option)  any  later //
// version. AdaBroker  is distributed  in the hope that it will be  useful, //
// but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- //
// TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public //
// License  for more details.  You should have received  a copy of the GNU  //
// General Public License distributed with AdaBroker; see file COPYING. If  //
// not, write to the Free Software Foundation, 59 Temple Place - Suite 330, //
// Boston, MA 02111-1307, USA.                                              //
//                                                                          //
// As a special exception,  if other files  instantiate  generics from this //
// unit, or you link  this unit with other files  to produce an executable, //
// this  unit  does not  by itself cause  the resulting  executable  to  be //
// covered  by the  GNU  General  Public  License.  This exception does not //
// however invalidate  any other reasons why  the executable file  might be //
// covered by the  GNU Public License.                                      //
//                                                                          //
//             AdaBroker is maintained by ENST Paris University.            //
//                     (email: broker@inf.enst.fr)                          //
//                                                                          //
//--------------------------------------------------------------------------//
#include "Ada_Giop_c.hh"
#include "Ada_exceptions.hh"

//------------------------//
// Ada_Giop_c::Ada_Giop_c //
//------------------------//

Ada_Giop_c::Ada_Giop_c () : Ada_netBufferedStream ()
{
  // everything is done in the parent constructor.
}

//------------------//
// Ada_Giop_c::Init //
//------------------//

void
Ada_Giop_c::Init (Rope *r)
{
  ADABROKER_TRY

    if (CPP_Object) {
      // If we already have an object, release it before creating a
      // new one.
      delete (GIOP_C*) CPP_Object;
    }

    else {
      if (omniORB::traceLevel > 5) {
        omniORB::log << "Ada_Giop_c::Init: initializing new Giop_c\n";
        omniORB::log.flush();
      }
    }

  CPP_Object = new GIOP_C (r);
  Init_Ok = true;

  ADABROKER_CATCH
}


//------------------//
// Ada_Giop_c::Free //
//------------------//

void
Ada_Giop_c::Free()
{
  ADABROKER_TRY

    if (CPP_Object) {
      delete (GIOP_C*) CPP_Object;
      CPP_Object = 0;
    }
    Init_Ok = false;

  ADABROKER_CATCH
}


//-------------------------------//
// Ada_Giop_c::InitialiseRequest //
//-------------------------------//

void
Ada_Giop_c::InitialiseRequest(const void          *objkey,
			      const size_t         objkeysize,
			      const char          *opname,
			      const size_t         opnamesize,
			      const size_t         msgsize,
			      const _CORBA_Boolean oneway)
{
  ADABROKER_TRY

    if (Init_Ok) {
      // If already initialized, call corresponding function on
      // CPP_Object.
      ((GIOP_C *) CPP_Object)->InitialiseRequest(objkey,
					         objkeysize,
					         opname,
					         opnamesize,
					         msgsize,
					         oneway);
    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
         (__FILE__,__LINE__,
          "Ada_Giop_c::InitialiseRequest: uninitialized object.");
  }
ADABROKER_CATCH
}

//-------------------------//
// Ada_Gio_c::ReceiveReply //
//-------------------------//

void
Ada_Giop_c::ReceiveReply(GIOP::ReplyStatusType &result)
{
  ADABROKER_TRY

    if (Init_Ok) {
      // Ifalready initialized, call corresponding function on
      // CPP_Object.

      if (omniORB::traceLevel > 5) {
	omniORB::log << "Ada_Giop_c::ReceiveReply: enter\n";
        omniORB::log.flush();
      }

      result = ((GIOP_C *) CPP_Object)->ReceiveReply();

      if (omniORB::traceLevel > 5) {
	omniORB::log << "Ada_Giop_c::ReceiveReply: leave\n";
        omniORB::log.flush();
      }

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Call of Ada_Giop_c::ReceiveReply: uninitialized object.");
    }

  ADABROKER_CATCH
}


//------------------------------//
// Ada_Giop_c::RequestCompleted //
//------------------------------//

void
Ada_Giop_c::RequestCompleted(_CORBA_Boolean skip)
{
  ADABROKER_TRY

    if (Init_Ok) {
      // If already initialized, call corresponding function on
      // CPP_Object.
      ((GIOP_C *) CPP_Object)->RequestCompleted (skip);
    } else {
      // Raise an Ada exception.
    throw omniORB::fatalException
      (__FILE__,
       __LINE__,
       "Ada_Giop_c::RequestCompleted: unitialized object.");
  }

  ADABROKER_CATCH
}


//-------------------------------//
// Ada_Giop_c::RequestHeaderSize //
//-------------------------------//

size_t
Ada_Giop_c::RequestHeaderSize(const size_t objkeysize,
			      const size_t opnamesize)
{
  ADABROKER_TRY

    return GIOP_C::RequestHeaderSize (objkeysize,opnamesize);

  ADABROKER_CATCH

    // Never reach this code. Just a default return for dummy
    // compilers.
    size_t default_result = 0;
    return default_result; 
}

