//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.19 $
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

Ada_Giop_s::Ada_Giop_s (GIOP_S *c_obj)
{
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
