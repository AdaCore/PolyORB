//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.16 $
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
#include "Ada_netBufferedStream.hh"
#include "Ada_exceptions.hh"


//----------------------------------------------//
// Ada_netBufferedStream::Ada_netBufferedStream //
//----------------------------------------------//

Ada_netBufferedStream::Ada_netBufferedStream ()
{
  CPP_Object = 0;
  Init_Ok = false;
};


//-----------------------------//
// Ada_netBufferedStream::Init //
//-----------------------------//

void
Ada_netBufferedStream::Init (Rope *r, _CORBA_Boolean RdLock,
                             _CORBA_Boolean WrLock, size_t Bufsize)
{
  ADABROKER_TRY

    // Create underlying omniobject_C2Ada object.
    CPP_Object = new NetBufferedStream (r,RdLock,WrLock,Bufsize);

    // Update Init_OK flag.
    Init_Ok = true;
    return;

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_Char a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_Char& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_Boolean a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_Boolean& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_Short a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_Short& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_UShort a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_UShort& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY
  
    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_Long a, Ada_netBufferedStream &s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}

//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_Long& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<=  *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_ULong a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY
    
    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_ULong& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream:;marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_Float a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_Float& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_Double a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);
    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_Double& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//----------------------------------------------------//
// Ada_netBufferedStream::isReUsingExistingConnection //
//----------------------------------------------------//

_CORBA_Boolean
Ada_netBufferedStream::isReUsingExistingConnection()
{
  ADABROKER_TRY

    if (Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      return CPP_Object->isReUsingExistingConnection();

    } else {
      //  Raise an Ada exception,
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::isReUsingExistingConnection : uninitialized object.");
    }

  ADABROKER_CATCH

    // Never reach this code. Just a default return for dummy
    // compilers.
    CORBA::Boolean default_result = false;
    return default_result; 
}







