//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.11 $
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
#include "Ada_memBufferedStream.hh"
#include "Ada_exceptions.hh"

//----------------------------------------------//
// Ada_memBufferedStream::Ada_memBufferedStream //
//----------------------------------------------//

Ada_memBufferedStream::Ada_memBufferedStream ()
{
  Init_Ok = false;
};

  
//-----------------------------//
// Ada_memBufferedStream::Init //
//-----------------------------//

void
Ada_memBufferedStream::Init (size_t Bufsize)
{
  ADABROKER_TRY

    // Create underlying omniobject_C2Ada object.
    CPP_Object = new MemBufferedStream (Bufsize);

    // Update Init_OK flag
    Init_Ok = true;
    return;

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_Char a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::marshall : initialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_Char& a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialited, call the corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_Boolean a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_Boolean& a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_Short a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_Short& a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_UShort a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_UShort& a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY
    
    if (s.Init_Ok) {
      // If already uninitialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_Long a, Ada_memBufferedStream& s)
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
       "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_Long& a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_ULong a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_ULong& a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_Float a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_Float& a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY
    
    if (s.Init_Ok) {
      // It already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_Double a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_Double& a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}
