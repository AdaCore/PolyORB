//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.7 $
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
#include "Ada_exceptions.hh"
#include "Ada_Iop.hh"

void
marshall(IOP::TaggedProfileList * t,
	 Ada_netBufferedStream  & s)
{
  ADABROKER_TRY

    if (t != NULL)
      *t >>= *(s.CPP_Object);
    else
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_Iop::marshall: null TaggedProfileList");

  ADABROKER_CATCH
}


void
unmarshall(IOP::TaggedProfileList * & t,
	   Ada_netBufferedStream    & s)
{
  ADABROKER_TRY

    // Create the TaggedProfileList corresponding to the object. It
    // will be released by the ORB when the last reference to the
    // omniObject using this TaggedProfileList will be released. See
    // CORBA::UnMarshalObjRef in objectRef.cc.

    t = new IOP::TaggedProfileList();
    *t <<= *(s.CPP_Object);

  ADABROKER_CATCH
}


void
marshall(IOP::TaggedProfileList * t,
	 Ada_memBufferedStream  & s)
{
  ADABROKER_TRY

    if (t != NULL)
      *t >>= *(s.CPP_Object);
    else
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_Iop::marshall: null TaggedProfileList");

  ADABROKER_CATCH
}


void
unmarshall(IOP::TaggedProfileList * & t,
	   Ada_memBufferedStream    & s)
{
  ADABROKER_TRY

    // Create TaggedProfileList corresponding to object. It will be
    // released by the ORB when the last reference to the omniObject
    // using this TaggedProfileList will be released. See
    // CORBA::UnMarshalObjRef in objectRef.cc.
    
    t = new IOP::TaggedProfileList();
    *t <<= *(s.CPP_Object);

  ADABROKER_CATCH
}


size_t
NP_alignedSize (IOP::TaggedProfileList* t, size_t initialoffset)
{
  ADABROKER_TRY

    if (t != NULL)
      return t->NP_alignedSize (initialoffset);
    else
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_Iop::NP_alignedSize: null TaggedProfileList");

  ADABROKER_CATCH

    // Never reach this code. Just a default return for dummy
    // compilers.
    size_t default_result = 0;
    return default_result; 
}


size_t
length (IOP::TaggedProfileList* t)
{
  ADABROKER_TRY

    if (t != NULL)
      return t->length ();
    else
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_Iop::length: null TaggedProfileList");

  ADABROKER_CATCH
    // Never reach this code. Just a default return for dummy
    // compilers.
    size_t default_result = 0;
    return default_result; 
}

