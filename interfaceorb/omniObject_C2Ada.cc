//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.30 $
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
#include "omniObject_C2Ada.hh"
#include "Ada_exceptions.hh"

//------------------------------------//
// omniObject_C2Ada::omniObject_C2Ada //
//------------------------------------//

omniObject_C2Ada::omniObject_C2Ada (Ada_OmniObject * to)
{
  To_Ada_OmniObject = to;
}

//--------------------------------------//
// omniObject_C2Ada::set_Ada_OmniObject //
//--------------------------------------//

void
omniObject_C2Ada::set_Ada_OmniObject (Ada_OmniObject * to)
{
  To_Ada_OmniObject = to;
}

//------------------------------------//
// omniObject_C2Ada::omniObject_C2Ada //
//------------------------------------//

omniObject_C2Ada::omniObject_C2Ada (const char             * repoId,
				    Rope                   * r,
				    _CORBA_Octet           * key,
				    size_t                   keysize,
				    IOP::TaggedProfileList * profiles,
				    _CORBA_Boolean           release)
  : omniObject (repoId,
		r,
		key,
		keysize,
		profiles,
		release)
{
  // Call omniObject constructor.
  To_Ada_OmniObject = 0;
}

//----------------------------//
// omniObject_C2Ada::dispatch //
//----------------------------//

_CORBA_Boolean
omniObject_C2Ada::dispatch (GIOP_S         & giop_s,
			    const char     * operation,
			    _CORBA_Boolean   response_expected)
{
  ADABROKER_TRY

    if (omniORB::traceLevel > 5) 
      cerr << "omniObject_C2Ada::dispatch : begin" << endl;
  
    // Declare arguments for the Ada function.
    Ada_Giop_s ada_giop_s (& giop_s);
  
    if (omniORB::traceLevel > 5)
      cerr << "omniObject_C2Ada::dispatch : Ada_Giop_s created" << endl;
  
    _CORBA_Boolean success;
  
    if (omniORB::traceLevel > 5) {
      cerr << "omniObject_C2Ada::dispatch : call the Ada code" << endl;
  
      if (To_Ada_OmniObject != 0) {
        cerr << "omniObject_C2Ada::dispatch : To_Ada_OmniObject not null"
	     << endl;
      } else {
        cerr << "omniObject_C2Ada::dispatch : To_Ada_OmniObject *IS* null"
	     << endl;
      }
      cerr << "omniObject_C2Ada::dispatch is_proxy ? " << is_proxy() << endl;
    }
  
    To_Ada_OmniObject->dispatch (ada_giop_s,
				 operation,
				 response_expected,
				 success);
  
    if (omniORB::traceLevel > 5)
      cerr << "omniObject_C2Ada::dispatch : return from Ada code" << endl;
    
    return success;
    // Call dispatch on the Ada_OmniObject pointed by
    // To_Ada_OmniObject. This function allows the C code to call the
    // Ada function dispatch.

  ADABROKER_CATCH

    // Never reach this code. Just a default return for dummy
    // compilers.
    bool default_result = false;
    return default_result; 
}

//------------------------------------------------//
// omniObject_C2Ada::_widenFromTheMostDerivedIntf //
//------------------------------------------------//

void*
omniObject_C2Ada::_widenFromTheMostDerivedIntf (const char    * repoId,
					       _CORBA_Boolean   is_cxx_type_id)
{
  if (!repoId) {
    CORBA::Object_ptr tmp = new CORBA::Object;
    tmp->PR_setobj (this);
    return (void *) tmp;
  }
  if (To_Ada_OmniObject->Ada_Is_A (repoId)) {
    return (void *) this;
  } else {
    return 0;
  }
}

//--------------------------------------//
// omniObject_C2Ada::get_Ada_OmniObject //
//--------------------------------------//

Ada_OmniObject *
omniObject_C2Ada::get_Ada_OmniObject ()
{
  return To_Ada_OmniObject;
}
