//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.6 $
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
#ifndef __ADA_CORBA_EXCEPTIONS_H__
#define __ADA_CORBA_EXCEPTIONS_H__

#include <omniORB2/CORBA.h>
#include "Ada_OmniObject.hh"

CORBA::Boolean
_omni_callTransientExceptionHandler(Ada_OmniObject* omniobj,
				    CORBA::ULong retries,
				    CORBA::ULong minor,
				    CORBA::CompletionStatus status);
// equivalent of _omni_callTransientExceptionHandler(omniObject*,
//             				             CORBA::ULong,
//				                     const CORBA::TRANSIENT&);
// (see CORBA.h L 2702)


CORBA::Boolean
_omni_callCommFailureExceptionHandler(Ada_OmniObject*,
				      CORBA::ULong,
				      CORBA::ULong minor,
				      CORBA::CompletionStatus status);
// equivalent of _omni_callCommFailureExceptionHandler(omniObject*,
//             				               CORBA::ULong,
//				                       const CORBA::COMM_FAILURE&);
// (see CORBA.h L 2707)


CORBA::Boolean
_omni_callSystemExceptionHandler(Ada_OmniObject*,
				 CORBA::ULong,
				 CORBA::ULong minor,
				 CORBA::CompletionStatus status);
// equivalent of _omni_callSystemExceptionHandler(omniObject*,
//             				          CORBA::ULong,
//				                  const CORBA::SystemException&);
// (see CORBA.h L 2712)

#endif
