//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.5 $
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
#ifndef __ADA_CORBA_ORB_H__
#define __ADA_CORBA_ORB_H__

#include <omniORB2/CORBA.h>

CORBA::ORB_ptr
Ada_ORB_init(int           argc,
             char       ** argv,
             const char *  orb_identifier,
             int           traceLevel);
// this function is a wrapper around CORBA::ORB_init
//in corbaOrb.cc L170
// it is called by Ada (Corba.Orb.adb)
// and is here to handle C++ exceptions
// it takes a int and not a int& as first paramteter
// because in Ada, it is an "in" parameter


CORBA::BOA_ptr
Ada_BOA_init(CORBA::ORB_ptr  orb,
	     int             argc,
	     char         ** argv,
	     const char    * boa_identifier)
// this function is a wrapper around CORBA::BOA_init
//in corbaBoa.cc L180
// it is called by Ada (Corba.Orb.adb)
// and is here to handle C++ exceptions
// it takes a int and not a int& as first paramteter
// because in Ada, it is an "in" parameter

#endif
