//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.14 $
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
#ifndef __ADA_GIOP_C_H__
#define __ADA_GIOP_C_H__

#include <omniORB2/omniInternal.h>
#include "Ada_netBufferedStream.hh"

class Ada_Giop_c : public Ada_netBufferedStream {
  
public:

  Ada_Giop_c ();
  // Default Constructor
  
  void Init (Rope *r);
  // Initialisation of Ada_Giop_c, calls the
  // underlying GIOP_C constructor
  
  void Free() ;
  // deletes the underlying C pointer

  void InitialiseRequest(const void          *objkey,
			 const size_t         objkeysize,
			 const char          *opname,
			 const size_t         opnamesize,
			 const size_t         msgsize,
			 const _CORBA_Boolean oneway);
  // wrapper around void InitialiseRequest(const void          *objkey,
  //               			   const size_t         objkeysize,
  //			                   const char          *opname,
  //			                   const size_t         opnamesize,
  //			                   const size_t         msgsize,
  //			                   const _CORBA_Boolean oneway);

  void ReceiveReply(GIOP::ReplyStatusType &result);
  // wrapper around GIOP::ReplyStatusType ReceiveReply();

  void RequestCompleted(_CORBA_Boolean skip);
  // wrapper around void RequestCompleted(_CORBA_Boolean skip=0);

  size_t RequestHeaderSize(const size_t objkeysize,
			   const size_t opnamesize);
  // wrapper around size_t GIOP_C::RequestHeaderSize
  
private:

  void* VTable1;
  // This field is only used by Ada. It is needed to interface C++ and Ada  

};

#endif
