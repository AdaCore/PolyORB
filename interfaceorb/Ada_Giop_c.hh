///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
////                                                               ////
////                         AdaBroker                             ////
////                                                               ////
////                 class Ada_Giop_c                              ////
////                                                               ////
////                                                               ////
////   Copyright (C) 1999 ENST                                     ////
////                                                               ////
////   This file is part of the AdaBroker library                  ////
////                                                               ////
////   The AdaBroker library is free software; you can             ////
////   redistribute it and/or modify it under the terms of the     ////
////   GNU Library General Public License as published by the      ////
////   Free Software Foundation; either version 2 of the License,  ////
////   or (at your option) any later version.                      ////
////                                                               ////
////   This library is distributed in the hope that it will be     ////
////   useful, but WITHOUT ANY WARRANTY; without even the implied  ////
////   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ////
////   PURPOSE.  See the GNU Library General Public License for    ////
////   more details.                                               ////
////                                                               ////
////   You should have received a copy of the GNU Library General  ////
////   Public License along with this library; if not, write to    ////
////   the Free Software Foundation, Inc., 59 Temple Place -       ////
////   Suite 330, Boston, MA 02111-1307, USA                       ////
////                                                               ////
////                                                               ////
////                                                               ////
////   Description                                                 ////
////   -----------                                                 ////
////     This class is both a C class and an Ada Class (see        ////
////     giop_c.ads). It is wrapped around GIOP_C in order to      ////
////     avoid the presence of non default constructors.           ////
////     So, it provides the same functions as GIOP_C except that  ////
////     constructors are replaced by Init functions.              ////
////     It has also a pointer on the underlying GIOP_C object.    ////
////                                                               ////
////                                                               ////
////                                                               ////
////   authors : Sebastien Ponce, Fabien Azavant                   ////
////   date    : 02/28/99                                          ////
////                                                               ////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


#include "omniORB2/omniInternal.h"
#include "Ada_netBufferedStream.hh"

class Ada_Giop_c : public Ada_netBufferedStream {
  
public:

  Ada_Giop_c ();
  // Default Constructor
  
  void Init (Rope *r);
  // Initialisation of Ada_Giop_c, calls the
  // underlying GIOP_C constructor
  
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

  GIOP::ReplyStatusType ReceiveReply();
  // wrapper around GIOP::ReplyStatusType ReceiveReply();

  void RequestCompleted(_CORBA_Boolean skip);
  // wrapper around void RequestCompleted(_CORBA_Boolean skip=0);
  
private:

  GIOP_C *C_Giop_c;
  // Pointer on the underlying Giop_s object
  
  bool Init_Ok;
  // This flag tells whether an init function was called or not
  
};


extern void raise_ada_exception (const char *msg);
// this function allows C code to raise Ada exception
// It is implemented in Ada and only raise a No_Initialisation
// exception with the message msg. (see corba.ads)

