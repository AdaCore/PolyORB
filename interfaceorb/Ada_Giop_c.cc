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


#include "Ada_Giop_c.hh"
#include "Ada_exceptions.hh"

// DEBUG is defined at the beginning of each file
// and undefined at the end
//#define DEBUG


// Default Constructor
//--------------------
Ada_Giop_c::Ada_Giop_c () : Ada_netBufferedStream::Ada_netBufferedStream ()
{
  // everything is done in the constructor of the parent
};

  
// Init
//-----
void
Ada_Giop_c::Init (Rope *r)
{
  if (C_Object) {
    // if we already have an object, release it
    // before creating a new one !
    delete (GIOP_C*) C_Object ;
  }

#ifdef DEBUG
  else {
    cerr << "Ada_Giop_c::Init : initializing new Giop_c" << endl ;
  }
#endif

  C_Object = new GIOP_C (r);
  Init_Ok = true;
};


// Free
//-----
void
Ada_Giop_c::Free()
{
  if (C_Object) {
    delete (GIOP_C*) C_Object ;
    C_Object = 0 ;
  }
  Init_Ok = false ;
}


// InitialiseRequest
//------------------
void
Ada_Giop_c::InitialiseRequest(const void          *objkey,
			      const size_t         objkeysize,
			      const char          *opname,
			      const size_t         opnamesize,
			      const size_t         msgsize,
			      const _CORBA_Boolean oneway)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    ((GIOP_C *) C_Object)->InitialiseRequest(objkey,
					     objkeysize,
					     opname,
					     opnamesize,
					     msgsize,
					     oneway);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_Giop_c::InitialiseRequest without initialising object.");
  }
};

// ReceiveReply
//-------------
void
Ada_Giop_c::ReceiveReply(GIOP::ReplyStatusType &result)
{
  try {
    if (Init_Ok) {
      // if Initialisation was made then call the corresponding
      // function on C_Object
      result = ((GIOP_C *) C_Object)->ReceiveReply();
    } else {
      // else raise an Ada Exception
      raise_ada_exception ("Call of Ada_Giop_c::ReceiveReply without initialising object.");
    }
  } catch (CORBA::SystemException &e) {
    Raise_Corba_Exception (e);
  } catch (omniORB::fatalException &e) {
    Raise_Corba_Exception (e);    
  } catch (...) {
    Raise_Ada_FatalException ("Ada_Giop_c.cc",
			      117,
			      "An unknown C exception was catched.\nI can not raise it in Ada.");
  }
};


// RequestCompleted
//------------------
void
Ada_Giop_c::RequestCompleted(_CORBA_Boolean skip)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    ((GIOP_C *) C_Object)->RequestCompleted (skip);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_Giop_c::RequestCompleted without initialising object.");
  }
};

#undef DEBUG
