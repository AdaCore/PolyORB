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
////     giop_c.ads). It is wrapped around GIOP_S in order to      ////
////     avoid the presence of non default constructors.           ////
////     So, it provides the same functions as GIOP_S except that  ////
////     constructors are replaced by Init functions.              ////
////     It has also a pointer on the underlying GIOP_S object.    ////
////                                                               ////
////                                                               ////
////                                                               ////
////   authors : Sebastien Ponce, Fabien Azavant                   ////
////   date    : 02/28/99                                          ////
////                                                               ////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


#include "Ada_Giop_s.hh"

// DEBUG
#include "omniORB2/CORBA.h"
// end DEGUN

// Default Constructor
//--------------------
Ada_Giop_s::Ada_Giop_s ()
{
  Init_Ok = false;
  C_Object = NULL;
};

  
// Constructor
//------------
Ada_Giop_s::Ada_Giop_s(GIOP_S *c_obj) {
  Init_Ok = true ;
  C_Object = c_obj ; 
};


// Init
//-----
void
Ada_Giop_s::Init (Strand *s)
{
  C_Object = new GIOP_S (s);
  Init_Ok = true;
};


// RequestReceived
//------------------
void
Ada_Giop_s::RequestReceived(_CORBA_Boolean skip)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    ( (GIOP_S *) C_Object )->RequestReceived(skip);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_Giop_s::RequestReceived without initialising object.");
  }
};


// InitialiseReply
//------------------
void
Ada_Giop_s::InitialiseReply(const int status,
			    const size_t  msgsize)
{
  if (Init_Ok) {
    cerr << "InitialiseReply " << status << "  " << msgsize << endl ;
    // if Initialisation was made then call the corresponding
    // function on C_Object
    try {
      GIOP::ReplyStatusType a = GIOP::NO_EXCEPTION ;
      ((GIOP_S *) C_Object)->InitialiseReply(a, msgsize);
      //    ((GIOP_S *) C_Object)->InitialiseReply((GIOP::ReplyStatusType) status,
      //				       msgsize);
      cerr << "InitialiseReply OKOKOKOK" << endl ;
    } catch (CORBA::INTERNAL &e) {
      cerr << "CAUGHT !!!!" << endl ;
    } catch (omniORB::fatalException e) {
      cerr << "Yup !!" << endl ;
    } catch (CORBA::MARSHAL &e) {
      cerr << "Oyehhh" << endl ;
    } catch (...) {
      cerr << "smth else " << endl ;
    }
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_Giop_s::InitialiseReply without initialising object.");
  }
};


// ReplyCompleted
//------------------
void
Ada_Giop_s::ReplyCompleted()
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    ((GIOP_S *) C_Object)->ReplyCompleted();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_Giop_s::ReplyCompleted without initialising object.");
  }
};










