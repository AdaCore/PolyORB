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
#include "Ada_exceptions.hh"

// DEBUG is defined at the beginning of each
// file and undefined at the end
//#define DEBUG

// Default Constructor
//--------------------
Ada_Giop_s::Ada_Giop_s () : Ada_netBufferedStream::Ada_netBufferedStream ()
{
  // everything is done in the constructor of the parent
};

  
// Constructor
//------------
Ada_Giop_s::Ada_Giop_s(GIOP_S *c_obj) {
  Init_Ok = true ;
  CPP_Object = c_obj ; 
};


// Init
//-----
void
Ada_Giop_s::Init (Strand *s)
{
  CPP_Object = new GIOP_S (s);
  Init_Ok = true;
};


// RequestReceived
//------------------
void
Ada_Giop_s::RequestReceived(_CORBA_Boolean skip)
{
ADABROKER_TRY
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on CPP_Object
    ( (GIOP_S *) CPP_Object )->RequestReceived(skip);
  } else {
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_Giop_s::RequestReceived without initialising object.") ;
  }
ADABROKER_CATCH
};


// InitialiseReply
//------------------
void
Ada_Giop_s::InitialiseReply(const int status,
			    const size_t  msgsize)
{
ADABROKER_TRY
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on CPP_Object
    ((GIOP_S *) CPP_Object)->InitialiseReply((GIOP::ReplyStatusType) status,
					   msgsize);
  } else {
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_Giop_s::InitialiseReply without initialising object.") ;
  }
ADABROKER_CATCH 
};


// ReplyCompleted
//------------------
void
Ada_Giop_s::ReplyCompleted()
{
ADABROKER_TRY
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on CPP_Object
    ((GIOP_S *) CPP_Object)->ReplyCompleted();
  } else {
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_Giop_s::ReplyCompleted without initialising object.") ;
  }
ADABROKER_CATCH 
};


// ReplyHeaderSize
//----------------
size_t
Ada_Giop_s::ReplyHeaderSize()
{
ADABROKER_TRY
  size_t result = GIOP_S::ReplyHeaderSize ();
#ifdef DEBUG
  cerr << "Ada_Giop_s::ReplyHeaderSize : returning  ... " << result << endl ;
#endif 
  return result ;
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  size_t default_result = 0;
  return default_result; 
}

#undef DEBUG
