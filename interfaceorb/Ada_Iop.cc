///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
////                                                               ////
////                         AdaBroker                             ////
////                                                               ////
////                 class Ada_Corba_Orb                           ////
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
////    This file contains wrapper functions around functions      ////
////   defined in IOP.h They are here to handle C++ exceptions     ////
////   which could make the Ada program halt.                      ////
////                                                               ////
////                                                               ////
////   authors : Sebastien Ponce, Fabien Azavant                   ////
////   date    : 02/28/99                                          ////
////                                                               ////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


#include "Ada_exceptions.hh"
#include "Ada_Iop.hh"

// DEBUG is defined at the beginning of each file
// and undefined at the end
//#define DEBUG


void marshall (IOP::TaggedProfileList* t, Ada_netBufferedStream &s)
{
ADABROKER_TRY
  // verify that t is not null
  if (t != NULL)
    *t >>= *(s.C_Object) ;
  else
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "null TaggedProfileList in Ada_Iop::marshall") ;
ADABROKER_CATCH
}


void unmarshall (IOP::TaggedProfileList* &t, Ada_netBufferedStream &s)
{
ADABROKER_TRY
  // We create here the TaggedProfileList associated with the object
  // it will be released by the ORB when the last reference to the
  // omniObject using this TaggedProfileList will be released
  // see CORBA::UnMarshalObjRef in objectRef.cc
  // it is done like that
  t = new IOP::TaggedProfileList() ;
  *t <<= *(s.C_Object) ;
ADABROKER_CATCH
}


void marshall (IOP::TaggedProfileList* t, Ada_memBufferedStream &s)
{
ADABROKER_TRY
  // verify that t is not null
  if (t != NULL)
    *t >>= *(s.C_Object) ;
  else
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "null TaggedProfileList in Ada_Iop::marshall") ;
ADABROKER_CATCH
}


void unmarshall (IOP::TaggedProfileList* &t, Ada_memBufferedStream &s)
{
ADABROKER_TRY
  // We create here the TaggedProfileList associated with the object
  // it will be released by the ORB when the last reference to the
  // omniObject using this TaggedProfileList will be released
  // see CORBA::UnMarshalObjRef in objectRef.cc
  // it is done like that
  t = new IOP::TaggedProfileList() ;
  *t <<= *(s.C_Object) ;
ADABROKER_CATCH
}


size_t NP_alignedSize (IOP::TaggedProfileList* t, size_t initialoffset)
{
ADABROKER_TRY
  // verify that t is not null
  if (t != NULL)
    return t->NP_alignedSize (initialoffset);
  else
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "null TaggedProfileList in Ada_Iop::NP_alignedSize") ;
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  size_t default_result = 0;
  return default_result; 
}


size_t length (IOP::TaggedProfileList* t)
{
ADABROKER_TRY
  // verify that t is not null
  if (t != NULL)
    return t->length ();
  else
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "null TaggedProfileList in Ada_Iop::length") ;
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  size_t default_result = 0;
  return default_result; 
}

#undef DEBUG

