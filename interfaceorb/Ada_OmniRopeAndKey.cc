///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
////                                                               ////
////                         AdaBroker                             ////
////                                                               ////
////                 class Ada_OmniRopeAndKey                      ////
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
////     omniRopeAndKey.ads). It is wrapped around omniRopeAndKey  ////
////     in order to avoid the presence of non default construc-   ////
////     tors.                                                     ////
////     So, it provides the same functions as omniRopeAndKey      ////
////     except that constructors are replaced by Init functions.  ////
////     It has also a pointer on the underlying omniRopeAndKey    ////
////     object                                                    ////
////                                                               ////
////                                                               ////
////   authors : Sebastien Ponce, Fabien Azavant                   ////
////   date    : 02/28/99                                          ////
////                                                               ////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


#include "Ada_OmniRopeAndKey.hh"
#include "Ada_exceptions.hh"


// Ada_OmniRopeAndKey
//-------------------
Ada_OmniRopeAndKey::Ada_OmniRopeAndKey ()
{
  Init_Ok = false;
  C_Object = 0 ;
};


// Init
//-----
void
Ada_OmniRopeAndKey::Init ()
{
ADABROKER_TRY
  if (C_Object) {
    delete C_Object ;
  }
  C_Object = new omniRopeAndKey ();
  Init_Ok = true;
ADABROKER_CATCH
};

// Free
//-----
void
Ada_OmniRopeAndKey::Free ()
{
ADABROKER_TRY
  if (C_Object) {
    delete C_Object ;
  }
  Init_Ok = false ;
ADABROKER_CATCH
};


// rope
//-----
Rope*
Ada_OmniRopeAndKey::rope()
{
ADABROKER_TRY
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    return C_Object->rope();
  } else {
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniRopeAndKey::rope without initialising object.") ;
  }
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  Rope* default_result = NULL;
  return default_result; 
};


// key
//-----
_CORBA_Octet*
Ada_OmniRopeAndKey::key()
{
ADABROKER_TRY
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    return C_Object->key();
  } else {
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniRopeAndKey::key without initialising object.") ;
  }
ADABROKER_CATCH 
  // never reach here just a default return for dummy compilers.
  _CORBA_Octet* default_result = NULL;
  return default_result; 
};


// keysize
//--------
_CORBA_ULong
Ada_OmniRopeAndKey::keysize()
{
ADABROKER_TRY
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    return C_Object->keysize();
  } else {
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniRopeAndKey::keysize without initialising object.") ;
  }
ADABROKER_CATCH 
  // never reach here just a default return for dummy compilers.
  _CORBA_ULong* default_result = NULL;
  return default_result; 
};


// equals
//-------
_CORBA_Boolean
Ada_OmniRopeAndKey::equals(Ada_OmniRopeAndKey other) {
ADABROKER_TRY
  if ( (Init_Ok) && (other.assertInit_Ok())) {
    // if Initialisation was made then 
    // compare effectively the two objects
    // this code is pasted from corbaObject.cc L160
    if (C_Object->keysize() != other.C_Object->keysize() ||
	memcmp((void*)(C_Object->key()),(void*)(other.C_Object->key()),
	       C_Object->keysize()) != 0) {
      return 0 ;
    }
    
    if (C_Object->rope() == other.C_Object->rope())
      return 1;
    else
      return 0;      

  } else {
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniRopeAndKey::equals without initialising object.") ;
  }
ADABROKER_CATCH 
  // never reach here just a default return for dummy compilers.
  _CORBA_Boolean default_result = false;
  return default_result; 
}


// assertInit_Ok
//--------------
bool
Ada_OmniRopeAndKey::assertInit_Ok () const {
  return Init_Ok;
}






