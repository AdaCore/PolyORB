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


// Ada_OmniRopeAndKey
//-------------------
Ada_OmniRopeAndKey::Ada_OmniRopeAndKey ()
{
  Init_Ok = false;
  C_omniRopeAndKey = NULL;
};


// Init
//-----
void
Ada_OmniRopeAndKey::Init (Rope *r,_CORBA_Octet *k, _CORBA_ULong ksize)
{
  C_omniRopeAndKey = new omniRopeAndKey (r,k,ksize);
  Init_Ok = true;
};


// Init
//-----
void
Ada_OmniRopeAndKey::Init ()
{
  C_omniRopeAndKey = new omniRopeAndKey ();
  Init_Ok = true;
};


// rope
//-----
Rope*
Ada_OmniRopeAndKey::rope()
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_omniRopeAndKey
    return C_omniRopeAndKey->rope();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniRopeAndKey::rope without initialising object.");
  }
};


// key
//-----
_CORBA_Octet*
Ada_OmniRopeAndKey::key()
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_omniRopeAndKey
    return C_omniRopeAndKey->key();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniRopeAndKey::key without initialising object.");
  }
};


// keysize
//--------
_CORBA_ULong
Ada_OmniRopeAndKey::keysize()
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_omniRopeAndKey
    return C_omniRopeAndKey->keysize();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniRopeAndKey::keysize without initialising object.");
  }
};


// equals
//-------
_CORBA_Boolean
Ada_OmniRopeAndKey::equals(Ada_OmniRopeAndKey other) {
  if (Init_Ok) {
    // if Initialisation was made then 
    // compare effectively the two objects
    // this code is pasted from corbaObject.cc L160
    if (C_omniRopeAndKey->keysize() != other.C_omniRopeAndKey->keysize() ||
	memcmp((void*)(C_omniRopeAndKey->key()),(void*)(other.C_omniRopeAndKey->key()),
	       C_omniRopeAndKey->keysize()) != 0) {
      return 0 ;
    }
    
    if (C_omniRopeAndKey->rope() == other.C_omniRopeAndKey->rope())
      return 1;
    else
      return 0;      

  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniRopeAndKey::equals without initialising object.");
  }
}


