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


#include <omniORB2/omniInternal.h>


class Ada_OmniRopeAndKey {
  
public:

  Ada_OmniRopeAndKey ();
  // Default Constructor
  
  void Init (Rope *r,_CORBA_Octet *k, _CORBA_ULong ksize);
  // Initialisation of Ada_OmniRopeAndKey, calls the
  // underlying omniRopeAndKey constructor
  
  void Init ();
  // Initialisation of Ada_OmniRopeAndKey, calls the
  // underlying omniRopeAndKey constructor
  
  Rope* rope();
  // wrapper around inline Rope* rope() const

  _CORBA_Octet* key();
  // wrapper around inline _CORBA_Octet* key() const
  
  _CORBA_ULong keysize();
  // wrapper around inline _CORBA_ULong keysize() const

  _CORBA_Boolean equals(Ada_OmniRopeAndKey other) ;
  // compares two objects, imported in Ada
  
  omniRopeAndKey *C_Object;
  // Pointer on the underlying Rope object

private:

  bool Init_Ok;
  // This flag tells whether an init function was called or not

  void* VTable;
  // This field is only used by Ada. It is needed to interface C++ and Ada  
  
};


extern void raise_ada_exception (const char *msg);
// this function allows C code to raise Ada exception
// It is implemented in Ada and only raise a No_Initialisation
// exception with the message msg. (see corba.ads)



