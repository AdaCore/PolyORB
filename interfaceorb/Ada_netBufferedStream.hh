///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
////                                                               ////
////                         AdaBroker                             ////
////                                                               ////
////                 class Ada_netBufferedStream                   ////
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
////     netBufferedStream.ads). It is wrapped around netBuffered- ////
////     Stream in order to avoid the presence of non default      ////
////     constructors.                                             ////
////     So, it provides the same functions as netBufferedStream   ////
////     except that constructors are replaced by Init functions.  ////
////     It has also a pointer on the underlying netBufferedStream ////
////     object                                                    ////
////                                                               ////
////                                                               ////
////                                                               ////
////   authors : Sebastien Ponce, Fabien Azavant                   ////
////   date    : 02/28/99                                          ////
////                                                               ////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


#include "omniORB2/omniInternal.h"

class Ada_netBufferedStream {

public:

  Ada_netBufferedStream ();
  // Default Constructor
  
  void Init (Strand *s, _CORBA_Boolean RdLock,
             _CORBA_Boolean WrLock, size_t Bufsize);
  // Initialisation of Ada_netBufferedStream, calls the
  // underlying netBufferedStream constructor

  void Init (Rope *r, _CORBA_Boolean RdLock,
             _CORBA_Boolean WrLock, size_t Bufsize);
  // Initialisation of Ada_netBufferedStream, calls the
  // underlying netBufferedStream constructor
  
  
  static void marshall (_CORBA_Char a, Ada_netBufferedStream& s);
  // wrapper around inline friend inline void operator>>= 

  static void unmarshall (_CORBA_Char& a, Ada_netBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_Boolean b, Ada_netBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_Boolean& b, Ada_netBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_Short a, Ada_netBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_Short& a, Ada_netBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_UShort a, Ada_netBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_UShort& a, Ada_netBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_Long a, Ada_netBufferedStream &s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_Long& a, Ada_netBufferedStream &s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_ULong a, Ada_netBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_ULong& a, Ada_netBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_Float a, Ada_netBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_Float& a, Ada_netBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_Double a, Ada_netBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_Double& a, Ada_netBufferedStream& s);
  // wrapper around friend inliney void operator<<=

  _CORBA_Boolean isReUsingExistingConnection();
  // wrapper around _CORBA_Boolean isReUsingExistingConnection() const
  // declared in rope.h
  
  
protected :

  NetBufferedStream *C_Object;
  // Pointer on the underlying netBufferedStream object

  bool Init_Ok;
  // This flag tells whether an init function was called or not

  void* VTable;
  // This field is only used by Ada. It is needed to interface C++ and Ada  

};
