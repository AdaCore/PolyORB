
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

#include "Ada_netBufferedStream.hh"


// Ada_netBufferedStream
//----------------------
Ada_netBufferedStream::Ada_netBufferedStream ()
{
  C_Object = 0 ;
  Init_Ok = false;
};


// Init
//-----
void 
Ada_netBufferedStream::Init (Strand *s, _CORBA_Boolean RdLock,
                             _CORBA_Boolean WrLock, size_t Bufsize)
{
  // Creation of the underlying omniobject_C2Ada object
  C_Object = new NetBufferedStream (s,RdLock,WrLock,Bufsize);
  // updating of Init_OK flag 
  Init_Ok = true;
  return;
};


// Init
//-----
void
Ada_netBufferedStream::Init (Rope *r, _CORBA_Boolean RdLock,
                             _CORBA_Boolean WrLock, size_t Bufsize)
{
  // Creation of the underlying omniobject_C2Ada object
  C_Object = new NetBufferedStream (r,RdLock,WrLock,Bufsize);
  // updating of Init_OK flag
  Init_Ok = true;
  return;
};


// marshall
//---------
void
Ada_netBufferedStream::marshall (_CORBA_Char a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a >>= *(s.C_Object);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};


// unmarshall
//-----------
void
Ada_netBufferedStream::unmarshall (_CORBA_Char& a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a <<= *(s.C_Object);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};


// marshall
//---------
void
Ada_netBufferedStream::marshall (_CORBA_Boolean a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a >>= *(s.C_Object) ;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};


// unmarshall
//-----------
void
Ada_netBufferedStream::unmarshall (_CORBA_Boolean& a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a <<= *(s.C_Object) ;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};


//marshall
//---------
void
Ada_netBufferedStream::marshall (_CORBA_Short a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a >>= *(s.C_Object);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};


// unmarshall
//-----------
void
Ada_netBufferedStream::unmarshall (_CORBA_Short& a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a <<= *(s.C_Object);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};


// marshall
//---------
void
Ada_netBufferedStream::marshall (_CORBA_UShort a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a >>= *(s.C_Object);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};


// unmarshall
//-----------
void
Ada_netBufferedStream::unmarshall (_CORBA_UShort& a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a <<= *(s.C_Object);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};


// marshall
//---------
void
Ada_netBufferedStream::marshall (_CORBA_Long a, Ada_netBufferedStream &s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a >>= *(s.C_Object) ;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_netBufferedStream::unmarshall (_CORBA_Long& a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a <<=  *(s.C_Object) ;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};


// marshall
//---------
void
Ada_netBufferedStream::marshall (_CORBA_ULong a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a >>= *(s.C_Object);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};


// unmarshall
//-----------
void
Ada_netBufferedStream::unmarshall (_CORBA_ULong& a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a <<= *(s.C_Object);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};


// marshall
//---------
void
Ada_netBufferedStream::marshall (_CORBA_Float a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a >>= *(s.C_Object);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};


// unmarshall
//-----------
void
Ada_netBufferedStream::unmarshall (_CORBA_Float& a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a <<= *(s.C_Object);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};


// marshall
//---------
void
Ada_netBufferedStream::marshall (_CORBA_Double a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a >>= *(s.C_Object);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};


// unmarshall
//-----------
void
Ada_netBufferedStream::unmarshall (_CORBA_Double& a, Ada_netBufferedStream& s)
{
  if (s.Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    a <<= *(s.C_Object) ;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};


// isReUsingExistingConnection
//----------------------------
_CORBA_Boolean
Ada_netBufferedStream::isReUsingExistingConnection()
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    return C_Object->isReUsingExistingConnection();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of isReUsingExistingConnection without initialising object.");
  }
};
