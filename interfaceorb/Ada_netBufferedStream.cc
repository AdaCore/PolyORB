////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is both a C class and an Ada Class (see             ////
////     netBufferedStream.ads). It is wrapped around netBuffered-      ////
////     Stream in order to avoid the presence of non default           ////
////     constructors.                                                  ////
////     So, it provides the same functions as netBufferedStream        ////
////     except that constructors are replaced by Init functions.       ////
////     It has also a pointer on the underlining netBufferedStream     ////
////     object                                                         ////
////                                                                    ////
////                                                                    ////
////                Date : 02/25/99                                     ////
////                                                                    ////
////                authors : Sebastien Ponce                           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////


#include "Ada_netBufferedStream.hh"


Ada_netBufferedStream::Ada_netBufferedStream ()
{
  Init_Ok = false;
};
// default constructor

  
void 
Ada_netBufferedStream::Init (Strand *s, _CORBA_Boolean RdLock,
                             _CORBA_Boolean WrLock, size_t Bufsize)
{
  // Creation of the underlining omniobject_C2Ada object
  C_netBufferedStream = new NetBufferedStream (s,RdLock,WrLock,Bufsize);
  // updating of Init_OK flag 
  Init_Ok = true;
  return;
};


void
Ada_netBufferedStream::Init (Rope *r, _CORBA_Boolean RdLock,
                             _CORBA_Boolean WrLock, size_t Bufsize)
{
  // Creation of the underlining omniobject_C2Ada object
  C_netBufferedStream = new NetBufferedStream (r,RdLock,WrLock,Bufsize);
  // updating of Init_OK flag
  Init_Ok = true;
  return;
};


void
Ada_netBufferedStream::marshall (_CORBA_Char a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_netBufferedStream::unmarshall (_CORBA_Char& a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_netBufferedStream::marshall (_CORBA_Boolean b, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    b >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_netBufferedStream::unmarshall (_CORBA_Boolean& b, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    b <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_netBufferedStream::marshall (_CORBA_Short a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_netBufferedStream::unmarshall (_CORBA_Short& a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_netBufferedStream::marshall (_CORBA_UShort a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_netBufferedStream::unmarshall (_CORBA_UShort& a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_netBufferedStream::marshall (_CORBA_Long a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_netBufferedStream::unmarshall (_CORBA_Long& a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_netBufferedStream::marshall (_CORBA_ULong a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_netBufferedStream::unmarshall (_CORBA_ULong& a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_netBufferedStream::marshall (_CORBA_Float a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_netBufferedStream::unmarshall (_CORBA_Float& a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_netBufferedStream::marshall (_CORBA_Double a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_netBufferedStream::unmarshall (_CORBA_Double& a, NetBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_netBufferedStream::operator<<= without initialising object.");
  }
};


_CORBA_Boolean
Ada_netBufferedStream::isReUsingExistingConnection()
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_netBufferedStream
    return C_netBufferedStream->isReUsingExistingConnection();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of isReUsingExistingConnection without initialising object.");
  }
};


extern void
Ada_netBufferedStream::raise_ada_exception (const char *msg);
// See implementation in omniobject.adb

