////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is both a C class and an Ada Class (see             ////
////     MemBufferedStream.ads). It is wrapped around MemBuffered-      ////
////     Stream in order to avoid the presence of non default           ////
////     constructors.                                                  ////
////     So, it provides the same functions as MemBufferedStream        ////
////     except that constructors are replaced by Init functions.       ////
////     It has also a pointer on the underlying MemBufferedStream      ////
////     object                                                         ////
////                                                                    ////
////                                                                    ////
////                Date : 02/25/99                                     ////
////                                                                    ////
////                authors : Sebastien Ponce                           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////


#include "Ada_memBufferedStream.hh"


Ada_MemBufferedStream::Ada_MemBufferedStream ()
{
  Init_Ok = false;
};
// default constructor

  
void
Ada_MemBufferedStream::Init (size_t Bufsize)
{
  // Creation of the underlying omniobject_C2Ada object
  C_MemBufferedStream = new MemBufferedStream (Bufsize);
  // updating of Init_OK flag
  Init_Ok = true;
  return;
};


void
Ada_MemBufferedStream::marshall (_CORBA_Char a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_MemBufferedStream::unmarshall (_CORBA_Char& a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_MemBufferedStream::marshall (_CORBA_Boolean b, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    b >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_MemBufferedStream::unmarshall (_CORBA_Boolean& b, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    b <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_MemBufferedStream::marshall (_CORBA_Short a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_MemBufferedStream::unmarshall (_CORBA_Short& a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_MemBufferedStream::marshall (_CORBA_UShort a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_MemBufferedStream::unmarshall (_CORBA_UShort& a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_MemBufferedStream::marshall (_CORBA_Long a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_MemBufferedStream::unmarshall (_CORBA_Long& a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_MemBufferedStream::marshall (_CORBA_ULong a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_MemBufferedStream::unmarshall (_CORBA_ULong& a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_MemBufferedStream::marshall (_CORBA_Float a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_MemBufferedStream::unmarshall (_CORBA_Float& a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator<<= without initialising object.");
  }
};

void
Ada_MemBufferedStream::marshall (_CORBA_Double a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a >>= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator>>= without initialising object.");
  }
};

void
Ada_MemBufferedStream::unmarshall (_CORBA_Double& a, MemBufferedStream& s)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_MemBufferedStream
    a <<= s;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_MemBufferedStream::operator<<= without initialising object.");
  }
};

