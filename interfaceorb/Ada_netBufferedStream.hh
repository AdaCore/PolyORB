////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is both a C class and an Ada Class (see             ////
////     netBufferedStream.ads). It is wrapped around netBuffered-      ////
////     Stream in order to avoid the presence of non default           ////
////     constructors.                                                  ////
////     So, it provides the same functions as netBufferedStream        ////
////     except that constructors are replaced by Init functions.       ////
////     It has also a pointer on the underlying netBufferedStream      ////
////     object                                                         ////
////                                                                    ////
////                                                                    ////
////                Date : 02/25/99                                     ////
////                                                                    ////
////                authors : Sebastien Ponce                           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////

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
  
  
  void marshall (_CORBA_Char a, NetBufferedStream& s);
  // wrapper around inline friend inline void operator>>= 

  void unmarshall (_CORBA_Char& a, NetBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  void marshall (_CORBA_Boolean b, NetBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  void unmarshall (_CORBA_Boolean& b, NetBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  void marshall (_CORBA_Short a, NetBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  void unmarshall (_CORBA_Short& a, NetBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  void marshall (_CORBA_UShort a, NetBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  void unmarshall (_CORBA_UShort& a, NetBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  void marshall (_CORBA_Long a, NetBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  void unmarshall (_CORBA_Long& a, NetBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  void marshall (_CORBA_ULong a, NetBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  void unmarshall (_CORBA_ULong& a, NetBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  void marshall (_CORBA_Float a, NetBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  void unmarshall (_CORBA_Float& a, NetBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  void marshall (_CORBA_Double a, NetBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  void unmarshall (_CORBA_Double& a, NetBufferedStream& s);
  // wrapper around friend inliney void operator<<=

  _CORBA_Boolean isReUsingExistingConnection();
  // wrapper around _CORBA_Boolean isReUsingExistingConnection() const
  // declared in rope.h
  
  
private:

  NetBufferedStream *C_netBufferedStream;
  // Pointer on the underlying netBufferedStream object

  bool Init_Ok;
  // This flag tells whether an init function was called or not

};

extern void raise_ada_exception (const char *msg);
// this function allows C code to raise Ada exception
// It is implemented in Ada and only raise a No_Initialisation
// exception with the message msg. (see corba.ads)






