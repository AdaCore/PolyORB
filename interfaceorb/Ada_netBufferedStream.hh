#ifndef __ADA_NETBUFFEREDSTREAM_H__
#define __ADA_NETBUFFEREDSTREAM_H__

#include <omniORB2/omniInternal.h>

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
  
  
  NetBufferedStream *CPP_Object;
  // Pointer on the underlying netBufferedStream object
  // nedded in Ada_Iop.cc
  
protected :

  bool Init_Ok;
  // This flag tells whether an init function was called or not

  void* VTable;
  // This field is only used by Ada. It is needed to interface C++ and Ada  

};

#endif
