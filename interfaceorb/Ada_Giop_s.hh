#include "omniORB2/omniInternal.h"
#include "Ada_netBufferedStream.hh"

class Ada_Giop_s : public Ada_netBufferedStream {
  
public:

  Ada_Giop_s ();
  // Default Constructor

  Ada_Giop_s(GIOP_S *c_obj) ;
  // Constructor used in omniObject_C2Ada::dispatch
  // to create an Ada_Giop_s out of a C++ Giop_s

  void Init (Strand *s);
  // Initialisation of Ada_Giop_s, calls the
  // underlying GIOP_S constructor

  void RequestReceived(_CORBA_Boolean skip);
  // wrapper around void RequestReceived(_CORBA_Boolean skip=0);

  void InitialiseReply(const int status,
        	       const size_t  msgsize);
  // wrapper around void InitialiseReply(const GIOP::ReplyStatusType status,
  //		                         const size_t  msgsize);

  void ReplyCompleted();
  // wrapper around void ReplyCompleted();

  static size_t ReplyHeaderSize();
  // wrapper around size_t GIOP_S::ReplyHeaderSize();
    
private:

  void* VTable1;
  // This field is only used by Ada. It is needed to interface C++ and Ada  

};


