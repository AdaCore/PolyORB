#include "omniORB2/omniInternal.h"
#include "Ada_netBufferedStream.hh"

class Ada_Giop_c : public Ada_netBufferedStream {
  
public:

  Ada_Giop_c ();
  // Default Constructor
  
  void Init (Rope *r);
  // Initialisation of Ada_Giop_c, calls the
  // underlying GIOP_C constructor
  
  void Free() ;
  // deletes the underlying C pointer

  void InitialiseRequest(const void          *objkey,
			 const size_t         objkeysize,
			 const char          *opname,
			 const size_t         opnamesize,
			 const size_t         msgsize,
			 const _CORBA_Boolean oneway);
  // wrapper around void InitialiseRequest(const void          *objkey,
  //               			   const size_t         objkeysize,
  //			                   const char          *opname,
  //			                   const size_t         opnamesize,
  //			                   const size_t         msgsize,
  //			                   const _CORBA_Boolean oneway);

  void ReceiveReply(GIOP::ReplyStatusType &result);
  // wrapper around GIOP::ReplyStatusType ReceiveReply();

  void RequestCompleted(_CORBA_Boolean skip);
  // wrapper around void RequestCompleted(_CORBA_Boolean skip=0);

  size_t RequestHeaderSize(const size_t objkeysize,
			   const size_t opnamesize);
  // wrapper around size_t GIOP_C::RequestHeaderSize
  
private:

  void* VTable1;
  // This field is only used by Ada. It is needed to interface C++ and Ada  

};


