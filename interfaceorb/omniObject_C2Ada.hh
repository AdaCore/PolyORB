////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class provides procedures and functions                   ////
////     for the server to call the Ada skeletons                       ////
////                                                                    ////
////                Date : 02/05/99                                     ////
////                                                                    ////
////                authors : Fabien Azavant, Sebastien Ponce           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////


#include <omniORB2/CORBA.h>

extern void* dispatch_ada(GIOP_S &,const char *operation,
		    _CORBA_Boolean response_expected);

class omniObject_C2Ada : public omniObject {

public:
  omniObject_C2Ada (const char *repoId, 
		    Rope *r,
		    _CORBA_Octet *key,
		    size_t keysize,
		    IOP::TaggedProfileList *profiles,
		    _CORBA_Boolean release,
		    void *AdaObj) :
    omniObject (repoId,r,key,keysize,profiles,release) {
    AdaObject = AdaObj;
  };
  
  ~omniObject_C2Ada () : omniObject () {};
  
  _CORBA_Boolean dispatch(GIOP_S &,const char *operation,
			  _CORBA_Boolean response_expected);
  
  void* _widenFromTheMostDerivedIntf(const char* type_id,
				     _CORBA_Boolean is_cxx_type_id=0);

private:
  void* AdaObject;

};
