////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////                      AdaBroker                                     ////
////                                                                    ////
////    This class is a descendant of proxyObjectFactory                ////
////    it is the same proxyObjectFactory that is used to               ////
////    create all Ada objects. The only difference is                  ////
////    a field which stores the repoID                                 ////
////                                                                    ////
////                Date : 03/03/99                                     ////
////                                                                    ////
////                authors : Sebastien Ponce, Fabien Azavant           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////

#include "proxyObjectFactory_C2Ada.hh"
#include "omniObject_C2Ada.hh"

const char*
proxyObjectFactory_C2Ada::irRepoId() const { 
  return pd_repoID ;
}

CORBA::Object_ptr
proxyObjectFactory_C2Ada::newProxyObject(Rope *r,
					 CORBA::Octet *key,
					 size_t keysize,
					 IOP::TaggedProfileList *profiles,
					 CORBA::Boolean release) {

  Ada_OmniObject* p = new Ada_OmniObject() ;
  p->Init(pd_repoID, r,key, keysize, profiles, release) ;

  omniObject *omniobj = p->getOmniObject() ;
  
  omni::objectIsReady(omniobj) ;
  
  CORBA::Object_ptr result = new CORBA::Object() ;
  result->PR_setobj(omniobj) ;
  return result ;
    ////////////////////////////////
    // WARNING  WARNING  WARNING  //
    //       MEMORY LEAK          //
    ////////////////////////////////
}


CORBA::Boolean
proxyObjectFactory_C2Ada::is_a(const char *base_repoId) const { 
  return (!strcmp(base_repoId, pd_repoID)) ;
}


void createProxyObjectFactory(const char* repoID) {
  proxyObjectFactory_C2Ada *p = new proxyObjectFactory_C2Ada(repoID) ;
  // no memory leak
  // a pointer to this object is kept by the ORB
}






