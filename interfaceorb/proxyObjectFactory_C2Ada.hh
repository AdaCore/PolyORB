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


#ifndef __CORBA_H_EXTERNAL_GUARD__
#define __CORBA_H_EXTERNAL_GUARD__
#include <omniORB2/CORBA.h>
#endif

class proxyObjectFactory_C2Ada : proxyObjectFactory {

public:

  proxyObjectFactory_C2Ada(const char* repoID) : proxyObjectFactory() {
    pd_repoID = repoID ;
  }

  virtual ~proxyObjectFactory_C2Ada() {}

  virtual const char* irRepoId() const ;

  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);

  virtual CORBA::Boolean is_a(const char *base_repoId) const;

  // _nil omitted

private:
  
  const char* pd_repoID ;
};

// This procedure wil be called from the Ada code
void createProxyObjectFactory(const char* repoID) ;
  
