#ifndef __CORBA_H_EXTERNAL_GUARD__
#define __CORBA_H_EXTERNAL_GUARD__
#include <omniORB2/CORBA.h>
#endif

class proxyObjectFactory_C2Ada : proxyObjectFactory {

public:

  proxyObjectFactory_C2Ada(const char* repoID) : proxyObjectFactory() {
    pd_repoID = repoID ;
  }
  // Default constructor
  
  virtual ~proxyObjectFactory_C2Ada() {}
  // Default Destructor

  virtual const char* irRepoId() const ;
  // returns the RepoId of the object

  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);
  // creates a new proxy object and returns it

  virtual CORBA::Boolean is_a(const char *base_repoId) const;
  // returns true if base_repoId corresponds to the local repoId


private:
  
  const char* pd_repoID ;
};

// This procedure will be called from the Ada code
void createProxyObjectFactory(const char* repoID) ;
// returns a new proxyObjectFactory_C2Ada  



