#ifndef __PROXYOBJECTFACTORY_C2ADA_H__
#define __PROXYOBJECTFACTORY_C2ADA_H__

#ifndef __CORBA_H_EXTERNAL_GUARD__
#define __CORBA_H_EXTERNAL_GUARD__
#include <omniORB2/CORBA.h>
#endif

class proxyObjectFactory_C2Ada : proxyObjectFactory {

public:

  proxyObjectFactory_C2Ada (const char * repoID, int interface)
    : proxyObjectFactory ()
    {
      pd_interface = interface;
      pd_repoID    = repoID;
    }
  // Default constructor.
  
  virtual ~proxyObjectFactory_C2Ada () {}
  // Default destructor.

  virtual const char* irRepoId() const;
  // Return the RepoId of the object.

  virtual CORBA::Object_ptr newProxyObject (Rope                   * r,
					    CORBA::Octet           * key,
					    size_t                   keysize,
					    IOP::TaggedProfileList * profiles,
					    CORBA::Boolean           release);
  // Return a new proxy object.

  virtual CORBA::Boolean is_a (const char * repoId) const;
  // Return True if repoId corresponds to the local repoId.

private:
  
  const char * pd_repoID;
  int          pd_interface;

};

void createProxyObjectFactory (const char * repoID, int interface);
// Procedure called from the Ada code.

#endif
