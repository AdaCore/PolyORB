///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
////                                                               ////
////                         AdaBroker                             ////
////                                                               ////
////                 class proxyObjectFactory_C2Ada                ////
////                                                               ////
////                                                               ////
////   Copyright (C) 1999 ENST                                     ////
////                                                               ////
////   This file is part of the AdaBroker library                  ////
////                                                               ////
////   The AdaBroker library is free software; you can             ////
////   redistribute it and/or modify it under the terms of the     ////
////   GNU Library General Public License as published by the      ////
////   Free Software Foundation; either version 2 of the License,  ////
////   or (at your option) any later version.                      ////
////                                                               ////
////   This library is distributed in the hope that it will be     ////
////   useful, but WITHOUT ANY WARRANTY; without even the implied  ////
////   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ////
////   PURPOSE.  See the GNU Library General Public License for    ////
////   more details.                                               ////
////                                                               ////
////   You should have received a copy of the GNU Library General  ////
////   Public License along with this library; if not, write to    ////
////   the Free Software Foundation, Inc., 59 Temple Place -       ////
////   Suite 330, Boston, MA 02111-1307, USA                       ////
////                                                               ////
////                                                               ////
////                                                               ////
////   Description                                                 ////
////   -----------                                                 ////
////    This class is a descendant of proxyObjectFactory           ////
////    it is the same proxyObjectFactory that is used to          ////
////    create all Ada objects. The only difference is             ////
////    a field which stores the repoID                            ////
////                                                               ////
////                                                               ////
////   authors : Sebastien Ponce, Fabien Azavant                   ////
////   date    : 02/28/99                                          ////
////                                                               ////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


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



