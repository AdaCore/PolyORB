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


#include "proxyObjectFactory_C2Ada.hh"
#include "omniObject_C2Ada.hh"
#include "Ada_exceptions.hh"


// DEBUG is defined at the beginning of each file
// and undefined at the end of each file
//#define DEBUG



// irRepoID
//---------
const char*
proxyObjectFactory_C2Ada::irRepoId() const { 
  return pd_repoID ;
}


// newProxyObject
//---------------
CORBA::Object_ptr
proxyObjectFactory_C2Ada::newProxyObject(Rope *r,
					 CORBA::Octet *key,
					 size_t keysize,
					 IOP::TaggedProfileList *profiles,
					 CORBA::Boolean release) {

#ifdef DEBUG
  cerr << "proxyObjectFactory_C2Ada::newProxyObject" << endl ;
#endif

  Ada_OmniObject* p = new Ada_OmniObject() ;
  p->initProxyObject(pd_repoID, r,key, keysize, profiles, release) ;

  omniObject *omniobj = p->getOmniObject() ;
  
  omni::objectIsReady(omniobj) ;
  // telling the ORB that this object is ready to use
  // connexions
  
  CORBA::Object_ptr result = new CORBA::Object() ;
  result->PR_setobj(omniobj) ;

  return result ;
    ////////////////////////////////
    // WARNING  WARNING  WARNING  //
    //       MEMORY LEAK          //
    ////////////////////////////////
    // As a matter of fact, this function must return a
    // CORBA::Object_Ptr, whereas the calling function (createObjRef)
    // calls PR_getobj as soon as it gets the result.
    // therefore, we have to create this CORBA::Object_ptr
    // that will never be referenced again, and we do not
    // not when it can be released.
    
}


// is_a
//-----
CORBA::Boolean
proxyObjectFactory_C2Ada::is_a(const char *base_repoId) const { 
  return (!strcmp(base_repoId, pd_repoID)) ;
}


// createProxyObjectFactory
//-------------------------
void createProxyObjectFactory(const char* repoID) {
ADABROKER_TRY
  proxyObjectFactory_C2Ada *p = new proxyObjectFactory_C2Ada(repoID) ;
  // no memory leak
  // a pointer to this object is kept by the ORB
ADABROKER_CATCH 
}


#undef DEBUG



