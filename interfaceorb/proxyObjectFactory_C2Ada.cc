//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.15 $
//                                                                          //
//         Copyright (C) 1999-2000 ENST Paris University, France.           //
//                                                                          //
// AdaBroker is free software; you  can  redistribute  it and/or modify it  //
// under terms of the  GNU General Public License as published by the  Free //
// Software Foundation;  either version 2,  or (at your option)  any  later //
// version. AdaBroker  is distributed  in the hope that it will be  useful, //
// but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- //
// TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public //
// License  for more details.  You should have received  a copy of the GNU  //
// General Public License distributed with AdaBroker; see file COPYING. If  //
// not, write to the Free Software Foundation, 59 Temple Place - Suite 330, //
// Boston, MA 02111-1307, USA.                                              //
//                                                                          //
// As a special exception,  if other files  instantiate  generics from this //
// unit, or you link  this unit with other files  to produce an executable, //
// this  unit  does not  by itself cause  the resulting  executable  to  be //
// covered  by the  GNU  General  Public  License.  This exception does not //
// however invalidate  any other reasons why  the executable file  might be //
// covered by the  GNU Public License.                                      //
//                                                                          //
//             AdaBroker is maintained by ENST Paris University.            //
//                     (email: broker@inf.enst.fr)                          //
//                                                                          //
//--------------------------------------------------------------------------//
#include "proxyObjectFactory_C2Ada.hh"
#include "omniObject_C2Ada.hh"
#include "Ada_exceptions.hh"
#include <iostream>

//------------------------------------//
// proxyObjectFactory_C2Ada::irRepoID //
//------------------------------------//

const char *
proxyObjectFactory_C2Ada::irRepoId () const 
{ 
  if (omniORB::traceLevel > 5) cerr << "(ada) ";
  return pd_repoID;
}


//------------------------------------------//
// proxyObjectFactory_C2Ada::newProxyObject //
//------------------------------------------//

CORBA::Object_ptr
proxyObjectFactory_C2Ada::newProxyObject (Rope                   * r,
					  CORBA::Octet           * key,
					  size_t                   keysize,
					  IOP::TaggedProfileList * profiles,
					  CORBA::Boolean           release) 
{
  ADABROKER_TRY

    if (omniORB::traceLevel > 5)
      cerr << "proxyObjectFactory_C2Ada::newProxyObject : "
           << pd_repoID
           << endl;
  
    omniObject_C2Ada * omniobj = new omniObject_C2Ada 
      (pd_repoID, r, key, keysize, profiles, release);

    omniobj->set_Ada_OmniObject (new Ada_OmniObject (omniobj, pd_interface));
  
    omni::objectIsReady (omniobj);
    // Tell ORB this object is ready to use connexions.
    
    CORBA::Object_ptr result = new CORBA::Object();
    result->PR_setobj (omniobj);

    return result;

    ////////////////////////////////
    // WARNING  WARNING  WARNING  //
    //       MEMORY LEAK          //
    ////////////////////////////////

    // As a matter of fact, this function must return a
    // CORBA::Object_Ptr, whereas the calling function (createObjRef)
    // calls PR_getobj as soon as it gets the result.  therefore, we
    // have to create this CORBA::Object_ptr that will never be
    // referenced again, and we do not know when it can be released.

  ADABROKER_CATCH

    // Never reach this code. Just a default return for dummy compilers.
    CORBA::Object_ptr default_result = NULL;
    return default_result; 
}


//--------------------------------//
// proxyObjectFactory_C2Ada::is_a //
//--------------------------------//

CORBA::Boolean
proxyObjectFactory_C2Ada::is_a (const char * repoId) const { 
  return (!strcmp (repoId, pd_repoID));
}


//----------------------------------------------------//
// proxyObjectFactory_C2Ada::createProxyObjectFactory //
//----------------------------------------------------//

void createProxyObjectFactory (const char * repoID, int interface) 
{
  ADABROKER_TRY
    
    if (omniORB::traceLevel > 5)
      cerr << "createProxyObjectFactory for object : " << repoID << endl;

    proxyObjectFactory_C2Ada * tmp = 
      new proxyObjectFactory_C2Ada (repoID, interface);
    // No memory leak :  a pointer to this object is kept by the ORB.

  ADABROKER_CATCH 
}

