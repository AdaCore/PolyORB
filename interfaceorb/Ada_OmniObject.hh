//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.40 $
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
#ifndef __ADA_OMNIOBJECT_H__
#define __ADA_OMNIOBJECT_H__

#include "omniObject_C2Ada.hh"
#include "Ada_OmniRopeAndKey.hh"
#include <omniORB2/CORBA.h>

class omniObject_C2Ada;

class Ada_OmniObject {

public:

  Ada_OmniObject (void);
  // default constructor
  
  Ada_OmniObject (omniObject_C2Ada * cpp_object,
		  int                interface);
  // constructor for proxy objects, only called in C++
  // that makes this Ada_OmniObject point on an already existent
  // omniObject_C2Ada
  
  virtual ~Ada_OmniObject ();

  static Ada_OmniObject * Constructor();
  // static constructor.
  // this is a workaround for gnat 3.11p where we cannot
  // write "new Object"
  // it is only called to create local objects
  
  static void Destructor (Ada_OmniObject * o);
  // static destructor that will be called from the Ada code
  // because the virtual destructor cannot be called from tha Ada code

  void initLocalObject (const char * repoID);
  // Initialisation of a local object via call to the
  // omniObject_C2Ada constructor on C_OmniObject
  // For a local object, we have to set the repository_id

  
  void initProxyObject (const char             * repoId,
			Rope                   * r,
			_CORBA_Octet           * key,
			size_t                   keysize,
			IOP::TaggedProfileList * profiles,
			_CORBA_Boolean           release); 
  // Initialize a proxy object via a call to the omniObject_C2Ada
  // constructor on CPP_Object.
    
  static Ada_OmniObject * objectDuplicate (Ada_OmniObject * omniobj);
  // Create an Ada_OmniObject referencing the same omniObject (used
  // for Omniobject.Duplicate).
  
  void objectIsReady ();
  // Call omni::objectIsReady on CPP_Object to tell the ORB that this
  // local object is ready to accept connexions.
  
  void disposeObject ();
  // Call omni::disposeObject on C_OmniObject. It has to be done only
  // for local object to tell the ORB they cannot receive connexions
  // any longer.

  bool non_existent ();
  // Return True if the ORB is sure that the implementation referenced
  // by this proxy object does not exist.
  
  _CORBA_Boolean is_equivalent (Ada_OmniObject * other);
  // Return True when CPP objects are equivalent.

  _CORBA_ULong hash (_CORBA_ULong maximum);
  // Return a hash value for this object.

  void setRopeAndKey (const Ada_OmniRopeAndKey & l,
		      _CORBA_Boolean             keepIOP = 1);
  // Calls setRopeAndKey function on CPP_Object.

  void  getRopeAndKey (Ada_OmniRopeAndKey & l,
		       _CORBA_Boolean     & success);
  // Call getRopeAndKey function on CPP_Object.

  void resetRopeAndKey ();
  // Call resetRopeAdnKey function on CPP_Object.
  
  void assertObjectExistent ();
  // Call assertObjectExistent function on CPP_Object.
  
  _CORBA_Boolean is_proxy ();
  // Call is_proxy function on CPP_Object.
  
  virtual void dispatch (Ada_Giop_s     &,
			 const char     * operation,
			 _CORBA_Boolean   response_expected,
			 _CORBA_Boolean & success);
  // Default dispatch function for all the hierarchy of Ada
  // Objects. Ada implementation (see omniobject.adb). This is a
  // procedure because some arguments are passed by reference.

  _CORBA_Boolean Ada_Is_A (const char * repoid);
  // Implemented in omniobject.ads. Return True if this object can be
  // widen/narrow into this interface.
  
  const char* getRepositoryID ();
  // Call NP_repositoryId on omniObject.
  
  static Ada_OmniObject * string_to_ada_object (const char * repoId);
  // Execute omni::stringToObject. Cast result into an Ada_OmniObject.
  // it can only be called by CORBA.ORB.String_To_Object.

  static Ada_OmniObject * Ada_resolve_initial_references
     (CORBA::ORB_ptr  theORB,
      const char     * identifier);

  
  static Ada_OmniObject * ada_create_objref (const char             * repoId,
					     IOP::TaggedProfileList * profiles,
					     _CORBA_Boolean           release);
  // Called by Ada code to create a CORBA.Object.Ref when
  // unmarshalling out of a bufferedstream.  It calls
  // omni::createObjRef in objectRef.cc L 391.

  static char* ada_object_to_string (Ada_OmniObject * objptr);
  //  Calls omni::objectToString on underlying object.
  
  IOP::TaggedProfileList * iopProfiles(); 
  // Call omniobject::iopProfiles() on underlying object.


  omniObject_C2Ada *getOmniObject();
  // Return underlying CPP_Object used in proxyObjectFactory_C2Ada.
  
private:

  void setRepositoryID (const char * repoId);
  // Set the repository id for a local object.
  
  void * Implobj;
  // Pointer used only by Ada side of this object.

  int  Interface;
  // Index used only by Ada side of this object.

public:

  omniObject_C2Ada * CPP_Object;
  // Pointer on the underlying omniObject_C2Ada object.

private:

  bool Init_Ok;
  // Flag to denote whether an object has been initialized.

  void* VTable;
  //  Field used only by Ada. It is needed to interface C++ and Ada.
  
};

#endif
