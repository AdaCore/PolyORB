//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.16 $
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
#ifndef __OMNIOBJECT_C2ADA_H__
#define __OMNIOBJECT_C2ADA_H__

#include <omniORB2/omniInternal.h>
#include "Ada_Giop_s.hh"
#include "Ada_OmniObject.hh"

class Ada_OmniObject;

class omniObject_C2Ada : public  omniObject {
  
public:
  
  omniObject_C2Ada (Ada_OmniObject * to);
  // Constructor for local objects. omniObjectManager is not needed.
  
  omniObject_C2Ada (const char             * repoId,
		    Rope                   * r,
		    _CORBA_Octet           * key,
		    size_t                   keysize,
		    IOP::TaggedProfileList * profiles,
		    _CORBA_Boolean           release);
  // Constructor for proxy objects.

  ~omniObject_C2Ada () {};
  // Destructor.
  
  void set_Ada_OmniObject (Ada_OmniObject * to);
  // Set To_Ada_OmniObject when Ada_OmniObject is defined after
  // omniObject_C2Ada one.

  virtual _CORBA_Boolean dispatch (GIOP_S        &,
				   const char    * operation,
				  _CORBA_Boolean   response_expected);
  // Override omniObject::dispatch. Call dispatch on Ada_Omniobject.

  virtual void * _widenFromTheMostDerivedIntf
      (const char * repoId,
       _CORBA_Boolean is_cxx_type_id = 0);
  // Override CORBA::Object_ptr.
  
  Ada_OmniObject * get_Ada_OmniObject ();
  // Return underlying Ada_Omniobject object.
  
private:

  Ada_OmniObject * To_Ada_OmniObject;
  // Pointer on a Ada_OmniObject which contains the real dispatch function.
  
  friend class Ada_OmniObject;
  // Ada_OmniObject must have full access to this class.

};

#endif
