///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
////                                                               ////
////                         AdaBroker                             ////
////                                                               ////
////                 class OmniObject_C2Ada                        ////
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
////     This class is a descendant of the omniObject              ////
////     class. It provides the sames functions plus a pointer     ////
////     on a Ada_OmniObject.                                      ////
////                                                               ////
////     Furthermore, the function dipatch is implemented and      ////
////     simply calls the Ada one. It allows the C code of         ////
////     omniORB to call the Ada objects.                          ////
////                                                               ////
////                                                               ////
////   authors : Sebastien Ponce, Fabien Azavant                   ////
////   date    : 02/28/99                                          ////
////                                                               ////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


#include "omniORB2/omniInternal.h"

#ifndef __Ada_OmniObject__
#define __omniObject_C2Ada__
#include "Ada_OmniObject.hh"
#endif

class Ada_OmniObject ;


class omniObject_C2Ada : public  omniObject {
  
public:
  
  omniObject_C2Ada(Ada_OmniObject* Ada_Ptr);
  // constructor for local objects
  // omniObjectManager is not needed
  
  omniObject_C2Ada(const char *repoId,
		   Rope *r,
		   _CORBA_Octet *key,
		   size_t keysize,
		   IOP::TaggedProfileList *profiles,
		   _CORBA_Boolean release,
		   Ada_OmniObject *Ada_Ptr);
  // constructor for proxy objects

  ~omniObject_C2Ada() {} ;
  // destructor of class
  
  virtual _CORBA_Boolean dispatch(GIOP_S &,const char *operation,
				  _CORBA_Boolean response_expected);
  // overwrites omniObject::dispatch
  // calls dispatch on Ada_Omniobject

  virtual void* _widenFromTheMostDerivedIntf(const char* repoId,
					     _CORBA_Boolean is_cxx_type_id=0) ;
  // surcharge CORBA::Object_ptr
  // see there for more details
  
  Ada_OmniObject * get_Ada_OmniObject ();
  // returns the underlying Ada_Omniobject object

  
private:

  Ada_OmniObject* Ada_OmniObject_Pointer;
  // pointer on a Ada_OmniObject which contains the real dispatch function
  
  friend class Ada_OmniObject;
  // Ada_OmniObject must have full acces to this class
};
