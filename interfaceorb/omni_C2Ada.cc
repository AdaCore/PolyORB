////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////       This class is a descendant of the omniObject class.          ////
////       It provides only two fonctions : string_to_object and        ////
////     object_to_string which are wrapped around the corresponding    ////
////     functions of the omni class but use the type Ada_OmniObject    ////
////     instead of omniobject.                                         ////
////                                                                    ////
////                                                                    ////
////                Date : 02/16/99                                     ////
////                                                                    ////
////                authors : Sebastien Ponce                           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////

#include "omni_C2Ada.hh"

char *
omni_C2Ada::objectToString(Ada_OmniObject *obj)
{
  // gets the underlying omniobject of obj
  omniObject *omniobj = obj->getOmniObject() ;
  // and calls the corresponding function on the omniObject
  return omni::objectToString (omniobj) ;
};
  
Ada_OmniObject *
omni_C2Ada::stringToObject(const char *str)
{
  // calls the corresponding function which returns an omniobject
  omniObject *omniobj = omni::stringToObject (str) ;
  // builds an omniObject_C2Ada out of the omniobject resulting
  omniObject_C2Ada *omniobj_c2ada = omniObject_C2Ada::get_omniObject_C2Ada (omniobj);
  // and returns its associated Ada_OmniObject
  return omniobj_c2ada->get_Ada_OmniObject ();
};

