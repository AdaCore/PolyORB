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

#include <omniORB2/omniInternal.h>
#include  "Ada_OmniObject.hh"

class omni_C2Ada : omni {

public:
  
  static char *objectToString(Ada_OmniObject *obj);
  // corresponds to the omni function objectToString but use
  // the type Ada_OmniObject instead of omniobject
  
  static Ada_OmniObject *stringToObject(const char *str);
  // corresponds to the omni function stringToObject but use
  // the type Ada_OmniObject instead of omniobject
  
};

