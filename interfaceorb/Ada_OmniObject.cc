////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is as well a C class as an Ada Class (see           ////
////     omniObject.ads). It is wrapped around omniObject_C2Ada         ////
////     in order to avoid the presence of non default construc-        ////
////     tors.                                                          ////
////     So, it provides the same functions as omniObject_C2Ada         ////
////     except that constructors are replaced by Init functions.       ////
////     It has also a pointer on the omniObject_C2Ada                  ////
////                                                                    ////
////                                                                    ////
////                Date : 02/16/99                                     ////
////                                                                    ////
////                authors : Sebastien Ponce                           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////


#include "Ada_OmniObject.hh"

void
Ada_OmniObject::Init (omniObjectManager *p)
{
  // Creation of the underlining omniobject_C2Ada object
  C_OmniObject = new omniObject_C2Ada (p,this);
  // updating of Init_OK flag
  Init_Ok = true;
  return;
}

void
Ada_OmniObject::Init (const char *repoId,
		      Rope *r,
		      _CORBA_Octet *key,
		      size_t keysize,
		      IOP::TaggedProfileList *profiles,
		      _CORBA_Boolean release)
{
  // Creation of the underlining omniobject_C2Ada object
  C_OmniObject = new omniObject_C2Ada (repoId,
				       r,
				       key,
				       keysize,
				       profiles,
				       release,
				       this);
  // updating of Init_OK flag
  Init_Ok = true;
  return;
};


void
Ada_OmniObject::PR_IRRepositoryId(const char* s) {
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_OmniObject
    C_OmniObject->PR_IRRepositoryId(s);
    return;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of PR_IRRepositoryId without initialising object.");
  }
};
      
 
Ada_OmniObject::Ada_OmniObject ()
{
  Init_Ok = false;
};
// default constructor

void
Ada_OmniObject::setRopeAndKey(const omniRopeAndKey& l,_CORBA_Boolean keepIOP=1)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_OmniObject
    C_OmniObject->setRopeAndKey(l,keepIOP);
    return;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of setRopeAndKey without initialising object.");
  }
};

_CORBA_Boolean
Ada_OmniObject::getRopeAndKey(omniRopeAndKey& l) {
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_OmniObject
    return C_OmniObject->getRopeAndKey(l);
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of getRopeAndKey without initialising object.");
  }
}
      
  
void
Ada_OmniObject::assertObjectExistent() {
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_OmniObject
    C_OmniObject->assertObjectExistent();
    return;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of assertObjectExistent without initialising object.");    
  }
}


extern _CORBA_Boolean dispatch(GIOP_S &,
			       const char *operation,
			       _CORBA_Boolean response_expected);
// See implementation in omniobject.ads
  
_CORBA_Boolean
Ada_OmniObject::is_proxy() {
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_OmniObject
    return C_OmniObject->is_proxy();
  } else {
    // else raise an Ada Exception
   raise_ada_exception ("Call of is_proxy without initialising object.");    
  }
}

extern void
Ada_OmniObject::raise_ada_exception (const char *msg);
// See implementation in omniobject.adb




