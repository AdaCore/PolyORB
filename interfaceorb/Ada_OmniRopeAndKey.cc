////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is both a C class and an Ada Class (see             ////
////     omniRopeAndKey.ads). It is wrapped around omniRopeAndKey       ////
////     in order to avoid the presence of non default construc-        ////
////     tors.                                                          ////
////     So, it provides the same functions as omniRopeAndKey           ////
////     except that constructors are replaced by Init functions.       ////
////     It has also a pointer on the underlining omniRopeAndKey        ////
////     object                                                         ////
////                                                                    ////
////                                                                    ////
////                Date : 02/18/99                                     ////
////                                                                    ////
////                authors : Sebastien Ponce                           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////


#include "Ada_OmniRopeAndKey.hh"

Ada_OmniRopeAndKey::Ada_OmniRopeAndKey ()
{
  Init_Ok = false;
  C_omniRopeAndKey = NULL;
};
// default constructor

void
Ada_OmniRopeAndKey::Init (Rope *r,_CORBA_Octet *k, _CORBA_ULong ksize)
{
  C_omniRopeAndKey = new omniRopeAndKey (r,k,ksize);
  Init_Ok = true;
};

void
Ada_OmniRopeAndKey::Init ()
{
  C_omniRopeAndKey = new omniRopeAndKey ();
  Init_Ok = true;
};

Rope*
Ada_OmniRopeAndKey::rope()
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_omniRopeAndKey
    return C_omniRopeAndKey->rope();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniRopeAndKey::rope without initialising object.");
  }
};

_CORBA_Octet*
Ada_OmniRopeAndKey::key()
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_omniRopeAndKey
    return C_omniRopeAndKey->key();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniRopeAndKey::key without initialising object.");
  }
};
  
_CORBA_ULong
Ada_OmniRopeAndKey::keysize()
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_omniRopeAndKey
    return C_omniRopeAndKey->keysize();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniRopeAndKey::keysize without initialising object.");
  }
};

extern void
Ada_OmniRopeAndKey::raise_ada_exception (const char *msg);
// See implementation in omniropeandkey.adb

