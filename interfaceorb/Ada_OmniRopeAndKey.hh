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

#include <omniORB2/omniInternal.h>

class Ada_OmniRopeAndKey {
  
public:

  Ada_OmniRopeAndKey ();
  // Default Constructor
  
  void Init (Rope *r,_CORBA_Octet *k, _CORBA_ULong ksize);
  // Initialisation of Ada_OmniRopeAndKey, calls the
  // underlining omniRopeAndKey constructor
  
  void Init ();
  // Initialisation of Ada_OmniRopeAndKey, calls the
  // underlining omniRopeAndKey constructor
  
  Rope* rope();
  // wrapper around inline Rope* rope() const

  _CORBA_Octet* key();
  // wrapper around inline _CORBA_Octet* key() const
  
  _CORBA_ULong keysize();
  // wrapper around inline _CORBA_ULong keysize() const

  
private:

  omniRopeAndKey *C_omniRopeAndKey;
  // Pointer on the underlining Rope object

  bool Init_Ok;
  // This flag tells whether an init function was called or not
  
  void raise_ada_exception (const char *msg);
  // this function allows C code to raise Ada exception
  // It is implemented in Ada and only raise a No_Initialisation
  // exception with the message msg. (see omniropeandkey.ads)

};







