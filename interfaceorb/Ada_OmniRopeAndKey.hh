#include <omniORB2/omniInternal.h>


class Ada_OmniRopeAndKey {
  
public:

  Ada_OmniRopeAndKey ();
  // Default Constructor
  
  void Init ();
  // Initialisation of Ada_OmniRopeAndKey, calls the
  // underlying omniRopeAndKey constructor
  
  void Free() ;
  // deletes the underlying CPP_Object

  Rope* rope();
  // wrapper around inline Rope* rope() const

  _CORBA_Octet* key();
  // wrapper around inline _CORBA_Octet* key() const
  
  _CORBA_ULong keysize();
  // wrapper around inline _CORBA_ULong keysize() const

  _CORBA_Boolean equals(Ada_OmniRopeAndKey other) ;
  // compares two objects, imported in Ada
  
  omniRopeAndKey *CPP_Object;
  // Pointer on the underlying Rope object

  bool assertInit_Ok () const ;
  // returns init_Ok;
  
private:

  bool Init_Ok;
  // This flag tells whether an init function was called or not

  void* VTable;
  // This field is only used by Ada. It is needed to interface C++ and Ada  
  
};

