#include <iostream.h>
#include "omnithread.h"

#include "chicken.hh"
#include "egg.hh"


class Egg_i ;
class Chicken_i ;

Chicken_i *static_chicken ;

class Chicken_i : public virtual _sk_Chicken {
public:
  Chicken_i() {
    cerr << "CCCtor of Chicken_i" << endl ;
  };
  virtual ~Chicken_i() {
    cerr << "DDDtor of Chicken_i" << endl ;
  };
  virtual Egg_ptr lay();
};

class Egg_i : public virtual _sk_Egg {
public:
  Egg_i() {
    cerr << "CCCtor of Egg_i" << endl ;
  };
  virtual ~Egg_i() {
    cerr << "DDDtor of Egg_i" << endl ;
  };
  virtual Chicken_ptr hatch();
};

Egg_ptr
Chicken_i::lay() {
  Egg_ptr e = new Egg_i() ;
  return e ;
}

Chicken_ptr
Egg_i::hatch() {
  cerr << "In Egg_i::hatch !!" << endl ;
  return static_chicken->_this() ;
}



int
main(int argc, char **argv)
{
  CORBA::ORB_ptr orb = CORBA::ORB_init(argc,argv,"omniORB2");
  CORBA::BOA_ptr boa = orb->BOA_init(argc,argv,"omniORB2_BOA");

  static_chicken = new Chicken_i() ;
  static_chicken->_obj_is_ready(boa) ;
  cerr << "static_chicken is ready to be laid !!" << endl ;
  
  Egg_i *myegg = new Egg_i();
  myegg->_obj_is_ready(boa);

  {
    Egg_var myeggRef = myegg->_this();
    CORBA::String_var p = orb->object_to_string(myeggRef);
    cerr << "Egg : '" << (char*)p << "'" << endl;
  }

  boa->impl_is_ready();
  // Tell the BOA we are ready. The BOA's default behaviour is to block
  // on this call indefinitely.

  return 0;
}
