#include <iostream.h>
#include <string>
#include "omnithread.h"

#include "classes.hh"

class vehicle_i : public virtual _sk_vehicle {
public:
  vehicle_i(){
    pd_mark = "here's the mark !" ;
  } ;
  virtual char* mark() ;
  virtual void mark(char* s) ;
  
  virtual bool can_drive(unsigned short age);


  char* pd_mark ;
};

class weapon_i : public virtual _sk_weapon {
public:
  weapon_i() {} ;
  void shoot() ;
};

class tank_i : public virtual _sk_tank {
public:
  tank_i(){} ;
  virtual char* mark() ;
  virtual void mark(char* s) ;
  
  virtual bool can_drive(unsigned short age);
  void shoot() ;
};

void
tank_i::shoot() {
  cerr << "**** BANG ****" << endl ;
};


char*
tank_i::mark() {
  return "toto" ;
}

void
tank_i::mark(char* s) {
}

bool
tank_i::can_drive(UShort age) {
  return ( age > 25) ;
} 

int
main(int argc, char **argv)
{
  CORBA::ORB_ptr orb = CORBA::ORB_init(argc,argv,"omniORB2");
  CORBA::BOA_ptr boa = orb->BOA_init(argc,argv,"omniORB2_BOA");

  tank_i *static_tank = new tank_i() ;
  static_tank->_obj_is_ready(boa) ;
  cerr << "static_tank is ready to shoot !!" << endl ;
  
  {
    tank_var mytank = static_tank->_this();
    CORBA::String_var p = orb->object_to_string(mytank);
    cerr << "tank : '" << (char*)p << "'" << endl;
  }

  boa->impl_is_ready();
  // Tell the BOA we are ready. The BOA's default behaviour is to block
  // on this call indefinitely.

  return 0;
}







