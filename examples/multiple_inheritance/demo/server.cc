#include <iostream.h>
#include "omnithread.h"

#include "weapon.hh"
#include "vehicle.hh"
#include "tank.hh"

class vehicle_i : public virtual _sk_vehicle {
public:
  vehicle_i(){} ;
  virtual string mark() {
    return pd_mark ;
  } ;
  virtual void mark(string s) {
    pd_mark = s ;
  }
  
  virtual bool can_drive(in unsigned short age) {
    return (age > 25 ) ;
  }
private:
  string pd_mark ;
};

class weapon_i : public virtual _sk_weapon {
public:
  weapon_i() {} ;
  virtual void shoot() {
    cerr << "**** BANG ***" << endl ;
  }
};

class tank_i : public virtual _sk_tank {
public:
  tank_i(){} ;
};



