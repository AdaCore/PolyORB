#ifndef __egg_hh__
#define __egg_hh__

#ifndef USE_omniORB_logStream
#define USE_omniORB_logStream
#endif

#ifndef __CORBA_H_EXTERNAL_GUARD__
#define __CORBA_H_EXTERNAL_GUARD__
#include <omniORB2/CORBA.h>
#endif

#ifndef __chicken_hh_EXTERNAL_GUARD__
#define __chicken_hh_EXTERNAL_GUARD__
#include <chicken.hh>
#endif

#ifdef _LC_attr
# error "A local CPP macro _LC_attr has already been defined."
#else
# ifdef  USE_stub_in_nt_dll
#  define _LC_attr _OMNIORB_NTDLL_IMPORT
# else
#  define _LC_attr
# endif
#endif

#ifndef __Egg__
#define __Egg__
class   Egg;
typedef Egg* Egg_ptr;
typedef Egg_ptr EggRef;
class _proxy_Egg;
class _sk_Egg;
class _nil_Egg;

class   Chicken;
typedef Chicken* Chicken_ptr;
typedef Chicken_ptr ChickenRef;
class _proxy_Chicken;
class _sk_Chicken;
class _nil_Chicken;

class Egg_Helper {
  public:
  static Egg_ptr _nil();
  static CORBA::Boolean is_nil(Egg_ptr p);
  static void release(Egg_ptr p);
  static void duplicate(Egg_ptr p);
  static size_t NP_alignedSize(Egg_ptr obj,size_t initialoffset);
  static void marshalObjRef(Egg_ptr obj,NetBufferedStream &s);
  static Egg_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(Egg_ptr obj,MemBufferedStream &s);
  static Egg_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<Egg,Egg_Helper> Egg_var;

#endif
#ifndef __Egg__
#define __Egg__
class   Egg;
typedef Egg* Egg_ptr;
typedef Egg_ptr EggRef;

class Egg_Helper {
  public:
  static Egg_ptr _nil();
  static CORBA::Boolean is_nil(Egg_ptr p);
  static void release(Egg_ptr p);
  static void duplicate(Egg_ptr p);
  static size_t NP_alignedSize(Egg_ptr obj,size_t initialoffset);
  static void marshalObjRef(Egg_ptr obj,NetBufferedStream &s);
  static Egg_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(Egg_ptr obj,MemBufferedStream &s);
  static Egg_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<Egg,Egg_Helper> Egg_var;

#endif
#define Egg_IntfRepoID "IDL:Egg:1.0"

class Egg : public virtual omniObject, public virtual CORBA::Object {
public:

  virtual Chicken_ptr  hatch() = 0;
  static Egg_ptr _duplicate(Egg_ptr);
  static Egg_ptr _narrow(CORBA::Object_ptr);
  static Egg_ptr _nil();

  static inline size_t NP_alignedSize(Egg_ptr obj,size_t initialoffset) {
    return CORBA::AlignedObjRef(obj,Egg_IntfRepoID,12,initialoffset);
  }

  static inline void marshalObjRef(Egg_ptr obj,NetBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Egg_IntfRepoID,12,s);
  }

  static inline Egg_ptr unmarshalObjRef(NetBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Egg_IntfRepoID,s);
    Egg_ptr _result = Egg::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static inline void marshalObjRef(Egg_ptr obj,MemBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Egg_IntfRepoID,12,s);
  }

  static inline Egg_ptr unmarshalObjRef(MemBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Egg_IntfRepoID,s);
    Egg_ptr _result = Egg::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static CORBA::Boolean _0RL_is_a(const char *base_repoId);

protected:

  Egg() {
    if (!is_proxy())
      omniObject::PR_IRRepositoryId(Egg_IntfRepoID);
    this->PR_setobj(this);
  }
  virtual ~Egg() {}
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id=0);

private:

  Egg(const Egg&);
  Egg &operator=(const Egg&);
};

class _sk_Egg :  public virtual Egg {
public:

  _sk_Egg() {}
  _sk_Egg(const omniORB::objectKey& k);
  virtual ~_sk_Egg() {}
  Egg_ptr _this() { return Egg::_duplicate(this); }
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual Chicken_ptr  hatch() = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Egg::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
private:
  _sk_Egg (const _sk_Egg&);
  _sk_Egg &operator=(const _sk_Egg&);
};

class _proxy_Egg :  public virtual Egg {
public:

  _proxy_Egg (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Egg_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_proxy_Egg() {}
  virtual Chicken_ptr  hatch();

protected:

  _proxy_Egg () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type) {
    return Egg::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _proxy_Egg (const _proxy_Egg&);
  _proxy_Egg &operator=(const _proxy_Egg&);
};

class _nil_Egg : public virtual Egg {
public:
  _nil_Egg() : omniObject(omniObject::nilObjectManager()) { this->PR_setobj(0); }
  virtual ~_nil_Egg() {}
  Chicken_ptr  hatch(){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Chicken_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Egg::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

class Egg_proxyObjectFactory : public proxyObjectFactory {
public:
  Egg_proxyObjectFactory () {}
  virtual ~Egg_proxyObjectFactory () {}
  virtual const char *irRepoId() const;
  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);
  virtual CORBA::Boolean is_a(const char *base_repoId) const;
  static Egg_ptr _nil() {
    if (!__nil_Egg) {
      __nil_Egg = new _nil_Egg;
    }
    return __nil_Egg;
  }
private:
  static Egg_ptr __nil_Egg;
};


#undef _LC_attr

#endif // __egg_hh__
