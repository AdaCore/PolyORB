#ifndef __chicken_hh__
#define __chicken_hh__

#ifndef USE_omniORB_logStream
#define USE_omniORB_logStream
#endif

#ifndef __CORBA_H_EXTERNAL_GUARD__
#define __CORBA_H_EXTERNAL_GUARD__
#include <omniORB2/CORBA.h>
#endif

#ifndef __egg_hh_EXTERNAL_GUARD__
#define __egg_hh_EXTERNAL_GUARD__
#include <egg.hh>
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

#ifndef __Chicken__
#define __Chicken__
class   Chicken;
typedef Chicken* Chicken_ptr;
typedef Chicken_ptr ChickenRef;
class _proxy_Chicken;
class _sk_Chicken;
class _nil_Chicken;

class   Egg;
typedef Egg* Egg_ptr;
typedef Egg_ptr EggRef;
class _proxy_Egg;
class _sk_Egg;
class _nil_Egg;

class Chicken_Helper {
  public:
  static Chicken_ptr _nil();
  static CORBA::Boolean is_nil(Chicken_ptr p);
  static void release(Chicken_ptr p);
  static void duplicate(Chicken_ptr p);
  static size_t NP_alignedSize(Chicken_ptr obj,size_t initialoffset);
  static void marshalObjRef(Chicken_ptr obj,NetBufferedStream &s);
  static Chicken_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(Chicken_ptr obj,MemBufferedStream &s);
  static Chicken_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<Chicken,Chicken_Helper> Chicken_var;

#endif
#ifndef __Chicken__
#define __Chicken__
class   Chicken;
typedef Chicken* Chicken_ptr;
typedef Chicken_ptr ChickenRef;

class Chicken_Helper {
  public:
  static Chicken_ptr _nil();
  static CORBA::Boolean is_nil(Chicken_ptr p);
  static void release(Chicken_ptr p);
  static void duplicate(Chicken_ptr p);
  static size_t NP_alignedSize(Chicken_ptr obj,size_t initialoffset);
  static void marshalObjRef(Chicken_ptr obj,NetBufferedStream &s);
  static Chicken_ptr unmarshalObjRef(NetBufferedStream &s);
  static void marshalObjRef(Chicken_ptr obj,MemBufferedStream &s);
  static Chicken_ptr unmarshalObjRef(MemBufferedStream &s);
};
typedef _CORBA_ObjRef_Var<Chicken,Chicken_Helper> Chicken_var;

#endif
#define Chicken_IntfRepoID "IDL:Chicken:1.0"

class Chicken : public virtual omniObject, public virtual CORBA::Object {
public:

  virtual Egg_ptr  lay() = 0;
  static Chicken_ptr _duplicate(Chicken_ptr);
  static Chicken_ptr _narrow(CORBA::Object_ptr);
  static Chicken_ptr _nil();

  static inline size_t NP_alignedSize(Chicken_ptr obj,size_t initialoffset) {
    return CORBA::AlignedObjRef(obj,Chicken_IntfRepoID,16,initialoffset);
  }

  static inline void marshalObjRef(Chicken_ptr obj,NetBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Chicken_IntfRepoID,16,s);
  }

  static inline Chicken_ptr unmarshalObjRef(NetBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Chicken_IntfRepoID,s);
    Chicken_ptr _result = Chicken::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static inline void marshalObjRef(Chicken_ptr obj,MemBufferedStream &s) {
    CORBA::MarshalObjRef(obj,Chicken_IntfRepoID,16,s);
  }

  static inline Chicken_ptr unmarshalObjRef(MemBufferedStream &s) {
    CORBA::Object_ptr _obj = CORBA::UnMarshalObjRef(Chicken_IntfRepoID,s);
    Chicken_ptr _result = Chicken::_narrow(_obj);
    CORBA::release(_obj);
    return _result;
  }

  static CORBA::Boolean _0RL_is_a(const char *base_repoId);

protected:

  Chicken() {
    if (!is_proxy())
      omniObject::PR_IRRepositoryId(Chicken_IntfRepoID);
    this->PR_setobj(this);
  }
  virtual ~Chicken() {}
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id=0);

private:

  Chicken(const Chicken&);
  Chicken &operator=(const Chicken&);
};

class _sk_Chicken :  public virtual Chicken {
public:

  _sk_Chicken() {}
  _sk_Chicken(const omniORB::objectKey& k);
  virtual ~_sk_Chicken() {}
  Chicken_ptr _this() { return Chicken::_duplicate(this); }
  void _obj_is_ready(CORBA::BOA_ptr boa) { boa->obj_is_ready(this); }
  CORBA::BOA_ptr _boa() { return CORBA::BOA::getBOA(); }
  void _dispose() { _boa()->dispose(this); }
  omniORB::objectKey _key();
  virtual Egg_ptr  lay() = 0;
  virtual CORBA::Boolean dispatch(GIOP_S &s,const char *op,CORBA::Boolean response);

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Chicken::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
private:
  _sk_Chicken (const _sk_Chicken&);
  _sk_Chicken &operator=(const _sk_Chicken&);
};

class _proxy_Chicken :  public virtual Chicken {
public:

  _proxy_Chicken (Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release) :
    omniObject(Chicken_IntfRepoID,r,key,keysize,profiles,release) {
      omni::objectIsReady(this);
  }
  virtual ~_proxy_Chicken() {}
  virtual Egg_ptr  lay();

protected:

  _proxy_Chicken () {}

  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type) {
    return Chicken::_widenFromTheMostDerivedIntf(repoId,is_cxx_type);
  }
private:

  _proxy_Chicken (const _proxy_Chicken&);
  _proxy_Chicken &operator=(const _proxy_Chicken&);
};

class _nil_Chicken : public virtual Chicken {
public:
  _nil_Chicken() : omniObject(omniObject::nilObjectManager()) { this->PR_setobj(0); }
  virtual ~_nil_Chicken() {}
  Egg_ptr  lay(){
    throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);
#ifdef NEED_DUMMY_RETURN
    // never reach here! Dummy return to keep some compilers happy.
    Egg_ptr _0RL_result = 0;
    return _0RL_result;
#endif
  }

protected:
  virtual void *_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id) {
    return Chicken::_widenFromTheMostDerivedIntf(repoId,is_cxx_type_id);
  }
};

class Chicken_proxyObjectFactory : public proxyObjectFactory {
public:
  Chicken_proxyObjectFactory () {}
  virtual ~Chicken_proxyObjectFactory () {}
  virtual const char *irRepoId() const;
  virtual CORBA::Object_ptr newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release);
  virtual CORBA::Boolean is_a(const char *base_repoId) const;
  static Chicken_ptr _nil() {
    if (!__nil_Chicken) {
      __nil_Chicken = new _nil_Chicken;
    }
    return __nil_Chicken;
  }
private:
  static Chicken_ptr __nil_Chicken;
};


#undef _LC_attr

#endif // __chicken_hh__
