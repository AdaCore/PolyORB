#include "chicken.hh"
#include <omniORB2/proxyCall.h>

static const char* _0RL_library_version = omniORB_2_7;


// Proxy call descriptor class. Mangled signature:
//  _cEgg
class _0RL_pc_57797d3edca43bdf_00000000
  : public OmniProxyCallDesc
{
public:
  inline _0RL_pc_57797d3edca43bdf_00000000(const char* _op, size_t _op_len) :
    OmniProxyCallDesc(_op, _op_len)  {}

  virtual void unmarshalReturnedValues(GIOP_C&);
  inline Egg_ptr result() { return pd_result; }

private:
  Egg_ptr pd_result;
};

void _0RL_pc_57797d3edca43bdf_00000000::unmarshalReturnedValues(GIOP_C& giop_client)
{
  pd_result = Egg::unmarshalObjRef(giop_client);
}

Egg_ptr  _proxy_Chicken::lay()
{
  _0RL_pc_57797d3edca43bdf_00000000 _call_desc("lay", 4);

  OmniProxyCallWrapper::invoke(this, _call_desc);
  return _call_desc.result();
}




CORBA::Boolean
_sk_Chicken::dispatch(GIOP_S &_0RL_s,const char *_0RL_op,CORBA::Boolean _0RL_response_expected)
{
  if (strcmp(_0RL_op,"lay") == 0)
  {
    _0RL_s.RequestReceived();
    Egg_var _0RL_result;
    _0RL_result = lay();
    size_t _0RL_msgsize = (size_t) GIOP_S::ReplyHeaderSize();
    _0RL_msgsize = Egg::NP_alignedSize((_0RL_result.operator->()),_0RL_msgsize);
    _0RL_s.InitialiseReply(GIOP::NO_EXCEPTION,(CORBA::ULong)_0RL_msgsize);
    Egg::marshalObjRef((_0RL_result.operator->()),_0RL_s);
    _0RL_s.ReplyCompleted();
    return 1;
  }
  else {
    return 0;
  }
}

_sk_Chicken::_sk_Chicken (const omniORB::objectKey& k)
{
  omniRopeAndKey l(0,(CORBA::Octet*)&k,(CORBA::ULong)sizeof(k));
  setRopeAndKey(l,0);
}

omniORB::objectKey
_sk_Chicken::_key()
{
  omniRopeAndKey l;
  getRopeAndKey(l);
  return (*((omniORB::objectKey*)l.key()));
}

Chicken_ptr
Chicken::_duplicate(Chicken_ptr obj)
{
  if (CORBA::is_nil(obj))
    return Chicken::_nil();
  CORBA::Object::_duplicate(obj);
  return obj;
}

Chicken_ptr
Chicken::_narrow(CORBA::Object_ptr obj)
{
  if (CORBA::is_nil(obj))
    return Chicken::_nil();
  Chicken_ptr e = (Chicken_ptr) (obj->PR_getobj()->_realNarrow(Chicken_IntfRepoID));
  if (e)
    return e;
  else
    return Chicken::_nil();
}

void *
Chicken::_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id)
{
  if (is_cxx_type_id) return 0;
  if (!repoId)
    return (void *)((CORBA::Object_ptr)this);
  if (strcmp(Chicken_IntfRepoID,repoId) == 0)
    return (void *)this;
  else {
    return 0;
  }
}

CORBA::Boolean
Chicken::_0RL_is_a(const char *base_repoId) {
  if (strcmp(base_repoId,(const char *)Chicken_IntfRepoID)==0)
    return 1;
  else
    return 0;
}

Chicken_ptr
Chicken_Helper::_nil() {
  return Chicken::_nil();
}

CORBA::Boolean
Chicken_Helper::is_nil(Chicken_ptr p) {
  return CORBA::is_nil(p);
}

void
Chicken_Helper::release(Chicken_ptr p) {
  CORBA::release(p);
}

void
Chicken_Helper::duplicate(Chicken_ptr p) {
  CORBA::Object::_duplicate(p);
}

size_t
Chicken_Helper::NP_alignedSize(Chicken_ptr obj,size_t initialoffset) {
  return Chicken::NP_alignedSize(obj,initialoffset);
}

void
Chicken_Helper::marshalObjRef(Chicken_ptr obj,NetBufferedStream &s) {
  Chicken::marshalObjRef(obj,s);
}

Chicken_ptr Chicken_Helper::unmarshalObjRef(NetBufferedStream &s) {
  return Chicken::unmarshalObjRef(s);
}

void
Chicken_Helper::marshalObjRef(Chicken_ptr obj,MemBufferedStream &s) {
  Chicken::marshalObjRef(obj,s);
}

Chicken_ptr Chicken_Helper::unmarshalObjRef(MemBufferedStream &s) {
  return Chicken::unmarshalObjRef(s);
}

const char *
Chicken_proxyObjectFactory::irRepoId() const
{
  return (const char *)Chicken_IntfRepoID;
}

CORBA::Object_ptr
Chicken_proxyObjectFactory::newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release)
{
  _proxy_Chicken *p = new _proxy_Chicken(r,key,keysize,profiles,release);
  if (!p) {
    throw CORBA::NO_MEMORY(0,CORBA::COMPLETED_NO);
  }
  return (CORBA::Object_ptr) p;
}

CORBA::Boolean
Chicken_proxyObjectFactory::is_a(const char *base_repoId) const
{
  return Chicken::_0RL_is_a(base_repoId);

}

Chicken_ptr
Chicken::_nil() {
  return Chicken_proxyObjectFactory::_nil();
}

static const Chicken_proxyObjectFactory Chicken_proxyObjectFactory;
Chicken_ptr Chicken_proxyObjectFactory::__nil_Chicken = 0;

