#include "egg.hh"
#include <omniORB2/proxyCall.h>

static const char* _0RL_library_version = omniORB_2_7;


// Proxy call descriptor class. Mangled signature:
//  _cChicken
class _0RL_pc_e6f7fc7eac13d3c8_00000000
  : public OmniProxyCallDesc
{
public:
  inline _0RL_pc_e6f7fc7eac13d3c8_00000000(const char* _op, size_t _op_len) :
    OmniProxyCallDesc(_op, _op_len)  {}

  virtual void unmarshalReturnedValues(GIOP_C&);
  inline Chicken_ptr result() { return pd_result; }

private:
  Chicken_ptr pd_result;
};

void _0RL_pc_e6f7fc7eac13d3c8_00000000::unmarshalReturnedValues(GIOP_C& giop_client)
{
  pd_result = Chicken::unmarshalObjRef(giop_client);
}

Chicken_ptr  _proxy_Egg::hatch()
{
  _0RL_pc_e6f7fc7eac13d3c8_00000000 _call_desc("hatch", 6);

  OmniProxyCallWrapper::invoke(this, _call_desc);
  return _call_desc.result();
}




CORBA::Boolean
_sk_Egg::dispatch(GIOP_S &_0RL_s,const char *_0RL_op,CORBA::Boolean _0RL_response_expected)
{
  if (strcmp(_0RL_op,"hatch") == 0)
  {
    _0RL_s.RequestReceived();
    Chicken_var _0RL_result;
    _0RL_result = hatch();
    size_t _0RL_msgsize = (size_t) GIOP_S::ReplyHeaderSize();
    _0RL_msgsize = Chicken::NP_alignedSize((_0RL_result.operator->()),_0RL_msgsize);
    _0RL_s.InitialiseReply(GIOP::NO_EXCEPTION,(CORBA::ULong)_0RL_msgsize);
    Chicken::marshalObjRef((_0RL_result.operator->()),_0RL_s);
    _0RL_s.ReplyCompleted();
    return 1;
  }
  else {
    return 0;
  }
}

_sk_Egg::_sk_Egg (const omniORB::objectKey& k)
{
  omniRopeAndKey l(0,(CORBA::Octet*)&k,(CORBA::ULong)sizeof(k));
  setRopeAndKey(l,0);
}

omniORB::objectKey
_sk_Egg::_key()
{
  omniRopeAndKey l;
  getRopeAndKey(l);
  return (*((omniORB::objectKey*)l.key()));
}

Egg_ptr
Egg::_duplicate(Egg_ptr obj)
{
  if (CORBA::is_nil(obj))
    return Egg::_nil();
  CORBA::Object::_duplicate(obj);
  return obj;
}

Egg_ptr
Egg::_narrow(CORBA::Object_ptr obj)
{
  if (CORBA::is_nil(obj))
    return Egg::_nil();
  Egg_ptr e = (Egg_ptr) (obj->PR_getobj()->_realNarrow(Egg_IntfRepoID));
  if (e)
    return e;
  else
    return Egg::_nil();
}

void *
Egg::_widenFromTheMostDerivedIntf(const char *repoId,CORBA::Boolean is_cxx_type_id)
{
  if (is_cxx_type_id) return 0;
  if (!repoId)
    return (void *)((CORBA::Object_ptr)this);
  if (strcmp(Egg_IntfRepoID,repoId) == 0)
    return (void *)this;
  else {
    return 0;
  }
}

CORBA::Boolean
Egg::_0RL_is_a(const char *base_repoId) {
  if (strcmp(base_repoId,(const char *)Egg_IntfRepoID)==0)
    return 1;
  else
    return 0;
}

Egg_ptr
Egg_Helper::_nil() {
  return Egg::_nil();
}

CORBA::Boolean
Egg_Helper::is_nil(Egg_ptr p) {
  return CORBA::is_nil(p);
}

void
Egg_Helper::release(Egg_ptr p) {
  CORBA::release(p);
}

void
Egg_Helper::duplicate(Egg_ptr p) {
  CORBA::Object::_duplicate(p);
}

size_t
Egg_Helper::NP_alignedSize(Egg_ptr obj,size_t initialoffset) {
  return Egg::NP_alignedSize(obj,initialoffset);
}

void
Egg_Helper::marshalObjRef(Egg_ptr obj,NetBufferedStream &s) {
  Egg::marshalObjRef(obj,s);
}

Egg_ptr Egg_Helper::unmarshalObjRef(NetBufferedStream &s) {
  return Egg::unmarshalObjRef(s);
}

void
Egg_Helper::marshalObjRef(Egg_ptr obj,MemBufferedStream &s) {
  Egg::marshalObjRef(obj,s);
}

Egg_ptr Egg_Helper::unmarshalObjRef(MemBufferedStream &s) {
  return Egg::unmarshalObjRef(s);
}

const char *
Egg_proxyObjectFactory::irRepoId() const
{
  return (const char *)Egg_IntfRepoID;
}

CORBA::Object_ptr
Egg_proxyObjectFactory::newProxyObject(Rope *r,CORBA::Octet *key,size_t keysize,IOP::TaggedProfileList *profiles,CORBA::Boolean release)
{
  _proxy_Egg *p = new _proxy_Egg(r,key,keysize,profiles,release);
  if (!p) {
    throw CORBA::NO_MEMORY(0,CORBA::COMPLETED_NO);
  }
  return (CORBA::Object_ptr) p;
}

CORBA::Boolean
Egg_proxyObjectFactory::is_a(const char *base_repoId) const
{
  return Egg::_0RL_is_a(base_repoId);

}

Egg_ptr
Egg::_nil() {
  return Egg_proxyObjectFactory::_nil();
}

static const Egg_proxyObjectFactory Egg_proxyObjectFactory;
Egg_ptr Egg_proxyObjectFactory::__nil_Egg = 0;

