#include <adabe.h>

adabe_predefined_type::adabe_predefined_type(AST_PredefinedType::PredefinedType t, UTL_ScopedName *n, UTL_StrList *p)
  : AST_PredefinedType(t, sn, p),
    AST_Decl(AST_Decl::NT_pre_defined, sn, p),
    adabe_name()
{
}

void 
adabe_predefined_type::produce_ads(dep_list with, string &body, string &previous)
{
  body += get_ada_predefined_type();
  set_already_defined();
}

/*
  void
  adabe_predefined_type::produce_adb(dep_list with,string &body, string &previous)
  {
  body += get_ada_predefined_type();
  }
  
  void 
  adabe_predefined_type::produce_impl_ads(dep_list with,string &body, string &previous)
  {
  body += get_ada_predefined_type();
  }
  
  void
  adabe_predefined_type::produce_impl_adb(dep_list with,string &body, string &previous)
  {
  body += get_ada_predefined_type();
  }
*/

string
adabe_predefined_type::dump_name(dep_list with, string &body, string &previous)
{
  return get_ada_predefined_type();
}

string
adabe_predefined_type::get_ada_predefined_type()
{
  string result = local_name()->get_string();
  result = "CORBA." + result;
  return result;  
}

IMPL_NARROW_METHODS1(adabe_predefined_type, AST_PredefinedType)
IMPL_NARROW_FROM_DECL(adabe_predefined_type)
 








