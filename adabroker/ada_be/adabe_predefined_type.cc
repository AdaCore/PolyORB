#include <idl.hh>
#include <idl_extern.hh>
#include <adabe.h>


adabe_predefined_type::adabe_predefined_type(AST_PredefinedType::PredefinedType t, UTL_ScopedName *n, UTL_StrList *p)
  : AST_PredefinedType(t, sn, p),
    AST_Decl(AST_Decl::NT_pre_defined, sn, p),
    adabe_name(AST_Decl::NT_pre_defined,sn,p),

{
}

void 
adabe_predefined_type::produce_ads(dep_list with,string &String, string &previousdefinition)
{
  String += get_ada_predefined_type();
}

void
adabe_predefined_type::produce_adb(dep_list with,string &String, string &previousdefinition)
{
  String += get_ada_predefined_type();
}

void 
adabe_predefined_type::produce_impl_ads(dep_list with,string &String, string &previousdefinition)
{
  String += get_ada_predefined_type();
}

void
adabe_predefined_type::produce_impl_adb(dep_list with,string &String, string &previousdefinition)
{
  String += get_ada_predefined_type();
}

string
adabe_predefined_type::dump_name(dep_list with,string &String, string &previousdefinition)
{
  return get_ada_predefined_type();
}

string
ada_predefined_type::get_ada_predefined_type()
{
  return local_name()->get_string();
}

//IMPL_NARROW_METHODS1(adabe_predefined_type, AST_PredefinedType)
//IMPL_NARROW_FROM_DECL(adabe_predefined_type)
 








