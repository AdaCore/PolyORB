//file adabe_array.cc

#include <adabe.h>

  
IMPL_NARROW_METHODS1(adabe_enum, AST_Enum);
IMPL_NARROW_FROM_DECL(adabe_enum);
IMPL_NARROW_FROM_SCOPE(adabe_enum);

void
adabe_enum::produce_ads(dep_list with,string &String, string &previousdefinition) {
  string temp;
  
  temp += "type " + get_ada_local_name() + "is (\n";
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  while (!activator.is_done()) {
    AST_Decl *d = activator.item();
    switch (d->node_type()) {
    case AST_Decl::NT_enum_val:
      temp+=adabe_enum_val::narrow_from_decl(d)->dump_name(with, String, previousdefinition);
      break;
    default:
      throw adabe_internal_error(__FILE__,__LINE__,"unexpected contening scope in enumeration type");
    }
    if (!activator.is_done()) temp += "'";
  }
  temp +=");\n";
  set_already_defined();
  previousdefinition += temp;
  
}

virtual void 
produce_marshal_ads(dep_list with, string &body, string &previous) {
  
  
}

virtual void 
produce_marshal_adb(dep_list with, string &body, string &previous) {
  
  
}

string adabe_enum::dump_name(dep_list with,string &String, string &previousdefinition) {
  if (!is_already_defined()) {
    string temp;
    produce_ads( with, String, temp);
    previousdefinition += temp;
  }
  return get_ada_full_name();
}
