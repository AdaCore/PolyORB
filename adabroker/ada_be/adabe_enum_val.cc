//file adabe_enumval.cc

#include <adabe.h>

  
IMPL_NARROW_METHODS1(adabe_enum_val, AST_EnumVal);
IMPL_NARROW_FROM_DECL(adabe_enum_val);

string adabe_enum_val::dump_name(dep_list with, string &String, string &previousdefinition) {
  compute_ada_name();
  return get_ada_local_name();
}

