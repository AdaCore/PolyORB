#include <adabe.h>

  
IMPL_NARROW_METHODS1(adabe_enum_val, AST_EnumVal)
IMPL_NARROW_FROM_DECL(adabe_enum_val)

adabe_enum_val::adabe_enum_val(unsigned long v, UTL_ScopedName *n, UTL_StrList *p):
  AST_EnumVal(v,n,p),
  adabe_name()
{
}


string
adabe_enum_val::dump_name(dep_list with, string &body, string &previous)
{
  compute_ada_name();
  return get_ada_local_name();
}

