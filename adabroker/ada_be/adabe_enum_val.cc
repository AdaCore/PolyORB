#include <adabe.h>

  
IMPL_NARROW_METHODS1 (adabe_enum_val, AST_EnumVal)
IMPL_NARROW_FROM_DECL (adabe_enum_val)

adabe_enum_val::adabe_enum_val (unsigned long v, UTL_ScopedName *n, UTL_StrList *p): AST_Decl (AST_Decl::NT_enum_val, n, p),
    AST_Constant (AST_Expression::EV_ulong,
		 AST_Decl::NT_enum_val,
		 new AST_Expression (v),
		 n,
		 p),
 
  AST_EnumVal (v, n, p),
  adabe_name (AST_Decl::NT_enum_val, n, p)
{
}


string
adabe_enum_val::dump_name (dep_list& with, string &previous)
{
  return get_ada_local_name ();
}

string
adabe_enum_val::marshal_name (dep_list& with, string &previous)
{
  return get_ada_local_name ();
}

