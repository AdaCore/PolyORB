#include <adabe.h>

adabe_predefined_type::adabe_predefined_type (AST_PredefinedType::PredefinedType t, UTL_ScopedName *n, UTL_StrList *p)
  : AST_PredefinedType (t, n, p),
    AST_Decl (AST_Decl::NT_pre_defined, n, p),
    adabe_name (AST_Decl::NT_pre_defined, n, p)
{
}

void 
adabe_predefined_type::produce_ads (dep_list& with, string &body, string &previous)
{
  body += get_ada_predefined_type ();
  set_already_defined ();
}

string
adabe_predefined_type::dump_name (dep_list& with, string &previous)
{
  return get_ada_predefined_type ();
}

string
adabe_predefined_type::marshal_name (dep_list& with, string &previous)
{
  return get_ada_predefined_type ();
}

string adabe_predefined_type::get_ada_predefined_type () 
{ 
  string name = ""; 
  switch (pt ()) 
    { 
    case AST_PredefinedType::PT_long:
      name = "CORBA.Long"; 
      break; 
    case AST_PredefinedType::PT_ulong: name = "CORBA.Unsigned_Long";
      break; 
    case AST_PredefinedType::PT_short: name = "CORBA.Short";
      break; 
    case AST_PredefinedType::PT_ushort: name = "CORBA.Unsigned_Short";
      break; 
    case AST_PredefinedType::PT_float: name = "CORBA.Float"; 
      break; 
    case AST_PredefinedType::PT_double: name = "CORBA.Double"; 
      break; 
    case AST_PredefinedType::PT_char: name = "CORBA.Char"; 
      break;
    case AST_PredefinedType::PT_boolean: name = "CORBA.Boolean"; 
      break; 
    case AST_PredefinedType::PT_octet: name = "CORBA.Octet"; 
      break; 
    case AST_PredefinedType::PT_void: name = "<void>"; 
      break;
    default: throw
	       adabe_internal_error (__FILE__,__LINE__,"Unexpected predefined type");
      break; 
    }
  return name; 
}

IMPL_NARROW_METHODS1 (adabe_predefined_type, AST_PredefinedType)
IMPL_NARROW_FROM_DECL (adabe_predefined_type)
 








