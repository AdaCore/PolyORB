#include <idl.hh>
#include <idl_extern.hh>
#include <adabe.h>

adabe_structure::adabe_structure(UTL_ScopedName *n, UTL_StrList *p);
	    : AST_Decl(AST_Decl::NT_struct, n, p),
	      UTL_Scope(AST_Decl::NT_struct),
	      adabe_name(AST_Decl::NT_struct, n, p),
{
}

void
adabe_structure::produce_ads(dep_list with,string &String, string &previousdefinition)
{
  compute_ada_names();
  INDENT(temp);
  temp += "type " + ada_name + "is record\n";
  INC_INDENT();
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      INDENT(temp);
      AST_Decl *d = i.item();
      //	if (d->node_type() == AST_Decl::NT_field)
      //	  {
      d->produce_ads(with, &temp, &previousdefinition);
      temp += "\n";
      //        }
      i.next();
    }
  DEC_INDENT();
  INDENT(temp);
  temp += "end record;\n";
  previousdefinition += temp;
}

void
adabe_structure::produce_adb(dep_list with,string &String, string &previousdefinition)
{
  if (!is_imported(with)) return get_ada_name();
  else return get_ada_full_name();
}

void
adabe_structure::produce_impl_ads(dep_list with,string &String, string &previousdefinition)
{
  produce_ads(with, &String, &previousdefinition);
}

void  
adabe_structure::produce_impl_adb(dep_list with,string &String, string &previousdefinition)
{
  if (!is_imported(with)) return get_ada_name();
  else return get_ada_full_name();
}

string
adabe_structure::dump_name(dep_list with,string &String, string &previousdefinition)
{
  if (!is_imported(with))
    {
      if (!is_already_defined) previousdefinition += produce_ads( with, &String, &previousdefinition);
      return get_ada_name();
    }
  return get_ada_full_name();	   
}
  
IMPL_NARROW_METHODS1(adabe_structure, AST_Structure)
IMPL_NARROW_FROM_DECL(adabe_structure)
IMPL_NARROW_FROM_SCOPE(adabe_structure)












