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
  string tmp = "";
  INDENT(tmp);
  tmp += "type " + get_ada_name() + "is record\n";
  INC_INDENT();
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      INDENT(tmp);
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_field)
	adabe_name::narrow_from_decl(d)->produce_ads(with, tmp, previousdefinition);
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in structure");
      i.next();
    }
  DEC_INDENT();
  INDENT(tmp);
  tmp += "end record;\n";
  INDENT(tmp);
  tmp += "type " + get_ada_name() + "_Ptr is access all " + get_ada_name() + ";\n";
  INDENT(tmp);
  tmp += "procedure free is new Unchecked_Deallocation(";
  tmp += get_ada_name() + ", " + get_ada_name ()+ "_Ptr);\n";  
  previousdefinition += tmp;
  set_already_defined();
}

/*
void
adabe_structure::produce_adb(dep_list with,string &String, string &previousdefinition)
{
  if (!is_imported(with)) return get_ada_name();
  else return get_ada_full_name();
}
  void
  adabe_structure::produce_impl_ads(dep_list with,string &String, string &previousdefinition)
  {
  INDENT(String);
  String += "type " + get_ada_name() + "is record\n";
  INC_INDENT();
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
  {
  INDENT(String);
  AST_Decl *d = i.item();
  if (d->node_type() == AST_Decl::NT_field)
  adabe_name::narrow_from_decl(d)->produce_impl_ads(with, String, previousdefinition);
  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in structure");
  i.next();
  }
  DEC_INDENT();
  INDENT(String);
  String += "end record;\n";
  
  }


  void  
  adabe_structure::produce_impl_adb(dep_list with,string &String, string &previousdefinition)
  {
  if (!is_imported(with)) return get_ada_name();
  else return get_ada_full_name();
  }
*/

string
adabe_structure::dump_name(dep_list with,string &String, string &previousdefinition)
{
  if (!is_imported(with))
    {
      if (!is_already_defined)
	produce_ads(with, String, previousdefinition);
      return get_ada_name();
    }
  return get_ada_full_name();	   
}
  
IMPL_NARROW_METHODS1(adabe_structure, AST_Structure)
IMPL_NARROW_FROM_DECL(adabe_structure)
IMPL_NARROW_FROM_SCOPE(adabe_structure)












