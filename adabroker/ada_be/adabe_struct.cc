#include <adabe.h>

adabe_structure::adabe_structure(UTL_ScopedName *n, UTL_StrList *p)
	    : AST_Decl(AST_Decl::NT_struct, n, p),
	      UTL_Scope(AST_Decl::NT_struct),
	      adabe_name(AST_Decl::NT_struct, n, p)
{
}

void
adabe_structure::produce_ads(dep_list& with, string &body, string &previous)
{
  compute_ada_name();
  body += "   type " + get_ada_local_name() + "is record\n";
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_field)
	dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in structure");
      i.next();
    }
  body += "   end record;\n";
  body += "   type " + get_ada_local_name() + "_Ptr is access all " + get_ada_local_name() + ";\n";
  body += "   procedure free is new Unchecked_Deallocation(";
  body += get_ada_local_name() + ", " + get_ada_local_name ()+ "_Ptr);\n";  
  set_already_defined();
}

/*
  void
  adabe_structure::produce_adb(dep_list& with,string &body, string &previous)
  {
  if (!is_imported(with)) return get_ada_local_name();
  else return get_ada_full_name();
  }
  void
  adabe_structure::produce_impl_ads(dep_list& with,string &body, string &previous)
  {
  INDENT(body);
  body += "type " + get_ada_local_name() + "is record\n";
  INC_INDENT();
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
  {
  INDENT(body);
  AST_Decl *d = i.item();
  if (d->node_type() == AST_Decl::NT_field)
  dynamic_cast<adabe_name *>(d)->produce_impl_ads(with, body, previous);
  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in structure");
  i.next();
  }
  DEC_INDENT();
  INDENT(body);
  body += "end record;\n";
  
  }


  void  
  adabe_structure::produce_impl_adb(dep_list& with,string &body, string &previous)
  {
  if (!is_imported(with)) return get_ada_local_name();
  else return get_ada_full_name();
  }
*/

void
adabe_structure::produce_marshal_ads(dep_list& with, string &body, string &previous)
{
}
void
adabe_structure::produce_marshal_adb(dep_list& with, string &body, string &previous)
{
}

string
adabe_structure::dump_name(dep_list& with, string &body, string &previous)
{
  if (!is_imported(with))
    {
      if (!is_already_defined())
	{
	  string tmp = "";
	  produce_ads(with, tmp, previous);
	  previous += tmp;
	}
      return get_ada_local_name();
    }
  return get_ada_full_name();	   
}
  
IMPL_NARROW_METHODS1(adabe_structure, AST_Structure)
IMPL_NARROW_FROM_DECL(adabe_structure)
IMPL_NARROW_FROM_SCOPE(adabe_structure)












