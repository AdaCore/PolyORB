#include <adabe.h>

adabe_typedef::adabe_typedef(AST_Type *bt, UTL_ScopedName *n, UTL_StrList *p)
	  : AST_Typedef(bt, n, p),
	    AST_Decl(AST_Decl::NT_typedef, n, p),
	    adabe_name(AST_Decl::NT_typedef, n, p)

{
}

void
adabe_typedef::produce_ads(dep_list& with, string &body, string &previous)
{
  compute_ada_name();
  body += "   type " + get_ada_local_name() + " is new ";
  AST_Decl *b = base_type();
  string name =  dynamic_cast<adabe_name *>(b)->dump_name(with, body, previous); //virtual method
  body += name;
  body += ";\n";
  body += "   type" + get_ada_local_name() + "_Ptr is access all " + get_ada_local_name() + ";\n";
  body += "   procedure free is new Unchecked_Deallocation(";
  body += get_ada_local_name() + ", " + get_ada_local_name ()+ "_Ptr);\n";
  set_already_defined();
}

/*
  void
  adabe_typedef::produce_adb(dep_list& with,string &body, string &previous)
  {
  if (!is_imported(with)) return get_ada_local_name();
  return get_ada_full_name();	   
  }
  
  void
  adabe_typedef::produce_impl_ads(dep_list& with,string &body, string &previous)
  {
  INDENTATION(body);
  body += "type" + get_ada_local_name() + "is new ";
  AST_Decl *b  base_type();
  string name =  dynamic_cast<adabe_name *>(b)->dump_name(with, &body, &previous); //virtual method
  body += name;
  body += ";\n";
  }
  
  void
  adabe_typedef::produce_impl_adb(dep_list& with,string &body, string &previous)
  {
  if (!is_imported(with)) return get_ada_local_name();
  return get_ada_full_name();
  }
*/

void
adabe_typedef::produce_marshal_ads(dep_list& with, string &body, string &previous)
{
}
void
adabe_typedef::produce_marshal_adb(dep_list& with, string &body, string &previous)
{
}



string
adabe_typedef::dump_name(dep_list& with, string &body, string &previous)
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

IMPL_NARROW_METHODS1(adabe_typedef, AST_Typedef)
IMPL_NARROW_FROM_DECL(adabe_typedef)








