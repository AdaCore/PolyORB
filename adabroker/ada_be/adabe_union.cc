#include <adabe.h>

adabe_union::adabe_union(AST_ConcreteType *dt, UTL_ScopedName *n, UTL_StrList *p)
	: AST_Union(dt, n, p),
	  AST_Decl(AST_Decl::NT_union, n, p),
          AST_Structure(AST_Decl::NT_union, n, p),
	  UTL_Scope(AST_Decl::NT_union),
	  adabe_name()
{
}

void
adabe_union::produce_ads(dep_list with, string &body, string &previous)
{
  ///////////WARNING//////////////
  //  the type of the discriminant should be check. From this type result a specific solution  

  compute_ada_name();
  body += "   type " + get_ada_local_name();
  adabe_name *b = adabe_name::narrow_from_decl(disc_type());
  b->compute_ada_name();
  string name = get_ada_local_name();
  body += "(Switch : "  + b->dump_name(with, body, previous);
  body += " := " + name + "'first) is record\n";
  body += "      case Switch is\n";
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_union_branch)
	adabe_union_branch::narrow_from_decl(d)->produce_ads(with, body, previous, disc_type());
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in union");
    }
  body += "      end case; \n";
  body += "   end record; \n";
  set_already_defined();
}

/*
  void
  adabe_union::produce_adb(dep_list with,string &body, string &previous)
  {
  if (!is_imported(with)) return get_ada_local_name();
  return get_ada_full_name();	   
  }
  
  void
  adabe_union::produce_impl_ads(dep_list with,string &body, string &previous)
  {
  produce_ads(with, body, previous);
  }
  
  void
  adabe_union::produce_impl_adb(dep_list with,string &body, string &previous)
  {
  if (!is_imported(with)) return get_ada_local_name();
  return get_ada_full_name();	   
  }
*/

void
adabe_union::produce_marshal_ads(dep_list with, string &body, string &previous)
{
}

void
adabe_union::produce_marshal_adb(dep_list with, string &body, string &previous)
{
}

string
adabe_union::dump_name(dep_list with, string &body, string &previous)
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

IMPL_NARROW_METHODS1(adabe_union, AST_Union)
IMPL_NARROW_FROM_DECL(adabe_union)
IMPL_NARROW_FROM_SCOPE(adabe_union)









