#include <idl.hh>
#include <idl_extern.hh>
#include <adabe.h>

adabe_union::adabe_union(AST_ConcreteType *dt, UTL_ScopedName *n, UTL_StrList *p)
	: AST_Union(dt, n, p),
	  AST_Decl(AST_Decl::NT_union, n, p),
          AST_Structure(AST_Decl::NT_union, n, p),
	  UTL_Scope(AST_Decl::NT_union),
	  adabe_name(AST_Decl::NT_union, n, p)
{
}

void
adabe_union::produce_ads(dep_list with,string &String, string &previousdefinition)
{
  ///////////WARNING//////////////
  //  the type of the discriminant should be check. From this type result a specific solution  

  string temp = "";
  compute_ada_names();
  indentation(temp);
  temp += "type " + get_ada_name();
  disc_type()->compute_ada_names();
  name = get_ada_name();
  temp += "(Switch : "  + disc_type(with, &temp, &previousdefinition)->dump_name();
  temp += " := " + name + "'first) is record\n";
  inc_indentation();
  indentation(temp);
  temp += "case Switch is\n";
  inc_indentation();
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      indentation(temp);
      AST_Decl *d = i.item();
      //	if (d->node_type() == AST_Decl::NT_UnionBranch)
      //	  {
      d->produce_ads(with, &temp, &previousdefinition);
      temp += "\n";
      //        }
      i.next();
    }
  dec_indentation();
  indentation(temp);
  temp += "end case; \n"
  dec_indentation();
  indentation(temp);
  temp += "end record; \n";
  previousdefinition += temp;
}

void
adabe_union::produce_adb(dep_list with,string &String, string &previousdefinition)
{
  if (!is_imported(with)) return get_ada_name();
  return get_ada_full_name();	   
}

void
adabe_union::produce_impl_ads(dep_list with,string &String, string &previousdefinition)
{
  produce_ads(with, &String, &previousdefinition);
}

void
adabe_union::produce_impl_adb(dep_list with,string &String, string &previousdefinition)
{
  if (!is_imported(with)) return get_ada_name();
  return get_ada_full_name();	   
}

string
adabe_union::dump_name(dep_list with,string &String, string &previousdefinition)
{
  if (!is_imported(with))
    {
      if (!is_already_defined()) produce_ads( with, &String, &previousdefinition);
      return get_ada_name();
    }
  return get_ada_full_name();	   
}

//IMPL_NARROW_METHODS1(adabe_union, AST_Union)
//IMPL_NARROW_FROM_DECL(adabe_union)
//IMPL_NARROW_FROM_SCOPE(adabe_union)







