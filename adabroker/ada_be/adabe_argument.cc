#include <idl.hh>
#include <idl_extern.hh>
#include <adabe.h>

adabe_argument::adabe_argument(AST_Argument::Direction d, AST_Type *ft, UTL_ScopedName *n,UTL_StrList *p)
	   : AST_Argument(d, ft, n, p),
	     AST_Field(AST_Decl::NT_argument, ft, n, p),
	     AST_Decl(AST_Decl::NT_argument, n, p),
             adabe_name(AST_Decl::NT_argument,n,p)
{
}

void
adabe_argument::produce_ads(dep_list with,string &String, string &previousdefinition)
{
  compute_ada_names();
  String += get_ada_name() + " :";
  switch (pd_direction) {
  case dir_IN :
    String += " in ";
    break;
  case dir_OUT :
    String += " out ";
    break;
  case dir_INOUT :
    String += " inout ";
    break;
  }
  String += field_type()->dump_name( with, &String, &previousdefinition); // virtual method
}

void
adabe_argument::produce_adb(dep_list with,string &String, string &previousdefinition)
{
  produce_ads(with, &String, &previousdefinition);
}

void
adabe_argument::produce_impl_ads(dep_list with,string &String, string &previousdefinition)
{
  produce_ads( with, &String, &previousdefinition); 
}

///////////////perhaps useless////////////////////////
void
adabe_argument::produce_impl_adb(dep_list with,string &String, string &previousdefinition)
{
  produce_ads(with, &String, &previousdefinition);
}

IMPL_NARROW_METHODS1(adabe_argument, AST_Argument)
IMPL_NARROW_FROM_DECL(adabe_argument)
  



