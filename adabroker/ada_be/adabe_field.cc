#include <idl.hh>
#include <idl_extern.hh>
#include <adabe.h>

adabe_field::adabe_field(UTL_ScopedName *n, UTL_StrList *p)
	: AST_Field(ft, n, p),
	  AST_Decl(AST_Decl::NT_field, n, p),
	  adabe_name(AST_Decl::NT_field,n,p)
{
}

void
adabe_field::produce_ads(dep_list with,string &String, string &previousdefinition) 
{
  compute_ada_names();
  String +=  get_ada_name();
  String += " : ";
  AST_Decl *b = field_type();
  String += adabe_name::narrow_from_decl(b)->dump_name( with, String, previousdefinition); 
}

void
adabe_field::produce_adb(dep_list with,string &String, string &previousdefinition) 
{
  produce_ads(with, &String, &previousdefinition);
}

  ///////////////// perhaps useless /////////////////////
void
adabe_field::produce_impl_ads(dep_list with,string &String, string &previousdefinition) 
{
  produce_ads( with, &String, &previousdefinition);
}

void
adabe_field::produce_impl_adb(dep_list with,string &String, string &previousdefinition) 
{
  produce_ads(with, &String, &previousdefinition);
}

  
IMPL_NARROW_METHODS1(adabe_field, AST_Field)
IMPL_NARROW_FROM_DECL(adabe_field)
IMPL_NARROW_FROM_SCOPE(adabe_field)












