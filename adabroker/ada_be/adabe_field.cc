#include <adabe.h>

adabe_field::adabe_field(UTL_ScopedName *n, UTL_StrList *p)
	: AST_Field(ft, n, p),
	  AST_Decl(AST_Decl::NT_field, n, p),
	  adabe_name()
{
}

void
adabe_field::produce_ads(dep_list with, string &body, string &previous) 
{
  compute_ada_names();
  body += "      " + get_ada_local_name();
  body += " : ";
  AST_Decl *b = field_type();
  body += adabe_name::narrow_from_decl(b)->dump_name(with, body, previous); 
  body += ";\n";
}

/*
  void
  adabe_field::produce_adb(dep_list with,string &body, string &previous) 
  {
  produce_ads(with, &body, &previous);
  }
  
  ///////////////// perhaps useless /////////////////////
  void
  adabe_field::produce_impl_ads(dep_list with,string &body, string &previous) 
  {
  produce_ads( with, &body, &previous);
  }
  
  void
  adabe_field::produce_impl_adb(dep_list with,string &body, string &previous) 
  {
  produce_ads(with, &body, &previous);
  }
*/

void
adabe_field::produce_marshal_adb(dep_list with, string &body, string &previous){};
  
IMPL_NARROW_METHODS1(adabe_field, AST_Field)
IMPL_NARROW_FROM_DECL(adabe_field)
IMPL_NARROW_FROM_SCOPE(adabe_field)












