#include <adabe.h>

adabe_argument::adabe_argument(AST_Argument::Direction d, AST_Type *ft, UTL_ScopedName *n,UTL_StrList *p)
	   : AST_Argument(d, ft, n, p),
	     AST_Field(AST_Decl::NT_argument, ft, n, p),
	     AST_Decl(AST_Decl::NT_argument, n, p),
             adabe_name()
{
}

void
adabe_argument::produce_ads(dep_list with, string &body, string &previous)
{
  compute_ada_names();
  String += get_ada_local_name() + " :";
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
  AST_Decl d* = field_type();
  String += adabe_name::narrow_from_decl(d)->dump_name(with, body, previous); // virtual method
}


/*
  void
  adabe_argument::produce_adb(dep_list with,string &body, string &previous)
  {
  produce_ads(with, body, previous);
  }

  void
  adabe_argument::produce_impl_ads(dep_list with,string &body, string &previous)
  {
  produce_ads( with, body, previous); 
  }
  
  ///////////////perhaps useless////////////////////////
  void
  adabe_argument::produce_impl_adb(dep_list with,string &body, string &previous)
  {
  produce_ads(with, body, previous);
  }
*/

adabe_argument::produce_proxies_ads(dep_list with, string &body, string &input)
{
  if input = "IN"
    {
      string tmp = "";
      bool verif = false;
      tmp += get_ada_local_name() + " :";
      switch (pd_direction) {
      case dir_IN :
      case dir_INOUT :
	tmp += " in ";
	verif = true;
	break;
      case dir_OUT :
	break;
      }
      AST_Decl d* = field_type();
      tmp += adabe_name::narrow_from_decl(d)->dump_name(with, body, previous); // virtual method
      if (verif) body += tmp + ", ";
    }
  if input = "OUT"
    {
      string tmp = "";
      bool verif = false;
      tmp += get_ada_local_name() + " :";
      switch (pd_direction) {
      case dir_OUT :
      case dir_INOUT :
	tmp += " out ";
	verif = true;
	break;
      case dir_IN :
	break;
      }
      AST_Decl d* = field_type();
      tmp += adabe_name::narrow_from_decl(d)->dump_name(with, body, previous); // virtual method
      if (verif) body += tmp + ", ";
    }
}

void
adabe_argument::produce_proxies_adb(dep_list with, string &body, string &previous)
{
  INDENT(String);
  body += "      Arg_" + get_ada_local_name() + " :";
  AST_Decl d* = field_type();
  body += adabe_name::narrow_from_decl(d)->dump_name(with, body, previous); // virtual method
  body += "_Ptr := null ;\n";
}

IMPL_NARROW_METHODS1(adabe_argument, AST_Argument)
IMPL_NARROW_FROM_DECL(adabe_argument)
  






