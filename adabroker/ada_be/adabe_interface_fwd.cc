#include <idl.hh>
#include <idl_extern.hh>
#include <adabe.h>

adabe_interface_fwd(UTL_ScopedName *n, UTL_StrList *p)
  : AST_InterfaceFwd(n, p),
    AST_Decl(AST_Decl::NT_interface_fwd, n, p),
    adabe_name(AST_Decl::NT_interface_fwd,n,p)
{
}
  
void
adabe_interface_fwd::produce_ads(dep_list with,string &String, string &previousdefinition)
{
  compute_ada_names();
  full_definition()->set_ada_name(get_ada_name()); 
  full_definition()->set_ada_full_name(get_ada_full_name());
  INDETATION(String);
  String += "with CORBA.Forward \n";
  INDENTATION(String);
  String += "package" + get_ada_name() + "_Forward is new CORBA.Forward \n"; 
}

void
adabe_interface_fwd::produce_ads(dep_list with,string &String, string &previousdefinition)
{
  produce_ads( with, String, previousdefinition);
}

IMPL_NARROW_METHODS1(adabe_interface_fwd, AST_InterfaceFwd)
IMPL_NARROW_FROM_DECL(adabe_interface_fwd)
IMPL_NARROW_FROM_SCOPE(adabe_interface_fwd)









