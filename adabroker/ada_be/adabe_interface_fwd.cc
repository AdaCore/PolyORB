#include <adabe.h>

adabe_interface_fwd::adabe_interface_fwd(UTL_ScopedName *n, UTL_StrList *p)
  : AST_InterfaceFwd(n, p),
    AST_Decl(AST_Decl::NT_interface_fwd, n, p),
    adabe_name(AST_Decl::NT_interface_fwd, n, p)
{
}
  
void
adabe_interface_fwd::produce_ads(dep_list& with, string &body, string &previous)
{
  compute_ada_name();  
  dynamic_cast<adabe_name *>(full_definition())->set_ada_local_name(get_ada_local_name()); 
  dynamic_cast<adabe_name *>(full_definition())->set_ada_full_name(get_ada_full_name());
  string file_name = get_ada_full_name() + "-forward.ads";
  ofstream file(lower(file_name.c_str()));
  file << "with CORBA.Forward \n";
  file << "package" + get_ada_local_name() + "_Forward is new CORBA.Forward \n";
  file.close();  
}

/*
  void
  adabe_interface_fwd::produce_ads(dep_list& with,string &body, string &previous)
  {
  produce_ads( with, body, previous);
  }
*/

IMPL_NARROW_METHODS1(adabe_interface_fwd, AST_InterfaceFwd)
IMPL_NARROW_FROM_DECL(adabe_interface_fwd)
IMPL_NARROW_FROM_SCOPE(adabe_interface_fwd)









