#include <adabe.h>

IMPL_NARROW_METHODS1(adabe_module, AST_Module);
IMPL_NARROW_FROM_DECL(adabe_module);
IMPL_NARROW_FROM_SCOPE(adabe_module);

void
adabe_module::produce_ads(dep_list with,string &String, string &previousdefinition)
{
  
  // before doing anything compute the ada name
  
  compute_ada_name();
  UTL_ScopeActiveIterator module_activator(this,UTL_Scope::IK_decls);
  while (!module_activator.is_done()) {
    AST_Decl *d = module_activator.item();
    switch(d->node_type()) {
    case AST_Decl::NT_array:
    case AST_Decl::NT_interface_fwd:
    case AST_Decl::NT_pre_defined:
    case AST_Decl::NT_const:
    case AST_Decl::NT_except:
    case AST_Decl::NT_union:
    case AST_Decl::NT_struct:
    case AST_Decl::NT_enum:
    case AST_Decl::NT_typedef:
      adabe_name::narrow_from_decl(d)->produce_ads(with, String, previousdefinition);
      break;
    case AST_Decl::NT_module: {
      adabe_module *module = adabe_module::narrow_from_decl(d);
      string P = "";
      string N = "";
      string W_string;
      dep_list W;
      string module_file_name = module->get_ada_full_name();
      ofstream module_file(module_file_name.c_str());
      module->produce_ads(W,P,N);
      W_string = *W.produce("with ");
      
      module_file << W_string;
      module_file << P;            //... Question to ask !!!
      module_file << N;
	module_file.close();
    }
    case AST_Decl::NT_interface: {
      adabe_interface *interface = adabe_interface::narrow_from_decl(d);
      string P = "";
      string N = "";
      string W_string;
      dep_list W;
      string interface_file_name = interface->get_ada_full_name();
      ofstream interface_file(interface_file_name.c_str());
      interface->produce_ads(W,P,N);
      W_string = *W.produce("with ");
      
      interface_file << W_string;
      interface_file << P;            //... Question to ask !!!
      interface_file << N;
      interface_file.close();
      }
    
    break;
      default:
	throw adabe_internal_error(__FILE__,__LINE__,"unexpected contening scope");
	break;
    }    
  }
}

string adabe_module::dump_name(dep_list with,string &String, string &previousdefinition) {
  if (!is_already_defined()){
    string temp;
    produce_ads( with, String, previousdefinition);
    previousdefinition += temp;
  }
  return get_ada_full_name();
}
