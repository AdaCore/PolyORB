// File adabe_root.cc

#include "strings.h"
#include "adabe.h"
#include <idl_extern.hh>

void
adabe_root::produce() {
  
  try {
    string name      = idl_global->stripped_filename()->get_string();
    string idl_file_name = "";
    string includes      = "";
    string previous      = "";
    string next          = "";
    dep_list with;
    idl_file_name =  name + "IDL_FILE";
    
    
    // CREATION OF THE MAIN FILES

    // main header file
    
    string ada_file_name = idl_file_name+".ads";
    ofstream header(ada_file_name.c_str());
    UTL_ScopeActiveIterator head_activator(this,UTL_Scope::IK_decls);
    while (!head_activator.is_done()) {
      AST_Decl *d = head_activator.item();
      switch(d->node_type()) {
      case AST_Decl::NT_sequence:
      case AST_Decl::NT_string:
      case AST_Decl::NT_array:
      case AST_Decl::NT_const:
      case AST_Decl::NT_except:
      case AST_Decl::NT_union:
      case AST_Decl::NT_struct:
      case AST_Decl::NT_enum:
      case AST_Decl::NT_typedef:
      case AST_Decl::NT_interface_fwd:
	adabe_name::narrow_from_decl(d)->produce_ads(with, previous, next);
	break;
      case AST_Decl::NT_module: {
	adabe_module *module = adabe_module::narrow_from_decl(d);
	string P = "";
	string S = "";
	string W_string;
	dep_list W;
	string module_file_name = module->get_ada_full_name();
	ofstream module_file(module_file_name.c_str());
	module->produce_ads(W,S,P);
	W_string = *W.produce("with ");
	
	module_file << W_string;
	module_file << P;            //... Question to ask !!!
	module_file << S;
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
    includes = *with.produce ("with ");
    header << includes;
    header << previous;
    header << next;
    header.close();
    
    // main body file
    
    ada_file_name = idl_file_name+".adb";
    ofstream body(ada_file_name.c_str());
    UTL_ScopeActiveIterator body_activator(this,UTL_Scope::IK_decls);

    while (!body_activator.is_done()) {
      AST_Decl *d = head_activator.item();
      switch(d->node_type()) {
      case AST_Decl::NT_sequence:
      case AST_Decl::NT_string:
      case AST_Decl::NT_array:
      case AST_Decl::NT_pre_defined:
      case AST_Decl::NT_module:
      case AST_Decl::NT_const:
      case AST_Decl::NT_except:
      case AST_Decl::NT_union:
      case AST_Decl::NT_struct:
      case AST_Decl::NT_enum:
      case AST_Decl::NT_typedef:
	adabe_name::narrow_from_decl(d)->produce_adb(with, previous, next);
	break;
      default:
	throw adabe_internal_error(__FILE__,__LINE__,"unexpected contening scope");
	break;
      }
    }
    body << includes;
    body << previous;
    body << next;
    body.close();
    
    // Preparing for a second scan
    
    set_undefined();
    
    // CREATION OF THE IMPLEMENTATION FILES
    
    // header of the implementation file

    ada_file_name = idl_file_name+"-impl.ads";
    ofstream impl_header(ada_file_name.c_str());
    UTL_ScopeActiveIterator impl_head_activator(this,UTL_Scope::IK_decls);
    while (!impl_head_activator.is_done()) {
      AST_Decl *d = impl_head_activator.item();
      switch(d->node_type()) {
      case AST_Decl::NT_sequence:
      case AST_Decl::NT_string:
      case AST_Decl::NT_array:
      case AST_Decl::NT_pre_defined:
      case AST_Decl::NT_module:
      case AST_Decl::NT_const:
      case AST_Decl::NT_except:
      case AST_Decl::NT_union:
      case AST_Decl::NT_struct:
      case AST_Decl::NT_enum:
      case AST_Decl::NT_typedef:
	adabe_name::narrow_from_decl(d)->produce_impl_ads(with, previous, next);
	break;
      default:
	throw adabe_internal_error(__FILE__,__LINE__,"unexpected contening scope");
	break;
      }
    }
    impl_header << includes;
    impl_header << previous;
    impl_header << next;
    impl_header.close();

    // body of the implementation file
    
    ada_file_name = idl_file_name+"-impl.adb";
    ofstream impl_body(ada_file_name.c_str());
    UTL_ScopeActiveIterator impl_body_activator(this,UTL_Scope::IK_decls);

    while (!impl_body_activator.is_done()) {
      AST_Decl *d = head_activator.item();
      switch(d->node_type()) {
      case AST_Decl::NT_sequence:
      case AST_Decl::NT_string:
      case AST_Decl::NT_array:
      case AST_Decl::NT_pre_defined:
      case AST_Decl::NT_module:
      case AST_Decl::NT_const:
      case AST_Decl::NT_except:
      case AST_Decl::NT_union:
      case AST_Decl::NT_struct:
      case AST_Decl::NT_enum:
      case AST_Decl::NT_typedef:
	adabe_name::narrow_from_decl(d)->produce_impl_adb(with, previous, next);
	break;
	
      default:
	throw adabe_internal_error(__FILE__,__LINE__,"unexpected contening scope");
	break;
      }
    }
    impl_body << includes;
    impl_body << previous;
    impl_body << next;
    impl_body.close();
   

   /*
      openning of the files (ads,adb, impl.ads impl.adb...)
      
      for each file lauch adabe_module::produce...
    */
  }
  catch (adabe_internal_error) {
    };
}

IMPL_NARROW_METHODS1(adabe_root, AST_Root)
IMPL_NARROW_FROM_DECL(adabe_root)
IMPL_NARROW_FROM_SCOPE(adabe_root)


