/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_root.cc                                           ***
***                                                                                            ***
***      This file provides the implementation of class adabe_root      declared in adabe.h    ***
***   (L 588). This class is the correspondant of the Sun's Front-End class AST_Root.          ***
***   It provides a "produce" function to generate all files, and a constructor.               ***
***                                                                                            ***
***                                                                                            ***
***   Copyright 1999                                                                           ***
***   Jean Marie Cottin, Laurent Kubler, Vincent Niebel                                        ***
***                                                                                            ***
***   This is free software; you can redistribute it and/or modify it under terms of the GNU   ***
***   General Public License, as published by the Free Software Foundation.                    ***
***                                                                                            ***
***  This back-end is distributed in the hope that it will be usefull, but WITHOUT ANY         ***
***  WARRANTY; without even the implied waranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR ***
***  PURPOSE.                                                                                  ***
***                                                                                            ***
***  See the GNU General Public License for more details.                                      ***
***                                                                                            ***
***                                                                                            ***
*************************************************************************************************/

#include <adabe.h>
 
static string remove_dot(string  name)
{
  char c;
  while ((c = name.find(".")) != -1) 
    name[c]='-';
  return name;
}

adabe_root::adabe_root(UTL_ScopedName *n, UTL_StrList *p)
  : AST_Root(n,p),
    AST_Module(n,p),
    AST_Decl(AST_Decl::NT_root,n,p),
    UTL_Scope(AST_Decl::NT_root),
    adabe_name(AST_Decl::NT_root,n,p) 
{
  adabe_global::set_root(this);
  set_in_main_file(I_TRUE);
  return;
}

void
adabe_root::produce() {
  try {    
    string name      = idl_global->stripped_filename()->get_string();
    int end_of_name = name.find(".idl");
    if (end_of_name > 0) name = name.substr(0, end_of_name);
    string idl_file_name = "";
    idl_file_name =  name + "_IDL_FILE";
    // **************************
    // CREATION OF THE MAIN FILES
    // **************************

    // main header file
    {
      string header_includes      = "";
      string header_previous      = "";
      string header_body          = "";
      bool   first = true;
      dep_list header_with;
      header_with.add("Ada.Unchecked_Deallocation") ;
      header_with.add("Corba");
      header_with.add("AdaBroker") ;
      UTL_ScopeActiveIterator header_activator(this,UTL_Scope::IK_decls);
      header_body = "Package " + get_ada_full_name()+" is\n";
      while (!header_activator.is_done())
	{
	  AST_Decl *d = header_activator.item();
	  header_activator.next(); 
	  adabe_global::set_adabe_current_file(this);
	  if (d->in_main_file())     // only to take the node issue from the idl file
	    {
#ifdef DEBUG_ROOT
	      cout << "In root, node type encountered :" <<  d->node_type() << endl;
#endif 
	      switch(d->node_type()) {
	      case AST_Decl::NT_const:
	      case AST_Decl::NT_sequence:
	      case AST_Decl::NT_string:
	      case AST_Decl::NT_array:
	      case AST_Decl::NT_except:
	      case AST_Decl::NT_union:
	      case AST_Decl::NT_struct:
	      case AST_Decl::NT_enum:
	      case AST_Decl::NT_typedef:
	      case AST_Decl::NT_interface_fwd:
		{
		  header_previous += header_body;
		  header_body ="";
		  dynamic_cast<adabe_name *>(d)->produce_ads(header_with, header_body, header_previous);
		}
		break;
		
	      case AST_Decl::NT_module:	    
		{
		  adabe_module *module = adabe_module::narrow_from_decl(d);
		  string module_previous = "";
		  string module_body = "";
		  string module_with_string;
		  dep_list module_with;
		  module->produce_ads(module_with,module_body,module_previous);
		  module_with_string = *module_with.produce("with ");
		  
		  string module_file_name =
		    remove_dot(module->get_ada_full_name())+".ads";
		  char *lower_case_name = lower(module_file_name.c_str());
		  ofstream module_file(lower_case_name); 	// Open the ads module file	
		  delete[] lower_case_name;
		  module_file << module_with_string;
		  module_file << module_previous; 
		  module_file << module_body;
		  module_file.close();
		}
		break;
	      case AST_Decl::NT_interface:	    
		{
		  adabe_interface *interface = adabe_interface::narrow_from_decl(d);
		  string interface_previous = "";
		  string interface_body = "";
		  string interface_with_string;
		  dep_list interface_with;
		  interface->produce_ads(interface_with,interface_body,interface_previous);
#ifdef DEBUG_ROOT
	      cout << "In root, after launching the interface produce" << endl;
#endif 		  

		  interface_with_string = *interface_with.produce("with ");
		  string interface_file_name = remove_dot(interface->get_ada_full_name())+".ads";
		  char *lower_case_name = lower(interface_file_name.c_str());
		  ofstream interface_file(lower_case_name); 
		  delete[] lower_case_name;
		  interface_file << interface_with_string;
		  interface_file << interface_previous;    
		  interface_file << interface_body;
		  interface_file.close();
#ifdef DEBUG_ROOT
		  cout << "In root, after closing the interface file" << endl;
#endif 		  
		}
		
		break;
	      case AST_Decl::NT_enum_val:
		break;
	      default:
		cerr << d->node_type() << endl;
		throw adabe_internal_error(__FILE__,__LINE__,"unexpected contening scope");
		break;
	      }
	    }
	}
	  
      // Opening of the header file
      
      string ada_file_name = idl_file_name+".ads";
#ifdef DEBUG_ROOT
      cout << "In root, main ads file : " << ada_file_name.c_str() << endl;
#endif
      char *lower_case_name = lower(ada_file_name.c_str());
      ofstream header(lower_case_name); 
      delete[] lower_case_name;
      
      header_includes = *header_with.produce ("with ");
      header << header_includes;
      header << header_previous;
      header << header_body;
      header << "end " << get_ada_full_name() << " ;" << endl;
      header.close();
	
    }
    


    // main body file
    
    {
      UTL_ScopeActiveIterator body_activator(this,UTL_Scope::IK_decls);
      
      while (!body_activator.is_done())
	{
	  AST_Decl *d = body_activator.item();
	   body_activator.next();
	  adabe_global::set_adabe_current_file(this);
	  if (d->in_main_file())     // only to take the node issue from the idl file
	    {
	      switch(d->node_type())
		{
		case AST_Decl::NT_module:
		  {
		    adabe_module *module = adabe_module::narrow_from_decl(d);
		    string body_module_previous = "";
		    string body_module_body = "";
		    dep_list body_module_with;
		    
		    module->produce_adb(body_module_with,body_module_body,body_module_previous);
		    
		    string body_module_file_name = module->get_ada_full_name()+".adb";
		    
		  }
		  break;
		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface = adabe_interface::narrow_from_decl(d);
		    string interface_previous = "";
		    string interface_body = "";
		    string interface_with_string;
		    dep_list interface_with;
		    
		    interface->produce_adb(interface_with,interface_body,interface_previous);
		    interface_with_string = *interface_with.produce("with ");
		    
		    string interface_file_name = remove_dot(interface->get_ada_full_name())+".adb";
		    char *lower_case_name = lower(interface_file_name.c_str());
		    ofstream interface_file(lower_case_name); 
		    delete[] lower_case_name;
		    interface_file << interface_with_string;
		    interface_file << interface_previous;    
		    interface_file << interface_body;
		    interface_file.close();
		  }
		default:
		  break;
		}
	    }
	}
    }
    
    // Preparing for a second scan
    
    // ************************************
    // CREATION OF THE IMPLEMENTATION FILES
    // ************************************
    
    if (adabe_global::impl_flags())
      {
	// header of the implementation file
	{
	  UTL_ScopeActiveIterator impl_head_activator(this,UTL_Scope::IK_decls);
	  while (!impl_head_activator.is_done())
	    {
	      AST_Decl *d = impl_head_activator.item();
	      impl_head_activator.next();
	      adabe_global::set_adabe_current_file(this);
	      if (d->in_main_file())     // only to take the node issue from the idl file
		{
		  switch(d->node_type())
		    {
		      
		    case AST_Decl::NT_module:                       // there's nothing to be done in the module (it's just here to write the implementation
		      {                                             // of the interfaces in the modules
			string impl_header_module_previous = ""; 
			string impl_header_module_body     = "";
			string impl_header_module_with_string;
			dep_list impl_header_module_with;
			
			adabe_module *module = adabe_module::narrow_from_decl(d);
			module->produce_impl_ads(impl_header_module_with,impl_header_module_body,impl_header_module_previous);
		      }
		      break;
		    case AST_Decl::NT_interface:
		      {
			adabe_interface *interface = adabe_interface::narrow_from_decl(d);
			string impl_header_interface_previous = "";
			string impl_header_interface_body = "";
			string impl_header_interface_with_string;
			dep_list impl_header_interface_with;
			
			interface->produce_impl_ads(impl_header_interface_with,impl_header_interface_body,impl_header_interface_previous);
			impl_header_interface_with_string = *impl_header_interface_with.produce("with ");
			
			string impl_header_interface_file_name =
			  remove_dot(interface->get_ada_full_name())+"-impl.ads";
			char *lower_case_name = lower(impl_header_interface_file_name.c_str());
			ofstream impl_header_interface_file(lower_case_name); 
			delete[] lower_case_name;
			impl_header_interface_file << impl_header_interface_with_string;
			impl_header_interface_file << impl_header_interface_previous;    
			impl_header_interface_file << impl_header_interface_body;
			impl_header_interface_file.close();
		      }
		      
		    default:
		      break;
		    }
		}
	    }
	}
	
	// body of the implementation file
	
	{
	  UTL_ScopeActiveIterator impl_body_activator(this,UTL_Scope::IK_decls);
	  
	  while (!impl_body_activator.is_done())
	    {
	      AST_Decl *d = impl_body_activator.item();
	      impl_body_activator.next();
	      adabe_global::set_adabe_current_file(this);
	      if (d->in_main_file())     // only to take the node issue from the idl file
		{
		  switch(d->node_type())
		    {
		    case AST_Decl::NT_module:
		      {
			adabe_module *module = adabe_module::narrow_from_decl(d);
			string impl_body_module_previous = "";
			string impl_body_module_body     = "";
			string impl_body_module_with_string;
			dep_list impl_body_module_with;
			
			module->produce_impl_adb(impl_body_module_with,impl_body_module_body,impl_body_module_previous);
		      }
		      break;
		      
		    case AST_Decl::NT_interface:
		      {
			adabe_interface *interface = adabe_interface::narrow_from_decl(d);
			string impl_body_interface_previous = "";
			string impl_body_interface_body = "";
			string impl_body_interface_with_string;
			dep_list impl_body_interface_with;
			
			interface->produce_impl_adb(impl_body_interface_with,impl_body_interface_body,impl_body_interface_previous);
			impl_body_interface_with_string = *impl_body_interface_with.produce("with ");
			
			string impl_body_interface_file_name =
			  remove_dot(interface->get_ada_full_name())+"-impl.adb";
			char *lower_case_name = lower(impl_body_interface_file_name.c_str());
			ofstream impl_body_interface_file(lower_case_name); 
			delete[] lower_case_name;
			impl_body_interface_file << impl_body_interface_with_string;
			impl_body_interface_file << impl_body_interface_previous;    
			impl_body_interface_file << impl_body_interface_body;
			impl_body_interface_file.close();
		      }
		    default:
		      break;
		    }
		}
	    }
	}
      }
    
    // *****************************
    // CREATION OF THE PROXIES FILES
    // *****************************
    

    // header of the proxies files
    {
      UTL_ScopeActiveIterator proxy_head_activator(this,UTL_Scope::IK_decls);
      while (!proxy_head_activator.is_done())
	{
	  AST_Decl *d = proxy_head_activator.item();
	  proxy_head_activator.next();
	  adabe_global::set_adabe_current_file(this);
	  if (d->in_main_file())     // only to take the node issue from the idl file
	    {
	      switch(d->node_type())
		{
		  
		case AST_Decl::NT_module:                       // there's nothing to be done in the module (it's just here to write the implementation
		  {                                             // of the interfaces in the modules
		    string proxy_header_module_previous = ""; 
		    string proxy_header_module_body     = "";
		    string proxy_header_module_with_string;
		    dep_list proxy_header_module_with;
		    
		    adabe_module *module = adabe_module::narrow_from_decl(d);
		    module->produce_proxies_ads(proxy_header_module_with,proxy_header_module_body,proxy_header_module_previous);
		  }
		  break;

		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface = adabe_interface::narrow_from_decl(d);
		    string proxy_header_interface_previous = "";
		    string proxy_header_interface_body = "";
		    string proxy_header_interface_with_string;
		    dep_list proxy_header_interface_with;
		    
		    interface->produce_proxies_ads(proxy_header_interface_with,proxy_header_interface_body,proxy_header_interface_previous);
		    proxy_header_interface_with_string = *proxy_header_interface_with.produce("with ");
		    
		    string proxy_header_interface_file_name =
		      remove_dot(interface->get_ada_full_name())+"-proxies.ads";
		    char *lower_case_name = lower(proxy_header_interface_file_name.c_str());
		    ofstream proxy_header_interface_file(lower_case_name); 
		    delete[] lower_case_name;
		    proxy_header_interface_file << proxy_header_interface_with_string;
		    proxy_header_interface_file << proxy_header_interface_previous;    
		    proxy_header_interface_file << proxy_header_interface_body;
		    proxy_header_interface_file.close();
		  }
		  
		default:
		  break;
		}
	    }
	}
    }

    // body of the proxies files
    
    {
      UTL_ScopeActiveIterator proxy_body_activator(this,UTL_Scope::IK_decls);
      
      while (!proxy_body_activator.is_done())
	{
	  AST_Decl *d = proxy_body_activator.item();
	  proxy_body_activator.next();
	  adabe_global::set_adabe_current_file(this);
	  if (d->in_main_file())     // only to take the node issue from the idl file
	    {
	      switch(d->node_type())
		{
		case AST_Decl::NT_module:
		  {
		    adabe_module *module = adabe_module::narrow_from_decl(d);
		    string proxy_body_module_previous = "";
		    string proxy_body_module_body     = "";
		    string proxy_body_module_with_string;
		    dep_list proxy_body_module_with;
		    
		    module->produce_proxies_adb(proxy_body_module_with,proxy_body_module_body,proxy_body_module_previous);
		  }
		  break;
		  
		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface = adabe_interface::narrow_from_decl(d);
		    string proxy_body_interface_previous = "";
		    string proxy_body_interface_body = "";
		    dep_list proxy_body_interface_with;
		    
		    interface->produce_proxies_adb(proxy_body_interface_with,proxy_body_interface_body,proxy_body_interface_previous);
		    string proxy_body_interface_with_string = *proxy_body_interface_with.produce("with ");
		    string proxy_body_interface_use_string = *proxy_body_interface_with.produce("use ");
		    
		    if (proxy_body_interface_body == "") break;
		    string proxy_body_interface_file_name =
		      remove_dot(interface->get_ada_full_name())+"-proxies.adb";
		    char *lower_case_name = lower(proxy_body_interface_file_name.c_str());
		    ofstream proxy_body_interface_file(lower_case_name); 
		    delete[] lower_case_name;
		    proxy_body_interface_file << proxy_body_interface_with_string;
		    proxy_body_interface_file << proxy_body_interface_use_string;
		    proxy_body_interface_file << proxy_body_interface_previous;    
		    proxy_body_interface_file << proxy_body_interface_body;
		    proxy_body_interface_file.close();
		  }
		default:
		  break;
		}
	    }
	}
    }
    
    // ******************************
    // CREATION OF THE SKELETON FILES
    // ******************************
    

    // header of the skeleton files
    {
      UTL_ScopeActiveIterator skel_head_activator(this,UTL_Scope::IK_decls);
      while (!skel_head_activator.is_done())
	{
	  AST_Decl *d = skel_head_activator.item();
	  adabe_global::set_adabe_current_file(this);
	  skel_head_activator.next();
	  if (d->in_main_file())     // only to take the node issue from the idl file
	    {
	      switch(d->node_type())
		{
		  
		case AST_Decl::NT_module:                       // there's nothing to be done in the module (it's just here to write the implementation
		  {                                             // of the interfaces in the modules
		    string skel_header_module_previous = ""; 
		    string skel_header_module_body     = "";
		    string skel_header_module_with_string;
		    dep_list skel_header_module_with;
		    
		    adabe_module *module = adabe_module::narrow_from_decl(d);
		    module->produce_skel_ads(skel_header_module_with,skel_header_module_body,skel_header_module_previous);
		  }
		  break;

		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface = adabe_interface::narrow_from_decl(d);
		    string skel_header_interface_previous = "";
		    string skel_header_interface_body = "";
		    string skel_header_interface_with_string;
		    dep_list skel_header_interface_with;
		    
		    interface->produce_skel_ads(skel_header_interface_with,skel_header_interface_body,skel_header_interface_previous);
		    skel_header_interface_with_string = *skel_header_interface_with.produce("with ");
		    
		    string skel_header_interface_file_name =
		      remove_dot(interface->get_ada_full_name())+"-skeleton.ads";
		    char *lower_case_name = lower(skel_header_interface_file_name.c_str());
		    ofstream skel_header_interface_file(lower_case_name); 
		    delete[] lower_case_name;
		    skel_header_interface_file << skel_header_interface_with_string;
		    skel_header_interface_file << skel_header_interface_previous;    
		    skel_header_interface_file << skel_header_interface_body;
		    skel_header_interface_file.close();
		  }
		  
		default:
		  break;
		}
	    }
	}
    }

    // body of the skeleton files
    
    {
      UTL_ScopeActiveIterator skel_body_activator(this,UTL_Scope::IK_decls);
      
      while (!skel_body_activator.is_done())
	{
	  AST_Decl *d = skel_body_activator.item();
	  skel_body_activator.next();
	  adabe_global::set_adabe_current_file(this);
	  if (d->in_main_file())     // only to take the node issue from the idl file
	    {
	      switch(d->node_type())
		{
		case AST_Decl::NT_module:
		  {
		    adabe_module *module = adabe_module::narrow_from_decl(d);
		    string skel_body_module_previous = "";
		    string skel_body_module_body     = "";
		    string skel_body_module_with_string;
		    dep_list skel_body_module_with;
		    
		    module->produce_skel_adb(skel_body_module_with,skel_body_module_body,skel_body_module_previous);
		  }
		  break;
		  
		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface = adabe_interface::narrow_from_decl(d);
		    string skel_body_interface_previous = "";
		    string skel_body_interface_body = "";
		    string skel_body_interface_with_string;
		    string skel_body_interface_use_string;
		    dep_list skel_body_interface_with;
		    
		    interface->produce_skel_adb(skel_body_interface_with,skel_body_interface_body,skel_body_interface_previous);
		    skel_body_interface_with_string = *skel_body_interface_with.produce("with ");
		    skel_body_interface_use_string = *skel_body_interface_with.produce("use ");
		    
		    string skel_body_interface_file_name =
		      remove_dot(interface->get_ada_full_name())+"-skeleton.adb";
		    char *lower_case_name = lower(skel_body_interface_file_name.c_str());
		    ofstream skel_body_interface_file(lower_case_name); 
		    delete[] lower_case_name;
		    skel_body_interface_file << skel_body_interface_with_string;
		    skel_body_interface_file << skel_body_interface_use_string;
		    skel_body_interface_file << skel_body_interface_previous;    
		    skel_body_interface_file << skel_body_interface_body;
		    skel_body_interface_file.close();
		  }
		default:
		  break;
		}
	    }
	}
    }
    
    set_undefined();

    // ******************************
    // CREATION OF THE MARSHALL FILES
    // ******************************

    // marshall header file
    {
      string marshal_header_includes      = "";
      string marshal_header_previous      = "";
      string marshal_header_body          = "";
      dep_list marshal_header_with;

      marshal_header_previous += "use type Corba.Unsigned_Long; \n";
      marshal_header_previous += "with NetbufferedStream ; use NetbufferedStream ;\n";
      marshal_header_previous += "with MembufferedStream ; use MembufferedStream ;\n";
      marshal_header_with.add ("Giop_C");
      marshal_header_with.add ("Corba");
      marshal_header_previous += "Package " + get_ada_full_name() + ".marshal is\n";
		  
      UTL_ScopeActiveIterator marshal_header_activator(this,UTL_Scope::IK_decls);
      while (!marshal_header_activator.is_done())
	{
	  AST_Decl *d = marshal_header_activator.item();
	  marshal_header_activator.next();
	  adabe_global::set_adabe_current_file(this);
	  if (d->in_main_file())     // only to take the node issue from the idl file
	    {
	      switch(d->node_type()) 
		{
		case AST_Decl::NT_sequence:
		case AST_Decl::NT_string:
		case AST_Decl::NT_array:
		case AST_Decl::NT_except:
		case AST_Decl::NT_union:
		case AST_Decl::NT_struct:
		case AST_Decl::NT_enum:
		case AST_Decl::NT_typedef:
		  {
		  marshal_header_previous += marshal_header_body ="";
		    dynamic_cast<adabe_name *>(d)->produce_marshal_ads(marshal_header_with, marshal_header_body, marshal_header_previous);
		  }
		  break;
		  
		case AST_Decl::NT_module:	    
		  {
		    adabe_module *module = adabe_module::narrow_from_decl(d);
		    string marshal_module_previous = "";
		    string marshal_module_body = "";
		    string marshal_module_with_string;
		    dep_list marshal_module_with;
		    
		    module->produce_marshal_ads(marshal_module_with,marshal_module_body,marshal_module_previous);
		    marshal_module_with_string = *marshal_module_with.produce("with ");
		    
		    string marshal_module_file_name =
		      remove_dot(module->get_ada_full_name())+"-marshal.ads";
		    char *lower_case_name = lower(marshal_module_file_name.c_str());
		    ofstream marshal_module_file(lower_case_name);
		    delete[] lower_case_name;
		    marshal_module_file << marshal_module_with_string;
		    marshal_module_file << marshal_module_previous; 
		    marshal_module_file << marshal_module_body;
		    marshal_module_file.close();
		  }
		  break;		  
		  
		case AST_Decl::NT_interface:	    
		  {
		    adabe_interface *interface = adabe_interface::narrow_from_decl(d);
		    string marshal_interface_previous = "";
		    string marshal_interface_body = "";
		    string marshal_interface_with_string;
		    dep_list marshal_interface_with;
		    
		    interface->produce_marshal_ads(marshal_interface_with,marshal_interface_body,marshal_interface_previous);
		    marshal_interface_with_string = *marshal_interface_with.produce("with ");
		    
		    string marshal_interface_file_name =
		      remove_dot(interface->get_ada_full_name())+"-marshal.ads";
		    char *lower_case_name = lower(marshal_interface_file_name.c_str());
		    ofstream marshal_interface_file(lower_case_name); 
		    delete[] lower_case_name;
		    marshal_interface_file << marshal_interface_with_string;
		    marshal_interface_file << marshal_interface_previous;    
		    marshal_interface_file << marshal_interface_body;
		    marshal_interface_file.close();
		  }
		  
		  break;
		default:
		  break;
		}
	    }
	}

      // Opening of the header file
      string marshal_ada_file_name = idl_file_name+"-marshal.ads";
      char *lower_case_name = lower(marshal_ada_file_name.c_str());
      ofstream marshal_header(lower_case_name); 
      delete[] lower_case_name;
      marshal_header_includes = *marshal_header_with.produce ("with ");
      marshal_header << marshal_header_includes;
      marshal_header << marshal_header_previous;
      marshal_header << marshal_header_body;
      marshal_header << "end " << get_ada_full_name() << ".marshal ;" << endl;
      marshal_header.close();
    }
    

    set_undefined();

    // marshal body file
    
    {
      UTL_ScopeActiveIterator marshal_body_activator(this,UTL_Scope::IK_decls);
      bool   first =true;
      string marshal_body_includes      = "";
      string marshal_body_previous      = "";
      string marshal_body_body          = "";
      dep_list marshal_body_with;
    
      while (!marshal_body_activator.is_done())
	{
	  AST_Decl *d = marshal_body_activator.item();
	  marshal_body_activator.next();
	  adabe_global::set_adabe_current_file(this);
	  if (d->in_main_file())     // only to take the node issue from the idl file
	    {
	      switch(d->node_type())
		{
		case AST_Decl::NT_sequence:
		case AST_Decl::NT_string:
		case AST_Decl::NT_array:
		case AST_Decl::NT_except:
		case AST_Decl::NT_union:
		case AST_Decl::NT_struct:
		case AST_Decl::NT_enum:
		case AST_Decl::NT_typedef:
		  {
		    if (first) {
		      marshal_body_previous = "Package body " + get_ada_full_name() + ".marshal is \n";
		      first = false;    
		    }
		    marshal_body_previous += marshal_body_body ="";
		    dynamic_cast<adabe_name *>(d)->produce_marshal_adb
		      (marshal_body_with, marshal_body_body, marshal_body_previous);
		  }
		  break;
		case AST_Decl::NT_module:
		  {
		    adabe_module *module = adabe_module::narrow_from_decl(d);
		    string marshal_body_module_previous    = "";
		    string marshal_body_module_body        = "";
		    dep_list marshal_body_module_with;

		    module->produce_marshal_adb(marshal_body_module_with,marshal_body_module_body,marshal_body_module_previous);
		    string marshal_body_module_with_string = *marshal_body_module_with.produce("with ");
		    string marshal_body_module_use_string = *marshal_body_module_with.produce("use ");
		    
		    if (marshal_body_module_body == "") break;
		    string marshal_module_file_name =
		      remove_dot(module->get_ada_full_name())+"-marshal.adb";
		    char *lower_case_name = lower(marshal_module_file_name.c_str());
		    ofstream marshal_module_file(lower_case_name);
		    delete[] lower_case_name;
		    marshal_module_file << marshal_body_module_with_string;
		    marshal_module_file << marshal_body_module_use_string;
		    marshal_module_file << marshal_body_module_previous; 
		    marshal_module_file << marshal_body_module_body;
		    marshal_module_file.close();
		    
		  }
		  break;
		  
		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface = adabe_interface::narrow_from_decl(d);
		    string marshal_interface_previous = "";
		    string marshal_interface_body = "";
		    dep_list marshal_interface_with;
		    
		    interface->produce_marshal_adb(marshal_interface_with,marshal_interface_body,marshal_interface_previous);
		    string marshal_interface_with_string = *marshal_interface_with.produce("with ");
		    string marshal_interface_use_string = *marshal_interface_with.produce("use ");

		    if (marshal_interface_body == "") break;
		    string marshal_interface_file_name =
		      remove_dot(interface->get_ada_full_name())+"-marshal.adb";
		    char *lower_case_name = lower(marshal_interface_file_name.c_str());
		    ofstream marshal_interface_file(lower_case_name); 
		    delete[] lower_case_name;
		    marshal_interface_file << marshal_interface_with_string;
		    marshal_interface_file << marshal_interface_use_string;
		    marshal_interface_file << marshal_interface_previous;    
		    marshal_interface_file << marshal_interface_body;
		    marshal_interface_file.close();
		  }
		default:
		  break;
		}
	    }
	}
      // Opening of the marshal body file
      if (!first)
	{ 
	  string marshal_ada_file_name =
	    remove_dot(idl_file_name)+"-marshal.adb";
	  char *lower_case_name = lower(marshal_ada_file_name.c_str());
	  ofstream marshal_body(lower_case_name); 
	  delete[] lower_case_name;
	  marshal_body_includes = *marshal_body_with.produce ("with ");
	  marshal_body << marshal_body_includes;
	  marshal_body << marshal_body_previous;
	  marshal_body << marshal_body_body;
	  marshal_body << "end " << get_ada_full_name() << ".marshal ;" << endl;
	  marshal_body.close();
	}
    }

  }
  catch (adabe_internal_error &e)
    {
      cout << "in : " << e.file() << "   Line : "<< e.line() << endl << e.errmsg() << endl;
    };
}

IMPL_NARROW_METHODS1(adabe_root, AST_Root)
IMPL_NARROW_FROM_DECL(adabe_root)
IMPL_NARROW_FROM_SCOPE(adabe_root)


