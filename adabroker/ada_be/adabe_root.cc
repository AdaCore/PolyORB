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
    // get the name of the IDL file 
    string name      = idl_global->stripped_filename()->get_string();
    
    // find the string .idl in the file name, and remove the end of the file
    // name
    int end_of_name = name.find(".idl");
    if (end_of_name > 0) name = name.substr(0, end_of_name);

    // then add _IDL_FILE at the end of the file name
    string idl_file_name = name + "_IDL_FILE";

    /////////////////////////////
    // CREATION OF THE MAIN FILES
    /////////////////////////////
    
    /////////////////////////////////////////////
    // generation of the main file of the package

    {
      // definition of the variables used in the production of
      // the main ads file
      string header_includes      = ""; // string containing the list of dependencies
      string header_previous      = ""; // string in which the node can put the definition needed 
      string header_body          = ""; // string containing the body of node declared
      dep_list header_with;             // list of dependencies
      
      // addition of some usefull file 
      header_with.add("Ada.Unchecked_Deallocation") ;
      header_with.add("CORBA");
      header_with.add("AdaBroker") ;
      
      UTL_ScopeActiveIterator header_activator(this,UTL_Scope::IK_decls);
      header_body = "Package " + get_ada_full_name()+" is\n";
      
      // loop over the scope to find the node to output
      while (!header_activator.is_done())
	{
	  AST_Decl *d = header_activator.item();

	  // that the pointer   to the active file to this one (the root file)
	  adabe_global::set_adabe_current_file(this);

	  // only to take the node issue from the idl file
	  // (no imported definition,or predefined type will be produced)
	  if (d->in_main_file())    
	    {
#ifdef DEBUG_ROOT
	      cout << "In root, node type encountered :" <<  d->node_type() << endl;
#endif 

	      // this switch is here to ensure that no unexpected node is in the root
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
		  // for a simple node :
		  // it will be put in the body file, and his local types will be
		  // stroed in the previous string
		  header_previous += header_body;   
		  header_body ="";
		  dynamic_cast<adabe_name *>(d)->produce_ads(header_with, header_body,
							     header_previous);
		}
		break;
		
	      case AST_Decl::NT_module:	    
		{
		  adabe_module *module = adabe_module::narrow_from_decl(d);
		  // if the node is a module, it must be defined in a new file

		  // Variables used for the new file
		  string module_previous = "";
		  string module_body = "";
		  string module_with_string;
		  dep_list module_with;

		  // Production of the strings that must be found in the
		  // module file
		  module->produce_ads(module_with,module_body,module_previous);
		  module_with_string = *module_with.produce("with ");


		  // computing of the module file name:
		  string module_file_name =
		    remove_dot(module->get_ada_full_name())+".ads";

		  // the name must be in lower case in order to be found by the
		  // ADA compiler
		  char *lower_case_name = lower(module_file_name.c_str());
		  ofstream module_file(lower_case_name); 	
		  delete[] lower_case_name;

		  // writing in the file 
		  module_file << module_with_string;
		  module_file << module_previous; 
		  module_file << module_body;
		  module_file.close();
		}
		break;
	      case AST_Decl::NT_interface:	    
		{
		  adabe_interface *interface = adabe_interface::narrow_from_decl(d);
		  
		  // as for the module, a new file is created
		  // new variables needed for the interface file
		  string interface_previous = "";
		  string interface_body = "";
		  string interface_with_string;
		  dep_list interface_with;
		  
		  // production of the strings that must be found in the interface file
		  interface->produce_ads(interface_with,interface_body,
					 interface_previous);
		  interface_with_string = *interface_with.produce("with ");

		  // computing of the interface file name
		  string interface_file_name =
		    remove_dot(interface->get_ada_full_name())+".ads";

		  
		  // he must be in lower case
		  char *lower_case_name = lower(interface_file_name.c_str());
		  ofstream interface_file(lower_case_name); 
		  delete[] lower_case_name;

		  // The strings are writed to the file :
		  interface_file << interface_with_string;
		  interface_file << interface_previous;    
		  interface_file << interface_body;
		  interface_file.close();
		}
		
		break;
	      case AST_Decl::NT_enum_val:
		// the enumeration values of an enumeration type can
		// be found in the root, but they shall not be mapped here
		break;
		
	      default:
		// if another node type is found, an exception is raised
		// no further verification will be made in the root in
		// for the other files (marshall ...).
#ifdef DEBUG_ROOT
7		cerr << "A node type of the type : " << d->node_type();
		cerr << " and named : " <<  idl_global->stripped_filename()->get_string();
		cerr << " has been found in the root" << endl;		
#endif
		throw adabe_internal_error(__FILE__,__LINE__,"unexpected contained node in the root");
		break;
	      }
	    
	    }
	  header_activator.next(); 
	}
      
      // Opening of the header file

      // computing the file name:
      string ada_file_name = idl_file_name+".ads";
      char *lower_case_name = lower(ada_file_name.c_str());

      ofstream header(lower_case_name); 
      delete[] lower_case_name;

      // and writing in the file :
      header_includes = *header_with.produce ("with ");
      header << header_includes;
      header << header_previous;
      header << header_body;
      header << "end " << get_ada_full_name() << " ;" << endl;
      header.close();

      // Note: even if the file is empty, it will be produced.
    }
    

    /////////////////////////////////////////////
    // generation of the main body of the package

    // this time, only interface file will be created
    {
      UTL_ScopeActiveIterator body_activator(this,UTL_Scope::IK_decls);

      // loop over the scope to find the interface (a module  may
      // contain an interface)
      while (!body_activator.is_done())
	{
	  adabe_global::set_adabe_current_file(this);
	  AST_Decl *d = body_activator.item();
	  
	  if (d->in_main_file())
	    {
	      switch(d->node_type())
		{
		case AST_Decl::NT_module:
		  {
		    adabe_module *module = adabe_module::narrow_from_decl(d);

		    // dummy variables given to the produce adb from the module :
		    // they won't be modified (can be suppressed in future version)
		    string body_module_previous = "";
		    string body_module_body = "";
		    dep_list body_module_with;
		    
		    module->produce_adb(body_module_with,body_module_body,
					body_module_previous);
		    
		  }
		  break;
		  
		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface = adabe_interface::narrow_from_decl(d);

		    // variables used in the interface file
		    string interface_previous = "";
		    string interface_body = "";
		    string interface_with_string;
		    dep_list interface_with;

		    // production of the interface body
		    interface->produce_adb(interface_with,interface_body,
					   interface_previous);
		    interface_with_string = *interface_with.produce("with ");

		    // computing the file name
		    string interface_file_name =
		      remove_dot(interface->get_ada_full_name())+".adb";
		    char *lower_case_name = lower(interface_file_name.c_str());
		    ofstream interface_file(lower_case_name); 
		    delete[] lower_case_name;

		    // writing in the file
		    interface_file << interface_with_string;
		    interface_file << interface_previous;    
		    interface_file << interface_body;
		    interface_file.close();
		  }
		  
		default:
		  // nothing to be done in the other cases
		  break;
		  
		} // end of the switch
	    } 
	  body_activator.next();
	} //end of the loop in the scope 
    }
    
    // Preparing for a second scan
    
    ///////////////////////////////////////
    // CREATION OF THE IMPLEMENTATION FILES
    ///////////////////////////////////////

    
    // these files are only produced if the corresponding
    // flag has been found 
    if (adabe_global::impl_flags())
      {
	////////////////////////////////////
	// header of the implementation file
     
	{

	  // Once more, only the interfaces will be mapped
	  UTL_ScopeActiveIterator impl_head_activator(this,UTL_Scope::IK_decls);
	  while (!impl_head_activator.is_done())
	    {
	      AST_Decl *d = impl_head_activator.item();

	      if (d->in_main_file())     // only to take the node issue from the idl file
		{
		  switch(d->node_type())
		    {
		      
		    case AST_Decl::NT_module:                      
		      {
			// dummy variables (can be suppressed)
			string impl_header_module_previous = ""; 
			string impl_header_module_body     = "";
			string impl_header_module_with_string;
			dep_list impl_header_module_with;

			// tries to find interface in the module
			adabe_module *module = adabe_module::narrow_from_decl(d);
			module->produce_impl_ads(impl_header_module_with,impl_header_module_body,
						 impl_header_module_previous);
		      }
		      break;

		    case AST_Decl::NT_interface:
		      {
			adabe_interface *interface = adabe_interface::narrow_from_decl(d);

			// Initialisation of the variables used in
			// the interface file
			string impl_header_interface_previous = "";
			string impl_header_interface_body = "";
			string impl_header_interface_with_string;
			dep_list impl_header_interface_with;

			// Computing the interface output
			interface->produce_impl_ads(impl_header_interface_with,
						    impl_header_interface_body,
						    impl_header_interface_previous);
			impl_header_interface_with_string = *impl_header_interface_with.produce("with ");

			// computing the interface file name
			string impl_header_interface_file_name =
			  remove_dot(interface->get_ada_full_name())+"-impl.ads";
			char *lower_case_name = lower(impl_header_interface_file_name.c_str());
			ofstream impl_header_interface_file(lower_case_name); 
			delete[] lower_case_name;

			// writing the strings in the file
			impl_header_interface_file << impl_header_interface_with_string;
			impl_header_interface_file << impl_header_interface_previous;    
			impl_header_interface_file << impl_header_interface_body;
			impl_header_interface_file.close();
		      }
		      
		    default:
		      break;
		    } // end of the switch
		}
	      
	      impl_head_activator.next();
	    } // end of the loop
	}
	
	//////////////////////////////////
	// body of the implementation file
	
	{
	  UTL_ScopeActiveIterator impl_body_activator(this,UTL_Scope::IK_decls);
	  
	  while (!impl_body_activator.is_done())
	    {
	      AST_Decl *d = impl_body_activator.item();

	      if (d->in_main_file())     // only to take the node issue from the idl file
		{
		  switch(d->node_type())
		    {
		    case AST_Decl::NT_module:
		      {
			adabe_module *module = adabe_module::narrow_from_decl(d);

			// dummy variables that maybe suppressed
			string impl_body_module_previous = "";
			string impl_body_module_body     = "";
			string impl_body_module_with_string;
			dep_list impl_body_module_with;

			// tries to find interface in the module
			module->produce_impl_adb(impl_body_module_with,impl_body_module_body,
						 impl_body_module_previous);
		      }
		      break;
		      
		    case AST_Decl::NT_interface:
		      {
			// Initialisation of the variables used in the interface file
			adabe_interface *interface = adabe_interface::narrow_from_decl(d);
			string impl_body_interface_previous = "";
			string impl_body_interface_body = "";
			string impl_body_interface_with_string;
			dep_list impl_body_interface_with;

			// computing the interface string
			interface->produce_impl_adb(impl_body_interface_with,impl_body_interface_body,
						    impl_body_interface_previous);
			impl_body_interface_with_string = *impl_body_interface_with.produce("with ");

			// computing the file name
			string impl_body_interface_file_name =
			  remove_dot(interface->get_ada_full_name())+"-impl.adb";
			char *lower_case_name = lower(impl_body_interface_file_name.c_str());
			ofstream impl_body_interface_file(lower_case_name); 
			delete[] lower_case_name;

			// writing in the file
			impl_body_interface_file << impl_body_interface_with_string;
			impl_body_interface_file << impl_body_interface_previous;    
			impl_body_interface_file << impl_body_interface_body;
			impl_body_interface_file.close();
		      }
		      
		    default:
		      break;
		    } // end of the switch
		}
	      impl_body_activator.next();
	    } // end of the loop
	}
      }
    
    ////////////////////////////////
    // CREATION OF THE PROXIES FILES
    ////////////////////////////////
    

    ////////////////////////////////////////////////
    // generation of the header of the proxies files
    
    {
      // as for the implementation, proxy files will only
      // be generated for interfaces
      UTL_ScopeActiveIterator proxy_head_activator(this,UTL_Scope::IK_decls);
      while (!proxy_head_activator.is_done())
	{
	  AST_Decl *d = proxy_head_activator.item();

	  if (d->in_main_file())     // only to take the node issue from the idl file
	    {
	      switch(d->node_type())
		{
		  
		case AST_Decl::NT_module:
		  {
		    adabe_module *module = adabe_module::narrow_from_decl(d);

		    // dummy variables that can be suppressed
		    string proxy_header_module_previous = ""; 
		    string proxy_header_module_body     = "";
		    string proxy_header_module_with_string;
		    dep_list proxy_header_module_with;

		    // tries to find interfaces in the module
		    module->produce_proxies_ads(proxy_header_module_with,proxy_header_module_body,
						proxy_header_module_previous);
		  }
		  break;

		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface = adabe_interface::narrow_from_decl(d);

		    // initialisation of the interface variables
		    string proxy_header_interface_previous = "";
		    string proxy_header_interface_body = "";
		    string proxy_header_interface_with_string;
		    dep_list proxy_header_interface_with;

		    // computing the interface mapping string
		    interface->produce_proxies_ads(proxy_header_interface_with,
						   proxy_header_interface_body,
						   proxy_header_interface_previous);
		    proxy_header_interface_with_string = *proxy_header_interface_with.produce("with ");

		    // computing the interface file name
		    string proxy_header_interface_file_name =
		      remove_dot(interface->get_ada_full_name())+"-proxies.ads";
		    char *lower_case_name = lower(proxy_header_interface_file_name.c_str());
		    ofstream proxy_header_interface_file(lower_case_name); 
		    delete[] lower_case_name;

		    // writing in the file
		    proxy_header_interface_file << proxy_header_interface_with_string;
		    proxy_header_interface_file << proxy_header_interface_previous;    
		    proxy_header_interface_file << proxy_header_interface_body;
		    proxy_header_interface_file.close();
		  } 
		  
		default:
		  break;
		  
		} // end of the switch
	    }
	  proxy_head_activator.next();
	} // end of the loop
    }

    //////////////////////////////////////////
    // generation of body of the proxies files
    
    {
      UTL_ScopeActiveIterator proxy_body_activator(this,UTL_Scope::IK_decls);
      
      while (!proxy_body_activator.is_done())
	{
	  AST_Decl *d = proxy_body_activator.item();

	  if (d->in_main_file())     // only to take the node issue from the idl file
	    {
	      switch(d->node_type())
		{
		case AST_Decl::NT_module:
		  {
		    adabe_module *module = adabe_module::narrow_from_decl(d);

		    // dummy variables that can be suppressed
		    string proxy_body_module_previous = "";
		    string proxy_body_module_body     = "";
		    string proxy_body_module_with_string;
		    dep_list proxy_body_module_with;

		    // tries to find interface in the module
		    module->produce_proxies_adb(proxy_body_module_with,proxy_body_module_body,
						proxy_body_module_previous);
		  }
		  break;
		  
		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface = adabe_interface::narrow_from_decl(d);

		    // initialisation of the strings for the interface file
		    string proxy_body_interface_previous = "";
		    string proxy_body_interface_body = "";
		    dep_list proxy_body_interface_with;

		    // computing the interface mapping
		    interface->produce_proxies_adb(proxy_body_interface_with,proxy_body_interface_body,
						   proxy_body_interface_previous);
		    string proxy_body_interface_with_string = *proxy_body_interface_with.produce("with ");
		    // this time, all the included file will have a "use"
		    string proxy_body_interface_use_string = *proxy_body_interface_with.produce("use ");

		    
		    if (proxy_body_interface_body == "")
		      break; // if the file is empty, nothing no file must be produced


		    // else, computing the file name
		    string proxy_body_interface_file_name =
		      remove_dot(interface->get_ada_full_name())+"-proxies.adb";
		    char *lower_case_name = lower(proxy_body_interface_file_name.c_str());
		    ofstream proxy_body_interface_file(lower_case_name); 
		    delete[] lower_case_name;

		    // and writing in the file
		    proxy_body_interface_file << proxy_body_interface_with_string;
		    proxy_body_interface_file << proxy_body_interface_use_string;
		    proxy_body_interface_file << proxy_body_interface_previous;    
		    proxy_body_interface_file << proxy_body_interface_body;
		    proxy_body_interface_file.close();
		  }
		default:
		  break;
		} //end of the switch
	    }
	  proxy_body_activator.next();
	} // end of the loop
    }
    
    /////////////////////////////////
    // CREATION OF THE SKELETON FILES
    /////////////////////////////////
    
    ////////////////////////////////////////////////
    // generation of te header of the skeleton files
    
    {
      // skeleton files are only produced for interfaces
      
      UTL_ScopeActiveIterator skel_head_activator(this,UTL_Scope::IK_decls);
      while (!skel_head_activator.is_done())
	{
	  AST_Decl *d = skel_head_activator.item();

	  if (d->in_main_file()) 
	    {
	      switch(d->node_type())
		{
		  
		case AST_Decl::NT_module: 
		  {
		    adabe_module *module = adabe_module::narrow_from_decl(d);

		    // variables that can be suppressed
		    string skel_header_module_previous = ""; 
		    string skel_header_module_body     = "";
		    string skel_header_module_with_string;
		    dep_list skel_header_module_with;

		    // tries to find interfaces in the module
		    module->produce_skel_ads(skel_header_module_with,skel_header_module_body,
					     skel_header_module_previous);
		  }
		  break;

		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface = adabe_interface::narrow_from_decl(d);

		    // initilisation of the variables used in the interface
		    string skel_header_interface_previous = "";
		    string skel_header_interface_body = "";
		    string skel_header_interface_with_string;
		    dep_list skel_header_interface_with;

		    // computing the mapping of the inetrface
		    interface->produce_skel_ads(skel_header_interface_with,skel_header_interface_body,
						skel_header_interface_previous);
		    skel_header_interface_with_string = *skel_header_interface_with.produce("with ");

		    // computing the file name
		    string skel_header_interface_file_name =
		      remove_dot(interface->get_ada_full_name())+"-skeleton.ads";
		    char *lower_case_name = lower(skel_header_interface_file_name.c_str());
		    ofstream skel_header_interface_file(lower_case_name); 
		    delete[] lower_case_name;

		    // writing in the file
		    skel_header_interface_file << skel_header_interface_with_string;
		    skel_header_interface_file << skel_header_interface_previous;    
		    skel_header_interface_file << skel_header_interface_body;
		    skel_header_interface_file.close();
		  }
		  
		default:
		  break;
		} // end of the switch
	    }

	  skel_head_activator.next();
	} // end of the loop
    }

    /////////////////////////////
    // body of the skeleton files
    
    {
      UTL_ScopeActiveIterator skel_body_activator(this,UTL_Scope::IK_decls);
      
      while (!skel_body_activator.is_done())
	{
	  AST_Decl *d = skel_body_activator.item();

	  if (d->in_main_file())     // only to take the node issue from the idl file
	    {
	      switch(d->node_type())
		{
		case AST_Decl::NT_module:
		  {
		    adabe_module *module = adabe_module::narrow_from_decl(d);

		    // variables that can be suppressed
		    string skel_body_module_previous = "";
		    string skel_body_module_body     = "";
		    string skel_body_module_with_string;
		    dep_list skel_body_module_with;


		    // tries to find interfaces in the module
		    module->produce_skel_adb(skel_body_module_with,skel_body_module_body,
					     skel_body_module_previous);
		  }
		  break;
		  
		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface = adabe_interface::narrow_from_decl(d);

		    // initialisation of the variables of the interface file
		    string skel_body_interface_previous = "";
		    string skel_body_interface_body = "";
		    string skel_body_interface_with_string;
		    string skel_body_interface_use_string;
		    dep_list skel_body_interface_with;

		    // computing the interface mapping
		    interface->produce_skel_adb(skel_body_interface_with,skel_body_interface_body,
						skel_body_interface_previous);
		    skel_body_interface_with_string = *skel_body_interface_with.produce("with ");
		    // all of the file imported have a "use"
		    skel_body_interface_use_string = *skel_body_interface_with.produce("use ");

		    // computing the interface file name
		    string skel_body_interface_file_name =
		      remove_dot(interface->get_ada_full_name())+"-skeleton.adb";
		    char *lower_case_name = lower(skel_body_interface_file_name.c_str());
		    ofstream skel_body_interface_file(lower_case_name); 
		    delete[] lower_case_name;

		    // writing in the file
		    skel_body_interface_file << skel_body_interface_with_string;
		    skel_body_interface_file << skel_body_interface_use_string;
		    skel_body_interface_file << skel_body_interface_previous;    
		    skel_body_interface_file << skel_body_interface_body;
		    skel_body_interface_file.close();
		  }
		  
		default:
		  break;
		} // end of the switch
	    }
	  skel_body_activator.next();
	} // end of the loop
    }
    
    set_undefined();
    // Put the mark in all of the nodes to "undefined"
    // (there where set as "defined" during the production
    // of the main ads
    
    /////////////////////////////////
    // CREATION OF THE MARSHALL FILES
    /////////////////////////////////

    /////////////////////////////////////////
    // generation of the marshall header file

    {
      // initialisation of the root mapping variables
      string marshal_header_includes      = "";
      string marshal_header_previous      = "";
      string marshal_header_body          = "";
      dep_list marshal_header_with;

      bool first = true;
      
      // some files must be added : !!!!!
      marshal_header_previous += "use type CORBA.Unsigned_Long; \n";
      marshal_header_with.add ("AdaBroker.GIOP_C");
      marshal_header_with.add ("CORBA");
      marshal_header_with.add("AdaBroker.NetBufferedStream");
      marshal_header_with.add("AdaBroker.MemBufferedStream");

      marshal_header_previous += "Package " + get_ada_full_name() + ".marshal is\n";
		  
      UTL_ScopeActiveIterator marshal_header_activator(this,UTL_Scope::IK_decls);
      while (!marshal_header_activator.is_done())
	{
	  AST_Decl *d = marshal_header_activator.item();
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
		    // preparing the mapping of the mapping of the node
		    string tmp1 = "";
		    string tmp2 = ""; 
		    // mapping the node
		    dynamic_cast<adabe_name *>(d)->produce_marshal_ads(marshal_header_with, tmp1, tmp2);
		    if (tmp1 != "") first = false;
		    marshal_header_body += tmp1;
		  }
		  break;
		  
		case AST_Decl::NT_module:	    
		  {
		    adabe_module *module = adabe_module::narrow_from_decl(d);

		    // initialisation of the variables for the module
		    string marshal_module_previous = "";
		    string marshal_module_body = "";
		    string marshal_module_with_string;
		    dep_list marshal_module_with;

		    // computing the mapping of the module
		    module->produce_marshal_ads(marshal_module_with,marshal_module_body,
						marshal_module_previous);

		    if (marshal_module_body == "")
		      // if the mapping of the module is empty, nothing must be written
		      break;

		    marshal_module_with_string = *marshal_module_with.produce("with ");

		    // computing the name of the module file
		    string marshal_module_file_name =
		      remove_dot(module->get_ada_full_name())+"-marshal.ads";
		    char *lower_case_name = lower(marshal_module_file_name.c_str());
		    ofstream marshal_module_file(lower_case_name);
		    delete[] lower_case_name;

		    // writing in the module file
		    marshal_module_file << marshal_module_with_string;
		    marshal_module_file << marshal_module_previous; 
		    marshal_module_file << marshal_module_body;
		    marshal_module_file.close();
		  }
		  break;		  
		  
		case AST_Decl::NT_interface:	    
		  {
		    adabe_interface *interface = adabe_interface::narrow_from_decl(d);

		    // initilisation of the variables for the interface
		    string marshal_interface_previous = "";
		    string marshal_interface_body = "";
		    string marshal_interface_with_string;
		    dep_list marshal_interface_with;

		    // mapping the interface
		    interface->produce_marshal_ads(marshal_interface_with,marshal_interface_body,
						   marshal_interface_previous);
		    marshal_interface_with_string = *marshal_interface_with.produce("with ");

		    // computing the interface file name
		    string marshal_interface_file_name =
		      remove_dot(interface->get_ada_full_name())+"-marshal.ads";
		    char *lower_case_name = lower(marshal_interface_file_name.c_str());
		    ofstream marshal_interface_file(lower_case_name); 
		    delete[] lower_case_name;

		    // writing in the main file
		    marshal_interface_file << marshal_interface_with_string;
		    marshal_interface_file << marshal_interface_previous;    
		    marshal_interface_file << marshal_interface_body;
		    marshal_interface_file.close();
		  }
		  
		  break;
		default:
		  break;
		} // end of the switch
	    }
	  marshal_header_activator.next();
	} // end of the loop

      if (!first)
	{

	  // computing the file name
	  string marshal_ada_file_name = idl_file_name+"-marshal.ads";
	  char *lower_case_name = lower(marshal_ada_file_name.c_str());
	  ofstream marshal_header(lower_case_name); 
	  delete[] lower_case_name;
	  
	  // writing in the file
	  marshal_header_includes = *marshal_header_with.produce ("with ");
	  marshal_header << marshal_header_includes;
	  marshal_header << marshal_header_previous;
	  marshal_header << marshal_header_body;
	  marshal_header << "end " << get_ada_full_name() << ".marshal ;" << endl;
	  marshal_header.close();
	}
    }
    
    // once more, all the nodes must be set to "undefined"
    set_undefined();

    //////////////////////////////////
    // generation of marshal body file
    
    {
      UTL_ScopeActiveIterator marshal_body_activator(this,UTL_Scope::IK_decls);

      // initialisation of the variables
      bool   first = true;                    // is it the first node encountered in the scope ?
      string marshal_body_includes      = "";
      string marshal_body_use           = "";
      string marshal_body_previous      = "";
      string marshal_body_body          = "";
      dep_list marshal_body_with;
    
      marshal_body_with.add("AdaBroker.NetBufferedStream");
      marshal_body_with.add ("AdaBroker.MemBufferedStream");
      marshal_body_previous = "Package body " + get_ada_full_name() + ".marshal is \n";

      while (!marshal_body_activator.is_done())
	{
	  AST_Decl *d = marshal_body_activator.item();
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
		    // computing the mapping of the node
		    string tmp1 = "";
		    string tmp2 = ""; 
		    dynamic_cast<adabe_name *>(d)->produce_marshal_adb
		      (marshal_body_with, tmp1, tmp2);
		    if (tmp1 != "") first = false;
		    marshal_body_body += tmp1;
		  }
		  break;
		  
		case AST_Decl::NT_module:
		  {
		    adabe_module *module = adabe_module::narrow_from_decl(d);

		    // variables used for the mapping of the module
		    string marshal_body_module_previous    = "";
		    string marshal_body_module_body        = "";
		    dep_list marshal_body_module_with;

		    // Computing the module mapping
		    module->produce_marshal_adb(marshal_body_module_with,marshal_body_module_body,
						marshal_body_module_previous);
		    string marshal_body_module_with_string = *marshal_body_module_with.produce("with ");
		    string marshal_body_module_use_string = *marshal_body_module_with.produce("use ");
		    
		    if (marshal_body_module_body == "")
		      // if the mapping of the module is empty, nothing must be written
		      break;

		    // comuting the file name
		    string marshal_module_file_name =
		      remove_dot(module->get_ada_full_name())+"-marshal.adb";
		    char *lower_case_name = lower(marshal_module_file_name.c_str());
		    ofstream marshal_module_file(lower_case_name);
		    delete[] lower_case_name;

		    // writing the module to his file 
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

		    // initialisation of the variables for the interface file
		    string marshal_interface_previous = "";
		    string marshal_interface_body = "";
		    dep_list marshal_interface_with;

		    // computing the mapping of the interface
		    interface->produce_marshal_adb(marshal_interface_with,marshal_interface_body,
						   marshal_interface_previous);
		    string marshal_interface_with_string = *marshal_interface_with.produce("with ");
		    string marshal_interface_use_string = *marshal_interface_with.produce("use ");

		    if (marshal_interface_body == "")
		      // if the mapping of the interface is empty, nothing must be written
		      break;
		    
		    // computing the file name
		    string marshal_interface_file_name =
		      remove_dot(interface->get_ada_full_name())+"-marshal.adb";
		    char *lower_case_name = lower(marshal_interface_file_name.c_str());
		    ofstream marshal_interface_file(lower_case_name); 
		    delete[] lower_case_name;

		    // writing in the file
		    marshal_interface_file << marshal_interface_with_string;
		    marshal_interface_file << marshal_interface_use_string;
		    marshal_interface_file << marshal_interface_previous;    
		    marshal_interface_file << marshal_interface_body;
		    marshal_interface_file.close();
		  }
		default:
		  break;
		} // end of the switch
	    }
	  marshal_body_activator.next();
	} // end of the loop

      if (!first)
	{
	  // if a node other than a module or an interface
	  // has been found, creates a file:

	  // computing of the file name
	  string marshal_ada_file_name =
	    remove_dot(idl_file_name)+"-marshal.adb";
	  char *lower_case_name = lower(marshal_ada_file_name.c_str());
	  ofstream marshal_body(lower_case_name); 
	  delete[] lower_case_name;

	  // writing in the file
	  marshal_body_includes = *marshal_body_with.produce ("with ");
	  marshal_body_use = *marshal_body_with.produce ("use ");
	  marshal_body << marshal_body_includes;	  
	  marshal_body << marshal_body_use;
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







