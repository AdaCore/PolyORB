//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.8 $
//                                                                          //
//         Copyright (C) 1999-2000 ENST Paris University, France.           //
//                                                                          //
// AdaBroker is free software; you  can  redistribute  it and/or modify it  //
// under terms of the  GNU General Public License as published by the  Free //
// Software Foundation;  either version 2,  or (at your option)  any  later //
// version. AdaBroker  is distributed  in the hope that it will be  useful, //
// but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- //
// TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public //
// License  for more details.  You should have received  a copy of the GNU  //
// General Public License distributed with AdaBroker; see file COPYING. If  //
// not, write to the Free Software Foundation, 59 Temple Place - Suite 330, //
// Boston, MA 02111-1307, USA.                                              //
//                                                                          //
// As a special exception,  if other files  instantiate  generics from this //
// unit, or you link  this unit with other files  to produce an executable, //
// this  unit  does not  by itself cause  the resulting  executable  to  be //
// covered  by the  GNU  General  Public  License.  This exception does not //
// however invalidate  any other reasons why  the executable file  might be //
// covered by the  GNU Public License.                                      //
//                                                                          //
//             AdaBroker is maintained by ENST Paris University.            //
//                     (email: broker@inf.enst.fr)                          //
//                                                                          //
//--------------------------------------------------------------------------//
#include <adabe.h>
 
static string remove_dot (string  name)
{
  char c;
  while ((c = name.find (".")) != -1) 
    name[c]='-';
  return name;

}

adabe_module::adabe_module (UTL_ScopedName *n,
			    UTL_StrList    *p)
  : AST_Decl (AST_Decl::NT_module, n, p),
    UTL_Scope (AST_Decl::NT_module),
    adabe_name (AST_Decl::NT_module, n, p) 
{
}
void
adabe_module::produce_ads (dep_list & with,
			   string   & body,
			   string   & previousdefinition)
{
  
  // before doing anything compute the ada name
  
  compute_ada_name (); 
  // with.add ("Ada.Unchecked_Deallocation");
  with.add ("CORBA");
  with.add ("Broca");
  body = "package " + get_ada_full_name ()+ " is\n";
  
  // For each declaration in the node produce the code
  
  UTL_ScopeActiveIterator module_activator (this, UTL_Scope::IK_decls);
  while (!module_activator.is_done ())
    {
      AST_Decl *d = module_activator.item ();
      module_activator.next ();
      adabe_global::set_adabe_current_file (this);
      switch (d->node_type ())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_pre_defined:
	case AST_Decl::NT_const:
	case AST_Decl::NT_except:
	case AST_Decl::NT_union:
	case AST_Decl::NT_struct:
	case AST_Decl::NT_enum:
	case AST_Decl::NT_typedef:
	  previousdefinition += body;
	  body ="";
	case AST_Decl::NT_interface_fwd:
	  dynamic_cast<adabe_name *>(d)->produce_ads
	    (with, body, previousdefinition);
	  break;
	case AST_Decl::NT_module:
	  {
	    adabe_module *module = adabe_module::narrow_from_decl (d);
	    string module_previous = "";
	    string module_body = "";
	    string module_with_string;
	    dep_list module_with;
	    
	    module->produce_ads (module_with, module_body, module_previous);
	    module_with_string = *module_with.produce ("with ");
	    
	    string module_file_name =
	      remove_dot (module->get_ada_full_name ()) + ".ads";
	    char *lower_case_name = lower (module_file_name.c_str ());
	    ofstream module_file (lower_case_name); 
	    delete[] lower_case_name;
	    module_file << module_with_string;
	    module_file << module_previous;
	    module_file << module_body;
	    module_file.close ();
	  }
	  break;
	  
	case AST_Decl::NT_interface:
	  {
	    adabe_interface *interface = adabe_interface::narrow_from_decl (d);
	    string interface_previous = "";
	    string interface_body = "";
	    string interface_with_string;
	    dep_list interface_with;

	    interface->produce_ads
	      (interface_with, interface_body, interface_previous);
	    interface_with_string = *interface_with.produce ("with ");
	    
	    string interface_file_name =
	      remove_dot (interface->get_ada_full_name ())+ ".ads";
	    char *lower_case_name = lower (interface_file_name.c_str ());
	    ofstream interface_file (lower_case_name); 
	    delete[] lower_case_name;
	    interface_file << interface_with_string;
	    interface_file << interface_previous;       
	    interface_file << interface_body;
	    interface_file.close ();
	  }
	  break;
	case AST_Decl::NT_enum_val:
	  break;

	default:
	  throw adabe_internal_error 
	    (__FILE__,__LINE__,"unexpected contening scope");
	  break;
	}
    }
  body += "end " + get_ada_full_name () + ";";

}

void
adabe_module::produce_adb (dep_list & with,
			   string   & body,
			   string   & previousdefinition)
  // does nothing except lauching produce adb for the interfaces
{
   UTL_ScopeActiveIterator module_activator (this, UTL_Scope::IK_decls);
  while (!module_activator.is_done ())
    {
      AST_Decl *d = module_activator.item ();
      module_activator.next ();
      adabe_global::set_adabe_current_file (this);
      switch (d->node_type ())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_interface_fwd:
	case AST_Decl::NT_pre_defined:
	case AST_Decl::NT_const:
	case AST_Decl::NT_except:
	case AST_Decl::NT_union:
	case AST_Decl::NT_struct:
	case AST_Decl::NT_enum:
	case AST_Decl::NT_typedef:
	  break;
	    
	case AST_Decl::NT_module:
	  {
	    adabe_module *module = adabe_module::narrow_from_decl (d);
	    string module_previous = "";
	    string module_body = "";
	    dep_list module_with;
	    
	    module->produce_adb (module_with, module_body, module_previous);
	  }
	  break;
	  
	case AST_Decl::NT_interface:
	  {
	    adabe_interface *interface = adabe_interface::narrow_from_decl (d);
	    string interface_previous = "";
	    string interface_body = "";
	    string interface_with_string;
	    string interface_use_string;
	    dep_list interface_with;

	    interface->produce_adb
	      (interface_with, interface_body, interface_previous);
	    interface_with_string = *interface_with.produce ("with ");
	    interface_use_string = *interface_with.produce ("use ");
	    
	    string interface_file_name =
	      remove_dot (interface->get_ada_full_name ()) + ".adb";
	    char *lower_case_name = lower (interface_file_name.c_str ());
	    ofstream interface_file (lower_case_name); 
	    delete[] lower_case_name;
	    interface_file << interface_with_string;
	    interface_file << interface_use_string;
	    interface_file << interface_previous;       
	    interface_file << interface_body;
	    interface_file.close ();
	  }
	  
	  break;
	default:
	  break;
	}
    }
}

void
adabe_module::produce_skel_ads (dep_list & with,
				string   & body,
				string   & previousdefinition)
  // does nothing except lauching produce imp_ads for the interfaces
{
  UTL_ScopeActiveIterator module_activator (this, UTL_Scope::IK_decls);
  while (!module_activator.is_done ())
    {
      AST_Decl *d = module_activator.item ();
      module_activator.next ();
      adabe_global::set_adabe_current_file (this);
      switch (d->node_type ())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_interface_fwd:
	case AST_Decl::NT_pre_defined:
	case AST_Decl::NT_const:
	case AST_Decl::NT_except:
	case AST_Decl::NT_union:
	case AST_Decl::NT_struct:
	case AST_Decl::NT_enum:
	case AST_Decl::NT_typedef:
	  break;
	case AST_Decl::NT_module:
	  {
	    adabe_module *module = adabe_module::narrow_from_decl (d);
	    string module_previous = "";
	    string module_body = "";
	    dep_list module_with;
	    
	    module->produce_skel_ads
	      (module_with, module_body, module_previous);
	  }
	  break;
	  
	case AST_Decl::NT_interface:
	  {
	    adabe_interface *interface = adabe_interface::narrow_from_decl (d);
	    string interface_previous = "";
	    string interface_body = "";
	    string interface_with_string;
	    dep_list interface_with;

	    interface->produce_skel_ads
	      (interface_with, interface_body, interface_previous);
	    interface_with_string = *interface_with.produce ("with ");
	    
	    string interface_file_name =
	      remove_dot (interface->get_ada_full_name ()) + "-skel.ads";
	    char *lower_case_name = lower (interface_file_name.c_str ());
	    ofstream interface_file (lower_case_name); 
	    delete[] lower_case_name;
	    interface_file << interface_with_string;
	    interface_file << interface_previous;       
	    interface_file << interface_body;
	    interface_file.close ();
	  }
	  
	  break;
	default:
	  break;
	}
    }
}
void
adabe_module::produce_skel_adb (dep_list & with,
				string   & body,
				string   & previousdefinition)
  // does nothing except lauching produce adb for the interfaces
{
   UTL_ScopeActiveIterator module_activator (this, UTL_Scope::IK_decls);
  while (!module_activator.is_done ())
    {
      AST_Decl *d = module_activator.item ();
      module_activator.next ();
      adabe_global::set_adabe_current_file (this);
      switch (d->node_type ())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_interface_fwd:
	case AST_Decl::NT_pre_defined:
	case AST_Decl::NT_const:
	case AST_Decl::NT_except:
	case AST_Decl::NT_union:
	case AST_Decl::NT_struct:
	case AST_Decl::NT_enum:
	case AST_Decl::NT_typedef:
	  break;
	case AST_Decl::NT_module:
	  {
	    adabe_module *module = adabe_module::narrow_from_decl (d);
	    string module_previous = "";
	    string module_body = "";
	    dep_list module_with;
	    
	    module->produce_skel_adb
	      (module_with, module_body, module_previous);
	  }
	  break;
	  
	case AST_Decl::NT_interface:
	  {
	    adabe_interface *interface = adabe_interface::narrow_from_decl (d);
	    string interface_previous = "";
	    string interface_body = "";
	    string interface_with_string;
	    string interface_use_string;
	    dep_list interface_with;

	    interface->produce_skel_adb
	      (interface_with, interface_body, interface_previous);
	    interface_with_string = *interface_with.produce ("with ");
	    interface_use_string = *interface_with.produce ("use ");
	    
	    string interface_file_name =
	      remove_dot (interface->get_ada_full_name ()) + "-skel.adb";
	    char *lower_case_name = lower (interface_file_name.c_str ());
	    ofstream interface_file (lower_case_name); 
	    delete[] lower_case_name;
	    interface_file << interface_with_string;
	    interface_file << interface_use_string;
	    interface_file << interface_previous;       
	    interface_file << interface_body;
	    interface_file.close ();
	  }
	  
	  break;
	default:
	  break;
	}
    }
}

void
adabe_module::produce_stream_ads (dep_list & with,
				   string   & body,
				   string   & previousdefinition)
{
  bool first = true;
  body += "use type CORBA.Unsigned_Long; \n";
  with.add ("CORBA");
  with.add ("Broca.Types"); // XXX Needed!

  body += "package " + get_ada_full_name () + ".Stream is\n";
  
  // For each declaration in the node produce the code
  
  UTL_ScopeActiveIterator module_activator (this, UTL_Scope::IK_decls);
  while (!module_activator.is_done ())
    {
      AST_Decl *d = module_activator.item ();
      module_activator.next ();
      adabe_global::set_adabe_current_file (this);
      switch (d->node_type ())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_except:
	case AST_Decl::NT_union:
	case AST_Decl::NT_struct:
	case AST_Decl::NT_enum:
	case AST_Decl::NT_typedef:
	case AST_Decl::NT_string:
	  {
	    string tmp1 = "";
	    string tmp2 = "";
	    dynamic_cast<adabe_name *>(d)->produce_stream_ads
	      (with, tmp1, tmp2);
	    if (tmp1 != "") first = false;
	    body += tmp1;
	    break;
	  }
	case AST_Decl::NT_module:
	  {
	    adabe_module *module = adabe_module::narrow_from_decl (d);
	    string module_previous = "";
	    string module_body = "";
	    string module_with_string;
	    dep_list module_with;
	    
	    module->produce_stream_ads 
	      (module_with, module_body, module_previous);
	    module_with_string = *module_with.produce ("with ");
	    
	    string module_file_name =
	      remove_dot (module->get_ada_full_name ()) + "-stream.ads";
	    char *lower_case_name = lower (module_file_name.c_str ());
	    ofstream module_file (lower_case_name); 
	    delete[] lower_case_name;
	    module_file << module_with_string;
	    module_file << module_previous;
	    module_file << module_body;
	    module_file.close ();
	  }
	  break;
	  
	case AST_Decl::NT_interface:
	  {
	    adabe_interface *interface = adabe_interface::narrow_from_decl (d);
	    string interface_previous = "";
	    string interface_body = "";
	    string interface_with_string;
	    dep_list interface_with;

	    interface->produce_stream_ads
	      (interface_with, interface_body, interface_previous);
	    interface_with_string = *interface_with.produce ("with ");
	    
	    string interface_file_name =
	      remove_dot (interface->get_ada_full_name ()) + "-stream.ads";
	    char *lower_case_name = lower (interface_file_name.c_str ());
	    ofstream interface_file (lower_case_name); 
	    delete[] lower_case_name;
	    interface_file << interface_with_string;
	    interface_file << interface_previous;       
	    interface_file << interface_body;
	    interface_file.close ();
	  }
	  break;

	default:
	  break;
	}
    }
  if (!first)
    body += "end " + get_ada_full_name () + ".Stream;";
  else body = "";

}

void
adabe_module::produce_stream_adb (dep_list & with,
				   string   & body,
				   string   & previousdefinition)
  // does nothing except lauching produce adb for the interfaces
{

  bool first = true;

  UTL_ScopeActiveIterator module_activator (this, UTL_Scope::IK_decls);
  with.add ("Broca.Marshalling");
  with.add ("Broca.Marshalling.Refs");

  body += "package body " + get_ada_full_name () + ".Stream is\n";
  while (!module_activator.is_done ())
    {
      AST_Decl *d = module_activator.item ();
      adabe_global::set_adabe_current_file (this);
      module_activator.next ();
      switch (d->node_type ())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_except:
	case AST_Decl::NT_union:
	case AST_Decl::NT_struct:
	case AST_Decl::NT_enum:
	case AST_Decl::NT_typedef:
	case AST_Decl::NT_string:
	  {
	    string tmp1 = "";
	    string tmp2 = "";
	    dynamic_cast<adabe_name *>(d)->produce_stream_adb 
	      (with, tmp1, tmp2);
	    if (tmp1 != "") first = false;
	    body += tmp1;
	    break;
	  }
	case AST_Decl::NT_module:
	  {
	    adabe_module *module = adabe_module::narrow_from_decl (d);
	    string module_previous     = "";
	    string module_body         = "";
	    dep_list module_with;
	    
	    module->produce_stream_adb
	      (module_with, module_body, module_previous);
	    string module_with_string = *module_with.produce ("with ");
	    string module_use_string = *module_with.produce ("use ");

	    if (module_body != "")
	      {
		string module_file_name =
		  remove_dot (module->get_ada_full_name ()) + "-stream.adb";
		char *lower_case_name = lower (module_file_name.c_str ());
		ofstream module_file (lower_case_name); 
		delete[] lower_case_name;
		module_file << module_with_string;
		module_file << module_use_string;
		module_file << module_previous;
		module_file << module_body;
		module_file.close ();
	      }
	  }
	  break;
	  
	case AST_Decl::NT_interface:
	  {
	    adabe_interface *interface = adabe_interface::narrow_from_decl (d);
	    string interface_previous = "";
	    string interface_body = "";
	    dep_list interface_with;

	    interface->produce_stream_adb
	      (interface_with, interface_body, interface_previous);
	    string interface_with_string = *interface_with.produce ("with ");
	    string interface_use_string = *interface_with.produce ("use ");
	    
	    if (interface_body == "") break;
	    string interface_file_name =
	      remove_dot (interface->get_ada_full_name ()) +"-stream.adb";
	    char *lower_case_name = lower (interface_file_name.c_str ());
	    ofstream interface_file (lower_case_name); 
	    delete[] lower_case_name;
	    interface_file << interface_with_string;
	    interface_file << interface_use_string;
	    interface_file << interface_previous;       
	    interface_file << interface_body;
	    interface_file.close ();
	  }
	  
	  break;
	default:
	  break;
	}
    }
  if (!first) {
    body += "end " + get_ada_full_name () + ".Stream;";
  }
  else body = "";
}

IMPL_NARROW_METHODS3 (adabe_module, AST_Module, adabe_name, UTL_Scope);
IMPL_NARROW_FROM_DECL (adabe_module);
IMPL_NARROW_FROM_SCOPE (adabe_module);

