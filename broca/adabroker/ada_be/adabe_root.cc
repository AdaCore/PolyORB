//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.7 $
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

//-------------------------//
//  adabe_root::adabe_root //
//-------------------------//

adabe_root::adabe_root (UTL_ScopedName *n, UTL_StrList *p)
  : AST_Root (n, p),
    AST_Module (n, p),
    AST_Decl (AST_Decl::NT_root, n, p),
    UTL_Scope (AST_Decl::NT_root),
    adabe_name (AST_Decl::NT_root, n, p) 
{
  adabe_global::set_root (this);
  set_in_main_file (I_TRUE);
  return;
}

//---------------------//
// adabe_root::produce //
//---------------------//

void adabe_root::produce () {
  try {    
    // Name of IDL file 
    string name = idl_global->stripped_filename ()->get_string ();
    
    // Remove .idl from filename
    int suffix = name.find (".idl");
    if (suffix > 0) name = name.substr (0, suffix);

    string root_name = name + "_IDL_FILE";

    // Produce root spec.
    {
      string root_withcode = "";
      string root_prologue = "";
      string root_maincode = "";
      dep_list root_withlist;            
      
      root_withlist.add ("CORBA");
      
      UTL_ScopeActiveIterator iterator (this, UTL_Scope::IK_decls);

      root_maincode = "package " + get_ada_full_name ()+" is\n";
      
      // Loop over the scope to find the node to output.
      while (!iterator.is_done ())
	{
	  adabe_global::set_adabe_current_file (this);

	  AST_Decl *d = iterator.item ();

	  //  Just take the node concerning the idl file.
	  if (d->in_main_file ())    
	    {
#ifdef DEBUG_ROOT
	      cout << "In root, node type encountered :" <<  d->node_type () << endl;
#endif 

	      // Ensure that no unexpected node is in the root.
	      switch (d->node_type ()) {
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
		  // For a simple node : it will be put in the body
		  // file, and his local types will be stored in the
		  // prologue string.

		  root_prologue += root_maincode;   
		  root_maincode = "";
		  dynamic_cast<adabe_name *>(d)->produce_ads
		    (root_withlist,
		     root_maincode,
		     root_prologue);
		}
		break;
		
	      case AST_Decl::NT_module:	    
		{
		  adabe_module *module =
		    adabe_module::narrow_from_decl (d);

		  string module_prologue = "";
		  string module_maincode = "";
		  string module_withcode = "";
		  dep_list module_withlist;

		  module->produce_ads 
		    (module_withlist,
		     module_maincode,
		     module_prologue);
		  module_withcode = *module_withlist.produce ("with ");

		  produce_file
		    (module->get_ada_full_name (),
		     is_spec,
		     module_withcode
		     + module_prologue
		     + module_maincode);
		}
		break;

	      case AST_Decl::NT_interface:	    
		{
		  adabe_interface *interface =
		    adabe_interface::narrow_from_decl (d);
		  
		  string interface_prologue = "";
		  string interface_maincode = "";
		  string interface_withcode;
		  dep_list interface_withlist;

		  interface->produce_ads 
		    (interface_withlist,
		     interface_maincode,
		     interface_prologue);
		  interface_withcode = *interface_withlist.produce ("with ");

		  produce_file
		    (interface->get_ada_full_name (),
		     is_spec,
		     interface_withcode
		     + interface_prologue
		     + interface_maincode);
		}
		break;

	      case AST_Decl::NT_enum_val:
		// Enumeration values of an enumeration type can be
		// found in root, but they shall not be mapped here.
		break;
		
	      default:
		// If another node type is found, an exception is
		// raised no further check will be made in root.
#ifdef DEBUG_ROOT
7		cerr << "A node type of the type : ";
                cerr << d->node_type ();
		cerr << " and named : ";
                cerr << <<  idl_global->stripped_filename ()->get_string ();
		cerr << " has been found in the root" << endl;		
#endif
		throw adabe_internal_error
		  (__FILE__,__LINE__,"unexpected contained node in the root");
		break;
	      }
	    
	    }
	  iterator.next (); 
	}

      root_maincode += "end " + get_ada_full_name () + ";\n";

      produce_file
	(root_name,
	 is_spec,
	 root_withcode
	 + root_prologue
	 + root_maincode);
    }
    

    // Produce root body (in fact, produce interfaces).
    {
      UTL_ScopeActiveIterator iterator (this, UTL_Scope::IK_decls);

      // Loop over the scope to find an interface (a module may
      // contain an interface).
      while (!iterator.is_done ())
	{
	  adabe_global::set_adabe_current_file (this);

	  AST_Decl *d = iterator.item ();
	  
	  if (d->in_main_file ())
	    {
	      switch (d->node_type ())
		{
		case AST_Decl::NT_module:
		  {
		    adabe_module *module =
		      adabe_module::narrow_from_decl (d);

		    string module_prologue = "";
		    string module_maincode = "";
		    dep_list module_withlist;
		    
		    module->produce_adb
		      (module_withlist,
		       module_maincode,
		       module_prologue);
		  }
		  break;
		  
		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface =
		      adabe_interface::narrow_from_decl (d);

		    string interface_prologue = "";
		    string interface_maincode = "";
		    string interface_withcode = "";
		    dep_list interface_withlist;

		    interface->produce_adb
		      (interface_withlist,
		       interface_maincode,
		       interface_prologue);
		    interface_withcode = *interface_withlist.produce ("with ");

		    produce_file
		      (interface->get_ada_full_name (),
		       is_body,
		       interface_withcode
		       + interface_prologue
		       + interface_maincode);
		  }
		  break;
		  
		default:
		  // Nothing to be done.
		  break;
		}
	    } 
	  iterator.next ();
	}
    }
    
    // Produce root skeleton spec.
    {
      UTL_ScopeActiveIterator iterator (this, UTL_Scope::IK_decls);

      while (!iterator.is_done ())
	{
	  AST_Decl *d = iterator.item ();
	  
	  if (d->in_main_file ())
	    {
	      switch (d->node_type ())
		{
		case AST_Decl::NT_module:                      
		  {
		    adabe_module *module =
		      adabe_module::narrow_from_decl (d);

		    string module_prologue = ""; 
		    string module_maincode = "";
		    dep_list module_withlist;
		    
		    module->produce_skel_ads
		      (module_withlist,
		       module_maincode,
		       module_prologue);
		  }
		  break;
		  
		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface =
		      adabe_interface::narrow_from_decl (d);
		    
		    string interface_prologue = "";
		    string interface_maincode = "";
		    string interface_withcode = "";
		    dep_list interface_withlist;
		    

		    interface->produce_skel_ads
		      (interface_withlist,
		       interface_maincode,
		       interface_prologue);
		    interface_withcode = *interface_withlist.produce ("with ");
		    
		    produce_file
		      (interface->get_ada_full_name (),
		       is_skel_spec,
		       interface_withcode
		       + interface_prologue
		       + interface_maincode);
		  }
		  break;
		  
		default:
		  break;
		}
	    }
	  iterator.next ();
	}
    }
    
    // Produce skeleton body.
    {
      UTL_ScopeActiveIterator iterator (this, UTL_Scope::IK_decls);
      
      while (!iterator.is_done ())
	{
	  AST_Decl *d = iterator.item ();
	  
	  if (d->in_main_file ())
	    {
	      switch (d->node_type ())
		{
		case AST_Decl::NT_module:
		  {
		    adabe_module *module =
		      adabe_module::narrow_from_decl (d);
		    
		    string module_prologue = "";
		    string module_maincode = "";
		    string module_withcode = "";
		    dep_list module_withlist;
		    
		    module->produce_skel_adb
		      (module_withlist, 
		       module_maincode,
		       module_prologue);
		  }
		  break;
		  
		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface =
		      adabe_interface::narrow_from_decl (d);

		    string interface_prologue = "";
		    string interface_maincode = "";
		    string interface_withcode = "";
		    dep_list interface_withlist;
		    
		    interface->produce_skel_adb
		      (interface_withlist,
		       interface_maincode,
		       interface_prologue);
		    interface_withcode = *interface_withlist.produce ("with ");
		    
		    produce_file
		      (interface->get_ada_full_name (),
		       is_skel_body,
		       interface_withcode
		       + interface_prologue
		       + interface_maincode);
		  }
		  break;
		  
		default:
		  break;
		}
	    }
	  iterator.next ();
	} 
    }
    
    // Mark all nodes to "undefined". There where set to "defined"
    // during the production of the root file.
    set_undefined ();
    
    // Produce root stream spec.
    {
      string root_withcode = "";
      string root_prologue = "";
      string root_maincode = "";
      dep_list root_withlist;

      bool first = true;
      
      root_prologue += "use type CORBA.Unsigned_Long; \n";
      root_withlist.add ("CORBA");

      root_prologue += "package " + get_ada_full_name () + ".Stream is\n";
		  
      UTL_ScopeActiveIterator iterator (this, UTL_Scope::IK_decls);
      while (!iterator.is_done ())
	{
	  AST_Decl *d = iterator.item ();
	  adabe_global::set_adabe_current_file (this);

	  if (d->in_main_file ())
	    {
	      switch (d->node_type ()) 
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
		    string entity_prologue = "";
		    string entity_maincode = ""; 

		    dynamic_cast<adabe_name *>(d)->produce_stream_ads
		      (root_withlist,
		       entity_maincode,
		       entity_prologue);

		    if (entity_maincode != "") first = false;
		    root_maincode += entity_maincode;

		    // WARNING: what are we supposed to do with
		    // entity_prologue.
		  }
		  break;
		  
		case AST_Decl::NT_module:	    
		  {
		    adabe_module *module =
		      adabe_module::narrow_from_decl (d);

		    string module_prologue = "";
		    string module_maincode = "";
		    string module_withcode;
		    dep_list module_withlist;

		    module->produce_stream_ads
		      (module_withlist,
		       module_maincode,
		       module_prologue);

		    if (module_maincode == "") break;

		    module_withcode = *module_withlist.produce ("with ");

		    produce_file
		      (module->get_ada_full_name (),
		       is_stream_spec,
		       module_withcode
		       + module_prologue
		       + module_maincode);
		  }
		  break;		  
		  
		case AST_Decl::NT_interface:	    
		  {
		    adabe_interface *interface =
		      adabe_interface::narrow_from_decl (d);

		    string interface_prologue = "";
		    string interface_maincode = "";
		    string interface_withcode;
		    dep_list interface_withlist;

		    interface->produce_stream_ads
		      (interface_withlist,
		       interface_maincode,
		       interface_prologue);
		    interface_withcode = *interface_withlist.produce ("with ");

		    produce_file 
		      (interface->get_ada_full_name (),
		       is_stream_spec,
		       interface_withcode
		       + interface_prologue
		       + interface_maincode);
		  }
		  break;

		default:
		  break;
		}
	    }
	  iterator.next ();
	}

      if (!first)
	{
	  root_withcode = *root_withlist.produce ("with ");
	  root_maincode = "end " + get_ada_full_name () + ".Stream;";

	  produce_file
	    (root_name,
	     is_stream_spec,
	     root_withcode
	     + root_prologue
	     + root_maincode);
	}
    }
    
    set_undefined ();

    // Produce root stream body.
    {
      UTL_ScopeActiveIterator iterator (this, UTL_Scope::IK_decls);

      bool   first = true;

      string root_withcode = "";
      string root_usecode  = "";
      string root_prologue = "";
      string root_maincode = "";
      dep_list root_withlist;
    
      root_prologue = "package body " + get_ada_full_name () + ".Stream is \n";

      while (!iterator.is_done ())
	{
	  AST_Decl *d = iterator.item ();

	  adabe_global::set_adabe_current_file (this);

	  if (d->in_main_file ())
	    {
	      switch (d->node_type ())
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
		    string entity_maincode = "";
		    string entity_prologue = ""; 

		    dynamic_cast<adabe_name *>(d)->produce_stream_adb
		      (root_withlist,
		       entity_maincode,
		       entity_prologue);

		    if (entity_maincode != "") first = false;
		    root_maincode += entity_maincode;
		  }
		  break;
		  
		case AST_Decl::NT_module:
		  {
		    adabe_module *module =
		      adabe_module::narrow_from_decl (d);

		    string module_prologue = "";
		    string module_maincode = "";
		    string module_withcode = "";
		    string module_usecode  = "";
		    dep_list module_withlist;

		    module->produce_stream_adb
		      (module_withlist,
		       module_maincode,
		       module_prologue);

		    if ((module_maincode == "") &&
			(module_prologue == "")) break;

		    module_withcode = *module_withlist.produce ("with ");
		    module_usecode  = *module_withlist.produce ("use ");
		    
		    produce_file
		      (module->get_ada_full_name (),
		       is_stream_body,
		       module_withcode
		       + module_usecode
		       + module_prologue
		       + module_maincode);
		  }
		  break;
		  
		case AST_Decl::NT_interface:
		  {
		    adabe_interface *interface =
		      adabe_interface::narrow_from_decl (d);

		    string interface_prologue = "";
		    string interface_maincode = "";
		    string interface_withcode = "";
		    string interface_usecode  = "";
		    dep_list interface_withlist;

		    interface_withlist.add ("Broca.Marshalling");
		    interface_withlist.add ("Broca.Refs");

		    interface->produce_stream_adb 
		      (interface_withlist,
		       interface_maincode,
		       interface_prologue);
		    interface_withcode = *interface_withlist.produce ("with ");
		    interface_usecode  = *interface_withlist.produce ("use ");

		    if ((interface_maincode == "") &&
			(interface_prologue == "")) break;
		    
		    produce_file 
		      (interface->get_ada_full_name (),
		       is_stream_body,
		       interface_withcode
		       + interface_usecode 
		       + interface_prologue
		       + interface_maincode);
		  }
		  break;

		default:
		  break;
		}
	    }
	  iterator.next ();
	}

      if (!first)
	{
	  // If a node other than a module or an interface has been
	  // found, create file.

	  root_withcode = *root_withlist.produce ("with ");
	  root_usecode  = *root_withlist.produce ("use ");
	  
	  root_maincode += "end " + get_ada_full_name () + ".Stream;\n";

	  produce_file
	    (root_name,
	     is_stream_body,
	     root_withcode
	     + root_usecode
	     + root_prologue
	     + root_maincode);
	}
    }

  }
  catch (adabe_internal_error &e)
    {
      cout << "in : "
	   << e.file ()
	   << "   Line : "
	   << e.line ()
	   << endl
	   << e.errmsg ()
	   << endl;
    };
}

IMPL_NARROW_METHODS1 (adabe_root, AST_Root)
IMPL_NARROW_FROM_DECL (adabe_root)
IMPL_NARROW_FROM_SCOPE (adabe_root)







