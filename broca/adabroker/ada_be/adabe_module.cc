//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.14 $
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

//-----------------------------//
//  adabe_module::adabe_module //
//-----------------------------//

adabe_module::adabe_module (UTL_ScopedName *n,
			    UTL_StrList    *p)
  : AST_Decl (AST_Decl::NT_module, n, p),
    UTL_Scope (AST_Decl::NT_module),
    adabe_name (AST_Decl::NT_module, n, p) 
{
}

//---------------------------//
// adabe_module::produce_ads //
//---------------------------//

void
adabe_module::produce_ads (dep_list & withlist,
			   string   & maincode,
			   string   & prologue)
{
  compute_ada_name (); 
  D (D_MODULE, "produce module spec for " + get_ada_full_name ());

  withlist.add ("CORBA");
  withlist.add ("Broca");
  maincode = "package " + get_ada_full_name ()+ " is\n";
  
  // For each declaration, produce spec.
  UTL_ScopeActiveIterator iterator (this, UTL_Scope::IK_decls);

  while (!iterator.is_done ())
    {
      adabe_global::set_adabe_current_file (this);

      AST_Decl *d = iterator.item ();

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
	  prologue += maincode;
	  maincode = "";
	  // No break (sure ???)

	case AST_Decl::NT_interface_fwd:
	  dynamic_cast<adabe_name *>(d)->produce_ads
	    (withlist,
	     maincode,
	     prologue);
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
	    string interface_withcode = "";
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
	  break;

	default:
	  throw adabe_internal_error 
	    (__FILE__,__LINE__,"unexpected contening scope");
	  break;
	}
      iterator.next ();
    }

  maincode += "end " + get_ada_full_name () + ";";
}

//---------------------------//
// adabe_module::produce_adb //
//---------------------------//

void
adabe_module::produce_adb (dep_list & withlist,
			   string   & maincode,
			   string   & prologue)
{
  compute_ada_name ();
  D (D_MODULE, "produce module body for " + get_ada_full_name ());
  
  UTL_ScopeActiveIterator iterator (this, UTL_Scope::IK_decls);
  
  while (!iterator.is_done ())
    {
      adabe_global::set_adabe_current_file (this);
      
      AST_Decl *d = iterator.item ();
      
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
	    string interface_usecode  = "";
	    dep_list interface_withlist;

	    interface->produce_adb
	      (interface_withlist,
	       interface_maincode,
	       interface_prologue);
	    interface_withcode = *interface_withlist.produce ("with ");
	    interface_usecode  = *interface_withlist.produce ("use ");
	    
	    produce_file
	      (interface->get_ada_full_name (),
	       is_body,
	       interface_withcode
	       + interface_usecode
	       + interface_prologue
	       + interface_maincode);
	  }
	  break;

	default:
	  break;
	}
       iterator.next ();
    }
}

//--------------------------------//
// adabe_module::produce_skel_ads //
//--------------------------------//

void
adabe_module::produce_skel_ads (dep_list & withlist,
				string   & maincode,
				string   & prologuedefinition)
{
  compute_ada_name ();
  D (D_MODULE, "produce module skel spec for " + get_ada_full_name ());
  
  UTL_ScopeActiveIterator iterator (this, UTL_Scope::IK_decls);

  while (!iterator.is_done ())
    {
      AST_Decl *d = iterator.item ();
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
      iterator.next ();
    }
}

//--------------------------------//
// adabe_module::produce_skel_adb //
//--------------------------------//

void
adabe_module::produce_skel_adb (dep_list & withlist,
				string   & maincode,
				string   & prologue)
{
  compute_ada_name ();
  D (D_MODULE, "produce module skel body for " + get_ada_full_name ());
  
   UTL_ScopeActiveIterator iterator (this, UTL_Scope::IK_decls);

   while (!iterator.is_done ())
     {
       adabe_global::set_adabe_current_file (this);

       AST_Decl *d = iterator.item ();
       
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
	     adabe_module *module =
	       adabe_module::narrow_from_decl (d);

	     string module_prologue = "";
	     string module_maincode = "";
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
	     string interface_usecode  = "";
	     dep_list interface_withlist;
	     
	     interface->produce_skel_adb
	       (interface_withlist,
		interface_maincode,
		interface_prologue);
	     interface_withcode = *interface_withlist.produce ("with ");
	     interface_usecode = *interface_withlist.produce ("use ");

	     produce_file
	       (interface->get_ada_full_name (),
		is_skel_body,
		interface_withcode
		+ interface_usecode
		+ interface_prologue
		+ interface_maincode);
	   }
	   break;

	 default:
	   break;
	 }
       iterator.next ();
     }
}

//----------------------------------//
// adabe_module::produce_stream_ads //
//----------------------------------//

void
adabe_module::produce_stream_ads (dep_list & withlist,
				  string   & maincode,
				  string   & prologue)
{
  compute_ada_name ();
  D (D_MODULE, "produce module stream spec for " + get_ada_full_name ());
  
  // bool empty = true;
  // Produce stream spec anyway.

  maincode += "use type CORBA.Unsigned_Long; \n";

  withlist.add ("CORBA");
  withlist.add ("Broca.Buffers");

  maincode += "package " + get_ada_full_name () + ".Stream is\n";
  
  UTL_ScopeActiveIterator iterator (this, UTL_Scope::IK_decls);

  while (!iterator.is_done ())
    {
      adabe_global::set_adabe_current_file (this);

      AST_Decl *d = iterator.item ();
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
	    string entity_maincode = "";
	    string entity_prologue = "";

	    dynamic_cast<adabe_name *>(d)->produce_stream_ads
	      (withlist,
	       entity_maincode,
	       entity_prologue);

	    // if (entity_maincode != "") first = false;
	    // Produce stream spec anyway.

	    maincode += entity_maincode;
	    // And what do we do with entity_prologue ???
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
	    
	    module->produce_stream_ads 
	      (module_withlist,
	       module_maincode,
	       module_prologue);
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
	    string interface_withcode = "";
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
      iterator.next ();
    }

  // if (!first) maincode += "end " + get_ada_full_name () + ".Stream;";
  // else maincode = "";
  maincode += "end " + get_ada_full_name () + ".Stream;";
}

//----------------------------------//
// adabe_module::produce_stream_adb //
//----------------------------------//

void
adabe_module::produce_stream_adb (dep_list & withlist,
				  string   & maincode,
				  string   & prologue)
{
  compute_ada_name ();
  D (D_MODULE, "produce module stream body for " + get_ada_full_name ());
  
  bool empty = true;

  UTL_ScopeActiveIterator iterator (this, UTL_Scope::IK_decls);

  withlist.add ("Broca.Marshalling");

  maincode += "package body " + get_ada_full_name () + ".Stream is\n";

  while (!iterator.is_done ())
    {
      adabe_global::set_adabe_current_file (this);

      AST_Decl *d = iterator.item ();

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
	    string entity_maincode = "";
	    string entity_prologue = "";

	    dynamic_cast<adabe_name *>(d)->produce_stream_adb 
	      (withlist,
	       entity_maincode,
	       entity_prologue);

	    if (entity_maincode != "") empty = false;
	    maincode += entity_maincode;
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
	    module_withcode = *module_withlist.produce ("with ");
	    module_usecode  = *module_withlist.produce ("use ");

	    if (module_maincode == "") break;

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

	    interface->produce_stream_adb
	      (interface_withlist,
	       interface_maincode,
	       interface_prologue);
	    interface_withcode = *interface_withlist.produce ("with ");
	    interface_usecode  = *interface_withlist.produce ("use ");
	    
	    if (interface_maincode == "") break;

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
      iterator.next ();
    }

  if (!empty) maincode += "end " + get_ada_full_name () + ".Stream;";
  else maincode = "";
}

IMPL_NARROW_METHODS3 (adabe_module, AST_Module, adabe_name, UTL_Scope);
IMPL_NARROW_FROM_DECL (adabe_module);
IMPL_NARROW_FROM_SCOPE (adabe_module);

