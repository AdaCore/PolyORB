//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.1 $
//                                                                          //
//         Copyright (C) 1999 ENST Paris University, France.                //
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

////////////////////////////////////////////////////////////////////////
////////////////      constructor    ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
adabe_interface::adabe_interface (UTL_ScopedName *n,
				  AST_Interface **ih,
				  long nih,
				  UTL_StrList *p)
  : AST_Interface (n, ih, nih, p),
    AST_Decl (AST_Decl::NT_interface, n, p),
    UTL_Scope (AST_Decl::NT_interface),
    adabe_name (AST_Decl::NT_interface, n, p)
{
  if (nih == -1) pd_is_forwarded = true;
  else pd_is_forwarded = false; 
  
  // Test local name. For Object, this is a CORBA reserved word.
  if ((string) local_name ()->get_string () == "Object") 
    {
      set_ada_local_name ("Object");
      set_ada_full_name ("CORBA.Object");
    }
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_ads     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////


void
adabe_interface::produce_ads (dep_list &with,
			      string &body,
			      string &previous)
  // This method produces the adb file for a given interface
{
  // Some useful variables
  adabe_interface *parent; // Direct parent of current interface
  string parent_name = ""; // Name of this parent
  string tmp = "";         // Temporary string

  // Preserve current working file in order to known what to import.
  adabe_global::set_adabe_current_file (this);
  
#ifdef DEBUG_INTERFACE
  cout << "beginning of produce_ads of the interface" << endl;
#endif

  // Add some with declarations for useful files.
  with.add ("CORBA");
  with.add ("CORBA.Object");
  
#ifdef DEBUG_INTERFACE
  cout << "before compute_ada_name of the interface" << endl;
#endif

  // Compute node adaname if we do not have a forward declaration.
  if (!pd_is_forwarded) compute_ada_name ();
  
#ifdef DEBUG_INTERFACE
  cout << "after compute_ada_name of the interface" << endl;
#endif

  // This is for To_CORBA_String.
  body += "pragma Elaborate_All (CORBA);\n\n";

  // Header of the package.
  body += "package " + get_ada_full_name () + " is\n\n";

  // First part of the ads file : the CORBA specification

  // If the package has no parent, Ref inherits directly from
  // CORBA.Object.Ref.
  if (n_inherits () == 0) 
    {
      parent_name = "CORBA.Object";
      body += "   type Ref is new CORBA.Object.Ref with null record;\n\n";
    }

  // If the package has parent(s).
  if (n_inherits () > 0)
    {
      // Find the direct parent of this interface. The direct parent
      // is simply the first one in the list of all parents.
      parent = adabe_interface::narrow_from_decl (inherits ()[0]);

      // Add this parent to the with list.
      parent_name = parent->get_ada_full_name ();
      with.add (parent_name);

      // Define type Ref as child of the parent Ref type.
      body += "   type Ref is new ";
      body += parent_name;
      body += ".Ref with null record;\n\n";
      
      {
	// Loop over constants, exceptions and types in the parent
	// interface.
	UTL_ScopeActiveIterator j (parent, UTL_Scope::IK_decls);
	while (!j.is_done ())
	  {
	    // Get the current constant, exception or type as
	    // adabe_name object.
	    AST_Decl *d = j.item ();
	    adabe_name *e = dynamic_cast<adabe_name *>(d);

	    // Declare it as a subtype in this package.
	    switch (d->node_type ())
	      {
	      case AST_Decl::NT_const:
	      case AST_Decl::NT_enum:
	      case AST_Decl::NT_except:
	      case AST_Decl::NT_struct:
	      case AST_Decl::NT_typedef:
	      case AST_Decl::NT_union:
	      case AST_Decl::NT_sequence:
	      case AST_Decl::NT_string:
	      case AST_Decl::NT_array:	    
		tmp += "   subtype " +  e->get_ada_local_name ();
		tmp += " is " + e->get_ada_full_name () + ";\n";	      
		break;
	      default:break;
	      }
	    // Get next element.
	    j.next ();
	  }
      }

      // In case of multiple inheritance, we must redefined all
      // functions and procedures of the second and subsequent parents
      // since these are not directly inherited.
      if (n_inherits () > 1)
	{
	  // Loop over all parents from the second one.
	  for (int i = 1; i < n_inherits (); i++)
	    {
	      // Get a parent as adabe_interface object.
	      parent = adabe_interface::narrow_from_decl (inherits ()[i]);

	      // Compute its name.
	      parent_name = parent->get_ada_full_name ();
	      
	      // Add it to the with list.
	      with.add (parent_name);

	      {
		// Loop over all declarations of the current parent.
		UTL_ScopeActiveIterator j (parent, UTL_Scope::IK_decls);
		while (!j.is_done ())
		  {
		    // Get the current declaration as an adabe_name object.
		    AST_Decl *d = j.item ();
		    adabe_name *e = dynamic_cast<adabe_name *>(d);

		    switch (d->node_type ())
		      {
			// declare parent types as a subtype in this package
			// according to the mapping Idl -> Ada
		      case AST_Decl::NT_const:
		      case AST_Decl::NT_enum:
		      case AST_Decl::NT_except:
		      case AST_Decl::NT_struct:
		      case AST_Decl::NT_typedef:
		      case AST_Decl::NT_union:
		      case AST_Decl::NT_sequence:
		      case AST_Decl::NT_string:
		      case AST_Decl::NT_array:			
			tmp += "   subtype " +  e->get_ada_local_name ();
			tmp += " is " + e->get_ada_full_name () + ";\n";
			break;

			// Redefine all functions and procedures.
		      case AST_Decl::NT_op:
		      case AST_Decl::NT_attr:
			{
			  string tempo1 = "";
			  string tempo2 = "";
			  e->produce_ads (with, tempo1, tempo2);
			  tmp += tempo2 + tempo1;
			}
			break;
		      default:break;
		      }

		    // Get next element.
		    j.next ();
		  }
		tmp += "\n\n";
	      }
	    }
	}
    }

  // Cast parent. Allows to cast a Ref Object into one of its
  // ancestors or children (if possible).
  body += "   function Unchecked_To_Ref\n";
  body += "     (Self : in CORBA.Object.Ref'Class)\n";
  body += "      return Ref;\n\n";
  body += "   function To_Ref\n";
  body += "     (Self : in CORBA.Object.Ref'Class)\n";
  body += "      return Ref;\n\n";

#if 0 // Any not yet implemented.
  // Extract a reference from Any.
  body += "   function From_Any\n";
  body += "     (From : in CORBA.Any)\n";
  body += "      return Ref;\n\n";
#endif 
  
  // Add all multiple inheritance declarations to the body
  body += tmp;

  // Header of the second part of the ads file : the declaration part
  {
    // Loop over the declarations.
    UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
    while (!i.is_done ())
      {
	// Get the current declaration as an adabe_name object.
	AST_Decl *d = i.item ();
	adabe_name *e = dynamic_cast<adabe_name *>(d);

#ifdef DEBUG_INTERFACE
	cout << "interface instruction node type:" << d->node_type () << endl;
	cout << "interface instruction node type:" << e->node_type () << endl;
#endif

	string tmp1 = "";
	string tmp2 = "";  

	// Produce the declaration (except for a NT_enum_val).
	if (e->node_type () != AST_Decl::NT_enum_val) {
	  e->produce_ads (with, tmp1, tmp2);
	  // Add it to the file.
	  body += tmp2 + tmp1;
	}
	// Get the next one.
	i.next ();
      }
  }

  // Third part of the ads file : declaration of variables and
  // functions specific to AdaBroker.
  
  // Repository ID : it is string that designate this interface
  body += "   Repository_Id : constant CORBA.String\n";
  body += "      := CORBA.To_CORBA_String (\"";
  body += repositoryID ();
  body += "\");\n\n";

  // Function Is_A : return true if the object is a Ref object
  body += "   function Is_A\n";
  body += "     (Self   : Ref;\n";
  body += "      Logical_Type_Id : CORBA.String)\n";
  body += "      return CORBA.Boolean;\n\n";

  // Fourth part of the ads file : additional declarations in case of
  // forward declarations.

  if (pd_is_forwarded == true)           
    {
      // Add a with clause for the forward package corresponding to
      // this object and rename it.
      with.add (get_ada_full_name () + "_Forward");
      body += "   package Convert_Forward is\n";
      body += "      new ";
      body += get_ada_full_name ();
      body += "_Forward.Convert (Ref);\n\n\n";
    }

  // End of package.
  body += "end " + get_ada_full_name () + ";\n";

  // Set this interface already defined.
  set_already_defined ();
}
  
////////////////////////////////////////////////////////////////////////
////////////////     produce_adb     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
/*  Subfunction for produce_adb.
    Disp recursively all RepositoryId PARENT belong too.
    FIXME: optimisation in case of multiple inherits.
    Simply tag each node.
    eg: interface A;
        interface B : A;
	interface C : A;
	interface D : B, C;
*/
void
do_is_a (adabe_interface *parent, string &body)
{
  body += "Type_Id = \"";
  body += parent->repositoryID ();
  body += "\" or else\n        ";
  for (int i = 0; i < parent->n_inherits (); i++)
    do_is_a (adabe_interface::narrow_from_decl (parent->inherits ()[i]), body);
}

void
adabe_interface::produce_adb (dep_list& with,
			      string &body,
			      string &previous)
  // Produce the adb file for a given interface.
{
  // Remember file on which we are working in order to known what must
  // be imported.
  adabe_global::set_adabe_current_file (this);

  // Add the corresponding proxy file to the with clauses.
  // with.add (get_ada_full_name () + ".Proxy");

  // Add useful packages to the with clauses.
  with.add ("Ada.Exceptions");
  with.add ("CORBA.Object");
  with.add ("Broca.Exceptions");
  with.add ("Broca.Refs");
  with.add ("Broca.Repository");
  // with.add ("AdaBroker.OmniORB");
  // with.add ("Interfaces.C");
  
  // Header of the package
  body += 
    "pragma Elaborate_All (Broca.Repository);\n"
    "\n"
    "package body " + get_ada_full_name () + " is\n"
    "\n";

  // First part of adb file : the CORBA specification

  // To_Ref cast operators
  body +=
    "   function Unchecked_To_Ref\n"
    "     (Self : in CORBA.Object.Ref'Class)\n"
    "      return Ref\n"
    "   is\n"
    "      Result : Ref;\n"
    "   begin\n"
    "      Broca.Refs.Set (Broca.Refs.Ref (Result),\n"
    "                      Broca.Refs.Get (Broca.Refs.Ref (Self)));\n"
    "      return Result;\n"
    "   end Unchecked_To_Ref;\n\n"
  
    "   function To_Ref\n"
    "     (Self : in CORBA.Object.Ref'Class)\n"
    "      return Ref\n"
    "   is\n"
    "      Res : Ref;\n"
    "   begin\n"
    "      Res := Unchecked_To_Ref (Self);\n"
    "      if Is_A (Res, Repository_Id) then\n"
    "         return Res;\n"
    "      else\n"
    "         Broca.Exceptions.Raise_Bad_Param;\n"
    "      end if;\n"
    "   end To_Ref;\n\n";

#if 0 // Not yet implemented
    "   function From_Any\n"
    "     (From : in CORBA.Any)\n"
    "      return Ref is\n"
    "   begin\n"
    "      return To_Ref (CORBA.Object.From_Any (From));\n"
    "   end From_Any;\n\n";
#endif

  // Direct parent of this package
  adabe_interface *parent;

  // In case of multiple inheritance, we must redefined all functions
  // and procedures of the second and subsequent parents since these
  // are not directly inherited.

  // Loop over the parents from the second one.
  for (int i = 1; i < n_inherits (); i++)
    {
      // Get a pointer on the current parent.
      parent = adabe_interface::narrow_from_decl (inherits ()[i]);

      // Add its corresponding poxies file to the with clauses.
      with.add (parent->get_ada_full_name () + ".Proxy");

      {
	// Loop over the declarations of the parent.
	UTL_ScopeActiveIterator j (parent, UTL_Scope::IK_decls);
	while (!j.is_done ())
	  {
	    // Get the current declaration as an adabe_name object.
	    AST_Decl *d = j.item ();
	    adabe_name *e = dynamic_cast<adabe_name *>(d);

	    // Redefine it in case of an operation or an attribute.
	    switch (d->node_type ())
	      {
	      case AST_Decl::NT_op:
	      case AST_Decl::NT_attr:
		{
		  string tempo1 = "";
		  string tempo2 = "";

		  // Produce declaration.
		  e->produce_adb (with, tempo1, tempo2);
		  body += tempo2 + tempo1;
		}
		break;
	      default:break;
	      }

	    // Get next declaration.
	    j.next ();
	  }
      }
    }

  // Second part of the ads file : the declaration part.
  {
    // Loop over the declarations.
    UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
    while (!i.is_done ())
      {
	// Get the current declaration as an AST_Decl object.
	AST_Decl *d = i.item ();

	// Produce code in case of an attribute, an operation or an
	// exception.
	switch (d->node_type ())
	  {
	  case AST_Decl::NT_attr:
	  case AST_Decl::NT_op:
	  case AST_Decl::NT_except:
	    {
	      string tmp1 = "";
	      string tmp2 = "";

	      // Get the current declaration as an adabe_name object
	      // and produce the corresponding code.
	      dynamic_cast<adabe_name *>(d)->produce_adb (with, tmp1, tmp2);

	      // Add it to the current file.
	      body += tmp2 + tmp1;
	    }
	    break;	    
	  default:break;
	  }

	// Get the next declaration.
	i.next ();
      }
  }

  // Third part of the ads file : implementation of variables and
  // functions specific to AdaBroker.

  // function Is_A    
  body +=
    "   function Is_A\n"
    "     (Self   : in Ref;\n"
    "      Logical_Type_Id : in CORBA.String)\n"
    "      return CORBA.Boolean\n"
    "   is\n"
    "      Type_Id : String := CORBA.To_Standard_String (Logical_Type_Id);\n"
    "   begin\n"
    "      if ";
  do_is_a (this, body);
  body +=
    "Type_Id = \"IDL:omg.org/CORBA/OBJECT:1.0\" then\n"
    "         return True;\n"
    "      else\n"
    "         return False;\n"
    "      end if;\n"
    "   end Is_A;\n\n";

  // Factory
  string factory_type = get_ada_full_name () + "_Factory_Type";
  string factory_name = get_ada_full_name () + "_Factory";
  body +=
    "   type " + factory_type + " is new Broca.Repository.Object_Class_Type\n"
    "      with null record;\n"
    "   function Create_Object (Class : access " + factory_type + ")\n"
    "                           return CORBA.Object.Ref'Class;\n"
    "\n"
    "   function Create_Object (Class : access " + factory_type + ")\n"
    "                           return CORBA.Object.Ref'Class is\n"
    "      Res : Ref;\n"
    "   begin\n"
    "      Broca.Refs.Set (Broca.Refs.Ref (Res),\n"
    "                      new Broca.Object.Object_Type);\n"
    "      return Res;\n"
    "   end Create_Object;\n"
    "\n"
    "   " + factory_name + " : constant Broca.Repository.Object_Class_Acc :=\n"
    "      new " + factory_type + "'\n"
    "        (Next => null, Type_Id => CORBA.RepositoryId (Repository_Id));\n"
    "begin\n"
    "   Broca.Repository.Register (" + factory_name + ");\n";

  // End of package.
  body += "end " + get_ada_full_name () + ";\n"; 
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_impl_ads     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_impl_ads (dep_list & with,
				   string   & body,
				   string   & previous)
  // Produce the impl.ads file for a given interface.
{
  // Remember file on which we are working.
  adabe_global::set_adabe_current_file (this);

  // Add OmniORB to the with clauses.
  with.add ("CORBA");
  with.add ("PortableServer");
  with.add ("Broca.Types");

  // Header of the package.
  body += "package " + get_ada_full_name () + ".Impl is\n\n";

  // Direct parent.
  adabe_interface * inher;

  // Temporary string used for the forward declarations.
  string tmp = "";

  if (n_inherits () == 0) {
    body += "   type Object is abstract new PortableServer.Servant_Base\n";
    body += "     with null record;\n\n";
  } else {

    // Find direct ancestor of interface. The direct ancestor is
    // simply the first one in the list of all ancestors.
    inher = adabe_interface::narrow_from_decl (inherits ()[0]);
    
    // Add this ancestor to the with list.
    string corps = inher->get_ada_full_name ();
    with.add (corps + ".Impl");
    
    // Define type Object as child of the ancestor Object type.
    body += "   type Object is abstract new " + corps;
    body += ".Impl.Object with null record;\n\n";

    // Now loop over all other ancestors to redefine the subprograms.
    if (n_inherits () > 1) {

      // Loop over all parents from the second one.
      for (int i = 1; i < n_inherits (); i++) {

	// Get a parent as adabe_interface object.
	inher = adabe_interface::narrow_from_decl (inherits ()[i]);
	
	// Compute name.
	string corps2 = inher->get_ada_full_name ();
	int len = corps2.length ();
	
	with.add (corps2 + ".Impl");
	{
	  // Loop over all declarations of the current parent.
	  UTL_ScopeActiveIterator j (inher, UTL_Scope::IK_decls);
	  while (!j.is_done ())
	    {
	      // Get the current declaration as an adabe_name object.
	      AST_Decl *d = j.item ();
	      adabe_name *e = dynamic_cast<adabe_name *>(d);
	      
	      switch (d->node_type ())
		{
		case AST_Decl::NT_op:
		case AST_Decl::NT_attr:
		  {
		    string tempo1 = "";
		    string tempo2 = "";
		    e->produce_impl_ads (with, tempo1, tempo2);
		    tmp += tempo2 + tempo1;
		  }
		  break;
		default:break;
		}

	      // Get next element.
	      j.next ();
	    }
	}
      }
    }

  } 
  body += tmp;
  

  // Porduce code for all the subprograms.
  UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
  while (!i.is_done ())
    {
      // Get the current declaration as an AST_Decl object.
      AST_Decl *d = i.item ();
      switch (d->node_type ())
	{
	  // Only deal with attributes and operations.
	case AST_Decl::NT_attr:
	case AST_Decl::NT_op:
	  {
	    string tmp1 = "";  // real code to be produced
	    string tmp2 = "";  // code 

	    // Get current declaration as an adabe_name object and
	    // produce code.
	    dynamic_cast<adabe_name *>(d)->produce_impl_ads (with, tmp1, tmp2);

	    // Add code to file.
	    body += tmp2 + tmp1;
	  }
	  break;
	default:break;
	}

      // Get next declaration.
      i.next ();
    }
  
  body += "private\n";

  // Primitives.
  body +=
    "   function Get_Type_Id (Obj : Object) return CORBA.RepositoryId;\n"
    "   procedure Giop_Dispatch\n"
    "     (Obj : access Object;\n"
    "      Operation : String;\n"
    "      Request_Id : CORBA.Unsigned_Long;\n"
    "      Reponse_Expected : CORBA.Boolean;\n"
    "      Stream : in out Broca.Types.Buffer_Descriptor);\n";

  // End of the package
  body += "end " + get_ada_full_name () + ".Impl;\n";
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_impl_adb     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_impl_adb (dep_list & with,
				   string   & body,
				   string   & previous)
  // This method produces the skel.adb file for a given interface
{
  // Note the file in which we are working in order to known what must
  // be imported and what not.
  adabe_global::set_adabe_current_file (this);

  string corps1 = "";
  if (n_inherits () > 0)
    {
      // Corps1 contains the name of the father.
      adabe_interface *inher_0 =
	adabe_interface::narrow_from_decl (inherits ()[0]);
      corps1 = inher_0->get_ada_full_name ();
      with.add (corps1 + ".Impl");
    }
  
  // Add corresponding marshal packages to the with clauses.
  with.add (get_ada_full_name () + ".Stream");

  // Add some useful packages to the with clauses.
  with.add ("CORBA");
  with.add ("Broca.Marshalling");
  with.add ("Broca.Giop");
  with.add ("Broca.Exceptions");

  // Header of the package.
  body += "package body " + get_ada_full_name () + ".Impl is\n\n";

  // This is used for redispatching.
  // FIXME: not very elegant.
  body +=
    "   type Object_Acc is access all Object'Class;\n"
    "\n";

  // Primitive Get_Type_Id
  // FIXME: use a constant ?
  body +=
    "   function Get_Type_Id (Obj : Object) return CORBA.RepositoryId is\n"
    "   begin\n"
    "      return CORBA.To_CORBA_String (\"";
  body += repositoryID ();
  body +=
    "\");\n"
    "   end Get_Type_Id;\n"
    "\n";

  // Produce procedure Dispatch.
  body +=
    "   procedure Giop_Dispatch\n"
    "     (Obj : access Object;\n"
    "      Operation : String;\n"
    "      Request_Id : CORBA.Unsigned_Long;\n"
    "      Reponse_Expected : CORBA.Boolean;\n"
    "      Stream : in out Broca.Types.Buffer_Descriptor)\n"
    "   is\n"
    "      use Broca.Marshalling;\n"
    "      use Broca.Types;\n"
    "      Reply_Size : Broca.Types.Buffer_Index_Type;\n"
    "   begin\n";

  // Generate what is necessary for the first parent
  UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
  while (!i.is_done ())
    {
      // Get the current declaration as an AST_Decl object.
      AST_Decl *d = i.item ();
      switch (d->node_type ())
	{
	  // Only deal with attributes and operations.
	case AST_Decl::NT_attr:
	case AST_Decl::NT_op:
	  {
	    string tmp1 = "";
	    string tmp2 = "";

	    // Get current declaration as an adabe_name object and
	    // produce code.
	    dynamic_cast<adabe_name *>(d)->produce_impl_adb (with, tmp1, tmp2);

	    // Add it to the file.
	    body += tmp1;
	  }
	  break;
	default:break;	
	}

      // Get next declaration.
      i.next ();
    } 
  
  // In case of multiple inheritance, generate what is needed for all
  // other parents.
  if (n_inherits () > 1) {
    // Loop over all parents from the second one.
    for (int i = 1; i < n_inherits (); i++) {
      // Get a parent as adabe_interface object.
      adabe_interface *inher = adabe_interface::narrow_from_decl
	(inherits ()[i]);
      
      // Compute its name
      string corps2 = inher->get_ada_full_name ();
      int len = corps2.length ();
      
      with.add (corps2);
      
      {
	// Loop over all declarations of the current parent.
	UTL_ScopeActiveIterator j (inher, UTL_Scope::IK_decls);
	while (!j.is_done ())
	  {
	    // Get the current declaration as an adabe_name object.
	    AST_Decl *d = j.item ();
	    adabe_name *e = dynamic_cast<adabe_name *>(d);
	    
	    switch (d->node_type ())
	      {
	      case AST_Decl::NT_op:
	      case AST_Decl::NT_attr:
		{
		  string tempo1 = "";
		  string tempo2 = "";
		  e->produce_impl_adb (with, tempo1, tempo2);
		  body += tempo2 + tempo1;
		}
		break;
	      default:break;
	      }

	    // Get next element.
	    j.next ();
	  }
      }
    }
  }

  if (n_inherits () == 0)
    body += "      Broca.Exceptions.Raise_Bad_Operation;\n";
  else
    {
      body += 
	"      " + corps1 + ".Impl.Dispatch\n"
	"         (Obj, Operation, Request_Id, Reponse_Expected, Stream);\n";
    }
  body += "   end Giop_Dispatch;\n\n";

  // end of the package
  body += "end " + get_ada_full_name () + ".Impl;\n";
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_stream_ads     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_stream_ads (dep_list & with,
				     string   & body,
				     string   & previous)
  // Produce marshal.ads file for a given interface.
{
  // Determine if the package is empty.
  bool is_empty = true;

  // Remember file on which we are working.
  adabe_global::set_adabe_current_file (this);

  // Header of the package.
  body += 
    "\n"
    "package " +  get_ada_full_name () + ".Stream is\n";

  // Loop over all declarations.
  UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
  while (!i.is_done ())
    {
      // Get the current declaration as an AST_Decl object.
      AST_Decl *d = i.item ();
      switch (d->node_type ())
	{
	  // Only deal with exceptions, unions, structures,
	  // enumerations, strings, arrays, sequences and
	  // typesdefs.Actually, deals with each declaration of a new
	  // type in order to define the corresponding marshall,
	  // unmarshall and align_size corresponding functions.
	case AST_Decl::NT_except:
	case AST_Decl::NT_union:
	case AST_Decl::NT_struct:
	case AST_Decl::NT_enum:
	case AST_Decl::NT_string:
	case AST_Decl::NT_array:
	case AST_Decl::NT_sequence:
	case AST_Decl::NT_typedef:
	  {
	    string tmp1 = "";
	    string tmp2 = "";	    

	    // Get the current declaration as an adabe_name object and
	    // produce code.
	    dynamic_cast<adabe_name *>(d)->produce_stream_ads
	      (with, tmp1, tmp2);

	    // Add code to file.
	    body += tmp1;

	    // Code was added.
	    is_empty = false;
	  }
	  break;
	default:break;	
	}

      // Get next declaration.
      i.next ();
    }

  if (is_empty)
    body += "pragma Warnings (Off, " + get_ada_full_name () + ".Stream);\n"; 
  else
    {
      // Add some useful packages to the with clauses.
      with.add ("CORBA");
    }

  // end of the package
  body += "end " + get_ada_full_name () + ".Stream;\n"; 
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_stream_adb     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_stream_adb (dep_list & with,
				      string   & body,
				      string   & previous)
  // Produce the marshal.adb file for a given interface.
{
  // Remember whether this packageis empty or not. If empty, nothing
  // to produce.
  bool empty = true;

  // Remember file on which we are working.
  adabe_global::set_adabe_current_file (this);

  // Add some packages to the with clauses.
  with.add ("CORBA.Object");
  with.add ("CORBA.Object.OmniORB");
  with.add ("AdaBroker.NetBufferedStream");
  with.add ("AdaBroker.MemBufferedStream");
  
  // Header of the package
  body += "package body ";
  body += get_ada_full_name ();
  body += ".Stream is\n\n";

  // Loop over all declarations.
  UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
  while (!i.is_done ())
    {
      // Get the current declaration as an AST_Decl object.
      AST_Decl *d = i.item ();
      switch (d->node_type ())
	{
	  // only deal with exceptions, unions, structures,
	  // enumerations, strings, arrays, sequences and
	  // typesdefs.Actually, deals with each declaration of a new
	  // type in order to define the corresponding marshall,
	  // unmarshall and align_size corresponding functions.
	case AST_Decl::NT_except:
	case AST_Decl::NT_union:
	case AST_Decl::NT_struct:
	case AST_Decl::NT_enum:
	case AST_Decl::NT_string:
	case AST_Decl::NT_array:
	case AST_Decl::NT_sequence:
	case AST_Decl::NT_typedef:
	  {
	    // If we got here, the package is not empty.
	    string tmp1 = "";
	    string tmp2 = "";	    

	    // Get the current object as an adabe_name object and
	    // produce code.
	    dynamic_cast<adabe_name *>(d)->produce_stream_adb
	      (with, tmp1, tmp2);

	    if (tmp1 != "") empty = false;

	    // Add code to file.
	    body += tmp1;
	  }
	  break;
	default:break;	
	}

      // Get next declaration.
      i.next ();
    }

  // End of package
  body += "end " + get_ada_full_name () + ".Stream;\n";

  // If package empty, do not produce code.
  if (empty) body = "";
}

////////////////////////////////////////////////////////////////////////
//////////////////////     miscellaneous     ///////////////////////////
////////////////////////////////////////////////////////////////////////
string
adabe_interface::dump_name (dep_list & with,
			    string   & previous)
  // Return the name of a given interface. If the interface is
  // imported, return a short name.  Otherwise, return a full name
  // after checking whether it is a forward declaration.
{
  // If imported, short name.
  if (!is_imported (with)) return "Ref";

  // Else if forwarded, full name + _forward.
  else if (pd_is_forwarded) return (get_ada_full_name ()+"_Forward.Ref");

  // else full name without _forward
  else return (get_ada_full_name ()+".Ref");
}


// ??? Difference with dump_name ???//
// marshal_name
//-------------
string
adabe_interface::marshal_name (dep_list& with, string &previous)
  // Return name of a given interface. If the interface is imported,
  // return a short name. Otherwise, return a full name after
  // verifying if it is a forward declaration or not

  // It is exactly the same as dump_name but not for every kind of object
  // that's why both are defined.
{
  // if imported, short name
  if (!is_imported (with))
    return "Ref";

  // else if forwarded, full name + _forward
  else if (pd_is_forwarded) return (get_ada_full_name ()+"_Forward.Ref");

  // else full name without _forward
  else return (get_ada_full_name ()+".Ref");
}

IMPL_NARROW_METHODS1 (adabe_interface, AST_Interface)
IMPL_NARROW_FROM_DECL (adabe_interface)
IMPL_NARROW_FROM_SCOPE (adabe_interface)
  



