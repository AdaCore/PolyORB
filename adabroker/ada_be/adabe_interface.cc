#include <adabe.h>

////////////////////////////////////////////////////////////////////////
////////////////      constructor    ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
adabe_interface::adabe_interface(UTL_ScopedName *n,
				 AST_Interface **ih,
				 long nih,
				 UTL_StrList *p)
  : AST_Interface(n, ih, nih, p),
    AST_Decl(AST_Decl::NT_interface, n, p),
    UTL_Scope(AST_Decl::NT_interface),
    adabe_name(AST_Decl::NT_interface, n, p)
{
  if (nih == -1) pd_is_forwarded = true;
  else pd_is_forwarded = false; 
  
  // Test the local name. If it is Object, then we have a CORBA
  // reserved word.
  if ((string) local_name()->get_string() == "Object") 
    {
      set_ada_local_name ("Object");
      set_ada_full_name ("CORBA.Object");
    }
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_ads     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_ads(dep_list &with,
			     string &body,
			     string &previous)
  // This method produces the adb file for a given interface
{
  // Some useful variables
  adabe_interface *parent; // Direct parent of current interface
  string parent_name = ""; // Name of this parent
  string tmp = "";         // Temporary string

  // Preserve current working file in order to known what to import
  adabe_global::set_adabe_current_file(this);
  
#ifdef DEBUG_INTERFACE
  cout << "beginning of produce_ads of the interface" << endl;
#endif

  // Add some with declarations for useful files
  with.add("CORBA.Object");
  with.add("CORBA");
  with.add("AdaBroker");
  with.add("AdaBroker.OmniORB");
  with.add("Ada.Unchecked_Deallocation");
  
#ifdef DEBUG_INTERFACE
  cout << "before compute_ada_name of the interface" << endl;
#endif

  // Compute the adaname of this node if we do not have a forward declaration
  if (!pd_is_forwarded) compute_ada_name();
  
#ifdef DEBUG_INTERFACE
  cout << "after compute_ada_name of the interface" << endl;
#endif

  // Header of the package
  body += "package " + get_ada_full_name() + " is \n\n";

  // first part of the ads file : the CORBA specification

  // If the package has no parent, CORBA.Object.Ref inherits
  // directly from CORBA.Object.Ref
  if (n_inherits() == 0) 
    {
      parent_name = "CORBA.Object";
      body += "   type Ref is new CORBA.Object.Ref with null record;\n";
    }

  // If the package has parent(s)
  if (n_inherits() > 0)
    {
      // Find the direct parent of this interface. The direct parent
      // is simply the first one in the list of all parents
      parent = adabe_interface::narrow_from_decl(inherits()[0]);

      // Add this parent to the with list
      parent_name = parent->get_ada_full_name();
      with.add(parent_name);

      // Define type Ref as child of the parent Ref type
      body += "   type Ref is new ";
      body += parent_name;
      body += ".Ref with null record;\n";
      
      {
	// Loop over all constants, exceptions and types in the
	// parent interface
	UTL_ScopeActiveIterator j(parent,UTL_Scope::IK_decls);
	while (!j.is_done())
	  {
	    // Get the current constant, exception or type as
	    // adabe_name object
	    AST_Decl *d = j.item();
	    adabe_name *e = dynamic_cast<adabe_name *>(d);

	    // Declare it as a subtype in this package according to
	    // the mapping IDL -> Ada
	    switch(d->node_type())
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
		tmp += "   subtype " +  e->get_ada_local_name();
		tmp += " is " + e->get_ada_full_name() + ";\n";	      
		break;
	      default:break;
	      }
	    // Get next element
	    j.next();
	  }
      }

      // In case of multiple inheritance, we must redefined all
      // functions and procedures of the second and subsequent parents
      // since these are not directly inherited.
      if (n_inherits() > 1)
	{
	  // Loop over all parents from the second one
	  for(int i = 1; i < n_inherits(); i++)
	    {
	      // Get a parent as adabe_interface object
	      parent = adabe_interface::narrow_from_decl(inherits()[i]);

	      // compute its name
	      parent_name = parent->get_ada_full_name();
	      
	      // add it to the with list
	      with.add(parent_name);

	      {
		// loop over all declarations of the current parent
		UTL_ScopeActiveIterator j(parent,UTL_Scope::IK_decls);
		while (!j.is_done())
		  {
		    // get the current declaration as an adabe_name object
		    AST_Decl *d = j.item();
		    adabe_name *e = dynamic_cast<adabe_name *>(d);

		    switch(d->node_type())
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
			tmp += "   subtype " +  e->get_ada_local_name();
			tmp += " is " + e->get_ada_full_name() + ";\n";
			break;
			// Redefine all functions and procedure
			// according to the mapping Idl -> Ada
		      case AST_Decl::NT_op:
		      case AST_Decl::NT_attr:
			{
			  string tempo1 = "";
			  string tempo2 = "";
			  e->produce_ads(with, tempo1, tempo2);
			  tmp += tempo2 + tempo1;
			}
			break;
		      default:break;
		      }
		    // Get next element
		    j.next();
		  }
		tmp += "\n\n\n";
	      }
	    }
	}
    }
  // Define pointer and Nil_Ref for Ref type
  body += "   Nil_Ref : constant Ref;\n";

  // Cast parent. Allows to cast a Ref Object into one of its
  // ancestors or children (if posible)
  body += "   function To_Ref\n";
  body += "     (Self : in CORBA.Object.Ref'Class)\n";
  body += "      return Ref;\n";
  body += "   function To_Ref\n";
  body += "     (Self : in AdaBroker.OmniORB.ImplObject'Class)\n";
  body += "      return Ref ;\n\n";
  
  // Add all multiple inheritance declarations to the body
  body += tmp;

  // Header of the second part of the ads file : the declaration part
  {
    // loop over the declarations of the Idl file
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	// get the current declaration to handle as an adabe_name object
	AST_Decl *d = i.item();
	adabe_name *e = dynamic_cast<adabe_name *>(d);

#ifdef DEBUG_INTERFACE
	cout << "interface instruction node type:" << d->node_type() << endl;
	cout << "interface instruction node type:" << e->node_type() << endl;
#endif

	string tmp1 = "";
	string tmp2 = "";  

	// Produce the declaration (except if we have a NT_enum_val
	if (e->node_type() != AST_Decl::NT_enum_val) {
	  e->produce_ads(with, tmp1, tmp2);
	  // Add it to the file
	  body += tmp2 + tmp1;
	}
	// Get the next one
	i.next();
      }
  }

  // Third part of the ads file : declaration of variables and
  // functions specific to AdaBroker and not existing in the CORBA
  // specification
  
  // Repository ID : it is string that designate this interface
  body += "   Repository_Id : constant CORBA.String\n";
  body += "      := CORBA.To_CORBA_String(\"";
  body += repositoryID();
  body += "\");\n\n";

  // Function Is_A : return true if the object is a Ref object
  body += "   function Is_A\n";
  body += "     (Self   : Ref;\n";
  body += "      RepoID : CORBA.String)\n";
  body += "      return CORBA.Boolean;\n\n";

  // Fourth part of the ads file : additional declarations in case of
  // forward declarations in the IDL file

  if (pd_is_forwarded == true)           
    {
      // Add a with clause for the forward package corresponding to
      // this object and rename it
      with.add(get_ada_full_name() + "_Forward");
      body += "   package Convert_Forward is\n";
      body += "      new ";
      body += get_ada_full_name();
      body += "_Forward.Convert (Ref);\n\n\n";
    }

  // Fifth and last part of ads file : the private part
  body += "private\n";

  // Definition of the nil reference
  body += "   Nil_Ref : constant Ref\n";
  body += "      := (CORBA.Object.Nil_Ref with null record);\n";

  // End of package
  body += "end " + get_ada_full_name() + ";\n";

  // Set this interface already defined
  set_already_defined();
}
  
////////////////////////////////////////////////////////////////////////
////////////////     produce_adb     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_adb(dep_list& with,
			     string &body,
			     string &previous)
  // this method produces the adb file for a given interface
{
  // note the file in which we are working in order to known what
  // must be imported and what not.
  adabe_global::set_adabe_current_file(this);

  // add the corresponding proxies file to the with clauses
  with.add (get_ada_full_name () + ".Proxies");

  // add packages Ada.Exceptions and CORBA.Object to the with clauses
  with.add("Ada.Exceptions");
  with.add("CORBA.Object");
  with.add("CORBA.Object.OmniORB");
  with.add("AdaBroker.OmniORB");
  with.add("Interfaces.C");
  
  // add CORBA.Object and type CORBA.String to the use clauses
  body += "use CORBA.Object;\n";
  body += "use CORBA.Object.OmniORB;\n";
  body += "use type CORBA.String;\n";

  // header of the package
  body += "package body " + get_ada_full_name() + " is\n\n";

  // first part of adb file : the CORBA specification

  // To_Ref cast operator
  body += "   function To_Ref\n";
  body += "     (Self : in CORBA.Object.Ref'Class)\n";
  body += "      return Ref\n";
  body += "   is\n";
  body += "      Index  : Interfaces.C.int       := Ref_To_Id (Self);\n";
  body += "      Origin : CORBA.Object.Ref'Class := Id_To_Ref (Index).all;\n";
  body += "      Result : Ref;\n";
  body += "   begin\n";
  body += "      if Is_A (Origin, Repository_Id) then\n";
  body += "         CORBA.Object.Ref (Result) := CORBA.Object.Ref (Self);\n";
  body += "         return Result;\n";
  body += "      end if;\n\n";
  body += "      Ada.Exceptions.Raise_Exception\n";
  body += "        (Constraint_Error'Identity,\n";
  body += "         \"cannot cast \" &\n";
  body += "         CORBA.To_Standard_String (Id_To_Rep (Index)) &\n";
  body += "         \" into \" &\n";
  body += "         CORBA.To_Standard_String (Repository_Id));\n";
  body += "   end To_Ref;\n\n\n";

  body += "   function To_Ref\n";
  body += "     (Self : in AdaBroker.OmniORB.ImplObject'Class)\n";
  body += "      return Ref\n";
  body += "   is\n";
  body += "      Result : Ref;\n";
  body += "   begin\n" ;
  body += "      CORBA.Object.Ref (Result) :=\n";
  body += "         CORBA.Object.OmniORB.To_Ref (Self, Repository_Id);\n";
  body += "      return Result;\n";
  body += "   end To_Ref;\n\n";

  // Direct parent of this package
  adabe_interface *parent;

  // In case of multiple inheritance, we must redefined all functions
  // and procedures of the second and subsequent parents since these
  // are not directly inherited.

  // Loop over the parents from the second one
  for(int i = 1; i < n_inherits(); i++)
    {
      // Get a pointer on the current parent
      parent = adabe_interface::narrow_from_decl(inherits()[i]);

      // Add its corresponding poxies file to the with clauses
      with.add(parent->get_ada_full_name() + ".Proxies");

      {
	// Loop over the declarations of the parent
	UTL_ScopeActiveIterator j(parent,UTL_Scope::IK_decls);
	while (!j.is_done())
	  {
	    // Get the current declaration as an adabe_name object
	    AST_Decl *d = j.item();
	    adabe_name *e = dynamic_cast<adabe_name *>(d);

	    // Redefine it in case of an operation or an attribute
	    switch(d->node_type())
	      {
	      case AST_Decl::NT_op:
	      case AST_Decl::NT_attr:
		{
		  string tempo1 = "";
		  string tempo2 = "";
		  // Production of the declaration
		  e->produce_adb(with, tempo1, tempo2);
		  body += tempo2 + tempo1;
		}
		break;
	      default:break;
	      }
	    // Get next declaration
	    j.next();
	  }
      }
    }

  // Second part of the ads file : the declaration part
  {
    // Loop over the declarations
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	// Get the current declaration as an AST_Decl object
	AST_Decl *d = i.item();

	// Production of code in case of an attribute, an operation or
	// an exception
	switch(d->node_type())
	  {
	  case AST_Decl::NT_attr:
	  case AST_Decl::NT_op:
	  case AST_Decl::NT_except:
	    {
	      string tmp1 = "";
	      string tmp2 = "";

	      // Gets the current declaration as an adabe_name object
	      // and produces the corresponding code
	      dynamic_cast<adabe_name *>(d)->produce_adb(with, tmp1, tmp2);

	      // Add it to the current file
	      body += tmp2 + tmp1;
	    }
	    break;	    
	  default:break;
	  }

	// Get the next declaration
	i.next();
      }
  }

  // Third part of the ads file : implementation of variables and
  // functions specific to AdaBroker and not existing in the CORBA
  // specification

  // function Is_A    
  body += "   function Is_A\n";
  body += "     (Self   : in Ref;\n";
  body += "      RepoID : in CORBA.String)\n";
  body += "      return CORBA.Boolean\n";
  body += "   is\n";
  body += "   begin\n";
  body += "      return Repository_Id = RepoID";

  if (n_inherits()==0) {

    // If there is no specified inheritance, it means that we inherit
    // from CORBA.Object.Ref
    body += "\n      or else CORBA.Object.Is_A (CORBA.Object.Nil_Ref, RepoID)";

  } else {

    // Else we inherit from several interfaces
    for(int i = 0; i < n_inherits(); i++)
      {
	parent = adabe_interface::narrow_from_decl(inherits()[i]);
	body += "\n            or else ";
	body += parent->get_ada_full_name();
	body += ".Is_A (";
	body += parent->get_ada_full_name();
	body += ".Nil_Ref, RepoID)";
      }
  }
  body += ";\n   end Is_A;\n\n\n";

  // last part of the ads file : the directly excuted code
  body += "begin\n";
  body += "   CORBA.Object.OmniORB.Register (Repository_Id, Nil_Ref);\n";

  // end of the package
  body += "end " + get_ada_full_name() + ";\n"; 
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_impl_ads     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_impl_ads(dep_list& with, string &body, string &previous)
  // this method produces the impl.ads file for a given interface
{
  // note the file in which we are working in order to known what
  // must be imported and what not.
  adabe_global::set_adabe_current_file(this);

  // add OmniORB to the with clauses
  with.add("AdaBroker.OmniORB");

  // header of the package
  body += "package " + get_ada_full_name() + ".Impl is\n\n";

  // definition of type object if there is no parent

  // pointer on the direct parent
  adabe_interface * inher;
  // a temporary string used for the forward declarations
  string tmp = "";

  if (n_inherits() == 0) {
    body += "   type Object is new AdaBroker.OmniORB.ImplObject with private;\n";
  } else {

    // find the direct ancestor of this interface. The direct ancestor
    // is simply the first one in the list of all ancestors
    inher = adabe_interface::narrow_from_decl(inherits()[0]);
    
    // add this ancestor to the with list
    string corps = inher->get_ada_full_name();
    with.add(corps + ".Impl");
    
    // define type Object as child of the ancestor Object type
    body += "   type Object is new " + corps + ".Impl.Object with private;\n";

    // Now loop over all other ancestors to redefine the subprograms
    if (n_inherits() > 1) {
      // loop over all parents from the second one
      for(int i = 1; i < n_inherits(); i++) {
	// get a parent as adabe_interface object
	inher = adabe_interface::narrow_from_decl(inherits()[i]);
	
	// compute its name
	string corps2 = inher->get_ada_full_name();
	int len = corps2.length();
	
	with.add(corps2 + ".Impl");
	{
	  // loop over all declarations of the current parent
	  UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
	  while (!j.is_done())
	    {
	      // get the current declaration as an adabe_name object
	      AST_Decl *d = j.item();
	      adabe_name *e = dynamic_cast<adabe_name *>(d);
	      
	      switch(d->node_type())
		{
		case AST_Decl::NT_op:
		case AST_Decl::NT_attr:
		  {
		    string tempo1 = "";
		    string tempo2 = "";
		    e->produce_impl_ads(with, tempo1, tempo2);
		    tmp += tempo2 + tempo1;
		  }
		  break;
		default:break;
		}
	      // get next element
	      j.next();
	    }
	  tmp += "\n\n\n";
	}
      }
    }

  } 
  body += "   type Object_Ptr is access all Object;\n\n\n";
  body += tmp;
  

  // now all the subprogram of this IDL interface
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      // get the current declaration as an AST_Decl object
      AST_Decl *d = i.item();
      switch(d->node_type())
	{
	  // only deal with attributes and operations
	case AST_Decl::NT_attr:
	case AST_Decl::NT_op:
	  {
	    string tmp1 = "";  // real code to be produced
	    string tmp2 = "";  // code 

	    // get the current declaration as an adabe_name object and
	    // produce the corresponding code
	    dynamic_cast<adabe_name *>(d)->produce_impl_ads(with, tmp1, tmp2);

	    // add this code to the file
	    body += tmp2 + tmp1;
	  }
	  break;
	default:break;
	}
      // get next declaration
       i.next();
    }

  body += "\n\n\n";
  body += "private\n\n";
  body += "   -- You may add fields to this record\n";

  // private definition of the Object type (empty but may be completed 
  // by user)
  if (n_inherits() == 0) {
    body += "   type Object is new AdaBroker.OmniORB.ImplObject with record\n";
    body += "      Null;\n";
    body += "   end record;\n\n";
  }
  else if (n_inherits() > 0)
    {
      inher = adabe_interface::narrow_from_decl(inherits()[0]);
      
      // add this ancestor to the with list
      string corps = inher->get_ada_full_name();
    
      // define type Object as child of the ancestor Object type
      body += "   type Object is new " + corps + ".Impl.Object with record\n";
      body += "      Null;\n";
      body += "   end record;\n\n";
    }
  // declaration of functions initialize, adjust and finalize since the 
  /// object type is controlled
  body += "   procedure Initialize (Self : in out Object);\n";
  body += "   procedure Adjust     (Self : in out Object);\n";
  body += "   procedure Finalize   (Self : in out Object);\n\n";

  // end of the package
  body += "end " + get_ada_full_name() + ".Impl;\n";
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_impl_adb     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_impl_adb(dep_list& with, string &body, string &previous)
  // this method produces the impl.adb file for a given interface
{
  // If there is an ancestor, get its name
  string ancestor;
  if (n_inherits()) {
    ancestor= adabe_interface::narrow_from_decl(inherits()[0])->get_ada_full_name();
    with.add(ancestor + ".Impl");
  }

  //  note the file in which we are working in order to known what
  // must be imported and what not.
  adabe_global::set_adabe_current_file(this);

  // add the corresponding skeleton package to the with clauses
  with.add(get_ada_full_name() + ".Skeleton");

  body += "\n\n";

  body += "package body " + get_ada_full_name() + ".Impl is \n\n\n";

  // In case of multiple inheritance, we have to redefine
  // ll the function of the parents but the first one
  if (n_inherits() > 1) {
    // loop over all parents from the second one
    for(int i = 1; i < n_inherits(); i++) {
      // get a parent as adabe_interface object
      adabe_interface *inher = adabe_interface::narrow_from_decl(inherits()[i]);
      
      // compute its name
      string corps2 = inher->get_ada_full_name();
      int len = corps2.length();
      
      with.add(corps2 + ".Impl");
      
      {
	// loop over all declarations of the current parent
	UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
	while (!j.is_done())
	  {
	    // get the current declaration as an adabe_name object
	    AST_Decl *d = j.item();
	    adabe_name *e = dynamic_cast<adabe_name *>(d);
	    
	    switch(d->node_type())
	      {
	      case AST_Decl::NT_op:
	      case AST_Decl::NT_attr:
		{
		  string tempo1 = "";
		  string tempo2 = "";
		  e->produce_impl_adb(with, tempo1, tempo2);
		  body += tempo2 + tempo1;
		}
		break;
	      default:break;
	      }
	    // get next element
	    j.next();
	  }
	body += "\n\n\n";
      }
    }
  }
    
  // now all the subprogram of this IDL interface
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      // get the current declaration as an AST_Decl object
      AST_Decl *d = i.item();
      switch(d->node_type())
	{
	  // only deal with attributes and operations
	case AST_Decl::NT_attr:
	case AST_Decl::NT_op:
	  {
	    string tmp1 = "";
	    string tmp2 = "";
	    // get the current declaration as an adabe_name object and
	    // produces the corresponding code
	    dynamic_cast<adabe_name *>(d)->produce_impl_adb(with, tmp1, tmp2);

	    // add the code to the file
	    body += tmp2 + tmp1;
	  }
	  break;
	default:break;
	}
      // get the next declaration
      i.next();
    }

  body += "\n\n\n";

  body += "   -----------------------------------------------------------\n";
  body += "   --  Implementations objects are controlled, you can add  --\n";
  body += "   --  instructions in the following functions as specified --\n";
  body += "   -----------------------------------------------------------\n\n";

  // procedure Initialize
  body += "   procedure Initialize(Self : in out Object) is\n";
  body += "   begin\n";
  body += "      ";
  if(n_inherits()) {
    body += ancestor + ".Impl.Initialize(" + ancestor + ".Impl.Object(";
  } else {
    body += "AdaBroker.OmniORB.Initialize(AdaBroker.OmniORB.ImplObject(";
  }
  body += "Self));\n";
  body += "      Initialize_Local_Object\n";
  body += "        (Self,\n";
  body += "         Repository_Id,\n";
  body += "         ";
  body += get_ada_full_name() + ".Skeleton.Dispatch'Access);\n";
  body += "      -- You can add things *BELOW* this line\n\n";
  body += "   end Initialize;\n\n\n";

  // procedure Adjust
  body += "   procedure Adjust(Self: in out Object) is\n";
  body += "   begin\n";
  body += "   ";
  if(n_inherits()) {
    body += ancestor + ".Impl.Adjust(" + ancestor + ".Impl.Object(";
  } else {
    body += "   AdaBroker.OmniORB.Adjust\n";
    body += "     (AdaBroker.OmniORB.ImplObject (";
  }
  body += "Self));\n";
  body += "      -- You can add things *BELOW* this line\n\n";
  body += "   end Adjust;\n\n\n";

  // procedure Finalize
  body += "   procedure Finalize(Self : in out Object) is\n";
  body += "   begin\n\n";
  body += "      -- You can add things *BEFORE* this line\n";
  body += "   ";
  if(n_inherits()) {
    body += ancestor + ".Impl.Finalize(" + ancestor + ".Impl.Object(";
  } else {
    body += "   AdaBroker.OmniORB.Finalize\n";
    body += "     (AdaBroker.OmniORB.ImplObject(";
  }
  body += "Self));\n";
  body += "   end Finalize;\n\n\n";

  // end of the package
  body += "end " + get_ada_full_name() + ".Impl;\n";
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_skel_ads     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_skel_ads(dep_list& with, string &body, string &previous)
  // this method produces the skel.ads file for a given package
{
  // note the file in which we are working in order to known what
  // must be imported and what not.
  adabe_global::set_adabe_current_file(this);

  // add the packages omniobject and giop_s to the with clauses
  with.add("AdaBroker.OmniORB");
  with.add("AdaBroker.GIOP_S");
  // header of the package
  body += "package " + get_ada_full_name() + ".Skeleton is\n\n";

  // procedure dispatch
  body += "   procedure Dispatch (Myself : in AdaBroker.OmniORB.ImplObject_Ptr;\n";
  body += "                       Orls : in out AdaBroker.GIOP_S.Object;\n";
  body += "                       Orl_Op : in Standard.String;\n";
  body += "                       Orl_Response_Expected : in CORBA.Boolean;\n";
  body += "                       Dispatch_Returns : out CORBA.Boolean);\n\n";

  // end of the package
  body += "end " + get_ada_full_name() + ".Skeleton ;\n";
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_proxies_ads     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_proxies_ads(dep_list& with, string &body, string &previous)
{
  adabe_global::set_adabe_current_file(this);
  with.add("AdaBroker.GIOP_C");
  with.add("AdaBroker.OmniProxyCallDesc");
  with.add("AdaBroker.Rope");
  with.add("AdaBroker.IOP");
  body += "package " + get_ada_full_name() + ".Proxies is \n";
 
  ////////////////////////////// Mapping the object factory ////////////////////////

  string Sprivate = "";
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      switch(d->node_type())
	{
	case AST_Decl::NT_attr:
	case AST_Decl::NT_op:
	  {
	    string tmp1 = "";
	    string tmp2 = "";
	    dynamic_cast<adabe_name *>(d)->produce_proxies_ads(with, tmp1, tmp2);
	    body += tmp1;
	    Sprivate += tmp2;	
	  }
	  break;
	default:break;	
	}
      i.next();
    }
    // In case of multiple inheritance, generate what
  // is needed for all other parents
  if (n_inherits() > 1) {
    // loop over all parents from the second one
    for(int i = 1; i < n_inherits(); i++) {
      // get a parent as adabe_interface object
      adabe_interface *inher = adabe_interface::narrow_from_decl(inherits()[i]);
      
      // compute its name
      string corps2 = inher->get_ada_full_name();
      int len = corps2.length();
      
      with.add(corps2);
      
      body += "   -----------------------" + spaces(len,'-') + "\n";
      body += "   -- inheritance from " + corps2 + "\n";
      body += "   -----------------------" + spaces(len,'-') + "\n\n";
      {
	// loop over all declarations of the current parent
	UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
	while (!j.is_done())
	  {
	    // get the current declaration as an adabe_name object
	    AST_Decl *d = j.item();
	    adabe_name *e = dynamic_cast<adabe_name *>(d);
	    
	    switch(d->node_type())
	      {
	      case AST_Decl::NT_op:
	      case AST_Decl::NT_attr:
		{
		  string tempo1 = "";
		  string tempo2 = "";
		  e->produce_proxies_ads(with, tempo1, tempo2);
		  body += tempo1;
		  Sprivate += tempo2;	
		}
		break;
	      default:break;
	      }
	    // get next element
	    j.next();
	  }
	body += "\n\n";
      }
    }
  }

  body += "private \n";
  body += Sprivate;
  body += "end " + get_ada_full_name() + ".Proxies;\n";
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_skel_adb     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_skel_adb(dep_list& with, string &body, string &previous)
  // this method produces the skel.adb file for a given interface
{
  // note the file in which we are working in order to known what
  // must be imported and what not.
  adabe_global::set_adabe_current_file(this);

  string corps1 = "";
  if (n_inherits() > 0)
    {
      // corps1 contains the name of the father 
      adabe_interface *inher_0 = adabe_interface::narrow_from_decl(inherits()[0]);
      corps1 = inher_0->get_ada_full_name();
      with.add(corps1 + ".Skeleton");
    }
  
  // add the corresponding impl and marshal packages to the with clauses
  with.add(get_ada_full_name() + ".Impl");
  with.add(get_ada_full_name() + ".Marshal");

  // add some usefull packages to the with clauses
  with.add ("AdaBroker.NetBufferedStream");
  with.add ("AdaBroker.MemBufferedStream");
  with.add ("AdaBroker.OmniRopeAndKey");
  with.add ("AdaBroker.GIOP");
  with.add ("AdaBroker.Exceptions");
  with.add ("CORBA");
  with.add ("CORBA.Object");
  with.add ("CORBA.Object.OmniORB");

  // header of the package
  body += "package body " + get_ada_full_name() + ".Skeleton is\n\n";

  // procedure dispatch
  body += "   procedure Dispatch (Myself : in AdaBroker.OmniORB.ImplObject_Ptr;\n";
  body += "                       Orls : in out AdaBroker.GIOP_S.Object;\n";
  body += "                       Orl_Op : in Standard.String;\n";
  body += "                       Orl_Response_Expected : in CORBA.Boolean;\n";
  body += "                       Dispatch_Returns : out CORBA.Boolean) is\n";
  body += "      Self : ";
  body += get_ada_local_name();
  body += ".Impl.Object_Ptr := ";
  body += get_ada_local_name();
  body += ".Impl.Object_Ptr(Myself);\n";
  body += "   begin\n";

  // Generate what is necessary for the first parent
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      // get the current declaration as an AST_Decl object
      AST_Decl *d = i.item();
      switch(d->node_type())
	{
	  // only deal with attributes and operations
	case AST_Decl::NT_attr:
	case AST_Decl::NT_op:
	  {
	    string tmp1 = "";
	    string tmp2 = "";
	    // get the current declaration as an adabe_name object and
	    // produces the corresponding code
	    dynamic_cast<adabe_name *>(d)->produce_skel_adb(with, tmp1, tmp2);
	    // add it to the file
	    body += tmp1;
	  }
	  break;
	default:break;	
	}
      // get the next declaration
      i.next();
    } 
  body += "\n\n";
  
  // In case of multiple inheritance, generate what
  // is needed for all other parents
  if (n_inherits() > 1) {
    // loop over all parents from the second one
    for(int i = 1; i < n_inherits(); i++) {
      // get a parent as adabe_interface object
      adabe_interface *inher = adabe_interface::narrow_from_decl(inherits()[i]);
      
      // compute its name
      string corps2 = inher->get_ada_full_name();
      int len = corps2.length();
      
      with.add(corps2);
      
      {
	// loop over all declarations of the current parent
	UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
	while (!j.is_done())
	  {
	    // get the current declaration as an adabe_name object
	    AST_Decl *d = j.item();
	    adabe_name *e = dynamic_cast<adabe_name *>(d);
	    
	    switch(d->node_type())
	      {
	      case AST_Decl::NT_op:
	      case AST_Decl::NT_attr:
		{
		  string tempo1 = "";
		  string tempo2 = "";
		  e->produce_skel_adb(with, tempo1, tempo2);
		  body += tempo2 + tempo1;
		}
		break;
	      default:break;
	      }
	    // get next element
	    j.next();
	  }
	body += "\n\n";
      }
    }
  }
  if (n_inherits() == 0)
    body += "      Dispatch_Returns := false;\n";
  else
    {
      body += "      " + corps1 + ".Skeleton.Dispatch(Myself,\n";
      body += "                             Orls,\n";
      body += "                             Orl_Op,\n";
      body += "                             Orl_Response_Expected,\n";
      body += "                             Dispatch_Returns);\n";
    }
  body += "   end;\n\n";

  // end of the package
  body += "end " + get_ada_full_name() + ".Skeleton ;\n";
}




////////////////////////////////////////////////////////////////////////
////////////////     produce_proxies_adb     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_proxies_adb(dep_list& with, string &body, string &previous)
  // this method produces the proxies.adb file for a given interface
{
  // this boolean indicates whether this package will be empty or not
  // if it is, it will not be produced
  bool empty = true;

  // note the file in which we are working in order to known what
  // must be imported and what not.
  adabe_global::set_adabe_current_file(this);

  // add some usefull package to the with clauses
  with.add ("AdaBroker.NetBufferedStream");
  with.add ("AdaBroker.MemBufferedStream");
  with.add ("CORBA");
  with.add ("CORBA.Object.OmniORB");

  // add the corresponding marshal package to the with clauses
  with.add( get_ada_full_name() + ".marshal");

  // header of the package
  body += "package body " + get_ada_full_name() + ".Proxies is \n";
 

  //////////////////////////////// 
  // Mapping the object factory //
  ////////////////////////////////

  // a string for private declarations
  string Sprivate = "";

  // loop over all declarations
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      // get the current declaration as an AST_Decl object
      AST_Decl *d = i.item();
      switch(d->node_type())
	{
	  // only deal with attributes and operations
	case AST_Decl::NT_attr:
	case AST_Decl::NT_op:
	  {
	    string tmp1 = "";
	    string tmp2 = "";
	    // if we got here, the package will not be empty
	    empty = false;

	    // get the current object as an adabe_name object and produces
	    // the correponding code
	    dynamic_cast<adabe_name *>(d)->produce_proxies_adb(with, tmp1, tmp2);

	    // add the code to the file
	    body += tmp1;
	  }
	  break;
	default:break;	
	}
      // get the next declaration
      i.next();
    }
  // In case of multiple inheritance, generate what
  // is needed for all other parents
  if (n_inherits() > 1) {
    // loop over all parents from the second one
    for(int i = 1; i < n_inherits(); i++) {
      // get a parent as adabe_interface object
      adabe_interface *inher = adabe_interface::narrow_from_decl(inherits()[i]);
      
      // compute its name
      string corps2 = inher->get_ada_full_name();
      int len = corps2.length();
      
      with.add (corps2);
      
      {
	// loop over all declarations of the current parent
	UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
	while (!j.is_done())
	  {
	    // get the current declaration as an adabe_name object
	    AST_Decl *d = j.item();
	    adabe_name *e = dynamic_cast<adabe_name *>(d);
	    
	    switch(d->node_type())
	      {
	      case AST_Decl::NT_op:
	      case AST_Decl::NT_attr:
		{
		  string tempo1 = "";
		  string tempo2 = "";
		  e->produce_proxies_adb(with, tempo1, tempo2);
		  body += tempo2 + tempo1;
		  empty = false;
		}
		break;
	      default:break;
	      }
	    // get next element
	    j.next();
	  }
	body += "\n\n";
      }
    }
  }
  
  // end of the package
  body += "end " + get_ada_full_name() + ".Proxies;\n";

  // if the package is empty, do not produce anything
  if (empty) body = "";
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_marshal_ads     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_marshal_ads(dep_list& with, string &body, string &previous)
  // this method produces marshal.ads file for a given interface
{
  // note the file in which we are working in order to known what
  // must be imported and what not.
  adabe_global::set_adabe_current_file(this);

  // add some usefull packages to the with clauses
  with.add ("AdaBroker.GIOP_C");
  with.add ("CORBA");
  with.add ("AdaBroker.NetBufferedStream");
  with.add ("AdaBroker.MemBufferedStream");

  // add some usefull packages and a type to the use clauses
  body += "use type CORBA.Unsigned_Long;\n";

  // header of the package
  body += "package ";
  body += get_ada_full_name();
  body += ".Marshal is\n\n";

  // loop over all declarations
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      // get the current declaration as an AST_Decl object
      AST_Decl *d = i.item();
      switch(d->node_type())
	{
	  // only deal with exceptions, unions, structures, enumerations,
	  // strings, arrays, sequences and typesdefs.Actually, deals with
	  // each declaration of a new type in order to define the
	  // corresponding marshall, unmarshall and align_size corresponding
	  // functions
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
	    // get the current declaration as an adabe_name object and
	    // produces the corresponding code
	    dynamic_cast<adabe_name *>(d)->produce_marshal_ads(with, tmp1, tmp2);

	    // add the code to the file
	    body += tmp1;
	  }
	  break;
	default:break;	
	}
      // get the next declaration
      i.next();
    }

  // end of the package
  body += "end " + get_ada_full_name() + ".Marshal;\n"; 
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_marshal_adb     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_marshal_adb(dep_list& with, string &body, string &previous)
  // this method produces the marshal.adb file for a given interface
{
  // this boolean indicates whether this package will be empty or not
  // if it is, it will not be produced
  bool empty = true;

  // note the file in which we are working in order to known what
  // must be imported and what not.
  adabe_global::set_adabe_current_file(this);

  // add some packages corba.object to the with clauses
  with.add("CORBA.Object");
  with.add ("AdaBroker.NetBufferedStream");
  with.add ("AdaBroker.MemBufferedStream");

  // header of the package
  body += "package body ";
  body += get_ada_full_name();
  body += ".Marshal is\n\n";

  // loop over all declarations
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      // get the current declaration as an AST_Decl object
      AST_Decl *d = i.item();
      switch(d->node_type())
	{
	  // only deal with exceptions, unions, structures, enumerations,
	  // strings, arrays, sequences and typesdefs.Actually, deals with
	  // each declaration of a new type in order to define the
	  // corresponding marshall, unmarshall and align_size corresponding
	  // functions
	case AST_Decl::NT_except:
	case AST_Decl::NT_union:
	case AST_Decl::NT_struct:
	case AST_Decl::NT_enum:
	case AST_Decl::NT_string:
	case AST_Decl::NT_array:
	case AST_Decl::NT_sequence:
	case AST_Decl::NT_typedef:
	  {
	    // if we got here, the package will not be empty
	    string tmp1 = "";
	    string tmp2 = "";	    

	    // get the current object as an adabe_name object and produces
	    // the correponding code
	    dynamic_cast<adabe_name *>(d)->produce_marshal_adb(with, tmp1, tmp2);
	    if (tmp1 != "") empty = false;


	    // add the code to the file
	    body += tmp1;
	  }
	  break;
	default:break;	
	}
      // get the next declaration
      i.next();
    }

  // end of the package
  body += "end " + get_ada_full_name() + ".Marshal;\n";

  // if the package is empty, do not produce anything
  if (empty) body = "";
}


////////////////////////////////////////////////////////////////////////
//////////////////////     miscellaneous     ///////////////////////////
////////////////////////////////////////////////////////////////////////
string
adabe_interface::dump_name(dep_list& with, string &previous)
  // this method returns the name of a given interface
  // if the interface is imported, then it gives a short name.
  // otherwise, it gives a full name after verifying if it is a forward
  // declaration or not
{
  // if imported, short name
  if (!is_imported(with))
    return "Ref";

  // else if forwarded, full name + _forward
  else if (pd_is_forwarded) return (get_ada_full_name()+"_Forward.Ref");

  // else full name without _forward
  else return (get_ada_full_name()+".Ref");
}


// marshal_name
//-------------
string
adabe_interface::marshal_name(dep_list& with, string &previous)
  // this method returns the name of a given interface
  // if the interface is imported, then it gives a short name.
  // otherwise, it gives a full name after verifying if it is a forward
  // declaration or not

  // It is exactly the same as dump_name but not for every kind of object
  // that's why both are defined.
{
  // if imported, short name
  if (!is_imported(with))
    return "Ref";
  // else if forwarded, full name + _forward
  else if (pd_is_forwarded) return (get_ada_full_name()+"_Forward.Ref");

  // else full name without _forward
  else return (get_ada_full_name()+".Ref");
}

IMPL_NARROW_METHODS1(adabe_interface, AST_Interface)
IMPL_NARROW_FROM_DECL(adabe_interface)
IMPL_NARROW_FROM_SCOPE(adabe_interface)
  



