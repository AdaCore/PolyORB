/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_interface.h                                       ***
***                                                                                            ***
***      This file provides the implementation of class adabe_interface declared in adabe.h    ***
***   (L 512). This class is the correspondant of the Sun's Front-End class AST_Interface.     ***
***   It provides produce functions for each generated file, a constructor and two little      ***
***   functions : dump_name and marshall_name whose job is to deal with local types.           ***
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

////////////////////////////////////////////////////////////////////////
////////////////      constructor    ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
adabe_interface::adabe_interface(UTL_ScopedName *n, AST_Interface **ih, long nih,
				 UTL_StrList *p)
  : AST_Interface(n, ih, nih, p),
    AST_Decl(AST_Decl::NT_interface, n, p),
    UTL_Scope(AST_Decl::NT_interface),
    adabe_name(AST_Decl::NT_interface, n, p)
{
  if (nih == -1) pd_is_forwarded = true;
  else pd_is_forwarded = false;  

  // Test the local name. If it is Object, then we have
  // a Corba reserved word.
  if ((string) local_name()->get_string() == "Object") 
    {
      set_ada_local_name ("Object");
      set_ada_full_name ("Corba.Object");
    }
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_ads     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_ads(dep_list &with, string &body, string &previous)
  // this method produces the adb file for a given interface
{
  // some useful variables
  adabe_interface *inher;  // direct ancestor of current interface
  string corps = "";       // name of this ancestor
  string tmp = "";         // temporary string

  // note the file in which we are working in order to known what
  // must be imported and what not.
  adabe_global::set_adabe_current_file(this);
  
#ifdef DEBUG_INTERFACE
  cout << "beginning of produce_ads of the interface" << endl;
#endif

  // Add some with declarations for usefull files
  with.add("Corba.Object");
  with.add("Corba");
  with.add("AdaBroker") ;
  with.add("Ada.Unchecked_Deallocation") ;
  
#ifdef DEBUG_INTERFACE
  cout << "befor compute_ada_name of the interface" << endl;
#endif

  // Compute the adaname of this node if we do not have a forward declaration
  if (!pd_is_forwarded) compute_ada_name();
  
#ifdef DEBUG_INTERFACE
  cout << "after compute_ada_name of the interface" << endl;
#endif

  // Header of the package
  body += "package " + get_ada_full_name() + " is \n\n";

  // first part of the ads file : the CORBA specification
  body += "   -----------------------------\n";
  body += "   --         The Spec        --\n";
  body += "   -----------------------------\n\n";

  // If the package has no ancestor, Corba.Object.Ref inherits directly
  // from Corba.Object.Ref
  if (n_inherits() == 0) 
    {
      corps = "Corba.Object";
      body += "   type Ref is new Corba.Object.Ref with null record;\n";
    }

  // If the package has ancester(s)
  if (n_inherits() > 0)
    {
      // find the direct ancestor of this interface. The direct ancestor
      // is simply the first one in the list of all ancestors
      inher = adabe_interface::narrow_from_decl(inherits()[0]);

      // add this ancestor to the with list
      corps = inher->get_ada_full_name();
      with.add(corps);

      // define type Ref as child of the ancestor Ref type
      body += "   type Ref is new " + corps + ".Ref with null record ;\n";
      
      {
	// loop over all constants, exceptions and types in the ancestor
	// interface
	UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
	while (!j.is_done())
	  {
	    // get the current constant, exception or type as adabe_name object
	    AST_Decl *d = j.item();
	    adabe_name *e = dynamic_cast<adabe_name *>(d);

	    // declare it as a subtype in this package according to the
	    // mapping Idl -> Ada
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
		tmp += " is " + e->get_ada_full_name() + " ;\n";	      
		break;
	      default:break;
	      }
	    // get next element
	    j.next();
	  }
      }

      // in case of multiple inheritance, we must redefined all functions
      // and procedures of the second and subsequent parents since these
      // are not directly inherited.
      if (n_inherits() > 1)
	{
	  // loop over all parents from the second one
	  for(int i = 1; i < n_inherits(); i++)
	    {
	      // get a parent as adabe_interface object
	      inher = adabe_interface::narrow_from_decl(inherits()[i]);

	      // compute its name
	      string corps2 = inher->get_ada_full_name();
	      int len = corps2.length();
	      
	      // add it to the with list
	      with.add(corps2);

	      // A beautiful header for this parent
	      tmp += "   -----------------------" + spaces(len,'-') + "\n";
	      tmp += "   -- inheritance from " + corps2 + "\n";
	      tmp += "   -----------------------" + spaces(len,'-') + "\n\n";
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
			tmp += " is " + e->get_ada_full_name() + " ;\n";
			break;
			// redefine all functions and procedure according
			// to the mapping Idl -> Ada
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
		    // get next element
		    j.next();
		  }
		tmp += "\n\n\n" ;
	      }
	    }
	}
    }
  // define pointer and nil_Ref for Ref type
  body += "   type Ref_Ptr is access all Ref ;\n\n";
  body += "   Nil_Ref : aliased constant Ref ;\n";

  // cast operator. Allows to cast a Ref Object into one of its ancestors
  // or children (if posible)
  body += "   function To_Ref(The_Ref : in Corba.Object.Ref'Class) return Ref ;\n\n\n";

  // add all multiple inheritance declarations to the body
  body += tmp;

  // header of the second part of the ads file : the declaration part
  body += "  --------------------------------\n";
  body += "  --   IDL declarations         --\n";
  body += "  --------------------------------\n\n";
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

	// produce the declaration (except if we have a NT_enum_val
	if (e->node_type() != AST_Decl::NT_enum_val) {
	  e->produce_ads(with, tmp1, tmp2);
	  // add it to the file
	  body += tmp2 + tmp1;
	}
	// get the next one
	i.next();
      }
  }

  // Header of the third part of the ads file : declaration of variables
  // and functions specific to AdaBroker and not existing in the CORBA
  // specification
  body += "\n   -----------------------------\n";
  body += "   --       Not in Spec       --\n";
  body += "   -----------------------------\n\n";
  
  // Repository ID : it is string that designate this interface
  body += "   Repository_Id : constant Corba.String := Corba.To_Corba_String(\"";
  body += repositoryID();
  body += "\") ;\n\n";

  // function Get_Repository_ID : return the repository ID
  body += "   function Get_Repository_Id(Self : in Ref)\n";
  body += "                              return Corba.String ;\n\n";

  // function is_a : return true if the object is a Ref object
  body += "   function Is_A(The_Ref : in Ref ;\n";
  body += "                 Repo_Id : in Corba.String)\n";
  body += "                 return Corba.Boolean ;\n\n";
  body += "   function Is_A(Repo_Id : in Corba.String)\n";
  body += "                 return Corba.Boolean ;\n\n";

  // Get_Nil_Ref : return a nil reference to a Ref object
  body += "   function Get_Nil_Ref(Self : in Ref)\n";
  body += "                        return Ref ;\n\n"; 

  // Free : deallocate the memory used by a Ref_Ptr
  body += "   procedure Free is new Ada.Unchecked_Deallocation(Ref, Ref_Ptr) ;\n\n\n" ;

  // fourth part of the ads file : additional declarations in case of
  // forward declarations in the Idl file
  if (pd_is_forwarded == true)           
    {
      // add a with clause for the forward package corresponding to this
      // object and rename it
      with.add(get_ada_full_name()+"_Forward");
      body += "   package Convert_Forward is new " + get_ada_full_name() + "_Forward.Convert(Ref) ;\n\n\n";
    }

  // fifth and last part of ads file : the private part
  body += "private\n\n";

  // definition of the nil reference
  body += "   Nil_Ref : aliased constant Ref := ( Corba.Object.Nil_Ref with null record) ;\n";

  // end of package
  body += "end " + get_ada_full_name() + " ;\n";

  // set this interface already defined
  set_already_defined();
}
  
////////////////////////////////////////////////////////////////////////
////////////////     produce_adb     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_adb(dep_list& with, string &body, string &previous)
  // this method produces the adb file for a given interface
{
  // note the file in which we are working in order to known what
  // must be imported and what not.
  adabe_global::set_adabe_current_file(this);

  // add the corresponding proxies file to the with clauses
  with.add (get_ada_full_name () + ".Proxies");

  // add packages Ada.Exceptions and Corba.Object to the with clauses
  with.add("Ada.Exceptions");
  with.add("Corba.Object");

  // add Corba.Object and type Corba.String to the use clauses
  body += "use Corba.Object ;\n";
  body += "use type Corba.String ;\n";

  // header of the package
  body += "package body " + get_ada_full_name() + " is \n\n";

  // first part of adb file : the CORBA specification
  body += "\n   -----------------------------\n";
  body += "   --         The Spec        --\n";
  body += "   -----------------------------\n\n";

  // To_Ref cast operator
  body += "   function To_Ref(The_Ref : in Corba.Object.ref'Class)\n";
  body += "                   return Ref is\n";
  body += "      Dynamic_Type : Corba.Object.Ref'Class := Get_Dynamic_Type(The_Ref) ;\n";
  body += "      Result : Ref ;\n";
  body += "      Repo_Id : Corba.String := Get_Repository_Id(Result) ;\n";
  body += "   begin\n";
  body += "      if Is_A(Dynamic_Type, Repo_Id) then\n";
  body += "         corba.Object.Internal_Copy(The_Ref, Result) ;\n"; 
  body += "         return Result ;\n"; 
  body += "      end if ;\n\n";
  body += "      Ada.Exceptions.Raise_Exception(Constraint_Error'Identity,\n";
  body += "                                     \"Cannot cast \"\n";
  body += "                                     & Corba.To_Standard_String(Get_Repository_Id(The_Ref))\n"; 
  body += "                                     & Corba.CRLF\n";
  body += "                                     & Corba.To_Standard_String(Repo_Id)) ;\n";
  body += "   end ;\n\n\n"; 

  // pointer on the direct parent of this package
  adabe_interface *inher;

  // in case of multiple inheritance, we must redefined all functions
  // and procedures of the second and subsequent parents since these
  // are not directly inherited.

  // loop over the parents from the second one
  for(int i = 1; i < n_inherits(); i++)
    {
      // get a pointer on the current parent
      inher = adabe_interface::narrow_from_decl(inherits()[i]);

      // add its corresponding poxies file to the with clauses
      with.add(inher->get_ada_full_name() + ".Proxies");

      // header of the declarations for this parent
      body += "   --------------------------------------\n";
      body += "   -- inheritance from " + inher->get_ada_full_name() + "\n";
      body += "   --------------------------------------\n\n";
      {
	// loop over the declarations of the parent
	UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
	while (!j.is_done())
	  {
	    // get the current declaration as an adabe_name object
	    AST_Decl *d = j.item();
	    adabe_name *e = dynamic_cast<adabe_name *>(d);

	    // redefine it in case of an operation or an attribute
	    switch(d->node_type())
	      {
	      case AST_Decl::NT_op:
	      case AST_Decl::NT_attr:
		{
		  string tempo1 = "";
		  string tempo2 = "";
		  // production of the declaration
		  e->produce_adb(with, tempo1, tempo2);
		  body += tempo2 + tempo1;
		}
		break;
	      default:break;
	      }
	    // get next declaration
	    j.next();
	  }
      }
    }

  // header of the second part of the ads file : the declaration part
  body += "   --------------------------------------------------\n";
  body += "   --          IDL declarations                    --\n";
  body += "   --------------------------------------------------\n\n";
  {
    // loop over the declarations
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	// get the current declaration as an AST_Decl object
	AST_Decl *d = i.item();

	// production of code in case of an attribute, an operation or 
	// an exception
	switch(d->node_type())
	  {
	  case AST_Decl::NT_attr:
	  case AST_Decl::NT_op:
	  case AST_Decl::NT_except:
	    {
	      string tmp1 = "";
	      string tmp2 = "";
	      // gets the current declaration as an adabe_name object and
	      // produces the corresponding code
	      dynamic_cast<adabe_name *>(d)->produce_adb(with, tmp1, tmp2);
	      // add it to the current file
	      body += tmp2 + tmp1;
	    }
	    break;	    
	  default:break;
	  }
	// get the next declaration
	i.next();
      }
  }

  // Header of the third part of the ads file : implementation of variables
  // and functions specific to AdaBroker and not existing in the CORBA
  // specification

  // header of the third part
  body += "   -----------------------------\n";
  body += "   --       Not in Spec       --\n";
  body += "   -----------------------------\n\n";

  // function Get Repository_Id
  body += "   -- Get_Repository_Id\n" ;
  body += "   --------------------\n" ;
  body += "   function Get_Repository_Id(Self : in Ref)\n";
  body += "                              return Corba.String is\n";
  body += "   begin\n";
  body += "      return Repository_Id ;\n";
  body += "   end ;\n\n\n";

  // function Is_A    
  body += "   -- Is_A\n" ;
  body += "   -------\n" ;
  body += "   function Is_A(The_Ref : in Ref ;\n"; 
  body += "                 Repo_Id : in Corba.String)\n";
  body += "                 return Corba.Boolean is\n";
  body += "   begin\n";
  body += "      return Is_A(Repo_Id) ;\n";
  body += "   end ;\n\n\n";

  // function Is_A
  body += "   -- Is_A\n" ;
  body += "   -------\n" ;
  body += "   function Is_A(Repo_Id : in Corba.String)\n";
  body += "                 return Corba.Boolean is\n";
  body += "   begin\n";
  body += "      return (Repository_Id = Repo_Id";
  if (n_inherits()==0) {
    // if there is no specified inheritance,
    // it means that we inherit from Corba.Object.Ref
      body += "\n              or Corba.Object.Is_A(Repo_Id)";
  } else {
    // else we can inherit from several interfaces
    for(int i = 0; i < n_inherits(); i++)
      {
	inher = adabe_interface::narrow_from_decl(inherits()[i]);
	body += "\n              or ";
	body += inher->get_ada_full_name();
	body += ".Is_A(Repo_Id)";
      }
  }
  body += ");\n";
  body += "   end ;\n\n\n";

  // function Get_Nil_Ref
  body += "   -- Get_Nil_Ref\n" ;
  body += "   --------------\n" ;
  body += "   function Get_Nil_Ref(Self : in Ref)\n";
  body += "                        return Ref is\n"; 
  body += "   begin\n";
  body += "      return Nil_Ref ;\n";
  body += "   end ;\n\n\n";

  // last part of the ads file : the directly excuted code
  body += "begin\n";
  body += "   Corba.Object.Register(Repository_Id, Nil_Ref'Access) ;\n";
  body += "   Corba.Object.Create_Proxy_Object_Factory(Repository_Id) ;\n";

  // end of the package
  body += "end " + get_ada_full_name() + " ;\n";  
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

  // add OmniObject to the with clauses
  with.add("Omniobject");

  // header of the package
  body += "package " + get_ada_full_name() + ".Impl is\n\n";

  // definition of type object if there is no parent

  // pointer on the direct parent
  adabe_interface * inher;
  // a temporary string used for the forward declarations
  string tmp = "";

  if (n_inherits() == 0) {
    body += "   type Object is new Omniobject.Implemented_Object with private ;\n";
  } else {

    // find the direct ancestor of this interface. The direct ancestor
    // is simply the first one in the list of all ancestors
    inher = adabe_interface::narrow_from_decl(inherits()[0]);
    
    // add this ancestor to the with list
    string corps = inher->get_ada_full_name();
    with.add(corps + ".Impl");
    
    // define type Object as child of the ancestor Object type
    body += "   type Object is new " + corps + ".Impl.Object with private ;\n";

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
	
	tmp += "   -----------------------" + spaces(len,'-') + "\n";
	tmp += "   -- inheritance from " + corps2 + "\n";
	tmp += "   -----------------------" + spaces(len,'-') + "\n\n";
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
		  break ;
		default:break;
		}
	      // get next element
	      j.next();
	    }
	  tmp += "\n\n\n" ;
	}
      }
    }

  } 
  body += "   type Object_Ptr is access all Object ;\n\n\n";
  body += tmp;
  

  // now all the subprogram of this IDL interface
  body += "   -----------------------\n" ;
  body += "   -- IDL definitions   --\n" ;
  body += "   -----------------------\n\n" ;
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

  body += "\n\n\n" ;
  body += "private\n\n" ;
  body += "   -- You may add fields to this record\n" ;

  // private definition of the Object type (empty but may be completed 
  // by user)
  if (n_inherits() == 0) {
    body += "   type Object is new Omniobject.Implemented_Object with record\n";
    body += "      Null ;\n" ;
    body += "   end record ;\n\n" ;
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
  body += "   --------------------------------------------------\n" ;
  body += "   ----          finalization operators          ----\n" ;
  body += "   --------------------------------------------------\n" ;
  body += "   procedure Initialize(Self : in out Object) ;\n" ;
  body += "   procedure Adjust(Self : in out Object) ;\n" ;
  body += "   procedure Finalize(Self : in out Object) ;\n\n" ;

  // end of the package
  body += "end " + get_ada_full_name() + ".Impl ;\n";
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_impl_adb     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_impl_adb(dep_list& with, string &body, string &previous)
  // this method produces the impl.adb file for a given interface
{
  // If there is an ancestor, get its name
  string ancestor ;
  if (n_inherits()) {
    ancestor= adabe_interface::narrow_from_decl(inherits()[0])->get_ada_full_name();
    with.add(ancestor + ".Impl");
  }

  //  note the file in which we are working in order to known what
  // must be imported and what not.
  adabe_global::set_adabe_current_file(this);

  // add the corresponding skeleton package to the with clauses
  with.add(get_ada_full_name() + ".Skeleton") ;

  body += "\n\n" ;

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
		  e->produce_impl_adb(with, tempo1, tempo2);
		  body += tempo2 + tempo1;
		}
		break ;
	      default:break;
	      }
	    // get next element
	    j.next();
	  }
	body += "\n\n\n" ;
      }
    }
  }
    
  // now all the subprogram of this IDL interface
  body += "   -----------------------\n" ;
  body += "   -- IDL definitions   --\n" ;
  body += "   -----------------------\n\n" ;
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

  body += "\n\n\n" ;

  body += "   -----------------------------------------------------------\n" ;
  body += "   --  Implementations objects are controlled, you can add  --\n" ;
  body += "   --  instructions in the following functions as specified --\n" ;
  body += "   -----------------------------------------------------------\n\n" ;

  // procedure Initialize
  body += "   -- Initialize\n" ;
  body += "   -------------\n" ;
  body += "   procedure Initialize(Self : in out Object) is\n" ;
  body += "   begin\n" ;
  body += "      " ;
  if(n_inherits()) {
    body += ancestor + ".Impl.Initialize(" + ancestor + ".Impl.Object(" ;
  } else {
    body += "Omniobject.Initialize(Omniobject.Implemented_Object(" ;
  }
  body += "Self)) ;\n" ;
  body += "      Init_Local_Object(Self,\n" ;
  body += "                        Repository_Id,\n" ;
  body += "                        " ;
  body += get_ada_full_name() + ".Skeleton.Dispatch'Access,\n" ;
  body += "                        " ;
  body += get_ada_full_name() + ".Is_A'Access) ;\n" ;
  body += "      -- You can add things *BELOW* this line\n\n" ;
  body += "   end Initialize ;\n\n\n" ;

  // procedure Adjust
  body += "   -- Adjust\n" ;
  body += "   ---------\n" ;
  body += "   procedure Adjust(Self: in out Object) is\n" ;
  body += "   begin\n" ;
  body += "   " ;
  if(n_inherits()) {
    body += ancestor + ".Impl.Adjust(" + ancestor + ".Impl.Object(" ;
  } else {
    body += "Omniobject.Adjust(Omniobject.Implemented_Object(" ;
  }
  body += "Self)) ;\n" ;
  body += "      -- You can add things *BELOW* this line\n\n" ;
  body += "   end Adjust ;\n\n\n" ;

  // procedure Finalize
  body += "   -- Finalize\n" ;
  body += "   -----------\n" ;
  body += "   procedure Finalize(Self : in out Object) is\n" ;
  body += "   begin\n\n" ;
  body += "      -- You can add things *BEFORE* this line\n" ;
  body += "   " ;
  if(n_inherits()) {
    body += ancestor + ".Impl.Finalize(" + ancestor + ".Impl.Object(" ;
  } else {
    body += "Omniobject.Finalize(Omniobject.Implemented_Object(" ;
  }
  body += "Self)) ;\n" ;
  body += "   end Finalize ;\n\n\n" ;

  // end of the package
  body += "end " + get_ada_full_name() + ".Impl ;\n";
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
  with.add("Omniobject");
  with.add("Giop_S");
  // header of the package
  body += "package " + get_ada_full_name() + ".Skeleton is\n\n";

  // procedure dispatch
  body += "   procedure Dispatch (Myself : in Omniobject.Implemented_Object_Ptr ;\n";
  body += "                       Orls : in out Giop_S.Object ;\n";
  body += "                       Orl_Op : in Standard.String ;\n";
  body += "                       Orl_Response_Expected : in Corba.Boolean ;\n";
  body += "                       Dispatch_Returns : out Corba.Boolean) ;\n\n";

  // end of the package
  body += "end " + get_ada_full_name() + ".Skeleton  ;\n";
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_proxies_ads     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_interface::produce_proxies_ads(dep_list& with, string &body, string &previous)
{
  adabe_global::set_adabe_current_file(this);
  with.add("Giop_C");
  with.add("Omniproxycalldesc");
  with.add("Rope");
  with.add("Iop");
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
		break ;
	      default:break;
	      }
	    // get next element
	    j.next();
	  }
	body += "\n\n" ;
      }
    }
  }

  body += "private \n";
  body += Sprivate;
  body += "end " + get_ada_full_name() + ".Proxies ;\n";
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
  with.add("Netbufferedstream");
  with.add("Membufferedstream");
  with.add("Omniropeandkey") ;
  with.add("Giop") ;
  with.add("Corba") ;
  with.add("Corba.Object");

  // header of the package
  body += "package body " + get_ada_full_name() + ".Skeleton is\n\n";

  // procedure dispatch
  body += "   procedure Dispatch (Myself : in Omniobject.Implemented_Object_Ptr ;\n";
  body += "                       Orls : in out Giop_S.Object ;\n";
  body += "                       Orl_Op : in Standard.String ;\n";
  body += "                       Orl_Response_Expected : in Corba.Boolean ;\n";
  body += "                       Dispatch_Returns : out Corba.Boolean) is\n";
  body += "      Self : ";
  body += get_ada_local_name();
  body += ".Impl.Object_Ptr := ";
  body += get_ada_local_name();
  body += ".Impl.Object_Ptr(Myself) ;\n";
  body += "   begin\n";

  // Generate what is necessary for the first parent
  body += "   -----------------------\n" ;
  body += "   -- IDL definitions   --\n" ;
  body += "   -----------------------\n\n" ;
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
  body += "\n\n" ;
  
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
		  e->produce_skel_adb(with, tempo1, tempo2);
		  body += tempo2 + tempo1;
		}
		break ;
	      default:break;
	      }
	    // get next element
	    j.next();
	  }
	body += "\n\n" ;
      }
    }
  }
  if (n_inherits() == 0)
    body += "      Dispatch_Returns := false ;\n";
  else
    {
      body += "      " + corps1 + ".Skeleton.Dispatch(Myself,\n";
      body += "                             Orls,\n";
      body += "                             Orl_Op,\n";
      body += "                             Orl_Response_Expected,\n";
      body += "                             Dispatch_Returns);\n";
    }
  body += "   end ;\n\n";

  // end of the package
  body += "end " + get_ada_full_name() + ".Skeleton  ;\n";
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
  with.add("Netbufferedstream");
  with.add("Membufferedstream");
  with.add("Corba");
  with.add("Corba.Object");

  // add the corresponding marshal package to the with clauses
  with.add( get_ada_full_name() + ".marshal") ;

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
		  e->produce_proxies_adb(with, tempo1, tempo2);
		  body += tempo2 + tempo1;
		  empty = false;
		}
		break ;
	      default:break;
	      }
	    // get next element
	    j.next();
	  }
	body += "\n\n" ;
      }
    }
  }
  
  // end of the package
  body += "end " + get_ada_full_name() + ".Proxies ;\n";

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
  with.add ("Giop_C");
  with.add ("Corba");
  with.add("Netbufferedstream");
  with.add("Membufferedstream");

  // add some usefull packages and a type to the use clauses
  body += "use type Corba.Unsigned_Long; \n";

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
  body += "end " + get_ada_full_name() + ".Marshal ;\n";  
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
  with.add("Corba.Object");
  with.add ("NetbufferedStream");
  with.add ("MembufferedStream");

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
  body += "end " + get_ada_full_name() + ".Marshal ;\n";

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
  else if (pd_is_forwarded) return (get_ada_full_name()+"_forward.Ref");

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
  else if (pd_is_forwarded) return (get_ada_full_name()+"_forward.Ref");

  // else full name without _forward
  else return (get_ada_full_name()+".Ref");
}

IMPL_NARROW_METHODS1(adabe_interface, AST_Interface)
IMPL_NARROW_FROM_DECL(adabe_interface)
IMPL_NARROW_FROM_SCOPE(adabe_interface)
  



