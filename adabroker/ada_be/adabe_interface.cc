#include <adabe.h>

adabe_interface::adabe_interface(UTL_ScopedName *n, AST_Interface **ih, long nih,
	       UTL_StrList *p)
            : AST_Interface(n, ih, nih, p),
	      AST_Decl(AST_Decl::NT_interface, n, p),
	      UTL_Scope(AST_Decl::NT_interface),
	      adabe_name(AST_Decl::NT_interface, n, p)
{
  if (nih == -1) pd_is_forwarded = true;
  else pd_is_forwarded = false;
}

void
adabe_interface::produce_ads(dep_list &with, string &body, string &previous)
{
  adabe_global::set_adabe_current_file(this);
#ifdef DEBUG_INTERFACE
  cout << "beginning of produce_ads of the interface" << endl;
#endif
  string tmp = "";
  string corps = "";
  adabe_interface *inher;
  with.add("Corba.Object");
  with.add("Corba");
#ifdef DEBUG_INTERFACE
  cout << "befor compute_ada_name of the interface" << endl;
#endif
  if (!pd_is_forwarded) compute_ada_name();  // forwarded then defined
#ifdef DEBUG_INTERFACE
  cout << "after compute_ada_name of the interface" << endl;
#endif
  body += "package " + get_ada_full_name() + " is /n";
  body += "\n   -----------------------------\n";
  body += "   --         The Spec        --\n";
  body += "   -----------------------------\n\n";

  if (n_inherits() == 0) 
    {
      corps = "Corba.Object";
      body += "   type Ref is new Corba.Object.Ref with null record;\n";
    }
 // forward declarated

  if (pd_is_forwarded == true)           
    {
      with.add(get_ada_full_name()+"_Forward");
      tmp += "   package Convert is new " + get_ada_full_name() + "_Forward.Convert(Ref) ;\n";
    }

  //inheritance

  if (n_inherits() > 0)
    {
      inher = adabe_interface::narrow_from_decl(inherits()[0]);
      with.add(inher->get_ada_full_name());
      corps = inher->get_ada_full_name();
      body += "   type Ref is new " + corps + ".Ref with null record ;\n";
      tmp += "\n -- inheritance from " + inher->get_ada_full_name();
      tmp += "\n --------------------------------------------------";
      {
	UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
	while (!j.is_done())
	  {
	    AST_Decl *d = j.item();
	    adabe_name *e = dynamic_cast<adabe_name *>(d);
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
		tmp += "   subtype" +  e->get_ada_local_name();
		tmp += " is " + e->get_ada_full_name() + " ;\n";	      
		break;
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
	    j.next();
	  }
      }
      if (n_inherits() > 1)
	{
	  for(int i = 1; i < n_inherits(); i++)
	    {
	      inher = adabe_interface::narrow_from_decl(inherits()[i]);
	      with.add(inher->get_ada_full_name());
	      tmp += "\n -- inheritance from " + inher->get_ada_full_name();
	      tmp += "\n --------------------------------------------------";
	      {
		UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
		while (!j.is_done())
		  {
		    AST_Decl *d = j.item();
		    adabe_name *e = dynamic_cast<adabe_name *>(d);
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
			tmp += "      subtype" +  e->get_ada_local_name();
			tmp += " is " + e->get_ada_full_name() + " ;\n";			
			break;
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
		    j.next();
		  }
	      }
	    }
	}
    }
  body += "   type Ref_Ptr is access all Ref'Class ;\n\n";
  body += "   Nil_Ref : aliased constant Ref ;\n";
  body += "   function To_Ref(The_Ref : in Corba.Object.Ref'CLASS) return Ref ;\n";
  body += tmp;

  // instructions
  body += "\n   --   Instructions          --\n";
  body += "--------------------------------\n";
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	string tmp1 = "";
	string tmp2 = "";
#ifdef DEBUG_INTERFACE
	cout << "interface instruction node type:" << d->node_type() << endl;
#endif
	adabe_name *e = dynamic_cast<adabe_name *>(d);

#ifdef DEBUG_INTERFACE
	cout << "interface instruction node type:" << e->node_type() << endl;
	//	adabe_operation *f = (adabe_operation *) e;
	//	cout << "interface instruction node type:" << f->node_type() << endl;
#endif
	e->produce_ads(with, tmp1, tmp2);
	body += tmp2 + tmp1;
	i.next();
      }
  }
  body += "\n   -----------------------------\n";
  body += "   --       Not in Spec       --\n";
  body += "   -----------------------------\n\n";
  
  body += "   Repository_Id : Corba.String := Corba.To_Corba_String(";//... repositoryID()
  body += "   function Get_Repository_Id(Self : in Ref) return Corba.String ;\n";
  body += "   function Is_A(The_Ref : in Ref; Repo_Id : in Corba.String) return Corba.Boolean ;\n";
  body += "   function Is_A(Repo_Id : in Corba.String) return Corba.Boolean ;\n";
  body += "   Get_Nil_Ref(Self : in Ref) return Ref ;\n"; 
  body += "\nprivate\n";
  body += "   Nil_Ref : aliased constant Ref := " + corps;
  body += "   .Nil_Ref with null record) ;\n";
  body += "end " + get_ada_full_name() + " ;\n";    
  set_already_defined();
}
  
void
adabe_interface::produce_adb(dep_list& with, string &body, string &previous)
{
  adabe_global::set_adabe_current_file(this);
  string tmp = "";
  adabe_interface *inher;

  with.add("Ada.Exceptions");
  with.add("Corba.Object");
  body += "use Corba.Object ;\n";
  body += "use type Corba.String ;\n";
  body += "pakage body" + get_ada_full_name() + " is \n";
  body += "\n   -----------------------------\n";
  body += "   --         The Spec        --\n";
  body += "   -----------------------------\n\n";
  body += "   function To_Ref(The_Ref : in Corba.Object.ref'Class) return Ref is\n";
  body += "      Dynamic_Type : Corba.Oject.Ref'Class := Get_Dynamic_Type(The_Ref) ;\n";
  body += "      Result : Ref ;\n";
  body += "      Repo_Id : Corba.String := Get_Repository_Id(Result) ;\n";
  body += "   begin\n";
  body += "      if Is_A(Dynamic_Type, Repo_Id) then\n";
  body += "         return (Corba.Object.Ref(The_Ref) with null record) ;\n"; 
  body += "      end if ;\n\n";
  body += "      Ada.Exceptions.Raise_Exception(Constraint_Error'Identity,\n";
  body += "                                    \"Cannot cast \"\n";
  body += "                                    & Corba.To_Standard_String(Get_Repository_Id(The_Ref))\n"; 
  body += "                                    & Corba.CRLF\n";
  body += "                                    & Corba.To_Standard_String(Repo_Id)) ;\n";
  body += "   end ;\n"; 

  //multiple inheritance definition

  for(int i = 1; i < n_inherits(); i++)
    {
      inher = adabe_interface::narrow_from_decl(inherits()[i]);
      with.add(inher->get_ada_full_name() + ".Proxies");
      body += "\n -- inheritance from " + inher->get_ada_full_name();
      body += "\n --------------------------------------------------";
      {
	UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
	while (!j.is_done())
	  {
	    AST_Decl *d = j.item();
	    adabe_name *e = dynamic_cast<adabe_name *>(d);
	    switch(d->node_type())
	      {
	      case AST_Decl::NT_op:
	      case AST_Decl::NT_attr:
		{
		  string tempo1 = "";
		  string tempo2 = "";
		  e->produce_adb(with, tempo1, tempo2);
		  body += tempo2 + tempo1;
		}
		break;
	      default:break;
	      }
	    j.next();
	  }
      }
    }

  // the instructions
  body += "\n --          IDL SPEC                            --\n";
  body += "\n --------------------------------------------------";
  {
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
	      dynamic_cast<adabe_name *>(d)->produce_adb(with, tmp1, tmp2);
	      body += tmp2 + tmp1;
	    }
	    break;	    
	  default:break;
	  }
	i.next();
      }
  }
  body += "\n   -----------------------------\n";
  body += "   --       Not in Spec       --\n";
  body += "   -----------------------------\n\n";
  body += "   function Get_Repository_Id(Self : in Ref) return Corba.String is\n";
  body += "   begin\n";
  body += "      return Repository_Id ;\n";
  body += "   end ;\n";    
  body += "   function Is_A(The_Ref : in Ref\n"; 
  body += "                 Repo_Id : in Corba.String)\n";
  body += "                 return Corba.Boolean is\n";
  body += "   begin\n";
  body += "      return Is_A(Repo_Id) ;\n";
  body += "   end ;\n";    
  body += "   function Is_A(Repo_Id : in Corba.String)\n";
  body += "                 return Corba.Boolean is\n";
  body += "   begin\n";
  body += "      return (Repository_Id = Repo_Id\n";
  for(int i = 0; i < n_inherits(); i++)
    {
      inher = adabe_interface::narrow_from_decl(inherits()[i]);
      body += "              or ";
      body += inher->get_ada_full_name();
      body += ".IS_A(Repo_Id)";
      if (i != n_inherits()-1) body += "\n";
    }
  body += ");\n";
  body += "   end ;\n";    
  body += "   Get_Nil_Ref(Self : in Ref) return Ref ;\n"; 
  body += "   begin\n";
  body += "      Nil_Ref ;\n";
  body += "   end ;\n\n";
  body += "begin\n";
  body += "   Corba.Object.Register(Repository_Id, Nil_Ref'Access) ;\n";
  body += "   Corba.Object.Create_Proxy_Object_Factory(Repository_Id) ;\n";   
  body += "end ; " + get_ada_full_name() + "\n";  
}

void
adabe_interface::produce_impl_ads(dep_list& with, string &body, string &previous)
{
  adabe_global::set_adabe_current_file(this);
  string prev = "";
  string tmp = "";
  adabe_interface * inher;
  with.add("Corba.Object");
  body += "package " + get_ada_full_name() + "_impl is /n";
  if (n_inherits() == 0) body += "   type Object is new Corba.Object.Object ";

 // forward declarated

  if (pd_is_forwarded == true)           
    {
      with.add(get_ada_full_name() + "_Forward");
      tmp += "   package Convert is access " + get_ada_full_name() + "_Forward.Convert(Object) ;\n";
    }
  if (n_inherits() > 0)
    {
      inher = adabe_interface::narrow_from_decl(inherits()[0]);      
      body += "   type Object is access " + inher->get_ada_full_name() + ".Object ";
      UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
      while (!j.is_done())
	{
	  AST_Decl *d = j.item();
	  switch(d->node_type()) {
	  case AST_Decl::NT_const:
	  case AST_Decl::NT_enum:
	  case AST_Decl::NT_except:
	  case AST_Decl::NT_struct:
	  case AST_Decl::NT_typedef:
	  case AST_Decl::NT_union:
	  case AST_Decl::NT_sequence:
	  case AST_Decl::NT_string:
	  case AST_Decl::NT_array:
	    {
	      adabe_name *e = dynamic_cast<adabe_name *>(d);
	      tmp += "   subtype" +  e->get_ada_local_name() + " is " + e->get_ada_full_name() + ";\n";
	    }
	    break;
	  default:break;
	  }
	  j.next();
	}
      if (n_inherits() == 1)  body += "with NULL record; \n";
      else
	{
	  body += "with record \n";
	  for(int i = 1; i < n_inherits(); i++)
	    {
	      inher = adabe_interface::narrow_from_decl(inherits()[i]);
	      body += "      Adabroker_father : access " + inher->get_ada_full_name() + ".Object; \n"; //...
	      UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
	      while (!j.is_done())
		{
		  AST_Decl *d = j.item();
		  switch(d->node_type()) {
		  case AST_Decl::NT_const:
		  case AST_Decl::NT_enum:
		  case AST_Decl::NT_except:
		  case AST_Decl::NT_struct:
		  case AST_Decl::NT_typedef:
		  case AST_Decl::NT_union:
		  case AST_Decl::NT_sequence:
		  case AST_Decl::NT_string:
		  case AST_Decl::NT_array:
		    {
		      adabe_name *e = dynamic_cast<adabe_name *>(d);
		      tmp += "   subtype" +  e->get_ada_local_name() + " is " + e->get_ada_full_name();
		    }
		    break;
		  default:break;
		  }
		  j.next();
		}
	    }
	  body += "   end record; \n";
	}
    }
  body += tmp;

  // instructions
  
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      string tmp1 = "";
      string tmp2 = "";
      dynamic_cast<adabe_name *>(d)->produce_ads(with, tmp1, tmp2);
      body += tmp2 + tmp1;
      i.next();
    }
  body += "end " + get_ada_full_name() + "\n";    

}

void
adabe_interface::produce_impl_adb(dep_list& with, string &body, string &previous)
{
  adabe_global::set_adabe_current_file(this);
  /*
    with.add("Ada.Tags");
    with.add("Ada.exceptions");
    with.add("Ada.Omniproxycallwrapper");
    with.add("Ada.Proxies");
    with.add("Ada.Object");
  */
  body += "package body" + get_ada_full_name() + " is /n";
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
	    dynamic_cast<adabe_name *>(d)->produce_adb(with, tmp1, tmp2);
	    body += tmp2 + tmp1;
	  }
	  break;
	default:break;
	}
      i.next();
    }
  body += "\n end; " + get_ada_full_name() + "\n";
}

void
adabe_interface::produce_skel_ads(dep_list& with, string &body, string &previous)
{
  adabe_global::set_adabe_current_file(this);
  with.add("Omniorb");
  with.add("Giop_S");
  with.add(get_ada_full_name() + ".impl");
  body += "package " + get_ada_full_name() + ".Skeleton is \n";
  body += "   procedure Adabroker_Init (Self : in out ";
  body += "   " + get_ada_full_name() + ".Impl.Object ; K : in OmniORB.ObjectKey); \n";
  body += "   procedure Adabroker_Dispatch (Self : in out ";
  body += get_ada_full_name();
  body += ".Impl.Object ; Orls : in Giop_S.Object ; Orl_Op : in Corba.String ; Orl_Response_Expected : in Corba.Boolean ; Returns : out Corba.Boolean ); \n";
  body += "end " + get_ada_full_name() + ".Skeleton  ;\n";
}

void
adabe_interface::produce_proxies_ads(dep_list& with, string &body, string &previous)
{
  adabe_global::set_adabe_current_file(this);
  with.add("Giop_C");
  with.add("Omniproxycalldesc");
  with.add("Proxyobject factory");
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
  body += "private \n";
  body += Sprivate;
  body += "end " + get_ada_full_name() + ".Proxies ;\n";
}

void
adabe_interface::produce_skel_adb(dep_list& with, string &body, string &previous)
{

}

void
adabe_interface::produce_proxies_adb(dep_list& with, string &body, string &previous)
{
  adabe_global::set_adabe_current_file(this);
  body += "package body " + get_ada_full_name() + ".Proxies is \n";
 
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
	    dynamic_cast<adabe_name *>(d)->produce_proxies_adb(with, tmp1, tmp2);
	    body += tmp1;
	  }
	  break;
	default:break;	
	}
      i.next();
    }
  body += "end " + get_ada_full_name() + ".Proxies ;\n";
}


void
adabe_interface::produce_marshal_ads(dep_list& with, string &body, string &previous)
{
  adabe_global::set_adabe_current_file(this);
  body += "package ";
  body += get_ada_full_name();
  body += ".Marshal is\n\n";
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      switch(d->node_type())
	{
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
	    dynamic_cast<adabe_name *>(d)->produce_marshal_ads(with, tmp1, tmp2);
	    body += tmp1;
	  }
	  break;
	default:break;	
	}
      i.next();
    }
  body += "end " + get_ada_full_name() + ".Marshal ;\n";  
}

void
adabe_interface::produce_marshal_adb(dep_list& with, string &body, string &previous)
{
  adabe_global::set_adabe_current_file(this);
  body += "package body ";
  body += get_ada_full_name();
  body += ".Marshal is\n\n";
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      switch(d->node_type())
	{
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
	    dynamic_cast<adabe_name *>(d)->produce_marshal_adb(with, tmp1, tmp2);
	    body += tmp1;
	  }
	  break;
	default:break;	
	}
      i.next();
    }
  body += "end " + get_ada_full_name() + ".Marshal ;\n";  
}



IMPL_NARROW_METHODS1(adabe_interface, AST_Interface)
IMPL_NARROW_FROM_DECL(adabe_interface)
IMPL_NARROW_FROM_SCOPE(adabe_interface)
  
  












