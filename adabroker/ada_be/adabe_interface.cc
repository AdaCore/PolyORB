// file adabe_interface 
#include <idl.hh>
#include <idl_extern.hh>
#include <adabe.h>

adabe_interface::adabe_interface(UTL_ScopedName *n, AST_Interface **ih, long nih,
	       UTL_StrList *p)
            : AST_Interface(n, ih, nih, p),
	      AST_Decl(AST_Decl::NT_interface, n, p),
	      UTL_Scope(AST_Decl::NT_interface),
	      ada_name(AST_Decl::NT_interface,n,p)
{
  if (nih == -1) pd_is_forwarded = true;
  else pd_is_forwarded = false;
}

void
adabe_interface::produce_ads(dep_list with,string &String, string &previousdefinition)
{
  string Previous = "";
  string tmp = "";
  adabe_interface * inher;
  with.add("Corba.Object");
  if (!pd_is_imported) ada_name.compute();  // forwarded then defined
  INDENT(String);
  String += "package " + get_ada_full_name() + " is /n";
  INC_INDENT();
  INDENT(String);
  if (pd_n_inherits == 0) String += "type Ref is new Corba.Object.Ref ";

 // forward declarated

  if (pd_is_forwarded == true)           
    {
      with.add(get_ada_full_name()+"_Forward");
      INDENT(temp);
      tmp += "package Convert is new " + get_ada_full_name() + "_Forward.Convert(Ref);\n";
    }
  if (pd_n_inherits > 0)
    {
      inher = pd_inherits[0];      
      String += "type Ref is new " + inher->get_ada_full_name() + ".Ref ";
      UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
      while (!j.is_done())
	{
	  INDENT(temp);
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
	    adabe_name *e = adabe_name::narrow_from_decl(d);
	    INDENT(tmp);
	    tmp += "subtype" +  e->get_ada_name() + " is " + e->get_ada_full_name() + ";\n";
	    break;
	  default:break;
	  }
	  j.next();
	}
      if (pd_n_inherits == 1)  String += "with NULL record; \n";
      else
	{
	  String += "with record \n";
	  INC_INDENT();
	  for(int i = 1; i < pd_n_inherits; i++)
	    {
	      inher = pd_inherits[i];
	      INDENT(String);
	      String += "Adabroker_father" + toString(i+1) + " : access " + inher.get_ada_full_name() + ".Ref; \n";
	      UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
	      while (!j.is_done())
		{
		  INDENT(temp);
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
		    adabe_name *e = adabe_name::narrow_from_decl(d);
		    tmp += "subtype" +  e->get_ada_name() + " is " + e->get_ada_full_name();
		    break;
		  default:break;
		  }
		  j.next();
		}
	    }
	  INDENT(String);
	  String += "end record; \n";
	}
    }
  String += tmp;
  DEC_INDENT();
  INDENT(String);
  String += "function To_Ref(The_Ref : in Corba.Object.ref'CLASS) return Ref; \n";
  
  // instructions
  
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      string tmp1 = "";
      string tmp2 = "";
      adabe_name::narrow_from_decl(d)->produce_ads(with,tmp1,tmp2);
      String += tmp2 + tmp1;  
    }
  DEC_INDENT();
  INDENT(String);
  String += "end " + get_ada_full_name() + "\n";    
  set_already_defined();
}
  
void
adabe_interface::produce_adb(dep_list with,string &String, string &previousdefinition)
{
  string Previous = "";
  string tmp = "";
  with.add("Ada.Tags");
  with.add("Ada.exceptions");
  with.add("Ada.Omniproxycallwrapper");
  with.add("Ada.Proxies");
  with.add("Ada.Object");
  INDENT(String);
  String += "pakage body" + get_ada_full_name() + " is /n";
  INC_INDENT();
  INDENT(String);
  String += "function To_Ref(The_Ref : in Corba.Object.ref'CLASS) return Ref \n";


//////////////////////////// a completer /////////////////////////////////////



  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      switch(d->node_type()) {
      case AST_Decl::NT_attr:
      case AST_Decl::NT_op:
	string tmp1 = "";
	string tmp2 = "";
	adabe_name::narrow_from_decl(d)->produce_adb(with,tmp1,tmp2);
	String += tmp2 + tmp1;
	break;
      default:break;
      }
    }
  DEC_INDENT();
  INDENT(String);
  String += "\n end; " + get_ada_full_name() + "\n";
}

void
adabe_interface::produce_impl_ads(dep_list with,string &String, string &previousdefinition)
{
  string Previous = "";
  string tmp = "";
  adabe_interface * inher;
  with.add("Corba.Object");
  if (!pd_is_imported) ada_name.compute();  // forwarded then defined
  INDENT(String);
  String += "package " + get_ada_full_name() + " is /n";
  INC_INDENT();
  INDENT(String);
  if (pd_n_inherits == 0) String += "type Object is new Corba.Object.Object ";

 // forward declarated

  if (pd_is_forwarded == true)           
    {
      with.add(get_ada_full_name()+"_Forward");
      INDENT(temp);
      tmp += "package Convert is access " + get_ada_full_name() + "_Forward.Convert(Object);\n";
    }
  if (pd_n_inherits > 0)
    {
      inher = pd_inherits[0];      
      String += "type Object is access " + inher->get_ada_full_name() + ".Object ";
      UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
      while (!j.is_done())
	{
	  INDENT(temp);
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
	    adabe_name *e = adabe_name::narrow_from_decl(d);
	    INDENT(tmp);
	    tmp += "subtype" +  e->get_ada_name() + " is " + e->get_ada_full_name() + ";\n";
	    break;
	  default:break;
	  }
	  j.next();
	}
      if (pd_n_inherits == 1)  String += "with NULL record; \n";
      else
	{
	  String += "with record \n";
	  INC_INDENT();
	  for(int i = 1; i < pd_n_inherits; i++)
	    {
	      inher = pd_inherits[i];
	      INDENT(String);
	      String += "Adabroker_father" + toString(i+1) + " : access " + inher.get_ada_full_name() + ".Object; \n";
	      UTL_ScopeActiveIterator j(inher,UTL_Scope::IK_decls);
	      while (!j.is_done())
		{
		  INDENT(temp);
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
		    adabe_name *e = adabe_name::narrow_from_decl(d);
		    tmp += "subtype" +  e->get_ada_name() + " is " + e->get_ada_full_name();
		    break;
		  default:break;
		  }
		  j.next();
		}
	    }
	  INDENT(String);
	  String += "end record; \n";
	}
    }
  String += tmp;
  DEC_INDENT();

  // instructions
  
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      string tmp1 = "";
      string tmp2 = "";
      adabe_name::narrow_from_decl(d)->produce_ads(with,tmp1,tmp2);
      String += tmp2 + tmp1;  
    }
  DEC_INDENT();
  INDENT(String);
  String += "end " + get_ada_full_name() + "\n";    

}

void
adabe_interface::produce_impl_adb(dep_list with,string &String, string &previousdefinition)
{
  /*
    with.add("Ada.Tags");
    with.add("Ada.exceptions");
    with.add("Ada.Omniproxycallwrapper");
    with.add("Ada.Proxies");
    with.add("Ada.Object");
  */
  INDENT(String);
  String += "package body" + get_ada_full_name() + " is /n";
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  INC_INDENT();
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      switch(d->node_type()) {
      case AST_Decl::NT_attr:
      case AST_Decl::NT_op:
	string tmp1 = "";
	string tmp2 = "";
	adabe_name::narrow_from_decl(d)->produce_adb(with,tmp1,tmp2);
	String += tmp2 + tmp1;
	break;
      default:break;
      }
    }
  DEC_INDENT();
  INDENT(String);
  String += "\n end; " + get_ada_full_name() + "\n";
}

void
adabe_interface::produce_skel_ads(dep_list with,string &String, string &previousdefinition)
{
  with.add("Omniorb");
  with.add("Giop_S");
  with.add(get_ada_full_name() + ".impl");
  INDENT(String);
  String += "package " + get_ada_full_name() + ".Skeleton is \n";
  INC_INDENT();
  INDENT(String);
  String += "procedure Adabroker_Init (Self : in out ";
  String += get_ada_full_name() + ".Impl.Object ; K : in OmniORB.ObjectKey); \n";
  INDENT(String);
  String += "procedure Adabroker_Dispatch (Self : in out ";
  String += get_ada_full_name();
  String += ".Impl.Object ; Orls : in Giop_S.Object ; Orl_Op : in Corba.String ; Orl_Response_Expected : in Corba.Boolean ; Returns : out Corba.Boolean ); \n";
  INDENT(String);
  DEC_INDENT();
  INDENT(String);
  String += "end " + get_ada_full_name() + ".Skeleton ;\n";
}

void
adabe_interface::produce_proxies_ads(dep_list with ; string &String ;string &previousdefinition)
{
  with.add("Giop_C");
  with.add("Omniproxycalldesc");
  with.add("Proxyobject factory");
  with.add("Rope");
  with.add("Iop");
  INDENT(String);  
  String += "package " + get_ada_full_name() + ".Proxies is \n";
  INC_INDENT();
 
  ////////////////////////////// Mapping the object factory ////////////////////////

  string Sprivate = "";
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      switch(d->node_type()) {
      case AST_Decl::NT_attr:
      case AST_Decl::NT_op:
	string tmp1 = "";
	string tmp2 = "";
	adabe_name::narrow_from_decl(d)->produce_proxies_ads(with,tmp1,tmp2);
	String += tmp1;
	Sprivate += tmp2;	
	break;
      default:break;
      }
    }
  String += "private \n";
  String += Sprivate;
  DEC_INDENT();
  INDENT(String);
  String += "end " + get_ada_full_name() + ".Skeleton ;\n";
}

IMPL_NARROW_METHODS1(adabe_interface, AST_Interface)
IMPL_NARROW_FROM_DECL(adabe_interface)
IMPL_NARROW_FROM_SCOPE(adabe_interface)









