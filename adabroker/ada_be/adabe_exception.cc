// File adabe_exception.cc
#include <adabe.h>

IMPL_NARROW_METHODS1(adabe_exception, AST_Exception);
IMPL_NARROW_FROM_DECL(adabe_exception);
IMPL_NARROW_FROM_SCOPE(adabe_exception);

void
adabe_exception::produce_ads (dep_list with,string &body, string &previous)
{
  bool first = true;
  compute_ada_name ();
  
  // beginning of the exception declaration ...
    
  body+=  get_ada_local_name() + " : exception\n";
  
  body +=  "type " +get_ada_local_name() +"_Members is new CORBA.IDL_Exception_Members with \n";
  
  // ...
  
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  while (!activator.is_done())
    {
      AST_Decl *d = activator.item();
      activator.next();
      switch(d->node_type())
	{
	case AST_Decl::NT_sequence:
	case AST_Decl::NT_enum_val:
	case AST_Decl::NT_field:
	case AST_Decl::NT_union:
	case AST_Decl::NT_struct:
	case AST_Decl::NT_enum:
	  {
	    adabe_name *adabe_field =  adabe_name::narrow_from_decl (d);
	    //	    adabe_name *adabe_type  =  adabe_name::narrow_from_decl (adabe_field->base_type);
	    if (first)
	      {
		first = false;
		body += "       records\n";
		  }
	    body += "\t\t";
	    adabe_field->produce_ads (with, body, previous);
	  }
	default:
	  throw adabe_internal_error (__FILE__,__LINE__,"unexpected decl in exception scope");
	    }
      if (first)
	{
	  body += "       null records;\n";
	}
      else
	{
	  body += "       end record\n";
	}
    }
  // Problem in the mapping  ????
  body += "\tprocedure Get_Members(From: in Ada.Exceptions.\n";
  body += "\t\tException_Occurence;\n";
  body += "\t\t\tTo:\t out  " + get_ada_local_name() + "_Members);\n";
}

void
adabe_exception::produce_marshal_ads (dep_list with,string &body, string &previous)
{
}

void
adabe_exception::produce_marshal_adb (dep_list with,string &body, string &previous)
{
}

string
adabe_exception::dump_name(dep_list with,string &body, string &previous) 
{
    if (!is_imported(with))
    {
      if (!is_already_defined())
	{
	  string tmp = "";
	  produce_ads(with, tmp, previous);
	  previous += tmp;
	}
      return get_ada_local_name();
    }
  return get_ada_full_name();	   
}

adabe_exception::adabe_exception(UTL_ScopedName *n, UTL_StrList *p)
  : AST_Decl(AST_Decl::NT_except, n, p),
    AST_Structure(AST_Decl::NT_except, n, p),
    UTL_Scope(AST_Decl::NT_except),
    adabe_name()
{
}


