// File adabe_exception.cc
#include <adabe.h>

IMPL_NARROW_METHODS1(adabe_exception, AST_Exception);
IMPL_NARROW_FROM_DECL(adabe_exception);
IMPL_NARROW_FROM_SCOPE(adabe_exception);

void
adabe_exception::produce_ads (dep_list& with,string &body, string &previous)
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
	case AST_Decl::NT_field:
	  {
	    adabe_name *adabe_field =  dynamic_cast<adabe_name *> (d);
	    //	    adabe_name *adabe_type  =  dynamic_cast<adabe_name *> (adabe_field->base_type);
	    if (first)
	      {
		first = false;
		body += "       records\n";
		  }
	    body += "\t\t";
	    adabe_field->produce_ads (with, body, previous);
	    break;
	  }
	default:
#ifdef DEBUG_EXCEPTION
	  cerr << "node type is : " << d->node_type() << endl;
#endif
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
  body += "\t\tException_Occurence ;\n";
  body += "\t\t\tTo:\t out  " + get_ada_local_name() + "_Members) ;\n";
  set_already_defined();
}

void
adabe_exception::produce_adb (dep_list& with,string &body, string &previous)
{
  body += "\tprocedure Get_Members(From: in Ada.Exceptions.\n";
  body += "\t\tException_Occurence ;\n";
  body += "\t\t\tTo:\t out  " + get_ada_local_name() + "_Members) is\n";
  body += "   begin\n";
  body += "      Corba.Exceptions.Get_Members (From,To) ;\n";
  body += "   end ;\n\n\n";
}

void
adabe_exception::produce_marshal_ads (dep_list& with,string &body, string &previous)
{
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += "_Members ;\n";
  body += "      S : in out Giop_C.Object) ;\n\n";

  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += "_Members ;\n";
  body += "      S : in out Giop_C.Object) ;\n\n";

  body += "   function Align_Size (A : in";
  body += get_ada_local_name();
  body += "_Members ;\n";
  body += "               Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "               N : in Corba.Unsigned_Long := 1)\n";
  body += "               return Corba.Unsigned_Long ;\n\n\n";

  set_already_defined ();
}

void
adabe_exception::produce_marshal_adb (dep_list& with,string &body, string &previous)
{
  string marshall = "";
  string unmarshall = "";
  string align_size = "";

  marshall += "   procedure Marshall(A : in ";
  marshall += get_ada_local_name();
  marshall += "_Members ;\n";
  marshall += "                      S : in out Giop_C.Object) is\n";
  marshall += "   begin\n";
  
  unmarshall += "   procedure UnMarshall(A : out ";
  unmarshall += get_ada_local_name();
  unmarshall += "_Members ;\n";
  unmarshall += "                        S : in out Giop_C.Object) is\n";
  unmarshall += "      Tmp : ";
  unmarshall += get_ada_local_name ();
  unmarshall += "_Members ;\n";
  unmarshall += "   begin\n";
  
  align_size += "   function Align_Size (A : in ";
  align_size += get_ada_local_name();
  align_size += "_Members ;\n";
  align_size += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  align_size += "                        N : in Corba.Unsigned_Long := 1)\n";
  align_size += "                        return Corba.Unsigned_Long is\n";
  align_size += "      Tmp : Corba.Unsigned_Long := Initial_Offset ;\n";
  align_size += "   begin\n";

  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  while (!activator.is_done())
    {
      AST_Decl *d = activator.item();
      switch(d->node_type())
	{
	case AST_Decl::NT_field:
	  {
	    (dynamic_cast<adabe_field *>(d))->produce_marshal_adb(with, body, marshall, unmarshall, align_size);
	    break;
	  }
	default:
	  cerr << d->node_type() << endl;
	  throw adabe_internal_error (__FILE__,__LINE__,"unexpected decl in exception scope");
	}      
      activator.next();
    }
  marshall += "   end ;\n";
  unmarshall += "      return Tmp ;\n";
  unmarshall += "   end ;\n";
  align_size += "      return Tmp ;\n";
  align_size += "   end ;\n";

  body += marshall;
  body += unmarshall;
  body += align_size;

  set_already_defined();
}

string
adabe_exception::dump_name(dep_list& with, string &previous) 
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

string
adabe_exception::marshal_name(dep_list& with,string &previous) 
{
  if (!is_marshal_imported(with))
    {
      if (!is_already_defined())
	{
	  string tmp = "";
	  produce_marshal_adb(with, tmp, previous);
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
    adabe_name(AST_Decl::NT_except, n, p)
{
}


