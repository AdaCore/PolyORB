#include <adabe.h>

adabe_typedef::adabe_typedef (AST_Type       * bt,
			      UTL_ScopedName * n,
			      UTL_StrList    * p)
  : AST_Typedef (bt, n, p),
    AST_Decl (AST_Decl::NT_typedef, n, p),
    adabe_name (AST_Decl::NT_typedef, n, p)

{
  pd_number_value = 0;
}

void
adabe_typedef::produce_ads (dep_list & with,
			    string   & body,
			    string   & previous)
{
  compute_ada_name ();
  AST_Decl *b = base_type ();
  adabe_name *c = dynamic_cast<adabe_name *>(b);
  if (((string) b->local_name ()->get_string ()) == "local type")
    {
      switch (b->node_type ())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_string:
	case AST_Decl::NT_union:
	  {
	    c->set_ada_local_name (get_ada_local_name ());
	    c->set_ada_full_name (get_ada_full_name ());
	    c->produce_ads (with, body, previous);
	    break;
	  }
	default:      
	  body += "   type " + get_ada_local_name () + " is new ";
	  string name = 
	    dynamic_cast<adabe_name *>(b)->dump_name (with, previous);
	  body += name;
	  body += ";\n\n";
	  break;
	}
    }
  else
    {
      body += "   type " + get_ada_local_name () + " is new ";
      string name =
	dynamic_cast<adabe_name *>(b)->dump_name (with, previous);
      body += name;
      body += ";\n\n";
    }
  if (!c->has_fixed_size ()) no_fixed_size ();
  switch (b->node_type ())
    {
    case AST_Decl::NT_typedef:
      set_number_value (dynamic_cast<adabe_typedef *>(b)->get_number_value ());
      break;
    case AST_Decl::NT_enum:
      set_number_value (dynamic_cast<adabe_enum *>(b)->get_number_value ());
      break;
    default:
      break;      
    }  
  set_already_defined ();
}

void
adabe_typedef::produce_stream_ads (dep_list & with,
				   string   & body,
				   string   & previous)
{
  AST_Decl *b = base_type ();
  if (((string) b->local_name ()->get_string ()) == "local type")
    {
      switch (b->node_type ())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_string:
	case AST_Decl::NT_union:
	  {
	    string arg2 = "";
	    adabe_name *c = dynamic_cast<adabe_name *>(b);
	    c->produce_stream_ads (with, body, arg2);
	    body += arg2;
	    set_already_defined ();
	    return;
	  }
	default : {}
	}
    }
  
  body += "   procedure Marshall\n";
  body += "     (A : in " + get_ada_local_name () + ";\n";
  body += "      S : in out AdaBroker.NetBufferedStream.Object'Class);\n\n";
  body += "   procedure Unmarshall\n";
  body += "     (A : out " + get_ada_local_name () + ";\n";
  body += "      S : in out AdaBroker.NetBufferedStream.Object'Class);\n\n";
  body += "   function Align_Size\n";
  body += "     (A              : in " + get_ada_local_name () + ";\n";
  body += "      Initial_Offset : in CORBA.Unsigned_Long;\n";
  body += "      N              : in CORBA.Unsigned_Long := 1)\n";
  body += "       return CORBA.Unsigned_Long;\n\n";

  set_already_defined ();
}

void
adabe_typedef::produce_stream_adb (dep_list & with,
				   string   & body,
				   string   & previous)
{
  string arg2 = "";
  AST_Decl *b = base_type ();
  if (((string) b->local_name ()->get_string ()) == "local type")
    {
      switch (b->node_type ())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_string:
	case AST_Decl::NT_union:
	  {
	    adabe_name *c = dynamic_cast<adabe_name *>(b);
	    c->produce_stream_adb (with, body, arg2);
	    body += arg2;
	    set_already_defined ();
	    return;
	  }
	  default : {}
	}
    }
  
  string name = (dynamic_cast<adabe_name *> (base_type ()))->marshal_name (with, arg2); 
  body += arg2;
  body += "   procedure Marshall\n";
  body += "     (A : in " + get_ada_local_name () + ";\n";
  body += "      S : in out AdaBroker.NetBufferedStream.Object'Class) is\n";
  body += "   begin\n";
  body += "      Marshall (" + name + "(A), S);\n";
  body += "   end;\n\n";
	    
  body += "   procedure Unmarshall\n";
  body += "     (A : out " + get_ada_local_name () + ";\n";
  body += "     S : in out AdaBroker.NetBufferedStream.Object'Class) is\n";
  body += "   begin\n";
  body += "      Unmarshall (" + name + "(A), S);\n";
  body += "   end;\n\n";
  
  body += "   function Align_Size\n";
  body += "     (A              : in " + get_ada_local_name () + ";\n";
  body += "      Initial_Offset : in CORBA.Unsigned_Long;\n";
  body += "      N              : in CORBA.Unsigned_Long := 1)\n";
  body += "      return CORBA.Unsigned_Long\n";
  body += "   is\n";
  body += "      Tmp : CORBA.Unsigned_Long := Initial_Offset;\n";
  body += "   begin\n";
  body += "      Tmp := Align_Size (" + name + "(A), Tmp);\n";
  body += "      return Tmp;\n";
  body += "   end;\n\n";

  set_already_defined ();
}

string
adabe_typedef::dump_name (dep_list & with,
			  string   & previous)
{
  if (!is_imported (with))
    {
      if (!is_already_defined ())
	{
	  string tmp = "";
	  produce_ads (with, tmp, previous);
	  previous += tmp;
	}
      return get_ada_local_name ();
    }
  return get_ada_full_name (); 

}

string
adabe_typedef::marshal_name (dep_list & with,
			     string   & previous)
{
  if (!is_marshal_imported (with))
    {
      if (!is_already_defined ())
	{
	  string tmp = "";
	  produce_stream_adb (with, tmp, previous);
	  previous += tmp;
	}
      return get_ada_local_name ();
    }
  return get_ada_full_name ();	   
}
IMPL_NARROW_METHODS1 (adabe_typedef, AST_Typedef)
IMPL_NARROW_FROM_DECL (adabe_typedef)
