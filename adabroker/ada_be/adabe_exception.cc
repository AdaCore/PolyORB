// File adabe_exception.cc
#include <adabe.h>

IMPL_NARROW_METHODS1(adabe_exception, AST_Exception);
IMPL_NARROW_FROM_DECL(adabe_exception);
IMPL_NARROW_FROM_SCOPE(adabe_exception);

void
adabe_exception::produce_ads (dep_list& with,string &body, string &previous)
{
  with.add ("Corba.Exceptions");
  with.add ("Ada.Exceptions");
  bool first = true;
  compute_ada_name ();
  
  // beginning of the exception declaration ...

  body+=  "   " + get_ada_local_name() + " : exception ;\n";
  
  body += "   type " + get_ada_local_name() +"_Members is new Corba.IDL_Exception_Members with ";
  
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
		body += "record\n" ;
		first = false;
	      }
	    adabe_field->produce_ads (with, body, previous);
	    break;
	  }
	default:
#ifdef DEBUG_EXCEPTION
	  cerr << "node type is : " << d->node_type() << endl;
#endif
	  throw adabe_internal_error (__FILE__,__LINE__,"unexpected decl in exception scope");
	}
    }
  if (first)
    {
      body += "null record ;\n\n";
    }
  else
    {
      body += "   end record ;\n\n";
    }
  
  body += "   ";
  body += get_ada_local_name();
  body += "_Repository_Id : Corba.String := Corba.To_Corba_String(\"";
  body += repositoryID();
  body += "\") ;\n";
  body += "   procedure Get_Members(From : in Ada.Exceptions.Exception_Occurrence ;\n";
  body += "                         To : out " + get_ada_local_name() + "_Members ) ;\n\n\n";
}

void
adabe_exception::produce_adb (dep_list& with,string &body, string &previous)
{
  body += "   procedure Get_Members(From: in Ada.Exceptions.Exception_Occurrence ;\n";
  body += "                         To: out " + get_ada_local_name() + "_Members) is\n";
  body += "   begin\n";
  body += "      Corba.Exceptions.Get_Members (From,To) ;\n";
  body += "   end ;\n\n\n";
}

void
adabe_exception::produce_skel_adb (dep_list &with, string &body)
{
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  bool has_member = !activator.is_done();

  string full_name = get_ada_full_name ();
  string pack = full_name.substr (0,full_name.find_last_of('.'));

  with.add (pack);
  body += "            when except : ";
  body += get_ada_local_name ();
  body += " =>\n";
  body += "               declare\n";
  body += "                  Repo_Id : Corba.String := ";
  body += get_ada_local_name ();
  body += "_Repository_Id ;\n";

  if (has_member) {
    body += "                  Member : ";
    body += get_ada_local_name ();
    body += "_Members ;\n";
  }
  body += "               begin\n";
  if (has_member) {
    body += "                  ";
    body += pack;
    body += ".Get_Members(except,Member) ;\n";
  }
  body += "                  -- compute the size of the replied message\n";
  body += "                  Mesg_Size := Giop_S.Reply_Header_Size ;\n";
  body += "                  Mesg_Size := Align_Size (Repo_Id, Mesg_Size) ;\n";
  if (has_member) {
    body += "                  Mesg_Size := Align_Size (Member, Mesg_Size) ;\n";
  }  
  body += "                  -- Initialisation of the reply\n";
  body += "                  Giop_S.Initialize_Reply (Orls, Giop.USER_EXCEPTION, Mesg_Size) ;\n";

  body += "                  -- Marshall the exception\n";

  body += "                  Marshall(Repo_Id, Orls) ;\n";
  if (has_member) {
    body += "                  Marshall(Member, Orls) ;\n";
  }
  body += "                  -- inform the orb\n";
  body += "                  Giop_S.Reply_Completed (Orls) ;\n";

  body += "                  Returns := True ;\n";
  body += "               end ;\n\n";
}

void
adabe_exception::produce_marshal_ads (dep_list& with,string &body, string &previous)
{
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  if (!activator.is_done())
    {
      body += "   procedure Marshall (A : in ";
      body += get_ada_local_name();
      body += "_Members ;\n";
      body += "                       S : in out Netbufferedstream.Object'Class) ;\n\n";
      
      body += "   procedure UnMarshall (A : out ";
      body += get_ada_local_name();
      body += "_Members ;\n";
      body += "                         S : in out Netbufferedstream.Object'Class) ;\n\n";
      
      body += "   function Align_Size (A : in ";
      body += get_ada_local_name();
      body += "_Members ;\n";
      body += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
      body += "                        N : in Corba.Unsigned_Long := 1)\n";
      body += "                        return Corba.Unsigned_Long ;\n\n\n";
    }
  set_already_defined ();
}

void
adabe_exception::produce_marshal_adb (dep_list& with,string &body, string &previous)
{
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  if (!activator.is_done())
    {
      string marshall = "";
      string unmarshall = "";
      string align_size = "";
      
      marshall += "   procedure Marshall(A : in ";
      marshall += get_ada_local_name();
      marshall += "_Members ;\n";
      marshall += "                      S : in out Netbufferedstream.Object'Class) is\n";
      marshall += "   begin\n";
      
      unmarshall += "   procedure UnMarshall(A : out ";
      unmarshall += get_ada_local_name();
      unmarshall += "_Members ;\n";
      unmarshall += "                        S : in out Netbufferedstream.Object'Class) is\n";
      unmarshall += "   begin\n";
      
      align_size += "   function Align_Size (A : in ";
      align_size += get_ada_local_name();
      align_size += "_Members ;\n";
      align_size += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
      align_size += "                        N : in Corba.Unsigned_Long := 1)\n";
      align_size += "                        return Corba.Unsigned_Long is\n";
      align_size += "      Tmp : Corba.Unsigned_Long := Initial_Offset ;\n";
      align_size += "   begin\n";
      align_size += "      for I in 1..N loop\n";
      
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
      marshall += "   end ;\n\n";
      unmarshall += "   end ;\n\n";
      align_size += "      end loop ;\n";
      align_size += "      return Tmp ;\n";
      align_size += "   end ;\n\n\n";
      
      body += marshall;
      body += unmarshall;
      body += align_size;
    }
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


