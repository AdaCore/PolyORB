#include <adabe.h>

IMPL_NARROW_METHODS1(adabe_exception, AST_Exception);
IMPL_NARROW_FROM_DECL(adabe_exception);
IMPL_NARROW_FROM_SCOPE(adabe_exception);

//////////////////////////////////////////////////////////////////
//////////////////////// produce_ads /////////////////////////////
//////////////////////////////////////////////////////////////////

void
adabe_exception::produce_ads (dep_list & with,
			      string   & body,
			      string   & previous)
{
  // Add two packages needed to handle exceptions.
  with.add ("AdaBroker.Exceptions");
  with.add ("Ada.Exceptions");

  // first is used to determine whether an exception has members or not.
  bool first = true;
    
  // Beginning of the exception declaration.
  body += "   " + get_ada_local_name() + " : exception;\n";
  body += "   type " + get_ada_local_name() +"_Members is\n";
  body += "      new CORBA.IDL_Exception_Members with ";
  
  // Map the members of this exception.
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  while (!activator.is_done())
    {
      AST_Decl *d = activator.item();
      switch(d->node_type())
	{
	case AST_Decl::NT_field:
	  {
	    adabe_name *adabe_field =  dynamic_cast<adabe_name *> (d);
	    if (first)
	      {
		// Use "record" for the first member.
		body += "record\n";
		first = false;
	      }
	    // Like structures, call the produce ads on the field.
	    adabe_field->produce_ads (with, body, previous);
	    break;
	  }
	  
	default:
	  // Throw an exception.
	  throw adabe_internal_error
	    (__FILE__,__LINE__,"unexpected decl in exception scope");
	}
      activator.next();
    } //
  if (first)
    {
      // The exception has no member.
      body += "null record;\n\n";
    }
  else
    {
      // the exception has members
      body += "   end record;\n\n";
    }

  // An exception needs a RepositoryID that designate it.
  body += "   " + get_ada_local_name();
  body += "_Repository_Id : CORBA.String\n";
  body += "      := CORBA.To_CORBA_String(\"";
  body += repositoryID();
  body += "\");\n";

  // We need a function to get the members of an exception.
  body += "   procedure Get_Members\n";
  body += "     (From : in Ada.Exceptions.Exception_Occurrence;\n";
  body += "      To   : out " + get_ada_local_name() + "_Members);\n\n\n";
}

//////////////////////////////////////////////////////////////////
////////////////////     produce_adb      ////////////////////////
//////////////////////////////////////////////////////////////////

void
adabe_exception::produce_adb(dep_list & with,
			     string   & body,
			     string   & previous)
{
  // The exception has only one operation to map: Get_Members.
  body += "   procedure Get_Members\n";
  body += "     (From : in Ada.Exceptions.Exception_Occurrence;\n";
  body += "      To   : out " + get_ada_local_name() + "_Members) is\n";
  body += "   begin\n";
  body += "      To := " + get_ada_local_name() + "_Members\n";
  body += "         (AdaBroker.Exceptions.Get_Members (From));\n";
  body += "   end Get_Members;\n\n\n";
}

//////////////////////////////////////////////////////////////////
////////////////       produce_skel_adb       ////////////////////
//////////////////////////////////////////////////////////////////

void
adabe_exception::produce_skel_adb(dep_list & with,
				  string   & body)
{
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  bool has_member = !activator.is_done();

  // Full name of the interface.
  string full_name = get_ada_full_name ();

  // Name of the package containing the interface.
  string pack = full_name.substr (0,full_name.find_last_of('.'));

  // Add the package in which the exception is defined.
  with.add (pack);

  // We now map the interface
  body += "            when E : ";
  body += get_ada_local_name ();
  body += " =>\n";
  body += "               declare\n";
  body += "                  RepoID : CORBA.String\n";
  body += "                     := ";
  body += get_ada_local_name ();
  body += "_Repository_Id;\n";
  body += "                  Size   : CORBA.Unsigned_Long;\n";

  
  if (has_member) {
    // Declare members.
    body += "                  Member : ";
    body += get_ada_local_name ();
    body += "_Members;\n";
  }
  body += "               begin\n";
  if (has_member) {
    body += "                  ";
    body += pack;
    body += ".Get_Members (E, Member);\n";
  }
  body += "                  -- Compute reply size\n";
  body += "                  Size := AdaBroker.GIOP_S.Reply_Header_Size;\n";
  body += "                  Size := Align_Size (RepoID, Size);\n";
  if (has_member) {
    // If there are members, add them when computing the size
    body += "                  Size := Align_Size (Member, Size);\n";
  }  
  body += "                  -- Initialize reply\n";
  body += "                  AdaBroker.GIOP_S.Initialize_Reply\n";
  body += "                    (Orls,\n";
  body += "                     AdaBroker.GIOP.User_Exception,\n";
  body += "                     Size);\n";

  body += "                  -- Marshall the exception\n";

  body += "                  Marshall (RepoID, Orls);\n";
  if (has_member) {
    body += "                  Marshall (Member, Orls);\n";
  }
  body += "                  -- Inform ORB\n";
  body += "                  AdaBroker.GIOP_S.Reply_Completed (Orls);\n";

  body += "                  Dispatch_Returns := True;\n";
  body += "                  return;\n";
  body += "               end;\n\n";
}

////////////////////////////////////////////////////////////////
//////////////       produce_proxies_adb         ///////////////    
////////////////////////////////////////////////////////////////
void
adabe_exception::produce_proxies_adb(dep_list & with,
				     string   & body)
{
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  bool has_member = !activator.is_done();

  string full_name = get_ada_full_name ();
  string pack = full_name.substr (0,full_name.find_last_of('.'));

  // Prepare the dispatch of the exception.
  with.add (pack);
  body += "      if RepoID = \"";
  body += repositoryID ();
  body += "\" then \n";
  body += "         declare\n";
  body += "            Member : ";
  body += get_ada_local_name ();
  body += "_Members;\n";
  body += "         begin\n";
  if (has_member)
    {
      // We need to raise a special exception : it can't be mapped
      // directly to an Ada exception.
      body += "            Unmarshall (Member, GIOP_Client);\n";
      body += "            AdaBroker.Exceptions.Raise_CORBA_Exception\n";
      body += "               (";
      body += get_ada_local_name ();
      body += "'Identity,\n";
      body += "                Member);\n";
    }
  else
    {
      // This time, we can simply raise the exception.
      body += "            raise ";
      body += get_ada_local_name ();
      body += ";\n";
    }
  body += "         end;\n";
  body += "      end if;\n\n";
}

////////////////////////////////////////////////////////////////
//////////////       produce_marshall_ads        ///////////////    
////////////////////////////////////////////////////////////////

void
adabe_exception::produce_marshal_ads(dep_list & with,
				     string   & body,
				     string   & previous)
{
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  // We must define marshall, unmarshall and align size only if
  // there's an argument in the scope of the exception.
  if (!activator.is_done())
    {
      body += "   procedure Marshall\n";
      body += "     (A : in ";
      body += get_ada_local_name();
      body += "_Members;\n";
      body += "      S : in out ";
      body += "AdaBroker.NetBufferedStream.Object'Class);\n\n";
      
      body += "   procedure Unmarshall\n";
      body += "     (A : out ";
      body += get_ada_local_name();
      body += "_Members;\n";
      body += "      S : in out ";
      body += "AdaBroker.NetBufferedStream.Object'Class);\n\n";
      
      body += "   function Align_Size\n";
      body += "     (A              : in ";
      body += get_ada_local_name();
      body += "_Members;\n";
      body += "      Initial_Offset : in CORBA.Unsigned_Long;\n";
      body += "      N              : in CORBA.Unsigned_Long := 1)\n";
      body += "      return CORBA.Unsigned_Long;\n\n\n";
    }
  set_already_defined ();
}

////////////////////////////////////////////////////////////////
//////////////       produce_marshall_adb        ///////////////    
////////////////////////////////////////////////////////////////
void
adabe_exception::produce_marshal_adb(dep_list & with,
				     string   & body,
				     string   & previous)
{
  // We must define marshall, unmarshall and align size only if
  // there's an argument in the scope of the exception.
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  if (!activator.is_done())
    {
      // local strings used to compute the three different functions.
      string marshall = "";
      string unmarshall = "";
      string align_size = "";

      // declaration of the function marshall.
      marshall += "   procedure Marshall\n";
      marshall += "     (A : in ";
      marshall += get_ada_local_name();
      marshall += "_Members;\n";
      marshall += "      S : in out ";
      marshall += "AdaBroker.NetBufferedStream.Object'Class) is\n";
      marshall += "   begin\n";

      // Declaration of the function unmarshall.
      unmarshall += "   procedure Unmarshall\n";
      unmarshall += "     (A : out ";
      unmarshall += get_ada_local_name();
      unmarshall += "_Members;\n";
      unmarshall += "      S : in out ";
      unmarshall += "AdaBroker.NetBufferedStream.Object'Class) is\n";
      unmarshall += "   begin\n";

      // Declaration of the function align_size.
      align_size += "   function Align_Size\n";
      align_size += "     (A              : in ";
      align_size += get_ada_local_name();
      align_size += "_Members;\n";
      align_size += "      Initial_Offset : in CORBA.Unsigned_Long;\n";
      align_size += "      N              : in CORBA.Unsigned_Long := 1)\n";
      align_size += "      return CORBA.Unsigned_Long is\n";
      align_size += "      Tmp : CORBA.Unsigned_Long := Initial_Offset;\n";
      align_size += "   begin\n";
      align_size += "      for I in 1 .. N loop\n";
      
      // We must now call the function over each field in the scope.
      while (!activator.is_done())
	{
	  AST_Decl *d = activator.item();
	  switch(d->node_type())
	    {
	    case AST_Decl::NT_field:
	      {
		(dynamic_cast<adabe_field *>(d))->produce_marshal_adb
		  (with, body, marshall, unmarshall, align_size);
		break;
	      }
	    default:
	      cerr << d->node_type() << endl;
	      throw adabe_internal_error
		(__FILE__,__LINE__,"unexpected decl in exception scope");
	    }      
	  activator.next();
	}
      marshall += "   end;\n\n";
      unmarshall += "   end;\n\n";
      align_size += "      end loop;\n";
      align_size += "      return Tmp;\n";
      align_size += "   end;\n\n\n";
      
      body += marshall;
      body += unmarshall;
      body += align_size;
    }
  set_already_defined();
}

string
adabe_exception::dump_name(dep_list & with,
			   string   & previous) 
{
  if (!is_imported(with))
    {
      if (!is_already_defined())
	{
	  // Has this exception already been defined ?
	  string tmp = "";
	  produce_ads(with, tmp, previous);
	  previous += tmp;
	}
      // This exception is defined in this file. Use local name.
      return get_ada_local_name();
    }
  // The exception is defined in another file. Use full name.
  return get_ada_full_name();	   
}

string
adabe_exception::marshal_name(dep_list& with,string &previous) 
{
  if (!is_marshal_imported(with))
    {
      if (!is_already_defined())
	{
	  // Has this exception already been defined ?
	  string tmp = "";
	  produce_marshal_adb(with, tmp, previous);
	  previous += tmp;
	}
      // This exception is defined in this file. se local name.
      return get_ada_local_name();
    }
  // The exception is defined in another file. Use full name.
  return get_ada_full_name();	   
}

adabe_exception::adabe_exception(UTL_ScopedName *n, UTL_StrList *p)
  : AST_Decl(AST_Decl::NT_except, n, p),
    AST_Structure(AST_Decl::NT_except, n, p),
    UTL_Scope(AST_Decl::NT_except),
    adabe_name(AST_Decl::NT_except, n, p)
{
}


