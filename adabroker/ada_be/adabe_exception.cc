/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_exception.cc                                      ***
***                                                                                            ***
***      This file provides the implementation of class adabe_exception  declared in adabe.h   ***
***   (L 359). This class is the correspondant of the Sun's Front-End class AST_Exception      ***
***   It provides produce functions for each generated file, a constructor and two little      ***
***   functions : dump_name and marshall_name whose job is to print the name of the type.      ***
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

IMPL_NARROW_METHODS1(adabe_exception, AST_Exception);
IMPL_NARROW_FROM_DECL(adabe_exception);
IMPL_NARROW_FROM_SCOPE(adabe_exception);

//////////////////////////////////////////////////////////////////
//////////////////////// produce_ads /////////////////////////////
//////////////////////////////////////////////////////////////////

void
adabe_exception::produce_ads (dep_list& with,string &body, string &previous)
{
  // Adding two packages needed for
  // the gestion of the exceptions
  with.add ("CORBA.Exceptions");
  with.add ("Ada.Exceptions");

  // first is used to determine if
  // the exception had memebers or not
  // at the end
  bool first = true;
    
  // beginning of the exception declaration

  body+=  "   " + get_ada_local_name() + " : exception ;\n";
  body += "   type " + get_ada_local_name() +"_Members is new CORBA.IDL_Exception_Members with ";
  
  // We must now map the members of
  // this exception
  
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
		// if it is the first, we must begin
		// the record
		body += "record\n" ;
		first = false;
	      }
	    // like for the structures we call
	    // the produce ads on the field
	    adabe_field->produce_ads (with, body, previous);
	    break;
	  }
	  
	default:
	  // throw an exception
	  throw adabe_internal_error (__FILE__,__LINE__,"unexpected decl in exception scope");
	} // of the switch
      activator.next();
    } // of the loop
  if (first)
    {
      // the exception has no member
      body += "null record ;\n\n";
    }
  else
    {
      // the exception has members
      body += "   end record ;\n\n";
    }

  // An exception need a RepositoryID that
  // designate it
  body += "   ";
  body += get_ada_local_name();
  body += "_Repository_Id : CORBA.String := CORBA.To_Corba_String(Standard.String'(\"";
  body += repositoryID();
  body += "\")) ;\n";

  // We need a function to get the memebers of the exception
  body += "   procedure Get_Members(From : in Ada.Exceptions.Exception_Occurrence ;\n";
  body += "                         To : out " + get_ada_local_name() + "_Members ) ;\n\n\n";
}

//////////////////////////////////////////////////////////////////
////////////////////     produce_adb      ////////////////////////
//////////////////////////////////////////////////////////////////

void
adabe_exception::produce_adb (dep_list& with,string &body, string &previous)
{
  // the excetion has only one operation to map:
  // the one which gets his members
  body += "   procedure Get_Members(From: in Ada.Exceptions.Exception_Occurrence ;\n";
  body += "                         To: out " + get_ada_local_name() + "_Members) is\n";
  body += "   begin\n";
  body += "      CORBA.Exceptions.Get_Members (From,To) ;\n";
  body += "   end ;\n\n\n";
}

//////////////////////////////////////////////////////////////////
////////////////       produce_skel_adb       ////////////////////
//////////////////////////////////////////////////////////////////

void
adabe_exception::produce_skel_adb (dep_list &with, string &body)
{
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  bool has_member = !activator.is_done();

  // the full name of the interface
  string full_name = get_ada_full_name ();
  // the name of the package containing the interface
  string pack = full_name.substr (0,full_name.find_last_of('.'));

  // we need to add the package, in which the
  // exception is defined
  with.add (pack);

  // We now map the interface
  body += "            when except : ";
  body += get_ada_local_name ();
  body += " =>\n";
  body += "               declare\n";
  body += "                  Repo_Id : CORBA.String := ";
  body += get_ada_local_name ();
  body += "_Repository_Id ;\n";
  body += "                  Mesg_Size : CORBA.Unsigned_Long ;\n";

  
  if (has_member) {
    // signalising the presence of members
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
  body += "                  Mesg_Size := AdaBroker.GIOP_S.Reply_Header_Size ;\n";
  body += "                  Mesg_Size := Align_Size (Repo_Id, Mesg_Size) ;\n";
  if (has_member) {
    // if there are members, we must add them when computing the size
    body += "                  Mesg_Size := Align_Size (Member, Mesg_Size) ;\n";
  }  
  body += "                  -- Initialisation of the reply\n";
  body += "                  AdaBroker.GIOP_S.Initialize_Reply (Orls, AdaBroker.GIOP.USER_EXCEPTION, Mesg_Size) ;\n";

  body += "                  -- Marshall the exception\n";

  body += "                  Marshall(Repo_Id, Orls) ;\n";
  if (has_member) {
    body += "                  Marshall(Member, Orls) ;\n";
  }
  body += "                  -- inform the orb\n";
  body += "                  AdaBroker.GIOP_S.Reply_Completed (Orls) ;\n";

  body += "                  Dispatch_Returns := True ;\n";
  body += "                  return ;\n";
  body += "               end ;\n\n";
}

////////////////////////////////////////////////////////////////
//////////////       produce_proxies_adb         ///////////////    
////////////////////////////////////////////////////////////////
void
adabe_exception::produce_proxies_adb (dep_list &with, string &body)
{
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  bool has_member = !activator.is_done();

  string full_name = get_ada_full_name ();
  string pack = full_name.substr (0,full_name.find_last_of('.'));

  // Preparing the dispatch of the
  // exception
  with.add (pack);
  body += "      if RepoId = \"";
  body += repositoryID ();
  body += "\" then \n";
  body += "         declare\n";
  body += "            member : ";
  body += get_ada_local_name ();
  body += "_Members ;\n";
  body += "         begin\n";
  if (has_member)
    {
      // we need to raise a special exception:
      // it can't be mapped directly into an
      // ADA exception
      body += "            Unmarshall(member,GIOP_Client) ;\n";
      body += "            CORBA.Raise_Corba_Exception(";
      body += get_ada_local_name ();
      body += "'Identity,\n";
      body += "                                        member) ;\n";
    }
  else
    {
      // this time we can simply raise the exception
      body += "            raise ";
      body += get_ada_local_name ();
      body += " ;\n";
    }
  body += "         end ;\n";
  body += "      end if ;\n\n";
}

////////////////////////////////////////////////////////////////
//////////////       produce_marshall_ads        ///////////////    
////////////////////////////////////////////////////////////////

void
adabe_exception::produce_marshal_ads (dep_list& with,string &body, string &previous)
{
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  // we must define marshall, unmarshall and align size
  // only if there's an argument in the scope of the exception
  if (!activator.is_done())
    {
      body += "   procedure Marshall (A : in ";
      body += get_ada_local_name();
      body += "_Members ;\n";
      body += "                       S : in out AdaBroker.NetBufferedStream.Object'Class) ;\n\n";
      
      body += "   procedure Unmarshall (A : out ";
      body += get_ada_local_name();
      body += "_Members ;\n";
      body += "                         S : in out AdaBroker.NetBufferedStream.Object'Class) ;\n\n";
      
      body += "   function Align_Size (A : in ";
      body += get_ada_local_name();
      body += "_Members ;\n";
      body += "                        Initial_Offset : in CORBA.Unsigned_Long ;\n";
      body += "                        N : in CORBA.Unsigned_Long := 1)\n";
      body += "                        return CORBA.Unsigned_Long ;\n\n\n";
    }
  set_already_defined ();
}

////////////////////////////////////////////////////////////////
//////////////       produce_marshall_adb        ///////////////    
////////////////////////////////////////////////////////////////
void
adabe_exception::produce_marshal_adb (dep_list& with,string &body, string &previous)
{
  // we must define marshall, unmarshall and align size
  // only if there's an argument in the scope of the exception
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  if (!activator.is_done())
    {
      // local strings used to compute the
      // three different function in the
      // same time
      string marshall = "";
      string unmarshall = "";
      string align_size = "";

      // declaration of the function marshal
      marshall += "   procedure Marshall(A : in ";
      marshall += get_ada_local_name();
      marshall += "_Members ;\n";
      marshall += "                      S : in out AdaBroker.NetBufferedStream.Object'Class) is\n";
      marshall += "   begin\n";

      // declaration of the function unmarshall
      unmarshall += "   procedure Unmarshall(A : out ";
      unmarshall += get_ada_local_name();
      unmarshall += "_Members ;\n";
      unmarshall += "                        S : in out AdaBroker.NetBufferedStream.Object'Class) is\n";
      unmarshall += "   begin\n";

      // declaration of the function align_size
      align_size += "   function Align_Size (A : in ";
      align_size += get_ada_local_name();
      align_size += "_Members ;\n";
      align_size += "                        Initial_Offset : in CORBA.Unsigned_Long ;\n";
      align_size += "                        N : in CORBA.Unsigned_Long := 1)\n";
      align_size += "                        return CORBA.Unsigned_Long is\n";
      align_size += "      Tmp : CORBA.Unsigned_Long := Initial_Offset ;\n";
      align_size += "   begin\n";
      align_size += "      for I in 1..N loop\n";
      
      // we must now call the function
      // over each field in the scope
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
	  // has this exception already been defined ?
	  string tmp = "";
	  produce_ads(with, tmp, previous);
	  previous += tmp;
	}
      // this exception is defined in this file, so
      // a local name is enough
      return get_ada_local_name();
    }
  // the exception is defined in aother file
  // we need to use a full name
  return get_ada_full_name();	   
}

string
adabe_exception::marshal_name(dep_list& with,string &previous) 
{
  if (!is_marshal_imported(with))
    {
      if (!is_already_defined())
	{
	  // has this exception already been defined ?
	  string tmp = "";
	  produce_marshal_adb(with, tmp, previous);
	  previous += tmp;
	}
      // this exception is defined in this file, so
      // a local name is enough
      return get_ada_local_name();
    }
  // because the exception is defined in another file
  // we need to use a full name
  return get_ada_full_name();	   
}

adabe_exception::adabe_exception(UTL_ScopedName *n, UTL_StrList *p)
  : AST_Decl(AST_Decl::NT_except, n, p),
    AST_Structure(AST_Decl::NT_except, n, p),
    UTL_Scope(AST_Decl::NT_except),
    adabe_name(AST_Decl::NT_except, n, p)
{
}


