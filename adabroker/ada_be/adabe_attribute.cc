#include <adabe.h>

////////////////////////////////////////////////////////////////////////
////////////////      constructor    ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
adabe_attribute::adabe_attribute(idl_bool ro, AST_Type *ft, UTL_ScopedName *n, UTL_StrList *p)
  : AST_Attribute(ro,ft,n,p),
    AST_Field(AST_Decl::NT_attr,ft,n,p),
    AST_Decl(AST_Decl::NT_attr,n,p),
    adabe_name(AST_Decl::NT_attr,n,p)
{
  // there's nothing specific to be done
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_ads     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_ads(dep_list& with, string &body, string &previous)
{
  //  compute_ada_name();

  // computing an empty string with
  // the same length  as the attribute name
  string space = "";
  for (unsigned int i=0;i<get_ada_local_name().length();i++) space += " ";

  // writing a function to read the attribute
  body += "   function Get_" + get_ada_local_name() +"(Self : in Ref)\n";
  body += "                 " + space + "return ";
  AST_Decl *d = field_type();
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  body += name + " ;\n\n"; // name is the attribute type

  // if the argument is not read only
  // we provide a function to set him
  // to a value
  if (!readonly())
    {
      body += "   procedure Set_" + get_ada_local_name();
      body += "(Self : in Ref ;\n";
      body += "                  " + space + "To : in ";
      body += name;
      body += ") ;\n\n\n";
    } else body += "\n";
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_adb     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_adb(dep_list& with, string &body, string &previous)
{
  with.add ("AdaBroker.OmniProxyCallWrapper");

  // computing an empty string with
  // the same length  as the attribute name
  string space = "";
  for (unsigned int i=0;i<get_ada_local_name().length();i++) space += " ";

  // Writing the Get_function (client-side)
  body += "   function Get_" + get_ada_local_name() +"(Self : in Ref)\n";
  body += "                 " + space + "return "; 
  AST_Decl *d = field_type();  
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  body += name + " is\n";
  string name_of_the_package = dynamic_cast<adabe_name *>(ScopeAsDecl(defined_in()))->get_ada_full_name();
  body += "      Opcd : " + name_of_the_package + ".Proxies.Get_" + get_ada_local_name() + "_Proxy ;\n";
  body += "   begin \n";
  body += "      " + name_of_the_package + ".Proxies.Init(Opcd) ;\n";
  body += "      AdaBroker.OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
  body += "      return " + name_of_the_package + ".Proxies.Get_Result(Opcd) ;\n";
  body += "   end ;\n\n\n";

  // Writing the Set function (if necessary)
  // for the client side
  if (!readonly())
    {
      body += "   procedure Set_";
      body += get_ada_local_name();
      body += "(Self : in Ref ;\n";
      body += "                  " + space + "To : in ";
      body += name;
      body += ") is \n";
      body += "      Opcd : ";
      body += name_of_the_package;
      body += ".Proxies.Set_";
      body += get_ada_local_name();
      body += "_Proxy ;\n";
      body += "   begin \n";
      body += "      " + name_of_the_package + ".Proxies.Init(Opcd, To) ;\n";
      body += "      AdaBroker.OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
      body += "   end ;\n\n\n";    
    }
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_impl_ads     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_impl_ads(dep_list& with, string &body, string &previous)
{
  // computing an empty string with
  // the same length  as the attribute name
  string space = "";
  for (unsigned int i=0;i<get_ada_local_name().length();i++) space += " ";

  // writing the get function for server side
  body += "   function Get_" + get_ada_local_name() +"(Self : access Object) return "; 
  AST_Decl *d = field_type();
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  body += name + " ;\n\n";

  // and the set function for the server side
  if (!readonly())
    {
      body += "   procedure Set_" + get_ada_local_name();
      body += "(Self : access Object ;\n";
      body += space + "                  To : in ";
      body += name;
      body += " ) ;\n\n";
    }
  
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_impl_adb     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_impl_adb(dep_list& with, string &body, string &previous)
{
  // it's up to the programmer
  // to implement the functions
  body += "   function Get_" + get_ada_local_name() +"(Self : access Object) return ";
  AST_Decl *d = field_type();
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  body += name + " is\n";
  body += "   begin\n";
  body += "   end ;\n\n\n";
  
  if (!readonly())
    {
      body += "   procedure Set_" + get_ada_local_name() +"(Self : access Object ; To : in ";
      body += name;
      body += ") is\n";
      body += "   begin\n";
      body += "   end ;\n\n\n"; 
    } 
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_proxies_ads     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_proxies_ads(dep_list& with, string &body, string &private_definition)
{
  // In this function, there' no need for
  // previous definition (all the types
  // are allready defined. But we need to
  // have two parts (a public and a private part)
  AST_Decl *d = field_type();
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, private_definition);

  // Declaration of the get function :
  // first the public part
  body += "   type Get_" + get_ada_local_name() +"_Proxy is\n";
  body += "      new AdaBroker.OmniProxyCallDesc.Object with private;\n\n";
  body += "   procedure Init\n";
  body += "     (Self : in out Get_" + get_ada_local_name() + "_Proxy);\n\n";
  body += "   function Operation\n";
  body += "     (Self : in Get_" + get_ada_local_name() + "_Proxy)\n";
  body += "      return CORBA.String;\n\n" ;
  body += "   procedure Unmarshal_Returned_Values\n";
  body += "     (Self : in out Get_" + get_ada_local_name() + "_Proxy;\n";
  body += "      GIOP_Client : in out AdaBroker.GIOP_C.Object);\n\n";
  body += "   function Get_Result\n";
  body += "     (Self : in Get_" + get_ada_local_name() + "_Proxy )\n";
  body += "      return " +  name + "; \n\n\n";

  // next the private part
  private_definition += "   type Get_" + get_ada_local_name() + "_Proxy is\n";
  private_definition += "      new AdaBroker.OmniProxyCallDesc.Object\n";
  private_definition += "      with record\n";
  private_definition += "         Private_Result : " + name + ";\n";
  private_definition += "      end record ;\n";
  private_definition += "   procedure Finalize\n";
  private_definition += "     (Self : in out Get_" + get_ada_local_name() + "_Proxy);\n\n";

  // Declaration of the set function
  if (!readonly())
    {
      // the public part
      body += "   type Set_" + get_ada_local_name() +"_Proxy is\n";
      body += "      new AdaBroker.OmniProxyCallDesc.Object\n";
      body += "      with private;\n\n";
      body += "   procedure Init\n";
      body += "     (Self : in out Set_" + get_ada_local_name() + "_Proxy;\n";
      body += "      Arg  : in " + name + ");\n\n";
      body += "   function Operation\n";
      body += "     (Self : in Set_" + get_ada_local_name() + "_Proxy)\n";
      body += "      return CORBA.String;\n\n" ;
      body += "   function Align_Size\n";
      body += "     (Self : in Set_" + get_ada_local_name() + "_Proxy;\n";
      body += "      Size_In : in CORBA.Unsigned_Long)\n";
      body += "      return CORBA.Unsigned_Long ;\n\n";
      body += "   procedure Marshal_Arguments\n";
      body += "     (Self : in Set_" + get_ada_local_name() + "_Proxy;\n";
      body += "      GIOP_Client : in out AdaBroker.GIOP_C.Object);\n\n";

      // the private part
      private_definition += "   type Set_" + get_ada_local_name() + "_Proxy is\n";
      private_definition += "      new AdaBroker.OmniProxyCallDesc.Object\n";
      private_definition += "      with record \n";
      private_definition += "      Arg : " + name + ";\n";
      private_definition += "   end record ;\n";
      private_definition += "   procedure Finalize\n";
      private_definition += "     (Self : in out Set_" + get_ada_local_name() + "_Proxy) ;\n\n";
    }  
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_proxies_adb     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_proxies_adb(dep_list &with, string &body, string &private_definition)
{
  AST_Decl *d = field_type();
  adabe_name *att = dynamic_cast<adabe_name *>(d);
  string name = att->dump_name(with, private_definition);

  // adding the two needed libraries :
  att->is_marshal_imported(with);
  with.add("AdaBroker.NetBufferedStream") ;

  // body of the functions defined in
  // the -proxies.ads file
  body += "   procedure Init\n";
  body += "     (Self : in out Get_" + get_ada_local_name() + "_Proxy) is\n";
  body += "   begin\n";
  body += "      Set_User_Exceptions (Self, False);\n";
  body += "   end Init;\n\n\n";
  body += "   function Operation\n";
  body += "     (Self : in Get_" + get_ada_local_name() + "_Proxy)\n";
  body += "      return CORBA.String is\n";
  body += "   begin\n";
  body += "      return CORBA.To_Corba_String\n";
  body += "         (Standard.String'(\"_get_" + get_ada_local_name() + "\"));\n";
  body += "   end Operation;\n\n\n";
  body += "   procedure Unmarshal_Returned_Values\n";
  body += "     (Self : in out Get_" + get_ada_local_name() + "_Proxy;\n";
  body += "      GIOP_Client : in out AdaBroker.GIOP_C.Object) is\n";
  body += "      Result : " + name + ";\n";
  body += "   begin\n";
  body += "      Unmarshall (Result, GIOP_client);\n";
  body += "      Self.Private_Result := Result;\n";
  body += "   end Unmarshal_Returned_Values;\n\n\n" ;
  body += "   function Get_Result\n";
  body += "     (Self : in Get_" + get_ada_local_name() + "_Proxy)\n";
  body += "      return " +  name + " is\n";
  body += "   begin\n";
  body += "      return Self.Private_Result;\n";
  body += "   end Get_Result;\n\n\n";
  body += "   procedure Finalize\n";
  body += "     (Self : in out Get_" + get_ada_local_name() + "_Proxy) is\n";
  body += "   begin\n";
  body += "      null;\n";
  body += "   end Finalize;\n\n\n";

  if (!readonly())
    {
      // the same for the set function
      body += "   procedure Init\n";
      body += "     (Self : in out Set_" + get_ada_local_name() + "_Proxy;\n";
      body += "      Arg : in " + name + ") is\n";
      body += "   begin\n";
      body += "      Set_User_Exceptions (Self, False);\n";
      body += "      Self.Arg := Arg;\n";
      body += "   end Init;\n\n\n";
      body += "   function Operation\n";
      body += "     (Self : in Set_" + get_ada_local_name() + "_Proxy)\n";
      body += "      return CORBA.String is\n";
      body += "   begin\n";
      body += "      return CORBA.To_Corba_String\n";
      body += "         (Standard.String'(\"_set_" + get_ada_local_name() + "\"));\n";
      body += "   end Operation;\n\n\n";
      body += "   function Align_Size\n";
      body += "     (Self : in Set_" + get_ada_local_name() + "_Proxy;\n";
      body += "      Size_In : in CORBA.Unsigned_Long)\n";
      body += "      return CORBA.Unsigned_Long is\n";
      body += "   begin\n";
      body += "      return Align_Size (Self.Arg, Size_In);\n";
      body += "   end Align_Size;\n\n\n";
      body += "   procedure Marshal_Arguments\n";
      body += "     (Self : in Set_" +	get_ada_local_name() + "_Proxy;\n";
      body += "      GIOP_Client : in out AdaBroker.GIOP_C.Object) is\n";
      body += "   begin\n";
      body += "      Marshall (Self.Arg, GIOP_client);\n";
      body += "   end Marshal_Arguments;\n\n\n";
      body += "   procedure Finalize\n";
      body += "     (Self : in out Set_" + get_ada_local_name() + "_Proxy) is\n";
      body += "   begin\n";
      body += "      null;\n";
      body += "   end Finalize;\n\n\n";
    }  
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_skel_adb     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_skel_adb(dep_list& with, string &body, string &private_definition)
{
  AST_Decl *d = field_type();
  adabe_name *e = dynamic_cast<adabe_name *>(d);
  string type_name = e->dump_name(with, private_definition);
  e->is_marshal_imported(with);
  string name = get_ada_local_name ();
  string full_name = get_ada_full_name ();
  string pack_name = full_name.substr(0,full_name.find_last_of('.')) ;

  body += "      if Orl_Op = \"_get_";
  body += name;
  body += "\" then\n";
  body += "         declare\n";
  body += "            Result : ";
  body += type_name;
  body += " ;\n";
  body += "            Size : CORBA.Unsigned_Long ;\n";
  body += "         begin\n";

  body += "            -- change state\n";
  body += "            AdaBroker.GIOP_S.Request_Received(Orls) ;\n";

  body += "            -- call the implementation\n";
  body += "            Result := ";
  body += pack_name;
  body += ".Impl.Get_";
  body += name;
  body += "(Self) ;\n";

  body += "            -- compute the size of the replied message\n";
  body += "            Size := AdaBroker.GIOP_S.Reply_Header_Size ;\n";
  body += "            Size := Align_Size (Result, Size) ;\n";

  body += "            -- Initialisation of the reply\n";
  body += "            AdaBroker.GIOP_S.Initialize_Reply (Orls, AdaBroker.GIOP.NO_EXCEPTION, Size) ;\n";

  body += "            -- Marshall the arguments\n";
  body += "            Marshall (Result, Orls) ;\n";

  body += "            -- inform the orb\n";
  body += "            AdaBroker.GIOP_S.Reply_Completed (Orls) ;\n";

  body += "            Dispatch_Returns := True ;\n";
  body += "            return ;\n";
  body += "         end ;\n";
  body += "      end if ;\n\n";

  if (!readonly())
    {
      body += "      if Orl_Op = \"_set_";
      body += name;
      body += "\" then\n";
      body += "         declare\n";
      body += "            Mesg : ";
      body += type_name;
      body += " ;\n";
      body += "            Size : CORBA.Unsigned_Long ;\n";
      body += "         begin\n";
      
      body += "            -- unmarshalls arguments\n";
      body += "            Unmarshall (Mesg,Orls) ;\n";
      
      body += "            -- change state\n";
      body += "            AdaBroker.GIOP_S.Request_Received(Orls) ;\n";
      
      body += "            -- call the implementation\n";
      body += "            ";
      body += pack_name;
      body += ".Impl.Set_";
      body += name;
      body += "(Self,Mesg) ;\n";
      
      body += "            -- compute the size of the replied message\n";
      body += "            Size := AdaBroker.GIOP_S.Reply_Header_Size ;\n";
      body += "            -- Initialisation of the reply\n";
      body += "            AdaBroker.GIOP_S.Initialize_Reply (Orls, AdaBroker.GIOP.NO_EXCEPTION, Size) ;\n";
      body += "            -- inform the orb\n";
      body += "            AdaBroker.GIOP_S.Reply_Completed (Orls) ;\n";
      
      body += "            Dispatch_Returns := True ;\n";
      body += "            return ;\n";
      body += "         end ;\n";
      body += "      end if ;\n\n";
    }
}
/*
////////////////////////////////////////////////////////////////////////
////////////////     produce_marshal_adb     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_marshal_adb(dep_list &with, string &body, string &previous)
{
  // this function simply add
  // the marshal function for
  // the field type 
  AST_Decl *d = field_type();
  adabe_name *e = dynamic_cast<adabe_name *>(d);
  if (!e->is_marshal_imported(with))
    {
      if (!e->is_already_defined())
	{
	  string tmp = "";
	  e->produce_marshal_adb(with, tmp, previous);
	  previous += tmp;
	}
    }
}
*/
////////////////////////////////////////////////////////////////////////
////////////////     miscellaneous           ///////////////////////////
////////////////////////////////////////////////////////////////////////
IMPL_NARROW_METHODS1(adabe_attribute, AST_Attribute)
IMPL_NARROW_FROM_DECL(adabe_attribute)
IMPL_NARROW_FROM_SCOPE(adabe_attribute)
















