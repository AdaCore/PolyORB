/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_attribute.cc                                      ***
***                                                                                            ***
***      This file provides the implementation of class adabe_attribute declared in adabe.h    ***
***   (L 443). This class is the correspondant of the Sun's Front-End class AST_Attribute.     ***
***   It provides produce functions for each generated file and a constructor.                 ***
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
adabe_attribute::adabe_attribute(idl_bool ro, AST_Type *ft, UTL_ScopedName *n, UTL_StrList *p)
  : AST_Attribute(ro,ft,n,p),
    AST_Field(AST_Decl::NT_attr,ft,n,p),
    AST_Decl(AST_Decl::NT_attr,n,p),
    adabe_name(AST_Decl::NT_attr,n,p)
{  
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_ads     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_ads(dep_list& with, string &body, string &previous)
{
  string space = "";
  for (unsigned int i=0;i<get_ada_local_name().length();i++) space += " ";
  compute_ada_name();
  body += "   function Get_" + get_ada_local_name() +"(Self : in Ref)\n";
  body += "                 " + space + "return "; 
  AST_Decl *d = field_type();
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  body += name + " ;\n\n";
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
  with.add ("OmniProxyCallWrapper");
  string space = "";
  for (unsigned int i=0;i<get_ada_local_name().length();i++) space += " ";
  body += "   function Get_" + get_ada_local_name() +"(Self : in Ref)\n";
  body += "                 " + space + "return "; 
  AST_Decl *d = field_type();  
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  body += name + " is\n";  
  string name_of_the_package = dynamic_cast<adabe_name *>(ScopeAsDecl(defined_in()))->get_ada_full_name();
  body += "      Opcd : " + name_of_the_package + ".Proxies.Get_" + get_ada_local_name() + "_Proxy ;\n";
  body += "   begin \n";
  body += "      " + name_of_the_package + ".Proxies.Init(Opcd) ;\n";
  body += "      OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
  body += "      return " + name_of_the_package + ".Proxies.Get_Result(Opcd) ;\n";
  body += "   end ;\n\n\n";
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
      body += "      OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
      body += "   end ;\n\n\n";    
    }
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_impl_ads     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_impl_ads(dep_list& with, string &body, string &previous)
{
  string space = "";
  for (unsigned int i=0;i<get_ada_local_name().length();i++) space += " ";
  body += "   function Get_" + get_ada_local_name() +"(Self : access Object) return "; 
  AST_Decl *d = field_type();
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  body += name + " ;\n\n";
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
  body += "   -- Get_" + get_ada_local_name() + "\n" ;
  body += "   --------------------------\n" ;
  body += "   function Get_" + get_ada_local_name() +"(Self : access Object) return ";
  AST_Decl *d = field_type();
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  body += name + " is\n";
  body += "   begin\n";
  body += "   end;\n\n\n"; 
  if (!readonly())
    {
      body += "   -- Set_" + get_ada_local_name() + "\n" ;
      body += "   --------------------------\n" ;
      body += "   procedure Set_" + get_ada_local_name() +"(Self : access Object ; To : in ";
      body += name;
      body += ") is\n";
      body += "   begin\n";
      body += "   end;\n\n\n"; 
    } 
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_proxies_ads     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_proxies_ads(dep_list& with, string &body, string &private_definition)
{  
  AST_Decl *d = field_type();
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, private_definition);
  body += "   -----------------------------------------------------------\n" ;
  body += "   ---               Get_" + get_ada_local_name() + "\n" ; 
  body += "   -----------------------------------------------------------\n\n" ;
  body += "   type Get_" + get_ada_local_name() +"_Proxy is new OmniProxyCallDesc.Object with private;\n\n";
  body += "   procedure Init(Self : in out Get_" + get_ada_local_name() + "_Proxy) ;\n\n";
  body += "   function Operation(Self : in Get_" + get_ada_local_name() + "_Proxy)\n";
  body += "                      return Corba.String ;\n\n" ;
  body += "   procedure Unmarshal_Returned_Values(Self : in out Get_" + get_ada_local_name() + "_Proxy ;\n";
  body += "                                       Giop_Client : in out Giop_C.Object);\n\n";
  body += "   function Get_Result (Self : in Get_" + get_ada_local_name() + "_Proxy )\n";
  body += "                        return " +  name + "; \n\n\n";

  private_definition += "   type Get_" + get_ada_local_name() + "_Proxy is new OmniProxyCallDesc.Object with record \n";
  private_definition += "      Private_Result : " + name + "_Ptr := null;\n";
  private_definition += "   end record ;\n";
  private_definition += "   procedure Finalize (Self : in out Get_" + get_ada_local_name() + "_Proxy) ;\n\n";
  
  if (!readonly())
    {
      body += "   -----------------------------------------------------------\n" ;
      body += "   ---               Set_" + get_ada_local_name() + "\n" ; 
      body += "   -----------------------------------------------------------\n\n" ;
      body += "   type Set_" + get_ada_local_name() +"_Proxy is new OmniProxyCallDesc.Object with private ;\n\n";
      body += "   procedure Init(Self : in out Set_" + get_ada_local_name() + "_Proxy ;\n";
      body += "                  Arg : in " + name + ") ;\n\n";
      body += "   function Operation(Self : in Set_" + get_ada_local_name() + "_Proxy)\n";
      body += "                      return Corba.String ;\n\n" ;
      body += "   function Align_Size(Self : in Set_" + get_ada_local_name() + "_Proxy ;\n";
      body += "                       Size_In : in Corba.Unsigned_Long)\n";
      body += "                       return Corba.Unsigned_Long ;\n\n";
      body += "   procedure Marshal_Arguments(Self : in Set_" + get_ada_local_name() + "_Proxy ;\n";
      body += "                               Giop_Client : in out Giop_C.Object);\n\n";

      private_definition += "   type Set_" + get_ada_local_name() + "_Proxy is new OmniProxyCallDesc.Object with record \n";
      private_definition += "      Arg : " + name + "_Ptr := null;\n";
      private_definition += "   end record ;\n";
      private_definition += "   procedure Finalize (Self : in out Set_" + get_ada_local_name() + "_Proxy) ;\n\n";
    }  
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_proxies_adb     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_proxies_adb(dep_list &with, string &body, string &private_definition)
{
  AST_Decl *d = field_type();
  with.add("Netbufferedstream") ;
  adabe_name *att = dynamic_cast<adabe_name *>(d);
  string name = att->dump_name(with, private_definition);
  att->is_marshal_imported(with);
  body += "   -----------------------------------------------------------\n" ;
  body += "   ---               Get_" + get_ada_local_name() + "\n" ; 
  body += "   -----------------------------------------------------------\n\n" ;
  body += "   -- Init\n" ;
  body += "   -------\n" ;
  body += "   procedure Init(Self : in out Get_" + get_ada_local_name() + "_Proxy) is\n";
  body += "   begin\n";
  body += "      Set_User_Exceptions(Self, False) ;\n";
  body += "   end ;\n\n\n";
  body += "   -- Operation\n" ;
  body += "   ------------\n" ;
  body += "   function Operation(Self : in Get_" + get_ada_local_name() + "_Proxy)\n";
  body += "                      return Corba.String is\n";
  body += "   begin\n";
  body += "      return Corba.To_Corba_String(\"_get_" + get_ada_local_name() + "\") ;\n";
  body += "   end ;\n\n\n";
  body += "   -- Unmarshal_Returned_Values\n" ;
  body += "   ----------------------------\n" ;
  body += "   procedure Unmarshal_Returned_Values(Self : in out Get_" + get_ada_local_name() + "_Proxy ;\n";
  body += "                                       Giop_Client : in out Giop_C.Object) is\n";
  body += "      Result : " + name + " ;\n";
  body += "   begin\n";
  body += "      Unmarshall(Result, Giop_client) ;\n";
  body += "      Self.Private_Result := new " + name + "'(Result) ;\n";
  body += "   end ;\n\n\n" ;
  body += "   -- Get_Result\n" ;
  body += "   -------------\n" ;
  body += "   function Get_Result (Self : in Get_" + get_ada_local_name() + "_Proxy )\n";
  body += "                        return " +  name + " is\n";
  body += "   begin\n";
  body += "      return Self.Private_Result.all ;\n";
  body += "   end ;\n\n\n";
  body += "   -- Finalize\n" ;
  body += "   -----------\n" ;
  body += "   procedure Finalize (Self : in out Get_" + get_ada_local_name() + "_Proxy) is\n";
  body += "   begin\n";
  body += "      Free(Self.Private_Result) ;\n";
  body += "   end ;\n\n\n";

  if (!readonly())
    {
      body += "   -----------------------------------------------------------\n" ;
      body += "   ---               Set_" + get_ada_local_name() + "\n" ; 
      body += "   -----------------------------------------------------------\n\n" ;
      body += "   -- Init\n" ;
      body += "   -------\n" ;
      body += "   procedure Init(Self : in out Set_" + get_ada_local_name() + "_Proxy ;\n";
      body += "                  Arg : in " + name + ") is\n";
      body += "   begin\n";
      body += "      Set_User_Exceptions(Self, False) ;\n";
      body += "      Self.Arg := new " + name + "'(Arg) ;\n";
      body += "   end ;\n\n\n";
      body += "   -- Operation\n" ;
      body += "   ------------\n" ;
      body += "   function Operation(Self : in Set_" + get_ada_local_name() + "_Proxy)\n";
      body += "                      return Corba.String is\n";
      body += "   begin\n";
      body += "      return Corba.To_Corba_String(\"_set_" + get_ada_local_name() + "\") ;\n";
      body += "   end ;\n\n\n";
      body += "   -- Align_Size\n" ;
      body += "   -------------\n" ;
      body += "   function Align_Size(Self : in Set_" + get_ada_local_name() + "_Proxy ;\n";
      body += "                       Size_In : in Corba.Unsigned_Long)\n";
      body += "                       return Corba.Unsigned_Long is\n";
      body += "   begin\n";
      body += "      return Align_Size(Self.Arg.all, Size_In) ;\n";
      body += "   end ;\n\n\n";
      body += "   -- Marshall_Arguments\n" ;
      body += "   ---------------------\n" ;
      body += "   procedure Marshal_Arguments(Self : in Set_" +	get_ada_local_name() + "_Proxy ;\n";
      body += "                               Giop_Client : in out Giop_C.Object) is\n";
      body += "   begin\n";
      body += "      Marshall(Self.Arg.all, Giop_client) ;\n";
      body += "   end ;\n\n\n";
      body += "   -- Finalize\n" ;
      body += "   -----------\n" ;
      body += "   procedure Finalize (Self : in out Set_" + get_ada_local_name() + "_Proxy) is\n";
      body += "   begin\n";
      body += "      Free(Self.Arg) ;\n";
      body += "   end ;\n\n\n";
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
  string full_type_name = e->get_ada_local_name ();
  string name = get_ada_local_name ();
  string full_name = get_ada_full_name ();
  string pack_name = full_name.substr(0,full_name.find_last_of('.')) ;

  body += "      if Orl_Op = \"_get_";
  body += name;
  body += "\" then\n";
  body += "         declare\n";
  body += "            Result : ";
  body += full_type_name;
  body += " ;\n";
  body += "            Mesg_Size : Corba.Unsigned_Long ;\n";
  body += "         begin\n";

  body += "            -- change state\n";
  body += "            Giop_S.Request_Received(Orls) ;\n";

  body += "            -- call the implementation\n";
  body += "            Result := ";
  body += pack_name;
  body += ".Impl.Get_";
  body += name;
  body += "(Self) ;\n";

  body += "            -- compute the size of the replied message\n";
  body += "            Mesg_Size := Giop_S.Reply_Header_Size ;\n";
  body += "            Mesg_Size := Align_Size (Result, Mesg_Size) ;\n";

  body += "            -- Initialisation of the reply\n";
  body += "            Giop_S.Initialize_Reply (Orls, Giop.NO_EXCEPTION, Mesg_Size) ;\n";

  body += "            -- Marshall the arguments\n";
  body += "            Marshall (Result, Orls) ;\n";

  body += "            -- inform the orb\n";
  body += "            Giop_S.Reply_Completed (Orls) ;\n";

  body += "            Returns := True ;\n";
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
      body += full_type_name;
      body += " ;\n";
      body += "            Mesg_Size : Corba.Unsigned_Long ;\n";
      body += "         begin\n";
      
      body += "            -- unmarshalls arguments\n";
      body += "            Unmarshall (Mesg,Orls) ;\n";
      
      body += "            -- change state\n";
      body += "            Giop_S.Request_Received(Orls) ;\n";
      
      body += "            -- call the implementation\n";
      body += "            ";
      body += pack_name;
      body += ".Impl.Set_";
      body += name;
      body += "(Self,Mesg) ;\n";
      
      body += "            -- compute the size of the replied message\n";
      body += "            Mesg_Size := Giop_S.Reply_Header_Size ;\n";
      body += "            -- Initialisation of the reply\n";
      body += "            Giop_S.Initialize_Reply (Orls, Giop.NO_EXCEPTION, Mesg_Size) ;\n";
      body += "            -- inform the orb\n";
      body += "            Giop_S.Reply_Completed (Orls) ;\n";
      
      body += "            Returns := True ;\n";
      body += "            return ;\n";
      body += "         end ;\n";
      body += "      end if ;\n\n";
    }
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_marshal_adb     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_attribute::produce_marshal_adb(dep_list &with, string &body, string &previous)
{
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

////////////////////////////////////////////////////////////////////////
////////////////     miscellaneous           ///////////////////////////
////////////////////////////////////////////////////////////////////////
IMPL_NARROW_METHODS1(adabe_attribute, AST_Attribute)
IMPL_NARROW_FROM_DECL(adabe_attribute)
IMPL_NARROW_FROM_SCOPE(adabe_attribute)
















