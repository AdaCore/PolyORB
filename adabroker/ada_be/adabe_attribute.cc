#include <adabe.h>


adabe_attribute::adabe_attribute(idl_bool ro, AST_Type *ft, UTL_ScopedName *n, UTL_StrList *p)
  : AST_Attribute(ro,ft,n,p),
    AST_Field(AST_Decl::NT_attr,ft,n,p),
    AST_Decl(AST_Decl::NT_attr,n,p),
    adabe_name(AST_Decl::NT_attr,n,p)
{  
}

void
adabe_attribute::produce_ads(dep_list& with, string &body, string &previous)
{
  string space = "";
  for (unsigned int i=0;i<get_ada_local_name().length();i++) space += " ";
  compute_ada_name();
  body += "   function Get_" + get_ada_local_name() +"(Self : in Ref)\n";
  body += "                  " + space + "return "; 
  AST_Decl *d = field_type();
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  body += name + " ;\n";
  if (!readonly())
    {
      body += "   procedure Set_" + get_ada_local_name();
      body += "(Self : in Ref ;\n";
      body += "                   " + space + "To : in ";
      body += name;
      body += ") ;\n";
    }
}

void
adabe_attribute::produce_adb(dep_list& with, string &body, string &previous)
{
  string space = "";
  for (unsigned int i=0;i<get_ada_local_name().length();i++) space += " ";
  body += "   function get_" + get_ada_local_name() +"(Self : in Ref)\n";
  body += "                 " + space + "return "; 
  AST_Decl *d = field_type();  
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  body += name + " ;\n";  
  string name_of_the_package = dynamic_cast<adabe_name *>(ScopeAsDecl(defined_in()))->get_ada_full_name();
  body += "      Opcd : " + name_of_the_package + ".Proxies.Get_" + get_ada_local_name() + "_Proxy ;\n";
  body += "      Result : " + name +" ;\n";
  body += "   begin \n";
  body += "      Assert_Ref_Not_Nil(Self) ;\n";
  body += "      Opcd := " + name_of_the_package + ".Proxies.Create() ;\n";
  body += "      OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
  body += "      Result := " + name_of_the_package + ".Proxies.Get_Result(Opcd) ;\n";
  body += "      " + name_of_the_package + ".Proxies.Free(Opcd) ;\n";
  body += "      return Result ;\n";
  body += "   end ;\n\n";
  if (!readonly())
    {
      body += "   procedure set_";
      body += get_ada_local_name();
      body += "(Self : in Ref,\n";
      body += "                  " + space + "To : in ";
      body += name;
      body += ") is \n";
      body += "      Opcd : ";
      body += name_of_the_package;
      body += ".Proxies.";
      body += get_ada_local_name();
      body += "_Proxy ;\n";
      body += "   begin \n";
      body += "      Assert_Ref_Not_Nil(Self) ;\n";
      body += "      Opcd := " + name_of_the_package + ".Proxies.Create(To) ;\n";
      body += "      OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
      body += "      " + name_of_the_package + ".Proxies.Free(Opcd) ;\n";
      body += "   end ;\n\n";    
    }
}

void
adabe_attribute::produce_impl_ads(dep_list& with, string &body, string &previous)
{
  body += "   function get_" + get_ada_local_name() +"(Self : access Object) return "; 
  AST_Decl *d = field_type();
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  body += name + ";\n";
  if (!readonly())
    {
      body += "   procedure set_" + name +"(Self : access Object, To : in ";
      body += name;
      body += ");\n";
    }
}

void
adabe_attribute::produce_impl_adb(dep_list& with, string &body, string &previous)
{
  body += "   function get_" + get_ada_local_name() +"(Self : access Object) return ";
  AST_Decl *d = field_type();
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  body += name + ";\n";
  body += "   begin\n\n";
  body += "   end; \n"; 
  if (!readonly())
    {
      body += "   procedure set_" + name +"(Self : access Object, To : in ";
      body += name;
      body += ") is\n";
      body += "   begin\n\n";
      body += "   end; \n"; 
    } 
}

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
  body += "                      return CORBA.String ;\n\n" ;
  body += "   procedure Unmarshal_Returned_Values(Self : in out Get_" + get_ada_local_name() + "_Proxy ;\n";
  body += "                                       Giop_Client : in Giop_C.Object);\n\n";
  body += "   function Get_Result (Self : in Get_" + get_ada_local_name() + "_Proxy )\n";
  body += "                        return " +  name + "; \n\n\n";

  private_definition += "   type Get_" + get_ada_local_name() + "_Proxy is new OmniProxyCallDesc.Object with record \n";
  private_definition += "      Result : " + name + "_Ptr := null;\n";
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
      body += "                      return CORBA.String ;\n\n" ;
      body += "   function Align_Size(Self : in Set_" + get_ada_local_name() + "_Proxy ;\n";
      body += "                       Size_In : in Corba.Unsigned_Long)\n";
      body += "                       return Corba.Unsigned_Long ;\n\n";
      body += "   procedure Marshal_Arguments(Self : in Set_" + get_ada_local_name() + "_Proxy ;\n";
      body += "                               Giop_Client : in out Giop_C.Object);\n\n";
      body += "   procedure Unmarshal_Returned_Values(Self : in out Set_" + get_ada_local_name() + "_Proxy ;\n";
      body += "                                       Giop_Client : in Giop_C.Object) ;\n\n";

      private_definition += "   type Set_" + get_ada_local_name() + "_Proxy is new OmniProxyCallDesc.Object with record \n";
      private_definition += "      Arg : " + name + "_Ptr := null;\n";
      private_definition += "   end record ;\n";
      private_definition += "   procedure Finalize (Self : in out Set_" + get_ada_local_name() + "_Proxy) ;\n\n";
    }  
}


void
adabe_attribute::produce_proxies_adb(dep_list &with, string &body, string &private_definition)
{
  AST_Decl *d = field_type();
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, private_definition);
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
  body += "                      return CORBA.String is\n";
  body += "   begin\n";
  body += "      return Corba.To_Corba_String(\"get_" + get_ada_local_name() + "\") ;\n";
  body += "   end ;\n\n\n";
  body += "   -- Unmarshal_Returned_Values\n" ;
  body += "   ----------------------------\n" ;
  body += "   procedure Unmarshal_Returned_Values(Self : in out Get_" + get_ada_local_name() + "_Proxy ;\n";
  body += "                                       Giop_Client : in Giop_C.Object) is\n";
  body += "      Result : " + name + " ;\n";
  body += "   begin\n";
  body += "      Unmarshall(Result, Giop_client) ;\n";
  body += "      Self.Result := new " + name + "'(Result) ;\n";
  body += "   end ;\n\n\n" ;
  body += "   -- Get_Result\n" ;
  body += "   -------------\n" ;
  body += "   function Get_Result (Self : in Get_" + get_ada_local_name() + "_Proxy )\n";
  body += "                        return " +  name + " is\n";
  body += "   begin\n";
  body += "      return Self.Result.all ;\n";
  body += "   end ;\n\n\n";
  body += "   -- Finalize\n" ;
  body += "   -----------\n" ;
  body += "   procedure Finalize (Self : in out Get_" + get_ada_local_name() + "_Proxy) is\n";
  body += "   begin\n";
  body += "      Free(Self.Result) ;\n";
  body += "   end ;\n\n\n";

  if (!readonly())
    {
      body += "   -----------------------------------------------------------\n" ;
      body += "   ---               Set_" + get_ada_local_name() + "\n" ; 
      body += "   -----------------------------------------------------------\n\n" ;
      body += "   -- Init\n" ;
      body += "   -------\n" ;
      body += "   procedure Init(Self : in out Set_" + get_ada_local_name() + "_Proxy is\n";
      body += "                  Arg : in " + name + ") is\n";
      body += "   begin\n";
      body += "      Set_User_Exceptions(Self, False) ;\n";
      body += "      Self.Arg := new " + name + "'(Arg) ;\n";
      body += "   end ;\n\n\n";
      body += "   -- Operation\n" ;
      body += "   ------------\n" ;
      body += "   function Operation(Self : in Set_" + get_ada_local_name() + "_Proxy) is\n";
      body += "                      return CORBA.String is\n";
      body += "   begin\n";
      body += "      return Corba.To_Corba_String(\"set_" + get_ada_local_name() + "\") ;\n";
      body += "   end ;\n\n\n";
      body += "   -- Align_Size\n" ;
      body += "   -------------\n" ;
       body += "   function Align_Size(Self : in Set_" + get_ada_local_name() + "_Proxy is\n";
      body += "                       Size_In : in Corba.Unsigned_Long)\n";
      body += "                       return Corba.Unsigned_Long is\n";
      body += "   begin\n";
      body += "      return Align_Size(Self.Arg.all, Size_In) ;\n";
      body += "   end ;\n\n\n";
      body += "   -- Marshall_Arguments\n" ;
      body += "   ---------------------\n" ;
      body += "   procedure Marshal_Arguments(Self : in Set_" + get_ada_local_name() + "_Proxy is\n";
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


void
adabe_attribute::produce_skeleton_adb(dep_list& with, string &body, string &private_definition)
{
}

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

IMPL_NARROW_METHODS1(adabe_attribute, AST_Attribute)
IMPL_NARROW_FROM_DECL(adabe_attribute)
IMPL_NARROW_FROM_SCOPE(adabe_attribute)
















