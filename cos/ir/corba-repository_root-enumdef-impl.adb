pragma Style_Checks (Off);

with CORBA.ORB.TypeCode;
with PolyORB.CORBA_P.Server_Tools;
with PortableServer;

with CORBA.Repository_Root.EnumDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.EnumDef.Skel);

package body CORBA.Repository_Root.EnumDef.Impl is

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : EnumDef_Forward.Ref)
     return Object_Ptr is
      Result : Portableserver.Servant;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
        (EnumDef.Convert_Forward.To_Ref (Fw_Ref),
         Result);
      return Object_Ptr (Result);
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return EnumDef_Forward.Ref is
      Ref : EnumDef.Ref;
   begin
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Ref);
      return EnumDef.Convert_Forward.To_Forward (Ref);
   end To_Forward;

   ------------
   --  INIT  --
   ------------
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
                   Members : CORBA.Repository_Root.EnumMemberSeq) is
   begin
      Typedefdef.Impl.Init (Typedefdef.Impl.Object_Ptr (Self),
                            Real_Object,
                            Def_Kind,
                            Id,
                            Name,
                            Version,
                            Defined_In,
                            IDLType_View);
      Self.Members := Members;
   end Init;

   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
   begin
      return CORBA.ORB.TypeCode.Create_Enum_Tc
        (Get_Id (Self), Get_Name (Self), Self.Members);
   end get_type;

   function get_members
     (Self : access Object)
     return CORBA.Repository_Root.EnumMemberSeq
   is
   begin
      return Self.Members;
   end get_members;

   procedure set_members
     (Self : access Object;
      To : in CORBA.Repository_Root.EnumMemberSeq) is
   begin
      Self.Members := To;
   end set_members;

end CORBA.Repository_Root.EnumDef.Impl;
