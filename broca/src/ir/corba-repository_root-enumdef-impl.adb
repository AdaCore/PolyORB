----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.ORB.Typecode;

with CORBA.Repository_Root.EnumDef.Skel;

with Broca.Server_Tools;
with PortableServer;

package body CORBA.Repository_Root.EnumDef.Impl is


   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : EnumDef_Forward.Ref)
     return Object_Ptr is
   begin
      return Object_Ptr
        (EnumDef.Object_Of
         (EnumDef.Convert_Forward.To_Ref
          (Fw_Ref)));
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return EnumDef_Forward.Ref is
      Ref : EnumDef.Ref;
   begin
      Broca.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Ref);
      return EnumDef.Convert_Forward.To_Forward (Ref);
   end To_Forward;

   ------------
   --  INIT  --
   ------------
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : Corba.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
                   Members : Corba.Repository_Root.EnumMemberSeq) is
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
      return CORBA.ORB.Typecode.Create_Enum_Tc (Get_Id (Self),
                                                Get_Name (Self),
                                                Self.Members);
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
