pragma Warnings (Off);
----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.Helper;
with CORBA.Repository_Root.IRObject.Impl;

with PolyORB.CORBA_P.Server_Tools;
with PortableServer;

with CORBA.Repository_Root.ValueMemberDef.Skel;

package body CORBA.Repository_Root.ValueMemberDef.Impl is


   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   Type_Def : CORBA.Repository_Root.IDLType.Ref;
                   IDL_Access : CORBA.Repository_Root.Visibility) is
   begin
      Contained.Impl.Init (Contained.Impl.Object_Ptr(Self),
                           Real_Object,
                           Def_Kind,
                           Id,
                           Name,
                           Version,
                           Defined_In);
      Self.Type_Def := Type_Def;
      Self.IDL_Access := IDL_Access;
   end Init;


   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
      Obj : PortableServer.Servant;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant (Self.Type_Def,
                                               Obj);
      --  The type should be the type of the Type_def
      return IDLType.Impl.Get_Type
        (IDLType.Impl.To_IDLType
         (IRObject.Impl.Object_Ptr
          (Obj)));
   end get_type;


   function get_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref
   is
      Result : CORBA.Repository_Root.IDLType.Ref;
   begin
      return Self.Type_Def;
   end get_type_def;


   procedure set_type_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref) is
   begin
      Self.Type_Def := To;
   end set_type_def;


   function get_access
     (Self : access Object)
     return CORBA.Repository_Root.Visibility
   is
   begin
      return Self.IDL_Access;
   end get_access;


   procedure set_access
     (Self : access Object;
      To : in CORBA.Repository_Root.Visibility) is
   begin
      Self.IDL_Access := To;
   end set_access;


   ----------------
   --  Describe  --
   ----------------
   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description
     is
      Result : CORBA.Repository_Root.Contained.Description;
      Desc : CORBA.Repository_Root.ValueMember;
   begin
      Desc := (Name => Get_Name (Self),
               Id => Get_Id (Self),
               Defined_In => Get_Defined_In (Self),
               Version => Get_Version (Self),
               IDL_Type => Get_Type (Self),
               Type_Def => IDLType.Convert_Forward.To_Forward (Self.Type_Def),
               IDL_Access => Self.IDL_Access);
      Result := (Kind => Get_Def_Kind (Self),
                 Value => CORBA.Repository_Root.Helper.To_Any (Desc));
      return Result;
   end Describe;

end CORBA.Repository_Root.ValueMemberDef.Impl;

