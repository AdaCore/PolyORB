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

package body CORBA.Repository_Root.AttributeDef.Impl is


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
                   Mode : CORBA.Repository_Root.AttributeMode) is
   begin
      Contained.Impl.Init (Contained.Impl.Object_Ptr(Self),
                           Real_Object,
                           Def_Kind,
                           Id,
                           Name,
                           Version,
                           Defined_In);
      Self.Type_Def := Type_Def;
      Self.Mode := Mode;
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
   begin
      return Self.Type_Def;
   end get_type_def;


   procedure set_type_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref) is
   begin
      Self.Type_Def := To;
   end set_type_def;


   function get_mode
     (Self : access Object)
     return CORBA.Repository_Root.AttributeMode
   is
   begin
      return Self.Mode;
   end get_mode;


   procedure set_mode
     (Self : access Object;
      To : in CORBA.Repository_Root.AttributeMode) is
   begin
      Self.Mode := To;
   end set_mode;


   ----------------
   --  Describe  --
   ----------------
   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description
     is
      Result : CORBA.Repository_Root.Contained.Description;
      Desc : CORBA.Repository_Root.AttributeDescription;
   begin
      Desc := (Name => Get_Name (Self),
               Id => Get_Id (Self),
               Defined_In => Get_Defined_In (Self),
               Version => Get_Version (Self),
               IDL_Type => Get_Type (Self),
               Mode => Self.Mode);
      Result := (Kind => Get_Def_Kind (Self),
                 Value => CORBA.Repository_Root.Helper.To_Any (Desc));
      return Result;
   end Describe;

end CORBA.Repository_Root.AttributeDef.Impl;
