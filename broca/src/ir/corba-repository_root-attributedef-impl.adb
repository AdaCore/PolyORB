----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.AttributeDef.Skel;
with CORBA.Repository_Root.Helper;

package body CORBA.Repository_Root.AttributeDef.Impl is


   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : Corba.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   IDL_Type : CORBA.TypeCode.Object;
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
      Self.IDL_Type := IDL_Type;
      Self.Type_Def := Type_Def;
      Self.Mode := Mode;
   end Init;


   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin

      --  Insert implementation of get_type

      return Result;
   end get_type;


   function get_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref
   is
      Result : CORBA.Repository_Root.IDLType.Ref;
   begin

      --  Insert implementation of get_type_def

      return Result;
   end get_type_def;


   procedure set_type_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref) is
   begin

      --  Insert implementation of set_type_def

      null;
   end set_type_def;


   function get_mode
     (Self : access Object)
     return CORBA.Repository_Root.AttributeMode
   is
      Result : CORBA.Repository_Root.AttributeMode;
   begin

      --  Insert implementation of get_mode

      return Result;
   end get_mode;


   procedure set_mode
     (Self : access Object;
      To : in CORBA.Repository_Root.AttributeMode) is
   begin

      --  Insert implementation of set_mode

      null;
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
               IDL_Type => Self.IDL_Type,
               Mode => Self.Mode);
      Result := (Kind => Get_Def_Kind (Self),
                 Value => CORBA.Repository_Root.Helper.To_Any (Desc));
      return Result;
   end Describe;

end CORBA.Repository_Root.AttributeDef.Impl;
