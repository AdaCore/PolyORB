----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Impl;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.ConstantDef.Skel;
with CORBA.Repository_Root.Helper;

package body CORBA.Repository_Root.ConstantDef.Impl is


   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : ConstantDef_Forward.Ref)
     return Object_Ptr is
   begin
      return Object_Ptr
        (ConstantDef.Object_Of
         (ConstantDef.Convert_Forward.To_Ref
          (Fw_Ref)));
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return ConstantDef_Forward.Ref is
      Ref : ConstantDef.Ref;
   begin
      Set (Ref, CORBA.Impl.Object_Ptr (Obj));
      return ConstantDef.Convert_Forward.To_Forward (Ref);
   end To_Forward;

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
                   Type_Def : CORBA.Repository_Root.IDLType.Ref;
                   Value : CORBA.Any) is
   begin
      Contained.Impl.Init (Contained.Impl.Object_Ptr(Self),
                           Real_Object,
                           Def_Kind,
                           Id,
                           Name,
                           Version,
                           Defined_In);
      Self.Type_Def := Type_Def;
      Self.Value := Value;
   end Init;

   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
   begin
      --  The type should be the type of the TypeCode
      return Corba.Get_Type (Self.Value);
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


   function get_value
     (Self : access Object)
     return CORBA.Any
   is
   begin
      return Self.Value;
   end get_value;


   procedure set_value
     (Self : access Object;
      To : in CORBA.Any) is
   begin
      Self.Value := To;
   end set_value;

   ----------------
   --  Describe  --
   ----------------
   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description
     is
      Result : CORBA.Repository_Root.Contained.Description;
      Desc : CORBA.Repository_Root.ConstantDescription;
   begin
      Desc := (Name => Get_Name (Self),
               Id => Get_Id (Self),
               Defined_In => Get_Defined_In (Self),
               Version => Get_Version (Self),
               IDL_Type => Get_Type (Self),
               Value => Self.Value);
      Result := (Kind => Get_Def_Kind (Self),
                 Value => CORBA.Repository_Root.Helper.To_Any (Desc));
      return Result;
   end Describe;


end CORBA.Repository_Root.ConstantDef.Impl;
