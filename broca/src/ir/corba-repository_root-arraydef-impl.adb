----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.ArrayDef.Skel;

package body CORBA.Repository_Root.ArrayDef.Impl is


   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   IDL_Type : CORBA.TypeCode.Object;
                   Length : CORBA.Unsigned_Long;
                   Element_Type_Def : CORBA.Repository_Root.IDLType.Ref) is
   begin
      IDLType.Impl.Init (IDLType.Impl.Object_Ptr (Self),
                         Real_Object,
                         Def_Kind,
                         IDL_Type);
      Self.Length := Length;
      Self.Element_Type_Def := Element_Type_Def;
   end Init;


   function get_length
     (Self : access Object)
     return CORBA.Unsigned_Long
   is
      Result : CORBA.Unsigned_Long;
   begin

      --  Insert implementation of get_length

      return Result;
   end get_length;


   procedure set_length
     (Self : access Object;
      To : in CORBA.Unsigned_Long) is
   begin

      --  Insert implementation of set_length

      null;
   end set_length;


   function get_element_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
   begin
      return IDLType.Impl.Get_Type
        (IDLType.Impl.Object_Ptr
         (IDLType.Object_Of
          (Self.Element_Type_Def)));
   end get_element_type;


   function get_element_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref
   is
      Result : CORBA.Repository_Root.IDLType.Ref;
   begin

      --  Insert implementation of get_element_type_def

      return Result;
   end get_element_type_def;


   procedure set_element_type_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref) is
   begin

      --  Insert implementation of set_element_type_def

      null;
   end set_element_type_def;

end CORBA.Repository_Root.ArrayDef.Impl;
