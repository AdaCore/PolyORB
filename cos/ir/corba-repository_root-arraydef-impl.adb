----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.ORB;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;

with PolyORB.CORBA_P.Server_Tools;
with PortableServer;

with CORBA.Repository_Root.ArrayDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.ArrayDef.Skel);

package body CORBA.Repository_Root.ArrayDef.Impl is


   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Length : CORBA.Unsigned_Long;
                   Element_Type_Def : CORBA.Repository_Root.IDLType.Ref) is
   begin
      IDLType.Impl.Init (IDLType.Impl.Object_Ptr (Self),
                         Real_Object,
                         Def_Kind);
      Self.Length := Length;
      Self.Element_Type_Def := Element_Type_Def;
   end Init;


   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
   begin
      return CORBA.ORB.Create_Array_Tc
        (Self.Length, get_element_type (Self));
   end get_type;


   function get_length
     (Self : access Object)
     return CORBA.Unsigned_Long
   is
   begin
      return Self.Length;
   end get_length;


   procedure set_length
     (Self : access Object;
      To : in CORBA.Unsigned_Long) is
   begin
      Self.Length := To;
   end set_length;


   function get_element_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
      Obj : PortableServer.Servant;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
        (Self.Element_Type_Def, Obj);
      return IDLType.Impl.get_type
        (IDLType.Impl.To_IDLType
         (IRObject.Impl.Object_Ptr
          (Obj)));
   end get_element_type;


   function get_element_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref
   is
   begin
      return Self.Element_Type_Def;
   end get_element_type_def;

   procedure set_element_type_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref) is
   begin
      Self.Element_Type_Def := To;
   end set_element_type_def;

end CORBA.Repository_Root.ArrayDef.Impl;
