----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Impl;

with Corba.Repository_Root; use Corba.Repository_Root;
with CORBA.Repository_Root.IDLType.Skel;
with CORBA.Repository_Root.IRObject.Impl;

package body CORBA.Repository_Root.IDLType.Impl is

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : IDLType_Forward.Ref)
                       return Object_Ptr is
   begin
      return Object_Ptr
        (IDLType.Object_Of
         (IDLType.Convert_Forward.To_Ref
          (Fw_Ref)));
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return IDLType_Forward.Ref is
      Ref : IDLType.Ref;
   begin
      Set (Ref, CORBA.Impl.Object_Ptr (Obj));
      return IDLType.Convert_Forward.To_Forward (Ref);
   end To_Forward;

   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   IDL_Type : CORBA.TypeCode.Object) is
   begin
      IRObject.Impl.Init (IRObject.Impl.Object_Ptr (Self), Real_Object, Def_Kind);
      Self.IDL_Type := IDL_Type;
   end Init;


   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
   begin
      return Self.IDL_Type;
   end get_type;

end CORBA.Repository_Root.IDLType.Impl;
