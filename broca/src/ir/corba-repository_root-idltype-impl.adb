----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with Corba.Repository_Root; use Corba.Repository_Root;
with CORBA.Repository_Root.IDLType.Skel;
with CORBA.Repository_Root.IRObject.Impl;

package body CORBA.Repository_Root.IDLType.Impl is

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
