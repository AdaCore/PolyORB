----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with Corba.Repository_Root; use Corba.Repository_Root;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.WstringDef.Skel;

package body CORBA.Repository_Root.WstringDef.Impl is


   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   IDL_Type : CORBA.TypeCode.Object;
                   Bound : CORBA.Unsigned_Long) is
   begin
     IDLType.Impl.Init (IDLType.Impl.Object_Ptr (Self),
                         Real_Object,
                         Def_Kind,
                         IDL_Type);
      Self.Bound := Bound;
   end Init;



   function get_bound
     (Self : access Object)
     return CORBA.Unsigned_Long
   is
      Result : CORBA.Unsigned_Long;
   begin

      --  Insert implementation of get_bound

      return Result;
   end get_bound;


   procedure set_bound
     (Self : access Object;
      To : in CORBA.Unsigned_Long) is
   begin

      --  Insert implementation of set_bound

      null;
   end set_bound;

end CORBA.Repository_Root.WstringDef.Impl;
