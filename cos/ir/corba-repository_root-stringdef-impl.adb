----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.ORB;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.IRObject.Impl;

with CORBA.Repository_Root.StringDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.StringDef.Skel);

package body CORBA.Repository_Root.StringDef.Impl is


   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Bound : CORBA.Unsigned_Long) is
   begin
      IDLType.Impl.Init (IDLType.Impl.Object_Ptr (Self),
                        Real_Object,
                        Def_Kind);
      Self.Bound := Bound;
   end Init;

   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
   begin
      return CORBA.ORB.Create_String_Tc (Self.Bound);
   end get_type;


   function get_bound
     (Self : access Object)
     return CORBA.Unsigned_Long
   is
   begin
      return Self.Bound;
   end get_bound;


   procedure set_bound
     (Self : access Object;
      To : in CORBA.Unsigned_Long) is
   begin
      Self.Bound := To;
   end set_bound;

end CORBA.Repository_Root.StringDef.Impl;
