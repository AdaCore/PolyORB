--  $Id$

with CORBA.AbstractBase;
with CORBA.Impl;
with Sequences.Unbounded;
pragma Elaborate (Sequences.Unbounded);

package CORBA.ContextList is

   pragma Elaborate_Body;

   type Ref is new CORBA.AbstractBase.Ref with null record;
   Nil_Ref : constant Ref;

   type Object is new CORBA.Impl.Object with private;
   type Object_Ptr is access all Object;

   procedure Finalize (Obj : in out Object);

   function Get_Count
     (Self : in Ref)
     return CORBA.Unsigned_Long;

   procedure Add
     (Self : in Ref;
      Exc : in CORBA.String);

   function Item
     (Self : in Ref;
      Index : in CORBA.Unsigned_Long)
     return CORBA.String;

   procedure Remove
     (Self : in Ref;
      Index : in CORBA.Unsigned_Long);

   function Create_Object return Object_Ptr;

private
   --  The actual implementation of an ExceptionList:
   --  a list of CORBA.String
   package Context_Sequence is new Sequences.Unbounded (CORBA.String);

   type Object is new CORBA.Impl.Object with record
     List : Context_Sequence.Sequence := Context_Sequence.Null_Sequence;
   end record;

   Nil_Ref : constant Ref
     := (CORBA.AbstractBase.Ref with null record);
end CORBA.ContextList;
