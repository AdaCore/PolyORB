--  $Id$

with CORBA.AbstractBase;
with CORBA.Impl;

with Sequences.Unbounded;
pragma Elaborate_All (Sequences.Unbounded);

package CORBA.NVList is

   type Ref is new CORBA.AbstractBase.Ref with null record;

   type Object is new CORBA.Impl.Object with private;
   type Object_Ptr is access all Object;

   procedure Finalize (Obj : in out Object);

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item       : in CORBA.Any;
      Item_Flags : in Flags);

   procedure Add_Item
     (Self : Ref;
      Item : in CORBA.NamedValue);

   --  Free and Free_Memory are no-ops in Ada.
   procedure Free (Self : Ref);
   procedure Free_Memory (Self : Ref)
     renames Free;

   function Get_Count (Self : Ref) return CORBA.Long;

   -----------------------------------------
   -- The following is AdaBroker-specific --
   -----------------------------------------

--     procedure Marshall
--       (Buffer : access Broca.Buffers.Buffer_Type;
--        Data   : Ref);
--     --   Marshall all the NamedValues of an NVList
--  
--     procedure Unmarshall
--       (Buffer : access Broca.Buffers.Buffer_Type;
--        Data : in out Ref);
--     --  Unmarshall all the NamedValues of an NVList

   function Create_Object return Object_Ptr;
   --  Create a new and empty Object

private

   --  The actual implementation of an NVList:
   --  a list of NamedValues.
   package NV_Sequence is new Sequences.Unbounded (CORBA.NamedValue);

   type Object is new CORBA.Impl.Object with record
      List : NV_Sequence.Sequence := NV_Sequence.Null_Sequence;
   end record;

   Nil_Ref : constant Ref
     := (CORBA.AbstractBase.Ref with null record);

end CORBA.NVList;
