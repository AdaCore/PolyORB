--  $Id$

package body CORBA.ContextList is

   ----------------
   --  Finalize  --
   ----------------
   procedure Finalize (Obj : in out Object) is
   begin
      Context_Sequence.Delete (Obj.List,
                               1,
                               Context_Sequence.Length (Obj.List));
   end Finalize;

   -----------------
   --  Get_Count  --
   -----------------
   function Get_Count
     (Self : in Ref)
      return CORBA.Unsigned_Long is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      return CORBA.Unsigned_Long (Context_Sequence.Length (Obj.List));
   end Get_Count;

   -----------
   --  Add  --
   -----------
   procedure Add
     (Self : in Ref;
      Exc : in CORBA.String) is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      Context_Sequence.Append (Obj.List, Exc);
   end Add;

   ----------
   -- Item --
   ----------
   function Item
     (Self : in Ref;
      Index : in CORBA.Unsigned_Long)
      return CORBA.String is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      return Context_Sequence.Element_Of (Obj.List, Positive (Index));
   end Item;

   --------------
   --  Remove  --
   --------------
   procedure Remove
     (Self : in Ref;
      Index : in CORBA.Unsigned_Long) is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      Context_Sequence.Delete (Obj.List, Positive (Index), 1);
   end Remove;

   -------------------
   -- Create_Object --
   -------------------
   function Create_Object return Object_Ptr
   is
      Actual_Ref : constant CORBA.ContextList.Object_Ptr
        := new Object;
   begin
      Actual_Ref.List := Context_Sequence.Null_Sequence;
      return Actual_Ref;
   end Create_Object;

end CORBA.ContextList;
