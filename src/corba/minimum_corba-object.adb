-- The following subprograms still have to be implemented :
-- Set_Policy_Overrides --

with GNAT.HTable;
with Droopi.Smart_Pointers; use Droopi.Smart_Pointers;

package body Minimum_CORBA.Object is

   ----------
   -- Hash --
   ----------

   function Hash
     (Self    : Ref;
      Maximum : CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
      type My_Range is new Long Range 0 .. Long (Maximum);
      function My_Hash is new GNAT.HTable.Hash (My_Range);
   begin
      return CORBA.Unsigned_Long
        (My_Hash (To_Standard_String (CORBA.ORB.Object_To_String (Self))));
   end Hash;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent
     (Self         : Ref;
      Other_Object : Ref'Class)
      return Boolean
   is
      use Droopi.Smart_Pointers;
   begin
      return (Entity_Of (Self) = Entity_Of (Other_Object));
   end Is_Equivalent;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Self : in Ref) return CORBA.Boolean is
   begin
      return Is_Nil (Droopi.Smart_Pointers.Ref (Self));
   end Is_Nil;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Ref) is
   begin
      Release (Droopi.Smart_Pointers.Ref (Self));
   end Release;

   --------------------------
   -- Set_Policy_Overrides --
   --------------------------

   procedure Set_Policy_Overrides
     (Self : in Ref;
      Policies : CORBA.Policy.PolicyList;
      Set_Add : SetOverrideType)
   is
   begin
      null;
   end Set_Policy_Overrides;

end Minimum_CORBA.Object;

