--  The following subprograms still have to be implemented :
--     Create_Request
--     Get_Policy
--     Is_A
--     Non_Existent

with GNAT.HTable;
with CORBA.ORB;
with Droopi.Smart_Pointers;

with CORBA.ORB;

package body CORBA.Object is

   use Droopi.Smart_Pointers;

   type Internal_Object is new Droopi.Smart_Pointers.Entity with record
      The_Object : Droopi.Objects.Object_Id_Access;
   end record;
   type Internal_Object_Access is access all Internal_Object;

   ----------
   -- Hash --
   ----------

   function Hash
     (Self    : Ref;
      Maximum : CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
      type My_Range is new Long range 0 .. Long (Maximum);
      function My_Hash is new GNAT.HTable.Hash (My_Range);
   begin
      return CORBA.Unsigned_Long
        (My_Hash (To_Standard_String (CORBA.ORB.Object_To_String (Self))));
   end Hash;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : in Ref;
      Logical_Type_Id : in Standard.String)
     return CORBA.Boolean
   is
   begin
      return Is_A (Self, Logical_Type_Id);
   end Is_A;

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

   ------------------
   -- Non_Existent --
   ------------------

   function Non_Existent (Self : Ref) return CORBA.Boolean is
   begin
      return Non_Existent (Self);
   end Non_Existent;

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
     (Self     : in Ref;
      Policies :    CORBA.Policy.PolicyList;
      Set_Add  :    SetOverrideType)
   is
   begin
      null;
   end Set_Policy_Overrides;

   ---------------------
   -- To_CORBA_Object --
   ---------------------

   function To_CORBA_Object
     (O : in Droopi.Objects.Object_Id)
     return Ref
   is
      Result : Ref;
      Internal : Internal_Object_Access;
   begin
      Internal := new Internal_Object;
      Internal.The_Object := new Droopi.Objects.Object_Id'(O);

      Droopi.Smart_Pointers.Set
        (Droopi.Smart_Pointers.Ref (Result),
         Entity_Ptr (Internal));
      return Result;
   end To_CORBA_Object;

   ----------------------
   -- To_Droopi_Object --
   ----------------------

   function To_Droopi_Object
     (R : in Ref)
     return Droopi.Objects.Object_Id
   is
   begin
      return Internal_Object_Access (Entity_Of (R)).The_Object.all;
   end To_Droopi_Object;

end CORBA.Object;
