--  This package corresponds to the CORBA 2.0 specification. It contains
--  the definition of type CORBA.Object.Ref, which is the base class of all
--  proxy objects

with Ada.Exceptions;
with Ada.Strings.Unbounded;

with AdaBroker.OmniORB;        use AdaBroker.OmniORB;
with AdaBroker.OmniRopeAndKey; use AdaBroker.OmniRopeAndKey;

with AdaBroker.Debug;
pragma Elaborate_All (AdaBroker.Debug);

with CORBA;
pragma Elaborate_All (CORBA);

package body CORBA.Object is

   Flag : constant Natural := AdaBroker.Debug.Is_Active ("corba.object");
   procedure O is new AdaBroker.Debug.Output (Flag);

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Self : in Ref) return Boolean is
   begin
      return  Self.OmniObj = null;
   end Is_Nil;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Ref) is
   begin
      Finalize (Self);
   end Release;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : in Ref;
      Logical_Type_Id : in CORBA.String)
      return CORBA.Boolean is
   begin
      return Repository_Id = Logical_Type_Id;
   end Is_A;

   ------------------
   -- Non_Existent --
   ------------------

   function Non_Existent (Self : in Ref) return CORBA.Boolean is
   begin
      if Is_Nil (Self) then
         return True;
      end if;
      return Non_Existent (Self.OmniObj.all);
   end Non_Existent;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent
     (Self  : in Ref;
      Other : in Ref)
      return CORBA.Boolean
   is
      R1, R2 : Controlled_Wrapper;
      S1, S2 : CORBA.Boolean;
   begin
      --  This code comes from corbaObject.cc L160. Refs are proxy objects

      Get_Rope_And_Key (Self.OmniObj.all,  R1.Real, S1);
      Get_Rope_And_Key (Other.OmniObj.all, R2.Real, S2);

      return S1
        and then S2
        and then R1.Real = R2.Real;
   end Is_Equivalent;

   ----------
   -- Hash --
   ----------

   function Hash
     (Self    : in Ref;
      Maximum : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
   begin
      if Is_Nil (Self) then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "cannot call hash on a nil reference");
      end if;

      return Hash (Self.OmniObj.all, Maximum);
   end Hash;

   ------------------------
   -- Get_Implementation --
   ------------------------

   function Get_Implementation
     (Self : in Ref'Class)
      return OmniObject_Ptr
   is
   begin
      return Self.OmniObj;
   end Get_Implementation;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Ref) is
   begin
      pragma Debug (O ("initialize : enter"));

      Self.OmniObj := null;

      pragma Debug (O ("initialize : leave"));
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Ref) is
   begin
      pragma Debug (O ("adjust : enter"));

      if Self.OmniObj /= null then
         Self.OmniObj := Duplicate_OmniObject (Self.OmniObj);
      end if;

      pragma Debug (O ("adjust : leave"));
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Ref) is
   begin
      pragma Debug (O ("finalize : enter"));

      if Self.OmniObj /= null then
         Destruct_OmniObject (Self.OmniObj);
         Self.OmniObj := null;
      end if;

      pragma Debug (O ("finalize : leave"));
   end Finalize;

end CORBA.Object;
