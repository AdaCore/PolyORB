with Broca.Object;     use Broca.Object;
with Broca.Repository; use Broca.Repository;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Marshalling.Refs is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.marshalling.refs");
   procedure O is new Broca.Debug.Output (Flag);

   ----------------------
   -- Compute_New_Size --
   ----------------------

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in Broca.Refs.Ref'Class) is
      A_Buf : Buffer_Descriptor;
   begin
      Encapsulate_IOR (A_Buf, 0, Object_Type'Class (Get (Value).all));
      --  XXX should cache A_Buf in object for subsequent call to
      --  Marshall.
      Compute_New_Size (Buffer, A_Buf);
   end Compute_New_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in Broca.Refs.Ref'Class) is
   begin
      --  XXX Check:
      --  Value of "From" parameter (0);
      --  Potential exception (if Get (Value) cannot be
      --  narrowed to Object_Type)
      Encapsulate_IOR (Buffer, 0, Object_Type'Class (Get (Value).all));
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out Broca.Refs.Ref'Class) is
      Object : constant Object_Ptr
        := new Object_Type;
   begin
      Decapsulate_IOR (Buffer, Object.all);
      declare
         Ref : Broca.Refs.Ref'Class
           := Create (CORBA.RepositoryId (Object.Type_Id));
      begin
         Set (Ref, Ref_Ptr (Object));
         Result := Ref;
      end;
   end Unmarshall;

end Broca.Marshalling.Refs;
