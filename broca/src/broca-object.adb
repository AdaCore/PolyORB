with CORBA;
with Broca.IOP;
with Broca.Buffers; use Broca.Buffers;

package body Broca.Object is

   ----------------------
   -- Compute_New_Size --
   ----------------------

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in Broca.Object.Object_Type) is
      A_Buf : Buffer_Descriptor;
      Old_Size : Buffer_Index_Type := Full_Size (Buffer);
   begin
      Encapsulate_IOR (A_Buf, Old_Size, Value);
      --  XXX should cache A_Buf in object for subsequent call to
      --  Marshall.
      Skip_Bytes (Buffer, Full_Size (A_Buf) - Old_Size);
      Destroy (A_Buf);
   end Compute_New_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in Broca.Object.Object_Type) is
   begin
      --  XXX Check:
      --  Value of "From" parameter (0);
      --  Potential exception (if Get (Value) cannot be
      --  narrowed to Object_Type)
      Encapsulate_IOR (Buffer, Size_Used (Buffer), Value);
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out Broca.Object.Object_Type) is
   begin
      Decapsulate_IOR (Buffer, Result);
   end Unmarshall;

   ------------------
   -- Find_Profile --
   ------------------

   function Find_Profile (Object : Object_Ptr) return IOP.Profile_Ptr is
   begin
      return Object.Profiles (Object.Profiles'First);
   end Find_Profile;

   ---------------------
   -- Encapsulate_IOR --
   ---------------------

   procedure Encapsulate_IOR
     (Buffer : in out Buffers.Buffer_Descriptor;
      From   : in Buffer_Index_Type;
      Object : in Object_Type'Class)
   is
   begin
      IOP.Encapsulate_IOR (Buffer, From, Object.Type_Id, Object.Profiles);
   end Encapsulate_IOR;

   ---------------------
   -- Decapsulate_IOR --
   ---------------------

   procedure Decapsulate_IOR
     (Buffer : in out Buffers.Buffer_Descriptor;
      Object : out Object_Type'Class)
   is
   begin
      IOP.Decapsulate_IOR (Buffer, Object.Type_Id, Object.Profiles);
   end Decapsulate_IOR;

end Broca.Object;
