with CORBA;
with Broca.IOP;
with Broca.Buffers; use Broca.Buffers;

package body Broca.Object is

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
