with CORBA;
with Broca.Refs;
with Broca.IOP;
with Broca.Buffers;

package Broca.Object is

   type Object_Type is new Broca.Refs.Ref_Type with
      record
         Type_Id  : CORBA.String;
         Profiles : IOP.Profile_Ptr_Array_Ptr;
      end record;

   type Object_Ptr is access all Object_Type'Class;

   function Find_Profile (Object : Object_Ptr) return IOP.Profile_Ptr;
   --  Find a profile for a message

   procedure Encapsulate_IOR
     (Buffer : in out Buffers.Buffer_Descriptor;
      From   : in Buffers.Buffer_Index_Type;
      Object : in Object_Type'Class);

   procedure Decapsulate_IOR
     (Buffer : in out Buffers.Buffer_Descriptor;
      Object : out Object_Type'Class);

end Broca.Object;
