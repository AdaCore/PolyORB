with Ada.Finalization;
with Broca.Buffers;

package Broca.Refs is
   --  REF_TYPE is the base type of all objects that can be referenced.
   --  It contains a COUNTER, which is the number of references to this object,
   --  and is automatically destroyed when the counter reachs 0.
   --  It is not abstract, because used for instanciate
   --   system.Address_to_access_conversions.
   type Ref_Type is new Ada.Finalization.Limited_Controlled with
     private;

   --  Disable counter of references.  This is used for object derived from
   --  Ref_type by the user (eg: servant, AdapterActivator...).
   --  Must be called just after creation, when COUNTER is -1 (otherwise,
   --  CORBA.internal is raised).
   procedure Disable_Usage (Obj : in out Ref_Type);

   procedure Compute_New_Size
     (Buffer : in out Broca.Buffers.Buffer_Descriptor;
      Value  : in Ref_Type);

   procedure Marshall
     (Buffer : in out Broca.Buffers.Buffer_Descriptor;
      Value  : in Ref_Type);

   type Ref_Ptr is access all Ref_Type'Class;

   --  Handle the usage counter, unless Obj is null or the counter is
   --  disabled.
   procedure Inc_Usage (Obj : Ref_Ptr);
   procedure Dec_Usage (Obj : in out Ref_Ptr);

   type Ref is new Ada.Finalization.Controlled with private;
   --  The base type of all references. Inside CORBA (and Broca), this
   --  type is often derived but never extended. It contains one
   --  field, which designate the referenced object.

   function Get (Self : Ref) return Ref_Ptr;
   --  Get inner Ref_Type object

   procedure Set (Self : in out Ref; Referenced : Ref_Ptr);
   --  Set the object (can destroyed the previous one, if it was the only
   --  reference).

   procedure Compute_New_Size
     (Buffer : in out Broca.Buffers.Buffer_Descriptor;
      Value  : in Ref);

   procedure Marshall
     (Buffer : in out Broca.Buffers.Buffer_Descriptor;
      Value  : in Ref);

private
   type Ref_Type is new Ada.Finalization.Limited_Controlled with
      record
         --  COUNTER is used to count the number of references, unless
         --  COUNTER is -1 (caused by disable_usage).
         Counter : integer := 0;
      end record;

   type Ref is new Ada.Finalization.Controlled with
      record
         A_Ref : Ref_Ptr := null;
      end record;

   procedure Initialize (Object : in out Ref);
   procedure Adjust (Object : in out Ref);
   procedure Finalize (Object : in out Ref);
end Broca.Refs;
