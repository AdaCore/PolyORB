with CORBA;
with Broca.Refs;
with Broca.IOP;

package Broca.Object is

   type Object_Type is new Broca.Refs.Ref_Type with
      record
         Type_Id  : CORBA.String;
         Profiles : IOP.Profile_Ptr_Array_Ptr;
      end record;

   type Object_Ptr is access all Object_Type;

   function Find_Profile (Object : Object_Ptr) return IOP.Profile_Ptr;
   --  Find a profile for a message

end Broca.Object;
