with Ada.Unchecked_Deallocation;

with CORBA.Object;

generic
package CORBA.Forward is

   type Ref is new CORBA.Object.Ref with null record;
   type Ref_Ptr is access all Ref;
   --  Just to give a name to pointers on Ref objects

   procedure Free (Self : in out Ref_Ptr);
   --  To deallocate Object_Ptr

   function To_Ref (The_Ref : in CORBA.Object.Ref'Class) return Ref;
   --  Added in AdaBroker, because To_Ref has to be overriden for all the
   --  descendants of CORBA.Object.Ref We just raise an exceptipon here The
   --  spec does not specify any function To_Ref in CORBA.Object.Ref, but
   --  it must be a mistake, since they say later taht To_Ref has to be
   --  defined for all IDL interfaces (CORBA.Object.Ref is indeed an IDL
   --  interface)

   generic
      type Ref_Type is new CORBA.Object.Ref with private;
   package Convert is
      function From_Forward (The_Forward : in Ref) return Ref_Type;
      function To_Forward (The_Ref : in Ref_Type) return Ref;
   end Convert;

private

   procedure Private_Free is new Ada.Unchecked_Deallocation (Ref, Ref_Ptr);

end CORBA.Forward;



