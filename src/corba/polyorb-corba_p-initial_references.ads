--  Support package for CORBA initial references.

with CORBA.Object;

with PolyORB.Utils.Strings.Lists;

package PolyORB.CORBA_P.Initial_References is

   type Create_Ptr is access function return CORBA.Object.Ref;
   --  Allocator type

   procedure Register_Initial_Reference
     (Id        : in Standard.String;
      Allocator : in Create_Ptr);
   --  Register (Id, Allocator) tuple

   procedure Register_Initial_Reference
     (Id  : in Standard.String;
      Ref : in CORBA.Object.Ref);
   --  Register (Id, Ref) tuple

   function Resolve_Initial_References
     (Id : in Standard.String)
     return CORBA.Object.Ref;
   --  Return a valid reference to an object if Id has been previously
   --  registred.
   --  If Id has been registred with a CORBA.Object.Ref, then returns it.
   --  If Id has been registred with an allocator, use this allocator
   --  to create a reference.

   function List_Initial_Services
     return PolyORB.Utils.Strings.Lists.List;
   --  List all registered references.

end PolyORB.CORBA_P.Initial_References;
