with CORBA.Object;

package Naming_Tools is

   --  This package allows an object to be chosen either by its IOR or by
   --  its name in the naming service.

   function Locate (IOR_Or_Name : String) return CORBA.Object.Ref;
   --  Locate an object by IOR or name

   procedure Register
     (Name   : in String;
      Ref    : in CORBA.Object.Ref;
      Rebind : in Boolean := False);
   --  Register an object by its name by binding or rebinding

   procedure Unregister (Name : in String);
   --  Unregister an object by its name by unbinding it.

end Naming_Tools;
