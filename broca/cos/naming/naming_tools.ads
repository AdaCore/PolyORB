with CORBA.Object;

package Naming_Tools is

   --  This package allows an object to be chosen either by its IOR or by
   --  its name in the naming service.

   function Locate (IOR_Or_Name : String) return CORBA.Object.Ref;
   --  Locate an object by IOR or name. If the string does not start with
   --  "IOR:", the name will be parsed (components are separated with /,
   --  starting with the root naming service) and looked up.

   procedure Register
     (Name   : in String;
      Ref    : in CORBA.Object.Ref;
      Rebind : in Boolean := False);
   --  Register an object by its name by binding or rebinding. A simple name
   --  must be given, no parsing will be made on the string.

   procedure Unregister (Name : in String);
   --  Unregister an object by its name by unbinding it.

end Naming_Tools;
