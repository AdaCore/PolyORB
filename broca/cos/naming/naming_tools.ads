with Ada.Finalization;
with CORBA.Object;
with CosNaming;

package Naming_Tools is

   --  This package allows an object to be chosen either by its IOR or by
   --  its name in the naming service.

   subtype NameComponent_Array is
     CosNaming.IDL_SEQUENCE_CosNaming_NameComponent.Element_Array;

   function Locate
     (Name : NameComponent_Array)
     return CORBA.Object.Ref;
   --  Locate an object given its name, given as an array of name components.

   function Locate
     (IOR_Or_Name : String)
     return CORBA.Object.Ref;
   --  Locate an object by IOR or name. If the string does not start with
   --  "IOR:", the name will be parsed before it is looked up.
   --  For the purpose of this parsing, components are separated with /,
   --  starting with the root naming service. For each component, the id
   --  and kind are separated with a period (the kind is expected not
   --  to contain a period); no period means an empty kind).

   procedure Register
     (Name   : in String;
      Ref    : in CORBA.Object.Ref;
      Rebind : in Boolean := False);
   --  Register an object by its name by binding or rebinding. A simple name
   --  must be given, no parsing will be made on the string. If Rebind is
   --  True, then a bind then a rebind will be attempted.

   procedure Unregister (Name : in String);
   --  Unregister an object by its name by unbinding it.

   type Server_Guard is limited private;
   procedure Register
     (Guard  : in out Server_Guard;
      Name   : in String;
      Ref    : in CORBA.Object.Ref;
      Rebind : in Boolean := False);
   --  A Server_Guard object is an object which is able to register a
   --  server reference in a naming service (see Register above), and
   --  destroy this name using Unregister when the object disappears
   --  (the program terminates or the Server_Guard object lifetime has
   --  expired).

private

   type Server_Guard is new Ada.Finalization.Limited_Controlled with record
      Name : CORBA.String := CORBA.To_CORBA_String ("");
   end record;

   procedure Finalize (Guard : in out Server_Guard);

end Naming_Tools;
