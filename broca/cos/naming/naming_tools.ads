with Ada.Finalization;
with CORBA.Object;
with CosNaming;

package Naming_Tools is

   --  This package allows an object to be chosen either by its IOR or by
   --  its name in the naming service.

   function Locate
     (Name : CosNaming.Name)
     return CORBA.Object.Ref;
   --  Locate an object given its name, given as an array of name components.

   function Locate
     (IOR_Or_Name : String;
      Sep : Character := '/')
     return CORBA.Object.Ref;
   --  Locate an object by IOR or name. If the string does not start with
   --  "IOR:", the name will be parsed before it is looked up, using
   --  Parse_Name below.

   procedure Register
     (Name   : in String;
      Ref    : in CORBA.Object.Ref;
      Rebind : in Boolean := False;
      Sep    : in Character := '/');
   --  Register an object by its name by binding or rebinding.
   --  The name will be parsed by Parse_Name below; any necessary contexts
   --  will be created on the name server.
   --  If Rebind is True, then a rebind will be performed if the name
   --  is already bound.

   procedure Unregister (Name : in String);
   --  Unregister an object by its name by unbinding it.

   type Server_Guard is limited private;
   procedure Register
     (Guard  : in out Server_Guard;
      Name   : in String;
      Ref    : in CORBA.Object.Ref;
      Rebind : in Boolean := False;
      Sep    : in Character := '/');
   --  A Server_Guard object is an object which is able to register a
   --  server reference in a naming service (see Register above), and
   --  destroy this name using Unregister when the object disappears
   --  (the program terminates or the Server_Guard object lifetime has
   --  expired).

   function Parse_Name
     (Name : String;
      Sep : Character := '/')
     return CosNaming.Name;
   --  Split a sequence of name component specifications separated
   --  with Sep characters into a name component array. Any leading
   --  Sep is ignored.

private

   type Server_Guard is new Ada.Finalization.Limited_Controlled with record
      Name : CORBA.String := CORBA.To_CORBA_String ("");
   end record;

   procedure Finalize (Guard : in out Server_Guard);

end Naming_Tools;
