--
--  $Id$
--
--  Mappings between IP names and IP addresses.
--

with Ada.Finalization;
with System.Garlic.Thin;

package System.Garlic.Naming is

   type String_Access is access all String;
   type String_Array is array (Positive range <>) of String_Access;

   subtype Address_Component is Natural range 0 .. 255;

   type Address is record
      H1, H2, H3, H4 : Address_Component;
   end record;
   --  An address such as 137.194.160.12

   type Address_Array is array (Positive range <>) of Address;

   type Host_Entry (N_Aliases, N_Addresses : Natural) is
      new Ada.Finalization.Controlled with record
         Name      : String_Access;
         Aliases   : String_Array  (1 .. N_Aliases);
         Addresses : Address_Array (1 .. N_Addresses);
      end record;
   --  A complete host structure. A host may have several IP addresses as
   --  well as several aliases.

   procedure Adjust   (Object : in out Host_Entry);
   procedure Finalize (Object : in out Host_Entry);

   Naming_Error : exception;
   --  This exception is raised when a name cannot be resolved.

   function Image (Add : Address) return String;
   --  The dotted form corresponding to an IP address.

   function Value (Add : String) return Address;
   --  The IP address corresponding to a dotted form.

   function Info_Of (Name : String)
     return Host_Entry;
   --  Host entry of an IP name.

   function Info_Of (Addr : Address)
     return Host_Entry;
   --  Host entry of an IP address.

   function Is_IP_Address (Something : String)
     return Boolean;
   --  Return True if the name looks like an IP address, False otherwise.

   function Info_Of_Name_Or_IP (Something : String)
     return Host_Entry;
   --  Host entry of an IP name or a dotted form..

   function Address_Of (Something : String) return Address;
   --  Address of an IP name or a dotted form.

   function Host_Name return String;
   --  Return the name of the current host.

   function Name_Of (Something : String) return String;
   --  Return the official name of an IP name or a dotted form.

   function To_In_Addr (Addr : Address) return Thin.In_Addr;
   --  Convert an IP address to a In_Addr structure.

   function To_Address (Addr : Thin.In_Addr) return Address;
   --  Convert a In_Addr structure to an IP address.

   function Any_Address return Address;
   --  Return the value of inaddr_any.

end System.Garlic.Naming;
