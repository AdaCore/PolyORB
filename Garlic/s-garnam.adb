------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . G A R L I C . N A M I N G                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Interfaces.C;             use Interfaces.C;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with System.Garlic.Constants;  use System.Garlic.Constants;
with System.Garlic.Debug;      use System.Garlic.Debug;
pragma Elaborate_All (System.Garlic.Debug);
with System.Garlic.Soft_Links;
with System.Garlic.Utils;      use System.Garlic.Utils;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body System.Garlic.Naming is

   use System.Garlic.Thin;

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARNAM", "(s-garnam): ");

   procedure D
     (Message : String;
      Key     : Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Default_Buffer_Size : constant := 16384;

   type Host_Entry (Length : Natural) is
      record
         Name : String (1 .. Length);
         Addr : Address;
      end record;
   --  A complete host structure. A host may have several IP addresses as
   --  well as several aliases.

   function Info_Of (Name : String)
     return Host_Entry;
   --  Host entry of an IP name

   function Info_Of (Addr : Address)
     return Host_Entry;
   --  Host entry of an IP address

   function Info_Of_Name_Or_IP (Something : String)
     return Host_Entry;
   --  Host entry of an IP name or a dotted form

   function Is_IP_Address (Something : String)
     return Boolean;
   --  Return True if the name looks like an IP address, False otherwise

   procedure Free is
      new Ada.Unchecked_Deallocation (char_array, char_array_access);

   function Parse_Entry (Host : Hostent)
     return Host_Entry;
   --  Parse an entry

   procedure Raise_Naming_Error
     (Errno   : Integer;
      Message : String);
   --  Raise the exception Naming_Error with an appropriate error message

   function To_Address (Addr : Thin.In_Addr) return Address;
   --  Convert a In_Addr structure to an IP address

   function Value (Add : String) return Address;
   --  The IP address corresponding to a dotted form

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of (Something : String) return Address is
   begin
      if Is_IP_Address (Something) then
         return Value (Something);
      else
         return Info_Of (Something).Addr;
      end if;
   end Address_Of;

   -----------------
   -- Any_Address --
   -----------------

   function Any_Address return Address is
   begin
      return To_Address (Inaddr_Any);
   end Any_Address;

   ---------------
   -- Host_Name --
   ---------------

   function Host_Name return String
   is
      Buffer : aliased char_array (1 .. 64);
      Res    : constant int := C_Gethostname (Buffer'Address, Buffer'Length);
   begin
      if Res = Failure then
         Raise_Naming_Error (Errno, "");
      end if;
      return To_Ada (Buffer);
   end Host_Name;

   -----------
   -- Image --
   -----------

   function Image (Add : Address) return String
   is

      function Image (A : Address_Component) return String;
      --  Return the string corresponding to its argument without
      --  the leading space.

      -----------
      -- Image --
      -----------

      function Image (A : Address_Component)
        return String
      is
         Im : constant String := Address_Component'Image (A);
      begin
         return Im (Im'First + 1 .. Im'Last);
      end Image;

      I1  : constant String  := Image (Add.H1);
      I2  : constant String  := I1 & '.' & Image (Add.H2);
      I3  : constant String  := I2 & '.' & Image (Add.H3);
      I4  : constant String  := I3 & '.' & Image (Add.H4);

   begin
      return I4;
   end Image;

   -------------
   -- Info_Of --
   -------------

   function Info_Of (Name : String)
     return Host_Entry
   is
      RA : Hostent_Access;
      HN : char_array := To_C (Name);
   begin
      Soft_Links.Enter_Critical_Section;
      RA := C_Gethostbyname (HN);
      if RA = null then
         Soft_Links.Leave_Critical_Section;
         Raise_Naming_Error (Errno, Name);
      end if;
      declare
         HE : constant Host_Entry := Parse_Entry (RA.all);
      begin
         Soft_Links.Leave_Critical_Section;
         return HE;
      end;
   end Info_Of;

   -------------
   -- Info_Of --
   -------------

   function Info_Of (Addr : Address)
     return Host_Entry
   is
      Add : aliased In_Addr := To_In_Addr (Addr);
      Res : Hostent_Access;
   begin
      Soft_Links.Enter_Critical_Section;
      Res := C_Gethostbyaddr (Add'Address,
                              C.int (Add'Size / CHAR_BIT),
                              Af_Inet);
      if Res = null then
         Soft_Links.Leave_Critical_Section;
         Raise_Naming_Error (Errno, Image (Addr));
      end if;
      declare
         Result : constant Host_Entry := Parse_Entry (Res.all);
      begin
         Soft_Links.Leave_Critical_Section;
         return Result;
      end;
   end Info_Of;

   ------------------------
   -- Info_Of_Name_Or_IP --
   ------------------------

   function Info_Of_Name_Or_IP (Something : String)
     return Host_Entry
   is
   begin
      if Is_IP_Address (Something) then
         return Info_Of (Value (Something));
      else
         return Info_Of (Something);
      end if;
   end Info_Of_Name_Or_IP;

   -------------------
   -- Is_Ip_Address --
   -------------------

   function Is_IP_Address (Something : String)
     return Boolean
   is
   begin
      for Index in Something'Range loop
         declare
            Current : Character renames Something (Index);
         begin
            if (Current < '0'
                or else Current > '9')
              and then Current /= '.' then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Is_IP_Address;

   -------------
   -- Name_Of --
   -------------

   function Name_Of (Something : String)
     return String
   is
      Hostent : Host_Entry
        := Info_Of_Name_Or_IP (Image (Address_Of (Something)));
   begin
      if Hostent.Length = 0 then
         Ada.Exceptions.Raise_Exception
           (Naming_Error'Identity, "No name for " & Something);
      end if;
      To_Lower (Hostent.Name);
      return Hostent.Name;
   end Name_Of;

   -----------------
   -- Parse_Entry --
   -----------------

   function Parse_Entry (Host : Hostent)
     return Host_Entry
   is
      C_Addr : constant In_Addr_Access_Array
        := In_Addr_Access_Pointers.Value (Host.H_Addr_List);

      Name   : constant String := Value (Host.H_Name);
      Result : Host_Entry (Name'Length);
   begin
      Result.Name := Name;
      Result.Addr := To_Address (C_Addr (C_Addr'First).all);
      return Result;
   end Parse_Entry;

   ------------------------
   -- Raise_Naming_Error --
   ------------------------

   procedure Raise_Naming_Error
     (Errno   : Integer;
      Message : String)
   is

      function Error_Message return String;
      --  Return the message according to Errno.

      -------------------
      -- Error_Message --
      -------------------

      function Error_Message return String is
      begin
         case Errno is
            when Host_Not_Found => return "Host not found";
            when Try_Again      => return "Try again";
            when No_Recovery    => return "No recovery";
            when No_Address     => return "No address";
            when others         => return "Unknown error" &
                                          Integer'Image (Errno);
         end case;
      end Error_Message;

   begin
      Ada.Exceptions.Raise_Exception
        (Naming_Error'Identity, Error_Message & ": " & Message);
   end Raise_Naming_Error;

   ----------------
   -- To_Address --
   ----------------

   function To_Address (Addr : In_Addr) return Address is
   begin
      return (H1 => Address_Component (Addr.S_B1),
              H2 => Address_Component (Addr.S_B2),
              H3 => Address_Component (Addr.S_B3),
              H4 => Address_Component (Addr.S_B4));
   end To_Address;

   ----------------
   -- To_In_Addr --
   ----------------

   function To_In_Addr (Addr : Address) return In_Addr is
   begin
      return (S_B1 => unsigned_char (Addr.H1),
              S_B2 => unsigned_char (Addr.H2),
              S_B3 => unsigned_char (Addr.H3),
              S_B4 => unsigned_char (Addr.H4));
   end To_In_Addr;

   -----------
   -- Value --
   -----------

   function Value (Add : String) return Address
   is
      function Convert is
         new Ada.Unchecked_Conversion (Source => Interfaces.Unsigned_32,
                                       Target => In_Addr);
      Converted : constant In_Addr := Convert (C_Inet_Addr (To_C (Add)));
   begin
      return (H1 => Address_Component (Converted.S_B1),
              H2 => Address_Component (Converted.S_B2),
              H3 => Address_Component (Converted.S_B3),
              H4 => Address_Component (Converted.S_B4));
   end Value;

end System.Garlic.Naming;
