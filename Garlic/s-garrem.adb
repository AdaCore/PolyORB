------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . G A R L I C . R E M O T E                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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

with GNAT.IO;
with Interfaces.C;                    use Interfaces.C;
with System.Garlic.Constants;         use System.Garlic.Constants;
with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Naming;            use System.Garlic.Naming;
with System.Garlic.Partitions;        use System.Garlic.Partitions;
with System.Garlic.Platform_Specific; use System.Garlic.Platform_Specific;
with System.Garlic.Thin;              use System.Garlic.Thin;

package body System.Garlic.Remote is

   package C renames Interfaces.C;
   --  Shortcuts

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARREM", "(s-garrem): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   function Is_Local_Host (Host : String) return Boolean;
   --  Return True if the Host we are trying to contact is the same as the
   --  local host.

   procedure Spawn (Command : in String);

   ------------
   -- Detach --
   ------------

   procedure Detach is
      Dev_Null      : C.int;
      Dev_Null_Name : constant C.char_array := To_C ("/dev/null");
   begin
      Dev_Null := C_Open (Dev_Null_Name, O_Rdwr);
      C_Dup2 (Dev_Null, 0);
      C_Dup2 (Dev_Null, 1);
      C_Dup2 (Dev_Null, 2);
      C_Setsid;
   end Detach;

   -----------------
   -- Full_Launch --
   -----------------

   procedure Full_Launch
     (Rsh_Command : in String;
      Host        : in String;
      Rsh_Options : in String;
      Command     : in String)
   is
      Arguments : constant String :=
        "--detach --boot_location '" & Get_Boot_Locations & "' &";
   begin
      if Supports_Local_Launch
        and then Host (Host'First) /= '`'
        and then Is_Local_Host (Host)
      then
         pragma Debug (D ("Run Spawn: """ & Command & """ " & Arguments));

         Spawn ("""" & Command & """ " & Arguments);
      else
         pragma Debug (D ("Run Spawn: " & Rsh_Command & " " & Host & " " &
                          Rsh_Options & " ""'" &
                          Command & "' " & Arguments &
                          """ < /dev/null > /dev/null 2>&1"));

         Spawn (Rsh_Command & " " & Host & " " & Rsh_Options & " ""'" &
                Command & "' " & Arguments &
                """ < /dev/null > /dev/null 2>&1");
      end if;
   end Full_Launch;

   --------------
   -- Get_Host --
   --------------

   function Get_Host
     (Partition : String)
     return String
   is
      Buffer : String (1 .. 64);
      Last   : Natural;
   begin
      GNAT.IO.Put ("Host for """ & Partition & """: ");
      GNAT.IO.Get_Line (Buffer, Last);
      return Buffer (1 .. Last);
   end Get_Host;

   -------------------
   -- Is_Local_Host --
   -------------------

   function Is_Local_Host
     (Host : String)
     return Boolean
   is
      Name_Of_Host : constant String := Name_Of (Host);
   begin
      return Host = "localhost"
        or else Name_Of_Host = "localhost"
        or else Name_Of_Host = Name_Of (Host_Name);
   end Is_Local_Host;

   -----------
   -- Spawn --
   -----------

   procedure Spawn (Command : in String)
   is
      C_Command : aliased String := Command & ASCII.NUL;
   begin
      if C_System (C_Command'Address) / 256 /= 0 then
         raise Program_Error;
      end if;
   end Spawn;


end System.Garlic.Remote;
