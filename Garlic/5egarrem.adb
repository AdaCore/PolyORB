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
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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

with Ada.Unchecked_Deallocation;

with GNAT.IO;
with GNAT.Sockets;                    use GNAT.Sockets;

with Interfaces.C;                    use Interfaces.C;

with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Options;
with System.Garlic.Partitions;        use System.Garlic.Partitions;
with System.Garlic.Platform_Specific; use System.Garlic.Platform_Specific;
with System.Garlic.Utils;             use System.Garlic.Utils;

package body System.Garlic.Remote is

   package C renames Interfaces.C;
   --  Shortcuts

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARREM", "(s-garrem): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   procedure C_Dup2
     (Fd1, Fd2 : in C.int);
   pragma Import (C, C_Dup2, "dup2");

   function C_Open
     (Path  : C.char_array;
      Oflag : C.int;
      Mode  : C.int := 0)
     return C.int;
   pragma Import (C, C_Open, "open");

   procedure C_Setsid;
   pragma Import (C, C_Setsid, "setsid");

   function C_System (Command : System.Address) return C.int;
   pragma Import (C, C_System, "system");

   function Is_Local_Host (Host : String) return Boolean;
   --  Return True if the Host we are trying to contact is the same as the
   --  local host.

   procedure Spawn (Command : in String);

   type Partition_Info;
   type Partition_List is access Partition_Info;

   type Partition_Info is record
      Remote_Host  : String_Access;
      Command_Line : String_Access;
      Next         : Partition_List;
   end record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Partition_Info, Partition_List);

   List : Partition_List;


   ------------
   -- Detach --
   ------------

   procedure Detach is
      Dev_Null      : C.int;
      Dev_Null_Name : constant C.char_array := To_C ("/dev/null");
   begin
      if Options.Detach then
         Dev_Null := C_Open (Dev_Null_Name, 2);
         C_Dup2 (Dev_Null, 0);
         C_Dup2 (Dev_Null, 1);
         C_Dup2 (Dev_Null, 2);
         C_Setsid;
      end if;
   end Detach;

   -----------------
   -- Full_Launch --
   -----------------

   procedure Full_Launch
     (Host        : in String;
      Command     : in String)
   is
      Arguments : constant String :=
        "--detach --boot_location '" & Get_Boot_Locations & "' &";
   begin
      if Supports_Local_Launch
        and then Host (Host'First) /= '`'
        and then Is_Local_Host (Host)
      then
         declare
            C1 : constant String := Quote (Command) & ' ' & Arguments;

         begin
            pragma Debug (D ("Run Spawn: " & C1));

            Spawn (C1);
         end;

      else
         declare
            C1 : constant String := Command;
            C2 : constant String := Quote (C1 & ' ' & Arguments);
            C3 : constant String := Host & ' ' & Rsh_Options;
            C4 : constant String := Rsh_Command & ' ' & C3;
            C5 : constant String := C4 & ' ' & C2;
            C6 : constant String := C5 & " < /dev/null > /dev/null 2>&1";

         begin

            pragma Debug (D ("Run Spawn: " & C6));

            Spawn (C6);
         end;
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
      Name_Of_Host : constant String
        := Official_Name (Get_Host_By_Name (Host));
   begin
      return Host = "localhost"
        or else Name_Of_Host = "localhost"
        or else Name_Of_Host = Official_Name (Get_Host_By_Name (Host_Name));
   end Is_Local_Host;

   ----------------------------------
   -- Launch_Registered_Partitions --
   ----------------------------------

   procedure Launch_Registered_Partitions
   is
      P : Partition_List;

   begin
      if Options.Nolaunch then
         return;
      end if;

      while List /= null loop
         P    := List;
         List := List.Next;

         Full_Launch
           (P.Remote_Host.all,
            P.Command_Line.all);

         Destroy (P.Remote_Host);
         Destroy (P.Command_Line);
         Free (P);
      end loop;
   end Launch_Registered_Partitions;

   ----------------------------------
   -- Register_Partition_To_Launch --
   ----------------------------------

   procedure Register_Partition_To_Launch
     (Name_Is_Host : in Boolean;
      General_Name : in String;
      Command_Line : in String)
   is
      P : Partition_List;

   begin
      P := new Partition_Info;
      P.Command_Line := new String'(Command_Line);
      if Name_Is_Host then
         P.Remote_Host := new String'(General_Name);
      else
         P.Remote_Host := new String'(Get_Host (General_Name));
      end if;
      P.Next := List;
      List   := P;
   end Register_Partition_To_Launch;

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
