------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . D E B U G                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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

--  This file should be compiled with assertion flag only in developper
--  mode for a very special case which allows to track non-termination
--  partitions.

with GNAT.IO;
pragma Elaborate_All (GNAT.IO);

with GNAT.OS_Lib;                     use GNAT.OS_Lib;
pragma Elaborate_All (GNAT.OS_Lib);

with Interfaces.C;                    use Interfaces.C;

with System.Garlic.Platform_Specific; use System.Garlic.Platform_Specific;
pragma Elaborate_All (System.Garlic.Platform_Specific);

package body System.Garlic.Debug is

   Max_Debugs : constant := 25;
   --  If you have more than this number of package to debug, the program
   --  will fail with an assertion error and you will have to increase this
   --  number. Anyway, this is reserved to developpers, so recompiling a
   --  body should not be much work.

   Not_Debugging : constant Debug_Key := 0;
   --  This value is used when we are not debugging

   Current : Debug_Key := 0;
   --  The current debug key

   Flags_Map : array (Debug_Key range 1 .. Max_Debugs,
                      Debug_Level) of Boolean :=
     (others => (others => False));
   --  Map of flags

   Banner_Map : array (Debug_Key range 1 .. Max_Debugs)
     of String_Access;
   --  Map of banners

   Reverse_Character_Map : array (Character) of Debug_Level
     := (others => No_Debug);
   --  Map characters on debug levels

   protected Semaphore is
      entry P;
      procedure V;
   private
      Free : Boolean := True;
   end Semaphore;
   --  The semaphore object which protects outputs from being mixed

   Termination_Filename : String_Access;
   --  Termination temp filename

   Termination_Sanity_FD : File_Descriptor := Invalid_FD;
   --  This file is created at elaboration time and deleted once the
   --  partition has cleanly terminated. This feature is used to detect
   --  incorrect termination.

   ----------------
   -- Debug_Mode --
   ----------------

   function Debug_Mode
     (Level : Debug_Level;
      Key   : Debug_Key)
      return Boolean is
   begin
      return Key /= Not_Debugging and then Flags_Map (Key, Level);
   end Debug_Mode;

   -------------------------------------
   --  Create_Termination_Sanity_File --
   -------------------------------------

   procedure Create_Termination_Sanity_File
   is
      Dir : String renames RTS_Sanity_Directory;

      function Getpid return int;
      pragma Import (C, Getpid, "getpid");

      Name : String (1 .. 32);
      Last : Natural := 0;

      procedure Write_Pid (Pid : int);
      procedure Write_Pid (Pid : int)
      is
      begin
         if Pid > 9 then
            Write_Pid (Pid / 10);
         end if;

         Last := Last + 1;
         Name (Last) := Character'Val ((Pid mod 10) + Character'Pos ('0'));
      end Write_Pid;

   begin
      if Dir'Length /= 0 and then Dir (Dir'Last) /= Directory_Separator then
         Last := Last + 1;
         Name (Last) := Directory_Separator;
      end if;

      Write_Pid (Getpid);

      Last := Last + 4;
      Name (Last - 3 .. Last) := ".dsa";

      Last := Last + 1;
      Name (Last) := Ascii.NUL;

      Termination_Filename := new String'(Dir & Name (1 .. Last));

      Termination_Sanity_FD :=
        Create_New_File (Termination_Filename.all'Address, Binary);

      if Termination_Sanity_FD = Invalid_FD then
         GNAT.IO.Put_Line
           ("Cannot create termination sanity file " &
            Termination_Filename.all);
         raise Program_Error;
      end if;

      Close (Termination_Sanity_FD);
   end Create_Termination_Sanity_File;

   ----------------------
   -- Debug_Initialize --
   ----------------------

   function Debug_Initialize
     (Variable : String;
      Banner   : String)
      return Debug_Key is
      Value : constant String_Access := Getenv (Variable);
      C     : Character;
      L     : Debug_Level;

   begin
      if Value = null or else Value.all = "" then
         return Not_Debugging;
      end if;
      if Current >= Max_Debugs then
         GNAT.IO.Put_Line ("Increase Max_Debugs'value in s-gardeb.adb");
         raise Program_Error;
      end if;
      Current := Current + 1;
      Banner_Map (Current) := new String'(Banner);
      for Index in 1 .. Value'Length loop
         C := Value (Index);
         if C >= 'a' then
            C := Character'Val (Character'Pos (C) - Character'Pos ('a') +
                                Character'Pos ('A'));
         end if;
         L := Reverse_Character_Map (C);
         if L /= No_Debug then
            Flags_Map (Current, L) := True;
         end if;
      end loop;
      return Current;
   end Debug_Initialize;

   ------------------------------------
   -- Delete_Termination_Sanity_File --
   ------------------------------------

   procedure Delete_Termination_Sanity_File is
      Success : Boolean;
   begin
      if Termination_Sanity_FD /= Invalid_FD then
         Delete_File (Termination_Filename.all'Address, Success);
      end if;
   end Delete_Termination_Sanity_File;

   ----------------------
   -- Print_Debug_Info --
   ----------------------

   procedure Print_Debug_Info
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key) is
      Banner : String_Access;
      Flag   : Boolean;

   begin
      if Key /= Not_Debugging then
         Banner := Banner_Map (Key);
         Flag   := Flags_Map (Key, Level);
         if Flag then
            pragma Assert (Banner /= null);
            Semaphore.P;
            GNAT.IO.Put (Banner.all);
            GNAT.IO.Put_Line (Message);
            Semaphore.V;
         end if;
      end if;
   end Print_Debug_Info;

   ---------------
   -- Semaphore --
   ---------------

   protected body Semaphore is

      -------
      -- P --
      -------

      entry P when Free is
      begin
         Free := False;
      end P;

      -------
      -- V --
      -------

      procedure V is
      begin
         Free := True;
      end V;

   end Semaphore;

begin
   for Level in Debug_Level loop
      Reverse_Character_Map (Debug_Letters (Level)) := Level;
   end loop;
   if RTS_Sanity_Directory'Length /= 0 then
      Create_Termination_Sanity_File;
   end if;
end System.Garlic.Debug;
