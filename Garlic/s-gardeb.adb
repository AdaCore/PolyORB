------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . D E B U G                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
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
with GNAT.OS_Lib;                     use GNAT.OS_Lib;
with Interfaces.C;                    use Interfaces.C;
with System.Garlic.Platform_Specific; use System.Garlic.Platform_Specific;

package body System.Garlic.Debug is

   Not_Debugging : constant Debug_Key := 0;
   --  This value is used when we are not debugging

   Current : Debug_Key := 0;
   --  The current debug key

   Banner_Map : array (Debug_Key) of String_Access;
   --  Map of banners

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

   -------------------------------------
   --  Create_Termination_Sanity_File --
   -------------------------------------

   procedure Create_Termination_Sanity_File
   is
      Dir : String renames RTS_Sanity_Directory;

      function Get_PID return int;
      pragma Import (C, Get_PID, "getpid");

      Name : String (1 .. 32);
      Last : Natural := 0;

      procedure Write_PID (PID : in int := Get_PID);
      --  Store PID in Name

      ---------------
      -- Write_PID --
      ---------------

      procedure Write_PID (PID : in int := Get_PID) is
      begin
         if PID < 10 then
            Last := Last + 1;
            Name (Last) := Character'Val (PID + Character'Pos ('0'));
         else
            Write_PID (PID / 10);
            Write_PID (PID mod 10);
         end if;
      end Write_PID;

   begin
      if Dir'Length /= 0 and then Dir (Dir'Last) /= Directory_Separator then
         Last := Last + 1;
         Name (Last) := Directory_Separator;
      end if;

      Write_PID;

      Termination_Filename :=
       new String'(Dir & Name (1 .. Last) & ".dsa" & Ascii.NUL);

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
      return Debug_Key
   is
      Value        : String_Access    := Getenv (Variable);
      Value_Not_OK : constant Boolean := Value'Length = 0
        or else (Value (Value'First) = 't' and then Value (Value'First) = 'T');
   begin
      Free (Value);
      if Value_Not_OK then
         return Not_Debugging;
      end if;
      if Current >= Debug_Key'Last then
         Semaphore.P;
         GNAT.IO.Put_Line ("Change Debug_Key range in s-gardeb.ads");
         Semaphore.V;
         raise Program_Error;
      end if;
      Current := Current + 1;
      Banner_Map (Current) := new String'(Banner);
      return Current;
   end Debug_Initialize;

   ----------------
   -- Debug_Mode --
   ----------------

   function Debug_Mode
     (Key   : Debug_Key)
      return Boolean
   is
   begin
      return Key /= Not_Debugging and then Banner_Map (Key) /= null;
   end Debug_Mode;

   ------------------------------------
   -- Delete_Termination_Sanity_File --
   ------------------------------------

   procedure Delete_Termination_Sanity_File is
      Success : Boolean;
   begin
      if Termination_Sanity_FD /= Invalid_FD then
         pragma Assert (Termination_Filename /= null);
         Delete_File (Termination_Filename.all'Address, Success);
      end if;
   end Delete_Termination_Sanity_File;

   ----------------------
   -- Print_Debug_Info --
   ----------------------

   procedure Print_Debug_Info
     (Message : in String;
      Key     : in Debug_Key)
   is
   begin
      if Key /= Not_Debugging then
         Semaphore.P;
         GNAT.IO.Put_Line (Banner_Map (Key).all & Message);
         Semaphore.V;
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
   if RTS_Sanity_Directory'Length /= 0 then
      Create_Termination_Sanity_File;
   end if;
end System.Garlic.Debug;
