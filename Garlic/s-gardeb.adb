------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . G A R L I C . D E B U G                    --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
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
--               GARLIC is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------


with GNAT.Os_Lib;
with System.IO;

package body System.Garlic.Debug is

   Max_Debugs : constant := 25;
   --  If you have more than this number of package to debug, the program
   --  will fail with an assertion error and you will have to increase this
   --  number. Anyway, this is reserved to developpers, so recompiling a
   --  body should not be much work.

   Not_Debugging : constant Debug_Key := 0;
   --  This value is used when we are not debugging.

   Current : Debug_Key := 0;
   --  The current debug key.

   Flags_Map : array (Debug_Key range 1 .. Max_Debugs,
                      Debug_Level) of Boolean :=
     (others => (others => False));
   --  Map of flags.

   Banner_Map : array (Debug_Key range 1 .. Max_Debugs)
     of GNAT.Os_Lib.String_Access;
   --  Map of banners.

   Assertions_Turned_On : Boolean := False;
   --  This boolean will be set to true by the elaboration if assertions
   --  are turned on.

   Reverse_Character_Map : array (Character) of Debug_Level
     := (others => No_Debug);
   --  Map characters on debug levels.

   protected Semaphore is
      entry P;
      procedure V;
   private
      Free : Boolean := True;
   end Semaphore;
   --  The semaphore object which protects outputs from being mixed.

   function Return_True return Boolean;
   --  Return True and set Assertions_Turned_On to True.

   function Real_Debug_Initialize
     (Variable : String;
      Banner   : String)
     return Debug_Key;
   --  This program is the real Debug_Initialize.

   procedure Real_Print_Debug_Info
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key);
   --  This program is the real Print_Debug_Info.

   function Real_Debug_Mode
     (Level : Debug_Level;
      Key   : Debug_Key)
      return Boolean;
   pragma Inline (Real_Debug_Mode);
   --  This program is the real Debug_Mode.

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

   ---------------------
   -- Real_Debug_Mode --
   ---------------------

   function Real_Debug_Mode
     (Level : Debug_Level;
      Key   : Debug_Key)
      return Boolean
   is
   begin
      return Key /= Not_Debugging and then Flags_Map (Key, Level);
   end Real_Debug_Mode;

   ----------------
   -- Debug_Mode --
   ----------------

   function Debug_Mode
     (Level : Debug_Level;
      Key   : Debug_Key)
      return Boolean
   is
   begin
      return Assertions_Turned_On and then Real_Debug_Mode (Level, Key);
   end Debug_Mode;

   -----------------
   -- Return_True --
   -----------------

   function Return_True return Boolean is
   begin
      Assertions_Turned_On := True;
      return True;
   end Return_True;

   ----------------------
   -- Debug_Initialize --
   ----------------------

   function Debug_Initialize
     (Variable : String;
      Banner   : String)
      return Debug_Key
   is
   begin
      if Assertions_Turned_On then
         return Real_Debug_Initialize (Variable, Banner);
      end if;
      return Not_Debugging;
   end Debug_Initialize;

   ----------------------
   -- Print_Debug_Info --
   ----------------------

   procedure Print_Debug_Info
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key)
   is
   begin
      pragma Debug (Real_Print_Debug_Info (Level, Message, Key));
      null;
   end Print_Debug_Info;

   ---------------------------
   -- Real_Print_Debug_Info --
   ---------------------------

   procedure Real_Print_Debug_Info
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key)
   is
      use type GNAT.Os_Lib.String_Access;
      Banner : GNAT.Os_Lib.String_Access;
      Flag   : Boolean;
   begin
      if Key /= Not_Debugging then
         Banner := Banner_Map (Key);
         Flag   := Flags_Map (Key, Level);
         if Flag then
            pragma Assert (Banner /= null);
            Semaphore.P;
            System.IO.Put (Banner.all);
            System.IO.Put_Line (Message);
            Semaphore.V;
         end if;
      end if;
   end Real_Print_Debug_Info;

   ---------------------------
   -- Real_Debug_Initialize --
   ---------------------------

   function Real_Debug_Initialize
     (Variable : String;
      Banner   : String)
      return Debug_Key
   is
      use type GNAT.Os_Lib.String_Access;
      Value : constant GNAT.Os_Lib.String_Access :=
        GNAT.Os_Lib.Getenv (Variable);
      C     : Character;
      L     : Debug_Level;
   begin
      if Value = null or else Value.all = "" then
         return Not_Debugging;
      end if;
      pragma Assert (Current < Max_Debugs,
                     "Increase Max_Debugs'value in s-gardeb.adb");
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
   end Real_Debug_Initialize;

begin
   pragma Assert (Return_True);
   for Level in Debug_Level loop
      pragma Assert (Reverse_Character_Map (Debug_Letters (Level)) = No_Debug);
      Reverse_Character_Map (Debug_Letters (Level)) := Level;
   end loop;
end System.Garlic.Debug;
