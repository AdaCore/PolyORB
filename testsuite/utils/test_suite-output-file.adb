------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               T E S T _ S U I T E . O U T P U T . T E X T                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Directory_Operations;
with GNAT.IO_Aux;

package body Test_Suite.Output.File is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use GNAT.Directory_Operations;
   use GNAT.IO_Aux;

   Error_File   : File_Type;
   Log_File     : File_Type;
   Tests_Failed : File_Type;

   Test_File  : File_Type;
   Test_Name  : Unbounded_String;
   In_Test    : Boolean := False;

   Initial_Dir : Unbounded_String;
   Current_Dir : Unbounded_String;
   Base_Output_Dir_Name : constant String := "output";

   ----------
   -- Open --
   ----------

   procedure Open (Output : File_Output)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14

   begin
      Initial_Dir := To_Unbounded_String (Get_Current_Dir);

      if not File_Exists (Base_Output_Dir_Name) then
         Make_Dir (Base_Output_Dir_Name);
      end if;

      Change_Dir (Base_Output_Dir_Name);

      Create (Error_File, Out_File, "error");
      Create (Log_File, Out_File, "log");
      Create (Tests_Failed, Out_File, "failed");

      Change_Dir (To_String (Initial_Dir));
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Output : File_Output)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      Close (Error_File);
      Close (Log_File);
      Close (Tests_Failed);
   end Close;

   -----------
   -- Error --
   -----------

   procedure Error (Output    : File_Output;
                    Error_Msg : String)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      Put_Line (Error_File, Error_Msg);
   end Error;

   ---------
   -- Log --
   ---------

   procedure Log (Output : File_Output;
                  Log_Msg : String)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      if In_Test then
         Put_Line (Test_File, Log_Msg);
      else
         Put_Line (Log_File, Log_Msg);
      end if;
   end Log;

   ---------------
   -- Separator --
   ---------------

   procedure Separator (Output : File_Output)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      if In_Test then
         New_Line (Test_File);
      else
         New_Line (Log_File);
      end if;
   end Separator;

   ------------------------------
   -- Open_Test_Output_Context --
   ------------------------------

   procedure Open_Test_Output_Context
     (Output : File_Output;
      Name   : String)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      Test_Name := To_Unbounded_String (Name);
      In_Test := True;

      Change_Dir (To_String (Current_Dir));
      Create (Test_File, Out_File, Name);
      Change_Dir (To_String (Initial_Dir));

      New_Line (Test_File);
      Put_Line (Test_File, "-- Begin of Test " & Name);
      New_Line (Test_File);
   end Open_Test_Output_Context;

   -------------------------------
   -- Close_Test_Output_Context --
   -------------------------------

   procedure Close_Test_Output_Context
     (Output : File_Output;
      Result : Boolean)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      New_Line (Test_File);
      Put_Line (Test_File, "-----------------------------");
      New_Line (Test_File);

      Close (Test_File);
      In_Test := False;

      if not Result then
         Put_Line (Tests_Failed, To_String (Test_Name));
      end if;
   end Close_Test_Output_Context;

   ----------------------------------
   -- Open_Scenario_Output_Context --
   ----------------------------------

   procedure Open_Scenario_Output_Context
     (Output : File_Output;
      Name   : String)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14

   begin
      Change_Dir (Base_Output_Dir_Name);
      if not File_Exists (Name) then
         Make_Dir (Name);
      end if;

      Change_Dir (To_String (Initial_Dir));

      Current_Dir := To_Unbounded_String
        (Base_Output_Dir_Name
         & Dir_Separator
         & Name);
   end Open_Scenario_Output_Context;

   -----------------------------------
   -- Close_Scenario_Output_Context --
   -----------------------------------

   procedure Close_Scenario_Output_Context
     (Output : File_Output;
      Result : Boolean)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Unreferenced (Result);
      pragma Warnings (On); --  WAG:3.14
   begin
      null;
   end Close_Scenario_Output_Context;

   --------------------
   -- Test_Execution --
   --------------------

   procedure Test_Execution
     (Output : File_Output;
      Msg    : String)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      Put (Test_File, Msg);
   end Test_Execution;

end Test_Suite.Output.File;
