------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               T E S T _ S U I T E . O U T P U T . T E X T                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;

package body Test_Suite.Output.Text is

   use Ada.Text_IO;

   ----------
   -- Open --
   ----------

   procedure Open (Output : Text_Output)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      null;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Output : Text_Output)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      null;
   end Close;

   -----------
   -- Error --
   -----------

   procedure Error (Output    : Text_Output;
                    Error_Msg : String)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      Put_Line (Standard_Error, Error_Msg);
   end Error;

   ---------
   -- Log --
   ---------

   procedure Log (Output : Text_Output;
                  Log_Msg : String)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      Put_Line (Log_Msg);
   end Log;

   ---------------
   -- Separator --
   ---------------

   procedure Separator (Output : Text_Output)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      New_Line;
   end Separator;

   ------------------------------
   -- Open_test_Output_Context --
   ------------------------------

   procedure Open_Test_Output_Context
     (Output : Text_Output;
      Name   : String)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      New_Line;
      Put_Line ("-- Begin of Test " & Name);
      New_Line;
   end Open_Test_Output_Context;

   -------------------------------
   -- Close_Test_Output_Context --
   -------------------------------

   procedure Close_Test_Output_Context
     (Output : Text_Output;
      Result : Boolean)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Unreferenced (Result);
      pragma Warnings (On); --  WAG:3.14
   begin
      New_Line;
      Put_Line ("-----------------------------");
      New_Line;
   end Close_Test_Output_Context;

   ----------------------------------
   -- Open_Scenario_Output_Context --
   ----------------------------------

   procedure Open_Scenario_Output_Context
     (Output : Text_Output;
      Name   : String)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      New_Line;
      Put_Line ("///////////// Begin of Scenario " & Name);
      New_Line;
   end Open_Scenario_Output_Context;

   -----------------------------------
   -- Close_Scenario_Output_Context --
   -----------------------------------

   procedure Close_Scenario_Output_Context
     (Output : Text_Output;
      Result : Boolean)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Unreferenced (Result);
      pragma Warnings (On); --  WAG:3.14
   begin
      New_Line;
      Put_Line ("///////////////////////////////////////////////////////////");
      New_Line;
   end Close_Scenario_Output_Context;

   --------------------
   -- Test_Execution --
   --------------------

   procedure Test_Execution
     (Output : Text_Output;
      Msg    : String)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14
   begin
      Put (Msg);
   end Test_Execution;

end Test_Suite.Output.Text;
