------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 T E S T _ S U I T E . S C E N A R I O S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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

--  $Id$

with GNAT.Directory_Operations.Iteration;
with PolyORB.Parameters.File;

with Test_Suite.Test_Case.Parser;

package body Test_Suite.Scenarios is

   Invalid_Scenario : exception;

   -------------------
   -- Open_Scenario --
   -------------------

   function Open_Scenario
     (Scenario_File : String;
      Index         : Positive;
      Output        : Test_Suite_Output'Class)
     return String
   is
      use PolyORB.Parameters;
      use PolyORB.Parameters.File;

   begin
      Load_Configuration_File (Scenario_File);

      declare
         Scenario_Name : constant String
           := Get_Conf ("scenario", "name");

         Scenario_Id : constant String
           := Get_Conf ("scenario", "id");

      begin
         if Scenario_Name = "" then
            raise Invalid_Scenario;
         end if;

         Open_Scenario_Output_Context (Output, Scenario_Name);

         Log (Output, "Opening scenario #"
              & Positive'Image (Index)
              & ": "
              & Scenario_Name);
         Log (Output, "Description: " & Scenario_Id);

         return Scenario_Name;
      end;
   end Open_Scenario;

   ------------------
   -- Run_Scenario --
   ------------------

   procedure Run_Scenario
     (Scenario_File : String;
      Index         : Positive;
      Output        : Test_Suite_Output'Class)
   is
      use Test_Suite.Test_Case.Parser;
      use Test_Suite.Test_Case;

   begin
      declare
         Count : Natural := 0;

         Scenario_Name : constant String
           := Open_Scenario (Scenario_File, Index, Output);

         Result : Boolean := True;

      begin

         loop
            declare
               Extracted_Test : Test'Class
                 := Extract_Test (Scenario_Name, Count, Output);

            begin
               exit when Extracted_Test in Null_Test;

               Result := Result and Run_Test (Extracted_Test, Output);
               Count := Count + 1;

               delay 1.0;
            end;
         end loop;

         Log (Output, "All tests done in scenario: " & Scenario_Name);
         Separator (Output);

         Close_Scenario_Output_Context (Output, Result);
         PolyORB.Parameters.Reset;
      end;

   exception
      when others =>
         Log (Output, "Error in scenario file: " & Scenario_File);
         Separator (Output);

   end Run_Scenario;

   -----------------------
   -- Run_All_Scenarios --
   -----------------------

   procedure Run_All_Scenarios
     (Directory_Name : String;
      Output         : Test_Suite_Output'Class)
   is
      procedure Run_Scenario_Wrapper
        (Scenario_File : String;
         Index         : Positive;
         Quit          : in out Boolean);

      procedure Run_Scenario_Wrapper
        (Scenario_File : String;
         Index         : Positive;
         Quit          : in out Boolean) is
      begin
         Run_Scenario (Scenario_File, Index, Output);
         Quit := False;
      end Run_Scenario_Wrapper;

      procedure Run_Scenario_With_Pattern is new
        GNAT.Directory_Operations.Iteration.Find (Run_Scenario_Wrapper);

   begin
      Log (Output, "Running all scenario from: " & Directory_Name);
      Separator (Output);

      Run_Scenario_With_Pattern (Directory_Name, "(.*)-(.*)\.conf");
   end Run_All_Scenarios;

end Test_Suite.Scenarios;
