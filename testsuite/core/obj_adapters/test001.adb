------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 1                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO;

with PolyORB.Initialization;
with PolyORB.Utils.Report;

with PolyORB.ORB.No_Tasking;
pragma Warnings (Off, PolyORB.ORB.No_Tasking);

with PolyORB.ORB_Controller.Workers;
pragma Warnings (Off, PolyORB.ORB_Controller.Workers);

with PolyORB.Setup.Tasking.No_Tasking;
pragma Warnings (Off, PolyORB.Setup.Tasking.No_Tasking);

with PolyORB.POA.Basic_POA;
with PolyORB.POA_Config.Root_POA;
--  OA to be tested

with Test_Common;

procedure Test001 is

   use Ada.Text_IO;
   use Ada.Exceptions;

   use PolyORB.POA.Basic_POA;

begin
   PolyORB.Initialization.Initialize_World;
   PolyORB.POA_Config.Set_Configuration
     (new PolyORB.POA_Config.Root_POA.Root_POA_Configuration);

   Test_Common.Test_Simple_OA (new Basic_Obj_Adapter);
   PolyORB.Utils.Report.End_Report;

exception
   when E : others =>
      Put_Line ("Got exception "
                & Exception_Name (E)
                & " : "
                & Exception_Message (E));
      PolyORB.Utils.Report.Output ("END TESTS", False);

end Test001;
