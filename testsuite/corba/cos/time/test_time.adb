------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            T E S T _ T I M E                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;

with CORBA.Impl;
with CORBA.ORB;

with PortableServer;

with CosTime.TimeService.Impl;
with CosTime.TIO;
with CosTime.UTO;
with TimeBase;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with PolyORB.Utils.Report;

procedure Test_Time is

   use Ada.Text_IO;

   use CosTime;
   use CosTime.TimeService;
   use CosTime.TIO;
   use CosTime.UTO;
   use TimeBase;

   use PolyORB.Utils.Report;

   Ref : CosTime.TimeService.Ref;

   UTO1, UTO2 : UTO.Ref;
   TIO1       : TIO.Ref;

   -------------
   -- Display --
   -------------

   procedure Display (Time : TIO.Ref);

   procedure Display (Time : TIO.Ref) is
      IT : constant IntervalT := Get_time_interval (Time);
   begin
      Put_Line ("Lower bound:" & TimeT'Image (IT.lower_bound));
      Put_Line ("Upper bound:" & TimeT'Image (IT.upper_bound));
   end Display;

   -------------
   -- Display --
   -------------

   procedure Display (Time : UTO.Ref);

   procedure Display (Time : UTO.Ref) is
   begin
      Put_Line ("Time:      " & TimeT'Image (Get_time (Time)));
      Put_Line ("Inaccuracy:" & InaccuracyT'Image (Get_inaccuracy (Time)));
      Put_Line ("Tdf:       " & TdfT'Image (Get_tdf (Time)));
   end Display;

begin
   New_Test ("CORBA COS Time");

   CORBA.ORB.Initialize ("ORB");

   PolyORB.CORBA_P.Server_Tools.Initiate_Server (True);

   declare
      Obj : constant CORBA.Impl.Object_Ptr
        := new CosTime.TimeService.Impl.Object;

   begin
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Ref);
   end;

   UTO1 := universal_time (Ref);
   Display (UTO1);
   Output ("Fetch UTO", True);

   Put_Line ("Waiting for 3 seconds");
   delay 3.0;

   UTO2 := universal_time (Ref);
   Display (UTO2);
   Output ("Fetch UTO", True);

   Put_Line ("Interval is");

   TIO1 := TIO.Convert_Forward.To_Ref (time_to_interval (UTO1, UTO2));
   Display (TIO1);

   End_Report;
end Test_Time;
