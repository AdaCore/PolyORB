------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            T E S T _ T I M E                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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

   procedure Display (Time : in TIO.Ref);

   procedure Display (Time : in TIO.Ref) is
      IT : constant IntervalT := Get_time_interval (Time);
   begin
      Put_Line ("Lower bound:" & TimeT'Image (IT.lower_bound));
      Put_Line ("Upper bound:" & TimeT'Image (IT.upper_bound));
   end Display;

   -------------
   -- Display --
   -------------

   procedure Display (Time : in UTO.Ref);

   procedure Display (Time : in UTO.Ref) is
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
