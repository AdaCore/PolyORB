------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--                            T E S T _ T I M E                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;                use Ada.Text_IO;
with Broca.Server_Tools;
with CORBA.Object;
with CosTime;                    use CosTime;
with CosTime.TimeService;        use CosTime.TimeService;
with CosTime.TimeService.Helper;
with CosTime.TimeService.Impl;
with CosTime.TIO;                use CosTime.TIO;
with CosTime.UTO;                use CosTime.UTO;
with PortableServer;
with TimeBase;                   use TimeBase;

package body Test_Time is

   procedure Display (Time : in TIO.Ref);
   procedure Display (Time : in UTO.Ref);

   type TimeService_Ptr is access CosTime.TimeService.Impl.Object;

   Ref : CosTime.TimeService.Ref;

   UTO1, UTO2 : UTO.Ref;
   TIO1       : TIO.Ref;

   -------------
   -- Display --
   -------------

   procedure Display (Time : in TIO.Ref) is
      IT : constant IntervalT := get_time_interval (Time);
   begin
      Put_Line ("Lower bound:" & TimeT'Image (IT.lower_bound));
      Put_Line ("Upper bound:" & TimeT'Image (IT.upper_bound));
   end Display;

   -------------
   -- Display --
   -------------

   procedure Display (Time : in UTO.Ref) is
   begin
      Put_Line ("Time:      " & TimeT'Image (get_time (Time)));
      Put_Line ("Inaccuracy:" & InaccuracyT'Image (get_inaccuracy (Time)));
      Put_Line ("Tdf:       " & TdfT'Image (get_tdf (Time)));
   end Display;

begin
   Broca.Server_Tools.Initiate_Server;
   Broca.Server_Tools.Initiate_Servant
     (PortableServer.Servant
      (Timeservice_Ptr'(new CosTime.TimeService.Impl.Object)),
      Ref);
   UTO1 := Universal_Time (Ref);
   Display (UTO1);
   Put_Line ("Waiting for 3 seconds");
   delay 3.0;
   UTO2 := Universal_Time (Ref);
   Display (UTO2);
   Put_Line ("Interval is");
   TIO1 := TIO.Convert_Forward.To_Ref (time_to_interval (UTO1, UTO2));
   Display (TIO1);
end Test_Time;
