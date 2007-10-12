------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      D H B . W O R K E R . I M P L                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

with CORBA;
with PortableServer;
with RTCORBA;

package DHB.Worker.Impl is

   type Object is new PortableServer.Servant_Base with private;
   type Object_Ptr is access all Object'Class;

   procedure Do_Some_Work
     (Self           : access Object;
      Kilo_Whetstone : in     DHB.KWIPS);

   procedure Do_Some_Work_With_Payload
     (Self           : access Object;
      Kilo_Whetstone : in     DHB.KWIPS;
      Payload        : in     DHB.Worker.U_sequence);

   function Get_KWIPS (Self : access Object) return DHB.KWIPS;

   procedure Ping (Self : access Object; Data : in CORBA.Unsigned_Long);

   function Round_Trip
     (Self : access Object;
      Data : in     CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   function Round_Trip_With_Payload
     (Self : access Object;
      Data : in     DHB.Worker.U_sequence)
     return DHB.Worker.U_sequence;

   function Running_Priority (Self : access Object) return RTCORBA.Priority;

private

   type Object is new PortableServer.Servant_Base with null record;

end DHB.Worker.Impl;
