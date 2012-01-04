------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      D H B . W O R K E R . I M P L                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with CORBA;
with PortableServer;
with RTCORBA;

package DHB.Worker.Impl is

   type Object is new PortableServer.Servant_Base with private;
   type Object_Ptr is access all Object'Class;

   procedure Do_Some_Work
     (Self           : access Object;
      Kilo_Whetstone : DHB.KWIPS);

   procedure Do_Some_Work_With_Payload
     (Self           : access Object;
      Kilo_Whetstone : DHB.KWIPS;
      Payload        : DHB.Worker.U_sequence);

   function Get_KWIPS (Self : access Object) return DHB.KWIPS;

   procedure Ping (Self : access Object; Data : CORBA.Unsigned_Long);

   function Round_Trip
     (Self : access Object;
      Data : CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   function Round_Trip_With_Payload
     (Self : access Object;
      Data : DHB.Worker.U_sequence)
     return DHB.Worker.U_sequence;

   function Running_Priority (Self : access Object) return RTCORBA.Priority;

private

   type Object is new PortableServer.Servant_Base with null record;

end DHB.Worker.Impl;
