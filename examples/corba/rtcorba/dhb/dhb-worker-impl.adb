------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      D H B . W O R K E R . I M P L                       --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Dynamic_Priorities;
with Ada.Text_IO;

with System;

with RTCORBA.PriorityMapping;
with PolyORB.RTCORBA_P.Setup;

with Constants;
with DHB.Worker.Skel;
pragma Elaborate (DHB.Worker.Skel);
pragma Warnings (Off, DHB.Worker.Skel);

with Whetstone;

package body DHB.Worker.Impl is

   ------------------
   -- Do_Some_Work --
   ------------------

   procedure Do_Some_Work
     (Self           : access Object;
      Kilo_Whetstone : in     DHB.KWIPS)
   is
      pragma Unreferenced (Self);

   begin
      Whetstone.Small_Whetstone (Positive (Kilo_Whetstone));
   end Do_Some_Work;

   -------------------------------
   -- Do_Some_Work_With_Payload --
   -------------------------------

   procedure Do_Some_Work_With_Payload
     (Self           : access Object;
      Kilo_Whetstone : in     DHB.KWIPS;
      Payload        : in     DHB.Worker.U_sequence)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Payload);
      use type CORBA.Short;
   begin
      if Kilo_Whetstone > 0 then
         Whetstone.Small_Whetstone (Positive (Kilo_Whetstone));
      end if;
   end Do_Some_Work_With_Payload;

   ---------------
   -- Get_KWIPS --
   ---------------

   function Get_KWIPS (Self : access Object) return DHB.KWIPS is
      pragma Unreferenced (Self);

   begin
      return KWIPS (Whetstone.Compute_KWIPS);
   end Get_KWIPS;

   ----------
   -- Ping --
   ----------

   procedure Ping (Self : access Object; Data : in CORBA.Unsigned_Long) is
      pragma Unreferenced (Self);
      pragma Unreferenced (Data);

   begin
      null;
   end Ping;

   ----------------
   -- Round_Trip --
   ----------------

   function Round_Trip
     (Self : access Object;
      Data : in     CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      pragma Unreferenced (Self);

   begin
      return Data;
   end Round_Trip;

   -----------------------------
   -- Round_Trip_With_Payload --
   -----------------------------

   function Round_Trip_With_Payload
     (Self : access Object;
      Data : in     DHB.Worker.U_sequence)
     return DHB.Worker.U_sequence
   is
      pragma Unreferenced (Self);

   begin
      return Data;
   end Round_Trip_With_Payload;

   ----------------------
   -- Running_Priority --
   ----------------------

   function Running_Priority (Self : access Object) return RTCORBA.Priority is
      pragma Unreferenced (Self);

      use Ada.Dynamic_Priorities;
      use Ada.Text_IO;

      Ada_Priority : constant System.Any_Priority := Get_Priority;

      CORBA_Priority : RTCORBA.Priority;
      Ok : Boolean;

   begin
      RTCORBA.PriorityMapping.To_CORBA
        (PolyORB.RTCORBA_P.Setup.Get_Priority_Mapping.all,
         RTCORBA.NativePriority (Ada_Priority),
         CORBA_Priority,
         Ok);

      if not Ok then
         raise Program_Error;
      end if;

      if Constants.Verbose then
         Put_Line ("In Worker servant, running thread at "
                   & "Ada native priority"
                   & System.Any_Priority'Image (Ada_Priority)
                   & ", CORBA priority (approximation)"
                   & RTCORBA.Priority'Image (CORBA_Priority));
      end if;

      return CORBA_Priority;
   end Running_Priority;

end DHB.Worker.Impl;
