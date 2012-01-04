------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      D H B . W O R K E R . I M P L                       --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Dynamic_Priorities;
with Ada.Text_IO;

with System;

with RTCORBA.PriorityMapping;
with PolyORB.RTCORBA_P.Setup;

with Constants;
with DHB.Worker.Skel;
pragma Warnings (Off);
--  Compiler wants Elaborate_All, but that causes cycles
pragma Elaborate (DHB.Worker.Skel);
pragma Warnings (On);
pragma Warnings (Off, DHB.Worker.Skel);

with Whetstone;

package body DHB.Worker.Impl is

   ------------------
   -- Do_Some_Work --
   ------------------

   procedure Do_Some_Work
     (Self           : access Object;
      Kilo_Whetstone : DHB.KWIPS)
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
      Kilo_Whetstone : DHB.KWIPS;
      Payload        : DHB.Worker.U_sequence)
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

   procedure Ping (Self : access Object; Data : CORBA.Unsigned_Long) is
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
      Data : CORBA.Unsigned_Long)
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
      Data : DHB.Worker.U_sequence)
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
