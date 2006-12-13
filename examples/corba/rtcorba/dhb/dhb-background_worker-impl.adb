------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           D H B . B A C K G R O U N D _ W O R K E R . I M P L            --
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

with Ada.Text_IO;

with RTCORBA.PriorityMapping;
with PolyORB.RTCORBA_P.Setup;
with PolyORB.Tasking.Threads;

with Constants;

with DHB.Background_Worker.Skel;
pragma Warnings (Off);
--  Compiler wants Elaborate_All, but that causes cycles
pragma Elaborate (DHB.Background_Worker.Skel);
pragma Warnings (On);
pragma Warnings (Off, DHB.Background_Worker.Skel);

with Whetstone;

package body DHB.Background_Worker.Impl is

   use Ada.Text_IO;
   use PolyORB.Tasking.Threads;

   type Background_Work_Runnable is new PolyORB.Tasking.Threads.Runnable
     with record
        Kilo_Whetstone : DHB.KWIPS;
        BG_Worker : Object_Ptr := null;
     end record;

   ---------
   -- Run --
   ---------

   procedure Run (R : access Background_Work_Runnable);

   procedure Run (R : access Background_Work_Runnable) is
   begin
      if Constants.Verbose then
         Put_Line ("Run: enter, doing" & DHB.KWIPS'Image (R.Kilo_Whetstone));
      end if;

      R.BG_Worker.Running := CORBA.Boolean'(True);
      Whetstone.Small_Whetstone (Integer (R.Kilo_Whetstone));
      R.BG_Worker.Running := CORBA.Boolean'(False);
      R.BG_Worker := null;

      if Constants.Verbose then
         Put_Line ("Run: leave");
      end if;
   end Run;

   ---------------
   -- Get_KWIPS --
   ---------------

   function Get_KWIPS (Self : access Object) return DHB.KWIPS is
      pragma Unreferenced (Self);

   begin
      return KWIPS (Whetstone.Compute_KWIPS);
   end Get_KWIPS;

   ------------------------
   -- Do_Background_Work --
   ------------------------

   procedure Do_Background_Work
     (Self           : access Object;
      Kilo_Whetstone : in     DHB.KWIPS;
      Priority       : in     RTCORBA.Priority)
   is
      Ada_Priority : RTCORBA.NativePriority;
      Ok : Boolean;
      New_Background_Worker : constant Runnable_Access
        := new Background_Work_Runnable;

   begin
      if Self.Running then
         return;
      end if;

      RTCORBA.PriorityMapping.To_Native
        (PolyORB.RTCORBA_P.Setup.Get_Priority_Mapping.all,
         Priority,
         Ada_Priority,
         Ok);

      if not Ok then
         raise Program_Error;
      end if;

      Background_Work_Runnable (New_Background_Worker.all).Kilo_Whetstone
        := Kilo_Whetstone;
      Background_Work_Runnable (New_Background_Worker.all).BG_Worker
        := Object_Ptr (Self);

      declare
         T : constant Thread_Access :=
           Run_In_Task
           (TF               => Get_Thread_Factory,
            Name             => "",
            Default_Priority => Integer (Ada_Priority),
            Storage_Size     => 0,
            R                => New_Background_Worker,
            C                => new Runnable_Controller);
         pragma Unreferenced (T);
      begin
         null;
      end;
   end Do_Background_Work;

   ----------------
   -- Is_Working --
   ----------------

   function Is_Working (Self : access Object) return CORBA.Boolean is
   begin
      return Self.Running;
   end Is_Working;

end DHB.Background_Worker.Impl;
