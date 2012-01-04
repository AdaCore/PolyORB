------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  RTCOSSCHEDULING.CLIENTSCHEDULER.IMPL                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Log;
with PolyORB.Parameters.File;

with CORBA.ORB;

with RTCORBA.Current.Helper;
with RTCosScheduling.Helper;

package body RTCosScheduling.ClientScheduler.Impl is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("rtcosscheduling.clientscheduler.impl");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   -----------------------------
   -- Load_Configuration_File --
   -----------------------------

   procedure Load_Configuration_File (Conf_File_Name : String) is
   begin
      PolyORB.Parameters.File.Load_Configuration_File (Conf_File_Name);
   end Load_Configuration_File;

   -----------------------
   -- Schedule_Activity --
   -----------------------

   procedure Schedule_Activity
     (Self          : access Object;
      Activity_Name : CORBA.String)
   is
      pragma Unreferenced (Self);

      Priority : Integer;

      Current : constant RTCORBA.Current.Local_Ref :=
                  RTCORBA.Current.Helper.To_Local_Ref
                    (CORBA.ORB.Resolve_Initial_References
                     (CORBA.ORB.To_CORBA_String ("RTCurrent")));

   begin
      pragma Debug (C, O ("Configuring activity: "
                       & CORBA.To_String (Activity_Name)));

      Priority := PolyORB.Parameters.Get_Conf
        ("activity " & CORBA.To_String (Activity_Name),
         "priority");

      pragma Debug (C, O ("Set priority to:" & Integer'Image (Priority)));

      RTCORBA.Current.Set_The_Priority (Current, RTCORBA.Priority (Priority));

   exception
      when others =>
         --  For now, we cannot distinguish between an inconsistent
         --  value of the priority, and an unknown activity.

         RTCosScheduling.Helper.Raise_UnknownName
           (UnknownName_Members'
            (CORBA.IDL_Exception_Members with null record));
   end Schedule_Activity;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return
        CORBA.Is_Equivalent
          (Logical_Type_Id, RTCosScheduling.ClientScheduler.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

end RTCosScheduling.ClientScheduler.Impl;
