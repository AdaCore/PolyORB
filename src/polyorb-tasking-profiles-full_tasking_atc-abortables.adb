------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          POLYORB.TASKING.PROFILES.FULL_TASKING_ATC.ABORTABLES            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Constants;
with PolyORB.Initialization;
with PolyORB.Parameters;
with PolyORB.Tasking.Abortables;
with PolyORB.Tasking.Threads;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.Full_Tasking_ATC.Abortables is

   package PTA renames PolyORB.Tasking.Abortables;
   package PTT renames PolyORB.Tasking.Threads;

   procedure Initialize;
   --  Initialize module

   --  Abortable_PO is a simple barrier used for Abortable control

   protected type Abortable_PO is
      entry Wait;
      procedure Signal;
   private
      Signalled : Boolean := False;
   end Abortable_PO;

   ------------------
   -- Abortable_PO --
   ------------------

   protected body Abortable_PO is

      ----------
      -- Wait --
      ----------

      entry Wait when Signalled is
      begin
         null;
      end Wait;

      ------------
      -- Signal --
      ------------

      procedure Signal is
      begin
         Signalled := True;
      end Signal;

   end Abortable_PO;

   type ATC_Abortable is new PTA.Abortable with record
      P : Abortable_PO;
   end record;

   overriding procedure Run (AR : not null access ATC_Abortable);
   overriding procedure Run_With_Timeout
     (AR      : not null access ATC_Abortable;
      Timeout : Duration;
      Expired : out Boolean);
   overriding procedure Abort_Run (AR : not null access ATC_Abortable);
   overriding function Create
                (R : not null access PTT.Runnable'Class) return ATC_Abortable;

   ---------------
   -- Abort_Run --
   ---------------

   overriding procedure Abort_Run (AR : not null access ATC_Abortable) is
   begin
      AR.P.Signal;
   end Abort_Run;

   ------------
   -- Create --
   ------------

   overriding function Create
     (R : not null access PTT.Runnable'Class) return ATC_Abortable
   is
   begin
      return ATC_Abortable'(R => R, others => <>);
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if Parameters.Get_Conf
           (Section => "tasking",
            Key     => "abortable_rpcs",
            Default => True)
      then
         PTA.Register_Abortable_Tag (ATC_Abortable'Tag);
      end if;
   end Initialize;

   ---------
   -- Run --
   ---------

   overriding procedure Run (AR : not null access ATC_Abortable) is
   begin
      select
         AR.P.Wait;
      then abort
         AR.R.Run;
      end select;
   end Run;

   ----------------------
   -- Run_With_Timeout --
   ----------------------

   overriding procedure Run_With_Timeout
     (AR      : not null access ATC_Abortable;
      Timeout : Duration;
      Expired : out Boolean)
   is
   begin
      Expired := False;
      if Timeout = Constants.Forever then
         AR.Run;
      else
         select
            delay Timeout;
            Expired := True;
         then abort
            AR.Run;
         end select;
      end if;
   end Run_With_Timeout;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.full_tasking_atc.abortables",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"tasking.threads",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Tasking.Profiles.Full_Tasking_ATC.Abortables;
