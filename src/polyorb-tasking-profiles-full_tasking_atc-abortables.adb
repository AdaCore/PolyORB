------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          POLYORB.TASKING.PROFILES.FULL_TASKING_ATC.ABORTABLES            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2010, Free Software Foundation, Inc.          --
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

   overriding procedure Run (AR : access ATC_Abortable);
   overriding procedure Abort_Run (AR : access ATC_Abortable);
   overriding function Create
                (R : not null access PTT.Runnable'Class) return ATC_Abortable;

   ---------
   -- Run --
   ---------

   procedure Run (AR : access ATC_Abortable) is
   begin
      select
         AR.P.Wait;
      then abort
         AR.R.Run;
      end select;
   end Run;

   ---------------
   -- Abort_Run --
   ---------------

   procedure Abort_Run (AR : access ATC_Abortable) is
   begin
      AR.P.Signal;
   end Abort_Run;

   ------------
   -- Create --
   ------------

   function Create
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
