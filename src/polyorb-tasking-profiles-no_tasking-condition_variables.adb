------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.TASKING.PROFILES.NO_TASKING.CONDITION_VARIABLES          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

--  Implementation of POSIX-like synchronisation objects under the
--  No_Tasking profile.

with PolyORB.Initialization;

with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.No_Tasking.Condition_Variables is

   The_Condition : aliased No_Tasking_Condition_Type;

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast
     (C : access No_Tasking_Condition_Type) is
      pragma Warnings (Off);
      pragma Unreferenced (C);
      pragma Warnings (On);
   begin
      null;
   end Broadcast;

   ------------
   -- Create --
   ------------

   function Create
     (MF   : access No_Tasking_Condition_Factory_Type;
      Name : String := "")
     return PTCV.Condition_Access is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Unreferenced (Name);
      pragma Warnings (On);
   begin
      return The_Condition'Access;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (MF : access No_Tasking_Condition_Factory_Type;
      C  : in out PTCV.Condition_Access)
  is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);
   begin
      C := null;
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PTCV.Register_Condition_Factory (PTCV.Condition_Factory_Access
                                    (The_Condition_Factory));
   end Initialize;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (C : access No_Tasking_Condition_Type) is
      pragma Warnings (Off);
      pragma Unreferenced (C);
      pragma Warnings (On);
   begin
      null;
   end Signal;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (C : access No_Tasking_Condition_Type;
      M : access PTM.Mutex_Type'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (M);
      pragma Unreferenced (C);
      pragma Warnings (On);
   begin
      raise Tasking_Error;
   end Wait;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.no_tasking.condition_variables",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"tasking.condition_variables",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Tasking.Profiles.No_Tasking.Condition_Variables;
