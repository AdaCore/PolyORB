------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.TASKING.PROFILES.FULL_TASKING.PORTABLE_MUTEXES           --
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

--  Implementation of mutexes under the Full_Tasking profile.
--  This is a variant that uses only standard Ada constructs. It is not
--  used by default.

with Ada.Unchecked_Deallocation;

with PolyORB.Initialization;

with PolyORB.Log;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.Full_Tasking.Portable_Mutexes is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.profiles.full_tasking.mutexes");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   -------------------------------------------------------------
   -- Underlying protected object for Full_Tasking_Mutex_Type --
   -------------------------------------------------------------

   protected type Mutex_PO is
      --  Protected object which is the real implementation of
      --  Mutex_Type

      entry Enter;
      --  Real implementation of Enter (Mutex_Type).

      procedure Leave;
      --  Real implementation of Leave (Mutex_Type).

   private
      Locked   : Boolean := False;
      --  False when the lock is free; else True;

   end Mutex_PO;

   ----------
   -- Free --
   ----------

   procedure Free is new Ada.Unchecked_Deallocation
     (PTM.Mutex_Type'Class, PTM.Mutex_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Mutex_PO, Mutex_PO_Access);

   ------------
   -- Create --
   ------------

   function Create
     (MF   : access Full_Tasking_Mutex_Factory_Type;
      Name : String := "")
      return PTM.Mutex_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (MF);

      pragma Unreferenced (Name);
      --  XXX The use of Name is not yet implemented

      pragma Warnings (On);

      M : constant Full_Tasking_Mutex_Access := new Full_Tasking_Mutex_Type;
   begin
      pragma Debug (O ("Create Mutex"));
      M.The_PO := new Mutex_PO;
      return PTM.Mutex_Access (M);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (MF : access Full_Tasking_Mutex_Factory_Type;
      M  : in out PTM.Mutex_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);

   begin
      pragma Debug (O ("Destroy mutex"));
      Free (Full_Tasking_Mutex_Access (M).The_PO);
      Free (M);
   end Destroy;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : access Full_Tasking_Mutex_Type) is
   begin
      M.The_PO.Enter;
   end Enter;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : access Full_Tasking_Mutex_Type) is
   begin
      M.The_PO.Leave;
   end Leave;

   ---------------
   -- Mutex_PO --
   ---------------

   protected body Mutex_PO is

      --------------------
      -- Mutex_PO.Enter --
      --------------------

      entry Enter when not Locked is
      begin
         pragma Debug (O ("Enter mutex"));

         Locked := True;
      end Enter;

      --------------------
      -- Mutex_PO.Leave --
      --------------------

      procedure Leave is
      begin
         pragma Assert (Locked);
         pragma Debug (O ("Leave mutex"));
         Locked := False;
      end Leave;

   end Mutex_PO;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      pragma Debug (O ("Initialize package Profiles.Full_Tasking.Mutexes"));
      PTM.Register_Mutex_Factory (PTM.Mutex_Factory_Access
                                    (The_Mutex_Factory));
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.full_tasking.mutexes",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"tasking.mutexes",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Tasking.Profiles.Full_Tasking.Portable_Mutexes;
