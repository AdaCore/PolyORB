------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . P R O F I L E S              --
--                . F U L L _ T A S K I N G . M U T E X E S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Implementation of mutexes under the Full_Tasking profile.

with Unchecked_Deallocation;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;
with PolyORB.Log;

package body PolyORB.Tasking.Profiles.Full_Tasking.Mutexes is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.profiles.full_tasking.mutexes");

   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Initialize;

   ----------
   -- Free --
   ----------

   procedure Free is new Unchecked_Deallocation
     (Full_Tasking_Mutex_Type'Class,
      Full_Tasking_Mutex_Access);

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

   ------------
   -- Create --
   ------------

   function Create
     (MF   : access Full_Tasking_Mutex_Factory_Type;
      Name : String := "")
     return PTM.Mutex_Access is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Unreferenced (Name);
      pragma Warnings (On);
      --  XXX The use of Name is not yet implemented
      M : Full_Tasking_Mutex_Access := new Full_Tasking_Mutex_Type;
   begin
      pragma Debug (O ("Create Mutex"));
      M.The_PO := new Mutex_PO;
      return PTM.Mutex_Access (M);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (MF : in out Full_Tasking_Mutex_Factory_Type;
      M  : in out PTM.Mutex_Access) is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);
   begin
      pragma Debug (O ("Detroy Mutex"));
      Free (Full_Tasking_Mutex_Access (M));
   end Destroy;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in out Full_Tasking_Mutex_Type) is
   begin
      M.The_PO.Enter;
   end Enter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      pragma Debug (O ("Initialize package Profiles.Full_Tasking.Mutexes"));
      PTM.Register_Mutex_Factory (PTM.Mutex_Factory_Access
                                    (The_Mutex_Factory));
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in out Full_Tasking_Mutex_Type) is
   begin
      M.The_PO.Leave;
   end Leave;

   ----------------
   -- Mutex_PO --
   ----------------

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
         pragma Assert (Locked = True);
         pragma Debug (O ("Leave mutex"));
         Locked := False;
      end Leave;

   end Mutex_PO;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"tasking.profiles.full_tasking.mutexes",
       Conflicts => Empty,
       Depends => Empty,
       Provides => +"tasking.mutexes",
       Init => Initialize'Access));
end PolyORB.Tasking.Profiles.Full_Tasking.Mutexes;
