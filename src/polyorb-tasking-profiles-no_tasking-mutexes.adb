------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . P R O F I L E S              --
--                  . N O _ T A S K I N G . M U T E X E S                   --
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

--  Implementation of POSIX-like mutexes under the
--  No_Tasking profile.

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.No_Tasking.Mutexes is

   The_Mutex : aliased No_Tasking_Mutex_Type;

   ------------
   -- Create --
   ------------

   function Create
     (MF   : access No_Tasking_Mutex_Factory_Type;
      Name : String := "")
     return PTM.Mutex_Access is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Unreferenced (Name);
      pragma Warnings (On);
   begin
      return The_Mutex'Access;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (MF : access No_Tasking_Mutex_Factory_Type;
      M  : in out PTM.Mutex_Access) is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);
   begin
      M := null;
   end Destroy;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : access No_Tasking_Mutex_Type)
   is
      pragma Warnings (Off);
      pragma Unreferenced (M);
      pragma Warnings (On);
   begin
      null;
   end Enter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PTM.Register_Mutex_Factory (PTM.Mutex_Factory_Access
                                    (The_Mutex_Factory));
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : access No_Tasking_Mutex_Type)
   is
      pragma Warnings (Off);
      pragma Unreferenced (M);
      pragma Warnings (On);
   begin
      null;
   end Leave;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"tasking.profiles.no_tasking.mutexes",
       Conflicts => Empty,
       Depends => Empty,
       Provides => +"tasking.mutexes",
       Init => Initialize'Access));
end PolyORB.Tasking.Profiles.No_Tasking.Mutexes;
