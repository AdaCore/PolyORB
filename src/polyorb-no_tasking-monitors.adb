------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B - N O _ T A S K I N G - M O N I T O R S           --
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

--  Implementation of monitors under the No_Tasking profile.

--  $Id$

with Unchecked_Deallocation;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.No_Tasking.Monitors is

   procedure Free is
      new Unchecked_Deallocation
     (No_Tasking_Monitor_Type'Class, No_Tasking_Monitor_Access);

   ------------
   -- Create --
   ------------

   function Create
     (MF   : access No_Tasking_Monitor_Factory_Type;
      Name : String := "")
     return PTM.Monitor_Access is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Unreferenced (Name);
      pragma Warnings (On);
   begin
      return new No_Tasking_Monitor_Type;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (MF : in out No_Tasking_Monitor_Factory_Type;
      M  : in out PTM.Monitor_Access) is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);
   begin
      Free (No_Tasking_Monitor_Access (M));
   end Destroy;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in out No_Tasking_Monitor_Type) is
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
      PTM.Register_Monitor_Factory (PTM.Monitor_Factory_Access
                                    (The_Monitor_Factory));
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in out No_Tasking_Monitor_Type) is
      pragma Warnings (Off);
      pragma Unreferenced (M);
      pragma Warnings (On);
   begin
      null;
   end Leave;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (M : in out No_Tasking_Monitor_Type) is
      pragma Warnings (Off);
      pragma Unreferenced (M);
      pragma Warnings (On);
   begin
      null;
   end Signal;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (M : in out No_Tasking_Monitor_Type;
      C : access PTM.Condition_Type'Class) is
      pragma Warnings (Off);
      pragma Unreferenced (M);
      pragma Warnings (On);
      B : Boolean;
   begin
      PTM.Evaluate (C.all, B);
      if not B then
         raise Program_Error;
      end if;
   end Wait;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"no_tasking_profile-monitors",
       Conflicts => Empty,
       Depends => Empty,
       Provides => +"tasking.monitors",
       Init => Initialize'Access));
end PolyORB.No_Tasking.Monitors;
