------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        POLYORB.TASKING.PROFILES.FULL_TASKING.THREADS.ANNOTATIONS         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

with Ada.Task_Attributes;
with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Initialization;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations is

   use PolyORB.Annotations;
   use PolyORB.Smart_Pointers;

   type Notepad_Entity is new Non_Controlled_Entity with record
      Notepad : PolyORB.Annotations.Notepad_Access := null;
   end record;

   overriding procedure Finalize (Object : in out Notepad_Entity);

   Nil_Ref : Ref;

   package Task_Notepad_Wrapper is new Ada.Task_Attributes (Ref, Nil_Ref);

   Current_TAF : Full_Tasking_TAF_Access;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Notepad_Entity) is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Notepad,
         Name => Notepad_Access);
   begin
      if Object.Notepad /= null then
         Destroy (Object.Notepad.all);
         Free (Object.Notepad);
      end if;
   end Finalize;

   --------------------------------
   -- Get_Current_Thread_Notepad --
   --------------------------------

   overriding function Get_Current_Thread_Notepad
     (TAF : access Full_Tasking_TAF)
     return PolyORB.Annotations.Notepad_Access
   is
      pragma Unreferenced (TAF);

      Thread_Entity : Entity_Ptr := Entity_Of (Task_Notepad_Wrapper.Value);

   begin
      if Thread_Entity = null then
         Thread_Entity := new Notepad_Entity;
         Notepad_Entity (Thread_Entity.all).Notepad := new Notepad;
         Set (Task_Notepad_Wrapper.Reference.all,
              Thread_Entity);
      end if;

      return Notepad_Entity (Thread_Entity.all).Notepad;
   end Get_Current_Thread_Notepad;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Current_TAF := new Full_Tasking_TAF;
      PolyORB.Tasking.Threads.Annotations.Register
        (PolyORB.Tasking.Threads.Annotations.TAF_Access (Current_TAF));
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.full_tasking.annotations",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"tasking.annotations",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations;
