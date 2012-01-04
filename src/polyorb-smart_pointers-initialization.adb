------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SMART_POINTERS.INITIALIZATION                   --
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

--  Initialization code for PolyORB.Smart_Pointers

with Ada.Tags;

with PolyORB.Initialization;

with PolyORB.Utils.Strings;

package body PolyORB.Smart_Pointers.Initialization is

   ------------------------------------
   -- Debugging hooks implementation --
   ------------------------------------

   function Entity_External_Tag (X : Unsafe_Entity'Class) return String;
   function Ref_External_Tag (X : Ref'Class) return String;
   --  Return the external representation of X'Tag.

   -------------------------
   -- Entity_External_Tag --
   -------------------------

   function Entity_External_Tag (X : Unsafe_Entity'Class) return String is
   begin
      return Ada.Tags.External_Tag (X'Tag);
   end Entity_External_Tag;

   ----------------------
   -- Ref_External_Tag --
   ----------------------

   function Ref_External_Tag (X : Ref'Class) return String is
   begin
      return Ada.Tags.External_Tag (X'Tag);
   end Ref_External_Tag;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;
   --  Initialize Smart_Pointers module

   procedure Initialize is
   begin
      Smart_Pointers.Initialize
        (The_Entity_External_Tag => Entity_External_Tag'Access,
         The_Ref_External_Tag    => Ref_External_Tag'Access,
         The_Default_Trace       => Get_Trace ("default"));
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"smart_pointers",
       Conflicts => Empty,
       Depends   => +"tasking.mutexes" & "parameters",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Smart_Pointers.Initialization;
