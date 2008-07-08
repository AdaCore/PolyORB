------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SMART_POINTERS.INITIALIZATION                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2007, Free Software Foundation, Inc.          --
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

--  Initialization code for PolyORB.Smart_Pointers

with Ada.Tags;

with PolyORB.Initialization;

with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.Strings;

package body PolyORB.Smart_Pointers.Initialization is

   ------------------------------------
   -- Debugging hooks implementation --
   ------------------------------------

   function Entity_External_Tag
     (X : Unsafe_Entity'Class)
      return String;
   function Ref_External_Tag
     (X : Ref'Class)
      return String;
   --  Return the external representation of X'Tag.

   function Entity_External_Tag
     (X : Unsafe_Entity'Class)
      return String is
   begin
      return Ada.Tags.External_Tag (X'Tag);
   end Entity_External_Tag;

   function Ref_External_Tag
     (X : Ref'Class)
      return String is
   begin
      return Ada.Tags.External_Tag (X'Tag);
   end Ref_External_Tag;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Tasking.Mutexes.Create (Counter_Lock);
      Smart_Pointers.Entity_External_Tag := Entity_External_Tag'Access;
      Smart_Pointers.Ref_External_Tag    := Ref_External_Tag'Access;
      Smart_Pointers.Default_Trace       := Get_Trace ("default");
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
