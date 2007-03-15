------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . S E T U P . O A . S I M P L E _ O A            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Obj_Adapters.Simple;
with PolyORB.Utils.Strings;

package body PolyORB.Setup.OA.Simple_OA is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      Obj_Adapter : PolyORB.Obj_Adapters.Obj_Adapter_Access;

   begin
      --  Create SOA object adapter

      Obj_Adapter := new PolyORB.Obj_Adapters.Simple.Simple_Obj_Adapter;
      PolyORB.Obj_Adapters.Create (Obj_Adapter);

      --  Link object adapter with ORB.

      PolyORB.ORB.Set_Object_Adapter (The_ORB, Obj_Adapter);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"simple_oa",
       Conflicts => Empty,
       Depends   => +"orb",
       Provides  => +"object_adapter",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Setup.OA.Simple_OA;
