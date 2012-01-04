------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . S E T U P . O A . S I M P L E _ O A            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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
