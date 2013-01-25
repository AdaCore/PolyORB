------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.SMART_POINTERS.CONTROLLED_ENTITIES                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

package body PolyORB.Smart_Pointers.Controlled_Entities is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (X : in out Entity_Controller) is
   begin
      Finalize (X.E.all);
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (X : in out Entity_Controller) is
   begin
      Initialize (X.E.all);
   end Initialize;

   -------------------
   -- Is_Controlled --
   -------------------

   overriding function Is_Controlled (X : Entity) return Boolean is
      pragma Unreferenced (X);
   begin
      return True;
   end Is_Controlled;

end PolyORB.Smart_Pointers.Controlled_Entities;
