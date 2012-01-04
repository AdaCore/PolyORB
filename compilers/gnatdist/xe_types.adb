------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             X E _ T Y P E S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

package body XE_Types is

   -----------
   -- Dummy --
   -----------

   procedure Dummy (E : Node_Id) is
   begin
      if Present (E) then
         null;
      end if;
   end Dummy;

   --------
   -- No --
   --------

   function No (E : Node_Id) return Boolean is
   begin
      return E = No_Node;
   end No;

   -------------
   -- Present --
   -------------

   function Present (E : Node_Id) return Boolean is
   begin
      return E /= No_Node;
   end Present;

end XE_Types;
