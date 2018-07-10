------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         T E S T _ S E R V A N T                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

package body Test_Servant is

   use PolyORB.Types;

   ---------------------
   -- Execute_Servant --
   ---------------------

   overriding function Execute_Servant
     (S   : not null access My_Servant;
      Req : PolyORB.Requests.Request_Access) return Boolean
   is
      pragma Unreferenced (S, Req);
   begin
      --  This dummy servant absorbs all requests and returns them as executed

      return True;
   end Execute_Servant;

   ----------
   -- Left --
   ----------

   function "=" (Left, Right : My_Servant)
                return Standard.Boolean is
   begin
      if Left.Nb = Right.Nb and then Left.Name = Right.Name then
         return True;
      end if;
      return False;
   end "=";

end Test_Servant;
