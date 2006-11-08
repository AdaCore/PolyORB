------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--             S Y S T E M . G A R L I C . P R I O R I T I E S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

package body System.Garlic.Priorities is

   -----------------------------
   -- Set_RPC_Handler_Prioity --
   -----------------------------

   procedure Set_RPC_Handler_Priority
     (A_Priority : Global_Priority)
   is
   begin
      RPC_Handler_Priority := A_Priority;
   end Set_RPC_Handler_Priority;

   -------------------------------------
   -- Set_RPC_Handler_Priority_Policy --
   -------------------------------------

   procedure Set_RPC_Handler_Priority_Policy
     (A_Policy : Priority_Policy)
   is
   begin
      RPC_Handler_Priority_Policy := A_Policy;
   end Set_RPC_Handler_Priority_Policy;

end System.Garlic.Priorities;
