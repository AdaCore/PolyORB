------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . D N S . T R A N S P O R T _ M E C H A N I S M S   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2010, Free Software Foundation, Inc.          --
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

with Ada.Unchecked_Deallocation;

package body PolyORB.DNS.Transport_Mechanisms is
   ---------------------------------
   -- Create_Transport_Mechanisms --
   ---------------------------------
   ------------------
   -- Is_Colocated --
   ------------------

   function Is_Colocated (Left, Right : Transport_Mechanism_List)
     return Boolean
   is
      use Transport_Mechanism_Lists;
      L_Iter : Iterator := First (Right);
      R_Iter : Iterator;
   begin
      Left_Iteration :
      while not Last (L_Iter) loop

         R_Iter := First (Left);

         Right_Iteration :
         while not Last (R_Iter) loop
            if Is_Colocated
                 (Value (L_Iter).all.all, Value (R_Iter).all.all) then
               return True;
            end if;
            Next (R_Iter);
         end loop Right_Iteration;

         Next (L_Iter);
      end loop Left_Iteration;

      return False;
   end Is_Colocated;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (List : in out Transport_Mechanism_List) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Transport_Mechanism'Class, Transport_Mechanism_Access);

      Component : Transport_Mechanism_Access;

   begin
      while List
              /= Transport_Mechanism_List (Transport_Mechanism_Lists.Empty)
      loop
         Extract_First (List, Component);
         Release_Contents (Component);
         Free (Component);
      end loop;
   end Release_Contents;

end PolyORB.DNS.Transport_Mechanisms;
