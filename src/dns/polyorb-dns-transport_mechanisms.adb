------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . D N S . T R A N S P O R T _ M E C H A N I S M S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010-2011, Free Software Foundation, Inc.          --
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

package body PolyORB.DNS.Transport_Mechanisms is

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of
     (M : Transport_Mechanism) return Utils.Sockets.Socket_Name
   is
   begin
      return M.Address.all;
   end Address_Of;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate
     (TMA : Transport_Mechanism) return Transport_Mechanism'Class
   is
   begin
      return Res : Transport_Mechanism'Class := TMA do
         Res.Address := new Utils.Sockets.Socket_Name'(Res.Address.all);
      end return;
   end Duplicate;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (M : access Transport_Mechanism) is
   begin
      Utils.Sockets.Free (M.Address);
   end Release_Contents;

end PolyORB.DNS.Transport_Mechanisms;
