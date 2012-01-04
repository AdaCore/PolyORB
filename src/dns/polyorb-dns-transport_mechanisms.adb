------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . D N S . T R A N S P O R T _ M E C H A N I S M S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010-2012, Free Software Foundation, Inc.          --
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
