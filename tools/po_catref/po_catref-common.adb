------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O _ C A T R E F . C O M M O N                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2013, Free Software Foundation, Inc.          --
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

with PO_Catref.Output;

package body PO_Catref.Common is

   use PO_Catref.Output;

   --------------------------------
   -- Output_Address_Information --
   --------------------------------

   procedure Output_Address_Information
     (Addr : PolyORB.Utils.Sockets.Socket_Name)
   is
   begin
      Put_Line ("Address", PolyORB.Utils.Sockets.Image (Addr));
   end Output_Address_Information;

   -------------------------------
   -- Output_Object_Information --
   -------------------------------

   procedure Output_Object_Information (Obj : PolyORB.Objects.Object_Id) is
   begin
      Put_Line ("Object_Id", PolyORB.Objects.Image (Obj));
   end Output_Object_Information;

end PO_Catref.Common;
