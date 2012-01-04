------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . Q O S . C O D E _ S E T S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.GIOP_P.Code_Sets;

package PolyORB.QoS.Code_Sets is

   pragma Elaborate_Body (Code_Sets);

   type QoS_GIOP_Code_Sets_Parameter is
     new QoS_Parameter (GIOP_Code_Sets) with
   record
      Char_Data  : PolyORB.GIOP_P.Code_Sets.Code_Set_Id;
      Wchar_Data : PolyORB.GIOP_P.Code_Sets.Code_Set_Id;
   end record;

   type QoS_GIOP_Code_Sets_Parameter_Access is
     access all QoS_GIOP_Code_Sets_Parameter;

end PolyORB.QoS.Code_Sets;
