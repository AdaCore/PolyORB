------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . B I N D I N G _ O B J E C T _ Q O S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Objects;
with PolyORB.QoS;

package PolyORB.Binding_Object_QoS is

   procedure Set_Binding_Object_QoS
     (BO  : access PolyORB.Binding_Objects.Binding_Object'Class;
      QoS :        PolyORB.QoS.QoS_Parameters);

   function Get_Binding_Object_QoS
     (BO  : access PolyORB.Binding_Objects.Binding_Object'Class)
      return PolyORB.QoS.QoS_Parameters;

   procedure Set_Binding_Object_QoS
     (BO   : access PolyORB.Binding_Objects.Binding_Object'Class;
      Kind :        PolyORB.QoS.QoS_Kind;
      QoS  :        PolyORB.QoS.QoS_Parameter_Access);

   function Is_Compatible
     (BO  : access PolyORB.Binding_Objects.Binding_Object'Class;
      QoS : PolyORB.QoS.QoS_Parameters) return Boolean;

   type QoS_Compatibility_Check_Proc is
     access function
     (BO_QoS : PolyORB.QoS.QoS_Parameter_Access;
      QoS    : PolyORB.QoS.QoS_Parameter_Access) return Boolean;

   procedure Register
     (Kind : PolyORB.QoS.QoS_Kind;
      Proc : QoS_Compatibility_Check_Proc);

end PolyORB.Binding_Object_QoS;
