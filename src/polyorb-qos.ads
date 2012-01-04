------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . Q O S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

--  This package defines the Quality of Service (QoS) parameters to be
--  associated with Requets, Object Adapters and Profiles

package PolyORB.QoS is

   pragma Preelaborate;

   --  List of supported QoS policies

   type QoS_Kind is
     (Static_Priority,
      Ada_Exception_Information,
      GIOP_Code_Sets,
      GIOP_Addressing_Mode,
      GIOP_Service_Contexts,
      GIOP_Tagged_Components,
      DSA_TM_Info,
      Compound_Security,
      Transport_Security,
      GIOP_Static_Buffer);

   --  Definition of QoS parameters

   type QoS_Parameter (Kind : QoS_Kind) is abstract tagged null record;

   type QoS_Parameter_Access is access all QoS_Parameter'Class;

   procedure Release_Contents (QoS : access QoS_Parameter);

   procedure Release (QoS : in out QoS_Parameter_Access);

   type QoS_Parameters is array (QoS_Kind) of QoS_Parameter_Access;

   function Image (QoS : QoS_Parameters) return String;
   --  For debugging purposes. Return an image of QoS

end PolyORB.QoS;
