------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . Q O S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

--  This package defines the Quality of Service (QoS) parameters to be
--  associated with Requets, Object Adapters and Profiles

package PolyORB.QoS is

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
      Transport_Security);

   --  Definition of QoS parameters

   type QoS_Parameter (Kind : QoS_Kind) is abstract tagged null record;

   type QoS_Parameter_Access is access all QoS_Parameter'Class;

   procedure Release_Contents (QoS : access QoS_Parameter);

   procedure Release (QoS : in out QoS_Parameter_Access);

   type QoS_Parameters is array (QoS_Kind) of QoS_Parameter_Access;

   function Image (QoS : QoS_Parameters) return String;
   --  For debugging purposes. Return an image of QoS

end PolyORB.QoS;
