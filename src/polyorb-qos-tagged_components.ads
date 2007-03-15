------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . Q O S . T A G G E D _ C O M P O N E N T S         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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

with PolyORB.Representations.CDR.Common;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.QoS.Tagged_Components is

   subtype Component_Id is PolyORB.Types.Unsigned_Long;

   type Encapsulation_Access is
     access all PolyORB.Representations.CDR.Common.Encapsulation;

   type GIOP_Tagged_Component is record
      Tag  : Component_Id;
      Data : Encapsulation_Access;
   end record;

   package GIOP_Tagged_Component_Lists is
     new Utils.Chained_Lists (GIOP_Tagged_Component);

   type QoS_GIOP_Tagged_Components_Parameter is
     new QoS_Parameter (GIOP_Tagged_Components) with
   record
      Components : GIOP_Tagged_Component_Lists.List;
   end record;

   type QoS_GIOP_Tagged_Components_Parameter_Access is
     access all QoS_GIOP_Tagged_Components_Parameter'Class;

   procedure Release_Contents
     (QoS : access QoS_GIOP_Tagged_Components_Parameter);

end PolyORB.QoS.Tagged_Components;
