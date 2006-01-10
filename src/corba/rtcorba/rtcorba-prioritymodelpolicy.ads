------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          R T C O R B A . P R I O R I T Y M O D E L P O L I C Y           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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

with CORBA.Policy;

package RTCORBA.PriorityModelPolicy is

   type Local_Ref is new CORBA.Policy.Ref with private;

   --  Implementation note: OMG Issue #5613 indicates:
   --
   --  "RTCORBA::PriorityModelPolicy cannot be created via
   --  ORB::create_policy() method because this policy has two
   --  attributes and and the Any that is passed to the
   --  ORB::create_policy() method can only hold one parameter."
   --
   --  Thus, no Any helpers are provided for this policy.

   function Get_Priority_Model
     (Self : Local_Ref)
     return RTCORBA.PriorityModel;

   function Get_Server_Priority
     (Self : Local_Ref)
     return RTCORBA.Priority;

private

   type Local_Ref is new CORBA.Policy.Ref with null record;

end RTCORBA.PriorityModelPolicy;
