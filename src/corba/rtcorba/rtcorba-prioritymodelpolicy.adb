------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          R T C O R B A . P R I O R I T Y M O D E L P O L I C Y           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.RTCORBA_P.PriorityModelPolicy;

package body RTCORBA.PriorityModelPolicy is

   use PolyORB.RTCORBA_P.PriorityModelPolicy;

   ------------------------
   -- Get_Priority_Model --
   ------------------------

   function Get_Priority_Model
     (Self : in Ref)
     return RTCORBA.PriorityModel is
   begin
      return Get_Priority_Model (PriorityModelPolicy_Type
                                 (Entity_Of (Self).all));
   end Get_Priority_Model;

   -------------------------
   -- Get_Server_Priority --
   -------------------------

   function Get_Server_Priority
     (Self : in Ref)
     return RTCORBA.Priority is
   begin
      return Get_Server_Priority (PriorityModelPolicy_Type
                                  (Entity_Of (Self).all));
   end Get_Server_Priority;

end RTCORBA.PriorityModelPolicy;
