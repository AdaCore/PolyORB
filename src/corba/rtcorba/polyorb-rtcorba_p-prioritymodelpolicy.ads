------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.RTCORBA_P.PRIORITYMODELPOLICY                   --
--                                                                          --
--                                 S p e c                                  --
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

--  $Id$

with PolyORB.Smart_Pointers;

with RTCORBA;

package PolyORB.RTCORBA_P.PriorityModelPolicy is

   type PriorityModelPolicy_Type is new PolyORB.Smart_Pointers.Entity
     with private;

   function Create
     (Priority_Model  : in RTCORBA.PriorityModel;
      Server_Priority : in RTCORBA.Priority)
     return PolyORB.Smart_Pointers.Entity_Ptr;

   function Get_Priority_Model
     (Self : in PriorityModelPolicy_Type)
     return RTCORBA.PriorityModel;

   function Get_Server_Priority
     (Self : in PriorityModelPolicy_Type)
     return RTCORBA.Priority;

private

   type PriorityModelPolicy_Type is new PolyORB.Smart_Pointers.Entity with
      record
         Priority_Model  : RTCORBA.PriorityModel;
         Server_Priority : RTCORBA.Priority;
      end record;

end PolyORB.RTCORBA_P.PriorityModelPolicy;
