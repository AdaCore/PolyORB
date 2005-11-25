------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T R A N S P O R T . H A N D L E R S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

--  Event handlers associated with all transport access points and
--  transport endpoints.

with PolyORB.Binding_Data;
with PolyORB.Filters;
with PolyORB.ORB;

package PolyORB.Transport.Handlers is

   type Transport_Event_Handler is
     abstract new PolyORB.Asynch_Ev.AES_Event_Handler with
      record
         ORB : PolyORB.ORB.ORB_Access;
      end record;

   --------------------------------
   -- Access point event handler --
   --------------------------------

   type TAP_AES_Event_Handler is
     abstract new Transport_Event_Handler with
      record
         TAP : PolyORB.Transport.Transport_Access_Point_Access;
         --  Factory of Transport_Endpoint components.

         Filter_Factory_Chain : Filters.Factories_Access;
         --  Factory of Filter (protocol stack) components.

         Profile_Factory : Binding_Data.Profile_Factory_Access;
         --  Factory of profiles capable of associating the
         --  address of TAP and the specification of the
         --  protocol implemented by Filter_Factory_Chain
         --  with an object id.
      end record;

   ----------------------------
   -- Endpoint event handler --
   ----------------------------

   type TE_AES_Event_Handler is
     new Transport_Event_Handler with
      record
         TE : PolyORB.Transport.Transport_Endpoint_Access;
         --  Back pointer to the corresponding endpoint.
      end record;

   procedure Handle_Event
     (H : access TE_AES_Event_Handler);

end PolyORB.Transport.Handlers;
