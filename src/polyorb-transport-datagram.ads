------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T R A N S P O R T . D A T A G R A M            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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

--  Abstract datagram transport service access points and transport endpoints.

with PolyORB.Transport.Handlers;

package PolyORB.Transport.Datagram is

   use PolyORB.Asynch_Ev;

   ------------------
   -- Access Point --
   ------------------

   type Datagram_Transport_Access_Point is
     abstract new Transport_Access_Point with private;
   type Datagram_Transport_Access_Point_Access is
     access all Datagram_Transport_Access_Point'Class;

   ---------------
   -- End Point --
   ---------------

   type Datagram_Transport_Endpoint
     is abstract new Transport_Endpoint with private;

   type Datagram_Transport_Endpoint_Access is
     access all Datagram_Transport_Endpoint'Class;

   function Handle_Message
     (TE  : access Datagram_Transport_Endpoint;
      Msg : Components.Message'Class)
     return Components.Message'Class;

   function Create_Endpoint
     (TAP : access Datagram_Transport_Access_Point)
      return Datagram_Transport_Endpoint_Access;
   --  This function create an Endpoint on the same socket
   --  This allow to receive data to datagram socket

private

   ----------------------------------------------------
   -- Connectionless transport service access points --
   ----------------------------------------------------

   type Datagram_TAP_AES_Event_Handler is
     new Handlers.TAP_AES_Event_Handler with null record;

   procedure Handle_Event
     (H : access Datagram_TAP_AES_Event_Handler);

   type Datagram_Transport_Access_Point is
     abstract new Transport_Access_Point with record
        Handler : aliased Datagram_TAP_AES_Event_Handler;
     end record;

   ----------------------------------------
   -- Connectionless transport endpoints --
   ----------------------------------------

   subtype Datagram_TE_AES_Event_Handler is
     Handlers.TE_AES_Event_Handler;

   type Datagram_Transport_Endpoint is
      abstract new Transport_Endpoint with null record;
   --  Only datagram in endpoints have a Handler.

end PolyORB.Transport.Datagram;
