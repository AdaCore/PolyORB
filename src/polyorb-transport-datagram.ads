------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T R A N S P O R T . D A T A G R A M            --
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

--  Abstract datagram transport service access points and transport endpoints.

--  $Id$

with PolyORB.Binding_Data;

package PolyORB.Transport.Datagram is

   use PolyORB.Asynch_Ev;
   use PolyORB.Binding_Data;

   Read_Error : exception;

   ------------------
   -- Access Point --
   ------------------

   type Datagram_Transport_Access_Point
      is abstract new Transport_Access_Point with private;
   type Datagram_Transport_Access_Point_Access is
     access all Datagram_Transport_Access_Point'Class;
   --  Datagram Access point

   type Datagram_TAP_AES_Event_Handler
   is new TAP_AES_Event_Handler with private;
   --  Datagram Access Point Event Handler

   procedure Handle_Event
     (H : access Datagram_TAP_AES_Event_Handler);

   ---------------
   -- End Point --
   ---------------

   type Datagram_Transport_Endpoint
      is abstract new Transport_Endpoint with private;
   type Datagram_Transport_Endpoint_Access
   is access all Datagram_Transport_Endpoint'Class;
   --  Datagram End point

   function Handle_Message
     (TE  : access Datagram_Transport_Endpoint;
      Msg : Components.Message'Class)
     return Components.Message'Class;

   type Datagram_TE_AES_Event_Handler
   is new TE_AES_Event_Handler with private;
   --  Datagram End Point Event Handler

   procedure Handle_Event
     (H : access Datagram_TE_AES_Event_Handler);

   function Create_Endpoint
     (TAP : access Datagram_Transport_Access_Point)
     return Datagram_Transport_Endpoint_Access;
   --  This function create an Endpoint on the same socket
   --  This allow to receive data to datagram socket

private

   type Datagram_TAP_AES_Event_Handler
   is new TAP_AES_Event_Handler with null record;

   type Datagram_TE_AES_Event_Handler_Access
   is access all Datagram_TE_AES_Event_Handler'Class;

   type Datagram_Transport_Access_Point
      is abstract new Transport_Access_Point with null record;

   type Datagram_Transport_Endpoint
      is abstract new Transport_Endpoint with null record;

   type Datagram_TE_AES_Event_Handler
   is new TE_AES_Event_Handler with null record;

end PolyORB.Transport.Datagram;
