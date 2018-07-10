------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T R A N S P O R T . D A T A G R A M            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2017, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

--  Abstract datagram transport service access points and transport endpoints.

with PolyORB.Transport.Handlers;

package PolyORB.Transport.Datagram is

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

   overriding function Handle_Message
     (TE  : not null access Datagram_Transport_Endpoint;
      Msg : Components.Message'Class) return Components.Message'Class;

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

   overriding procedure Handle_Event
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
