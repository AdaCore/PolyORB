------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . T R A N S P O R T . C O N N E C T E D           --
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

--  Abstract connected transport service access points and transport
--  endpoints.

--  $Id$

with PolyORB.Binding_Data;

package PolyORB.Transport.Connected is

   use PolyORB.Asynch_Ev;
   use PolyORB.Binding_Data;

   Connection_Closed : exception;

   ------------------
   -- Access Point --
   ------------------

   type Connected_Transport_Access_Point
      is abstract new Transport_Access_Point with private;
   type Connected_Transport_Access_Point_Access
   is access all Connected_Transport_Access_Point'Class;
   --  Conected Access point

   procedure Accept_Connection
     (TAP :     Connected_Transport_Access_Point;
      TE  : out Transport_Endpoint_Access)
      is abstract;
   --  Accept a pending new connection on TAP and create
   --  a new associated TE.

   type Connected_TAP_AES_Event_Handler
   is new TAP_AES_Event_Handler with private;
   --  Connected Access Point Event Handler

   procedure Handle_Event
     (H : access Connected_TAP_AES_Event_Handler);

   ---------------
   -- End Point --
   ---------------

   type Connected_Transport_Endpoint
      is abstract new Transport_Endpoint with private;
   type Connected_Transport_Endpoint_Access
   is access all Connected_Transport_Endpoint'Class;
   --  Connected End point

   function Handle_Message
     (TE  : access Connected_Transport_Endpoint;
      Msg : Components.Message'Class)
     return Components.Message'Class;

   type Connected_TE_AES_Event_Handler
   is new TE_AES_Event_Handler with private;
   --  Connected End Point Event Handler

   procedure Handle_Event
     (H : access Connected_TE_AES_Event_Handler);

   function Is_Data_Available
     (TE : Connected_Transport_Endpoint;
      N  : Natural)
     return Boolean
      is abstract;
   --  Return True iff N bytes or more are available on TE for direct read.
   --  Return False otherwise, or if TE does not support such a mechanism.

private

   type Connected_Transport_Access_Point
      is abstract new Transport_Access_Point with null record;

   type Connected_TAP_AES_Event_Handler
   is new TAP_AES_Event_Handler with null record;

   type Connected_Transport_Endpoint
      is abstract new Transport_Endpoint with null record;

   type Connected_TE_AES_Event_Handler
   is new TE_AES_Event_Handler with null record;

end PolyORB.Transport.Connected;
