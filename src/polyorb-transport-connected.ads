------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . T R A N S P O R T . C O N N E C T E D           --
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

--  Abstract connected transport service access points and transport
--  endpoints.

with PolyORB.Transport.Handlers;

package PolyORB.Transport.Connected is

   use PolyORB.Asynch_Ev;

   ------------------
   -- Access Point --
   ------------------

   type Connected_Transport_Access_Point is
      abstract new Transport_Access_Point with private;

   type Connected_Transport_Access_Point_Access is
      access all Connected_Transport_Access_Point'Class;

   procedure Accept_Connection
     (TAP :     Connected_Transport_Access_Point;
      TE  : out Transport_Endpoint_Access)
      is abstract;
   --  Accept a pending new connection on TAP and create a new associated
   --  TE. In case of error, TE is null on return.

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

   function Is_Data_Available
     (TE : Connected_Transport_Endpoint;
      N  : Natural)
     return Boolean
      is abstract;
   --  Return True iff N bytes or more are available on TE for direct read.
   --  Return False otherwise, or if TE does not support such a mechanism.

private

   -----------------------------------------------
   -- Connected transport service access points --
   -----------------------------------------------

   type Connected_TAP_AES_Event_Handler is
     new Handlers.TAP_AES_Event_Handler with null record;

   procedure Handle_Event
     (H : access Connected_TAP_AES_Event_Handler);

   type Connected_Transport_Access_Point is
     abstract new Transport_Access_Point with record
        Handler : aliased Connected_TAP_AES_Event_Handler;
     end record;

   -----------------------------------
   -- Connected transport endpoints --
   -----------------------------------

   subtype Connected_TE_AES_Event_Handler is
     Handlers.TE_AES_Event_Handler;

   type Connected_Transport_Endpoint is
     abstract new Transport_Endpoint with record
        Handler : aliased Connected_TE_AES_Event_Handler;
     end record;

end PolyORB.Transport.Connected;
