------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . T R A N S P O R T . C O N N E C T E D           --
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

--  Abstract connected transport service access points and transport
--  endpoints.

with PolyORB.Transport.Handlers;

package PolyORB.Transport.Connected is

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

   overriding function Handle_Message
     (TE  : not null access Connected_Transport_Endpoint;
      Msg : Components.Message'Class) return Components.Message'Class;

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

   overriding procedure Handle_Event
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
