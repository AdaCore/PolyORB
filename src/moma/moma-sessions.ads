------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        M O M A . S E S S I O N S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  A Session defines an execution context in which Message_Producers and
--  Message_Consumers live. We use the capabilities of PolyORB's POA to
--  associate a thread to each session.

--  XXX this package requires first to complete the implementation of POA
--  policies. Its definition and the completion of its API is left 'as is'.

--  XXX Need to add functions to implement durable subscription to a topic.

with MOMA.Connections;
with MOMA.Destinations;
with MOMA.Types;

package MOMA.Sessions is

   type Session is record
      Transacted : Boolean;
      Acknowledge_Mode : MOMA.Types.Acknowledge_Type;
   end record;

   function Create_Session
     (Connection       : MOMA.Connections.Connection;
      Transacted       : Boolean;
      Acknowledge_Mode : MOMA.Types.Acknowledge_Type)
     return Session;
   --  Create a session from a Connection.

   procedure Close;

   procedure Commit;

   function Get_Transacted return Boolean;

   procedure Recover;

   procedure Rollback;

   procedure Subscribe
     (Topic : MOMA.Destinations.Destination;
      Pool  : MOMA.Destinations.Destination;
      Sub   : Boolean := True);
   --  Subscribe / Unsubscribe a Pool to a Topic.
   --  Topic's reference must be a router.
   --  Pool's reference must be a message pool.
   --  If Sub is true then it is a subscription, if false an unsubscription.

   procedure Unsubscribe
     (Topic : MOMA.Destinations.Destination;
      Pool  : MOMA.Destinations.Destination);
   --  Unsubscribe a Pool from a Topic.
   --  Provided for convenience only, as the Subscribe function may be used
   --  for the same purpose.

end MOMA.Sessions;
