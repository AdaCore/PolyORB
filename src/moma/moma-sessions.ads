------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        M O M A . S E S S I O N S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
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

--  A Session defines an execution context in which Message_Producers and
--  Message_Consumers live. We use the capabilities of PolyORB's POA to
--  associate a thread to each session.

--  XXX this package requires first to complete the implementation of POA
--  policies. Its definition and the completion of its API is left 'as is'.

--  XXX Need to add functions to implement durable subscription to a topic.

--  $Id$

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
