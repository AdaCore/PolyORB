------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . P R O T O C O L S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Support for object method invocation protocols.

--  $Id$

with PolyORB.Any.NVList;
with PolyORB.Components;
with PolyORB.Filters; use PolyORB.Filters;
with PolyORB.Requests; use PolyORB.Requests;
with PolyORB.Soft_Links; use PolyORB.Soft_Links;
with PolyORB.ORB.Interface;

package PolyORB.Protocols is

   --  A protocol is a factory of sessions. Each session corresponds
   --  to a connection to a remote protocol entity.

   --  This package needs some comments and in particular the callback
   --  and the demux stuff.

   type Protocol is abstract new Filters.Factory with private;
   type Protocol_Access is access all Protocol'Class;

   type Session is abstract new Filters.Filter with private;
   type Session_Access is access all Session'Class;

   procedure Create
     (Proto   : access Protocol;
      Session : out Filter_Access)
      is abstract;
   --  Create a Session for protocol Proto using filter Lower.

   --  XXXXX: does not create request_watcher !

   procedure Destroy_Session (S : in out Session_Access);
   --  Destroy the session associated with S, return any associated
   --  resources to the system, and assign null to S.

   procedure Set_Request_Watcher
     (S : in Session_Access;
      W : PolyORB.Soft_Links.Watcher_Access);
   --

   function Get_Request_Watcher
     (S : in Session_Access)
     return PolyORB.Soft_Links.Watcher_Access;
   --  Return the request watcher associated with session.

   function Get_Pending_Request
     (S : in Session_Access)
     return ORB.Interface.Queue_Request;
   --  Return the request watcher associated with session.
   --  XXX The comment above is wrong, please update it.
   --  XXX The use of the term 'Pending_Request' is inappropriate
   --      in this context. A pending request is a request object
   --      on the client side of a connection which has been
   --      sent to the target object and for which a response
   --      is expected. Please find another, more explicit term.

   -----------------------------------------------------
   -- Protocol primitives (interface to upper layers) --
   -----------------------------------------------------

   procedure Invoke_Request (S : access Session; R :  Request_Access)
      is abstract;
   --  Send a method invocation message for request R on session S.

   procedure Abort_Request (S : access Session; R :  Request_Access)
      is abstract;
   --  Abort pending invocation of R.

   procedure Send_Reply (S : access Session; R :  Request_Access)
      is abstract;
   --  Send back a reply on S notifying caller of the result
   --  of executing R.

   ------------------------------------------------
   -- Callback point (interface to lower layers) --
   ------------------------------------------------

   procedure Handle_Connect_Indication (S : access Session)
      is abstract;
   --  A new server connection has been accepted as session S.

   procedure Handle_Connect_Confirmation (S : access Session)
      is abstract;
   --  A new client connection has been established as session S.

   procedure Handle_Data_Indication (S : access Session) is abstract;
   --  Invoked when some data arrives for session S.

   procedure Handle_Disconnect (S : access Session) is abstract;
   --  Invoked when the underlying connection is closed.

   procedure Handle_Unmarshall_Arguments
     (S    : access Session;
      Args : in out Any.NVList.Ref);
   --  Invoked when the application needs unmarshalled arguments
   --  for a request. Must be implemented by protocols that
   --  allow deferred arguments unmarshalling.

   ---------------------
   -- Message demuxer --
   ---------------------

   function Handle_Message
     (Sess : access Session;
      S : Components.Message'Class)
     return Components.Message'Class;
   --  Demultiplex Messages to the above specialized operations.

private

   type Protocol is abstract new Filters.Factory with null record;

   type Session is abstract new Filters.Filter with record
      Server          : Components.Component_Access;
      Request_Watcher : PolyORB.Soft_Links.Watcher_Access := null;
      Pending_Request : ORB.Interface.Queue_Request;
      --  XXX Change 'Pending' to something else (see above).
      --  XXX Storage of a Message'Class is questionable (Messages
      --    are supposed to be synchronously delivered, as per the
      --    documentation in the spec of PolyORB.Components.
      --    Store-and-forward behaviour is not expected.)
   end record;

end PolyORB.Protocols;
