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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Support for object method invocation protocols.

--  $Id$

with Ada.Streams;

with PolyORB.Any.NVList;
with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Filters; use PolyORB.Filters;
with PolyORB.Requests; use PolyORB.Requests;
with PolyORB.Soft_Links; use PolyORB.Soft_Links;
with PolyORB.Jobs;
with PolyORB.Utils.Chained_Lists;

package PolyORB.Protocols is

   --  A protocol is a factory of sessions. Each session corresponds
   --  to a connection to a remote protocol entity.

   --  This package needs some comments and in particular the callback
   --  and the demux stuff.

   type Protocol is abstract new Filters.Factory with private;
   type Protocol_Access is access all Protocol'Class;

   type Session is abstract new Filters.Filter with private;
   type Session_Access is access all Session'Class;

   type Request_Info is record
      Job : Jobs.Job_Access;
   end record;

   package Request_Queue is new PolyORB.Utils.Chained_Lists (Request_Info);

   procedure Create
     (Proto   : access Protocol;
      Session : out Filter_Access)
      is abstract;
   --  Create a Session for protocol Proto using filter Lower.
   --  Request_Watcher should not be created here, it will
   --  be positioned by the thread policy, if necessary.

   procedure Finalize (S : in out Session);

   --------------------------------------------------
   -- Primitives needed with some tasking policies --
   --------------------------------------------------

   procedure Set_Request_Watcher
     (S : in Session_Access;
      W : PolyORB.Soft_Links.Watcher_Access);
   --  Set the request watcher associated with session

   function Get_Request_Watcher
     (S : in Session_Access)
     return PolyORB.Soft_Links.Watcher_Access;
   --  Return the request watcher associated with session.

   procedure Get_First_Request
     (S      : in out Session_Access;
      Result : out Request_Info);
   --  Return in Result the first request in the list associated with session.

   procedure Add_Request
     (S : in out Session_Access;
      RI : Request_Info);
   --  Add a request RI at the end of the list associated with the session.

   -----------------------------------------------------
   -- Protocol primitives (interface to upper layers) --
   -----------------------------------------------------

   procedure Invoke_Request
     (S : access Session;
      R : Requests.Request_Access;
      P : access Binding_Data.Profile_Type'Class)
      is abstract;
   --  Send a method invocation message for request R on session S.

   procedure Abort_Request
     (S : access Session;
      R :  Request_Access)
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

   procedure Handle_Data_Indication
     (S : access Session;
      Data_Amount : Ada.Streams.Stream_Element_Count)
      is abstract;
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

   Protocol_Error : exception;

private

   type Protocol is abstract new Filters.Factory with null record;

   type Session is abstract new Filters.Filter with record
      Server          : Components.Component_Access;
      Request_Watcher : PolyORB.Soft_Links.Watcher_Access := null;
      Request_List    : Request_Queue.List;
   end record;

end PolyORB.Protocols;
