------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . P R O T O C O L S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Support for object method invocation protocols.

with Ada.Streams;

with PolyORB.Any.NVList;
with PolyORB.Binding_Data;
with PolyORB.Binding_Objects;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.Filters;
with PolyORB.Requests;
with PolyORB.Annotations;

package PolyORB.Protocols is

   --  A protocol is a factory of sessions. Each session corresponds
   --  to a connection to a remote protocol entity.

   --  XXX This package needs some comments and in particular the
   --  callback and the demux stuff.

   use PolyORB.Filters;
   use PolyORB.Requests;

   type Protocol is abstract new Filters.Factory with private;
   type Protocol_Access is access all Protocol'Class;

   type Session is abstract new Filters.Filter with private;
   type Session_Access is access all Session'Class;

   procedure Create
     (Proto   : access Protocol;
      Session :    out Filter_Access)
      is abstract;
   --  Create a Session for protocol Proto using filter Lower.

   --------------------------------------------------
   -- Primitives needed with some tasking policies --
   --------------------------------------------------

   procedure Set_Task_Info
     (S : Session_Access;
      N :    PolyORB.Annotations.Notepad_Access);
   --  Set the notes associated with session

   function Get_Task_Info
     (S : Session_Access)
      return PolyORB.Annotations.Notepad_Access;
   --  Return the notes  associated with session.

   -----------------------------------------------------
   -- Protocol primitives (interface to upper layers) --
   -----------------------------------------------------

   procedure Invoke_Request
     (S : access Session;
      R :        Requests.Request_Access;
      P : access Binding_Data.Profile_Type'Class)
      is abstract;
   --  Send a method invocation message for request R on session S.
   --  P designates the profile of the target object reference that
   --  was bound to establish session S.

   procedure Abort_Request
     (S : access Session;
      R :        Request_Access)
      is abstract;
   --  Abort pending invocation of R.

   procedure Send_Reply
     (S : access Session;
      R :  Request_Access)
      is abstract;
   --  Send back a reply on S notifying caller of the result
   --  of executing R.

   ------------------------------------------------
   -- Callback point (interface to lower layers) --
   ------------------------------------------------

   procedure Handle_Connect_Indication
     (S : access Session)
      is abstract;
   --  A new server connection has been accepted as session S.

   procedure Handle_Connect_Confirmation
     (S : access Session)
      is abstract;
   --  A new client connection has been established as session S.

   procedure Handle_Data_Indication
     (S           : access Session;
      Data_Amount :        Ada.Streams.Stream_Element_Count)
      is abstract;
   --  Invoked when some data arrives for session S.

   procedure Handle_Disconnect
     (S : access Session; Error : Errors.Error_Container) is abstract;
   --  Invoked when the underlying connection is closed. Any pending request
   --  must be marked as completed, and the corresponding target reference
   --  must be unbound.

   procedure Handle_Unmarshall_Arguments
     (S     : access Session;
      Args  : in out Any.NVList.Ref;
      Error : in out Errors.Error_Container);
   --  Invoked when the application needs unmarshalled arguments
   --  for a request. Must be implemented by protocols that
   --  allow deferred arguments unmarshalling.

   procedure Handle_Flush (S : access Session) is abstract;
   --  Flush all pending received data in S, and make S read to
   --  receive a new incoming message.

   ---------------------
   -- Message demuxer --
   ---------------------

   function Handle_Message
     (Sess : access Session;
      S    :        Components.Message'Class)
     return Components.Message'Class;
   --  Demultiplex Messages to the above specialized operations.

private

   type Protocol is abstract new Filters.Factory with null record;

   type Session is abstract new Filters.Filter with record
      Server : Components.Component_Access;
      --  The ORB instance

      Dependent_Binding_Object : Binding_Objects.Binding_Object_Access;
      --  The enclosing binding object, if this session is on server side
      --  (used to keep the BO referenced and prevent it from being destroyed
      --  while the request is being processed).

      N      : PolyORB.Annotations.Notepad_Access := null;
   end record;

end PolyORB.Protocols;
