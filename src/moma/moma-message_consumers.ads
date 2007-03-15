------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               M O M A . M E S S A G E _ C O N S U M E R S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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

--  A Message_Consumer object is the client view of the message receiving
--  process. It is the facade to all communications carried out with
--  a message pool to receive messages; it contains the stub to access
--  Message_Consumer servants (see MOMA.Provider for more details).

--  NOTE: A MOMA client must use only this package to receive messages from a
--  message pool.

with Ada.Real_Time;

with MOMA.Destinations;
with MOMA.Messages;
with MOMA.Sessions;
with MOMA.Types;

package MOMA.Message_Consumers is

   type Message_Consumer is private;
   --  Destination   : origin of all messages received.
   --  Ref           : reference to the provider servant

   type Message_Consumer_Acc is access Message_Consumer;

   function Create_Consumer
     (Session : MOMA.Sessions.Session;
      Dest    : MOMA.Destinations.Destination)
     return Message_Consumer_Acc;
   --  Create a new message consumer.

   function Create_Consumer
     (Session          : MOMA.Sessions.Session;
      Dest             : MOMA.Destinations.Destination;
      Message_Selector : MOMA.Types.String)
     return Message_Consumer_Acc;
   --  XXX Not implemented.

   procedure Close;
   --  XXX not implemented. Rename it to Destroy ?

   function Get_Message_Selector return String;
   --  XXX not implemented.

   function Receive
     (Self : Message_Consumer;
      Priority : MOMA.Types.Priority := MOMA.Types.Invalid_Priority)
     return MOMA.Messages.Message'Class;
   --  Get next message from the pool if it is non empty; otherwise
   --  the call is blocking until a new message is received by the
   --  pool.  Use Priority as priority for the corresponding
   --  invocations, unless it is set to Invalid_Priority.
   --  XXX not all cases are tested !

   function Receive
     (Timeout : Ada.Real_Time.Time)
     return MOMA.Messages.Message;
   --  Get next message from the pool if it is non empty; otherwise will
   --  wait during Timeout until a new message arrives.
   --  XXX not implemented.

   function Receive_No_Wait return MOMA.Messages.Message;
   --  Get next message from the pool if it is non empty; exit otherwise.
   --  XXX not implemented.

   --  Accessors to Message_Consumer internal data.

   function Get_Ref
     (Self : Message_Consumer)
     return MOMA.Types.Ref;

   procedure Set_Ref
     (Self : in out Message_Consumer;
      Ref  :        MOMA.Types.Ref);

   function Get_Destination
     (Self : Message_Consumer)
     return MOMA.Destinations.Destination;

   procedure Set_Destination
     (Self : in out Message_Consumer;
      Dest :        MOMA.Destinations.Destination);

private

   type Message_Consumer is record
      Destination    : MOMA.Destinations.Destination;
      Ref            : MOMA.Types.Ref;
   end record;

   pragma Inline (Get_Ref);
   pragma Inline (Set_Ref);
   pragma Inline (Get_Destination);
   pragma Inline (Set_Destination);

end MOMA.Message_Consumers;
