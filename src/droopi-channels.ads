--  A communication channel (a transport service protocol entity).

--  $Id$

with Ada.Streams; use Ada.Streams;

with Droopi.Buffers;
with Droopi.Protocols;
with Droopi.Servers;
with Droopi.Sockets;

package Droopi.Channels is

   pragma Elaborate_Body;

   type Channel is tagged limited private;
   type Channel_Access is access all Channel'Class;
   --  One communication channel.

   function Create
     (Socket  : Sockets.Socket_Type;
      Session : Protocols.Session_Access;
      Server  : Servers.Server_Access)
     return Channel_Access;
   --  Create a channel connected downwards to Socket,
   --  and upwards to Session.
   --  Suspend is executed when Data must be waited for.

   procedure Destroy (C : in out Channel_Access);

   ---------------------------------------------------
   -- Channel primitives (interface to upper layer) --
   ---------------------------------------------------

   procedure Send_Data
     (C : access Channel;
      B : access Buffers.Buffer_Type);
   --  Send data onto the channel.

   procedure Receive_Data
     (C     : access Channel;
      B     : access Buffers.Buffer_Type;
      Size  : Stream_Element_Count;
      Exact : Boolean := True);
   --  Signal C that data is to be received into B.
   --  If Exact is False, will return as soon as data is
   --  received; if Exact is True, will return when exactly
   --  Size bytes of data have been received.

   -----------------------------------------------
   -- Callback point (interface to lower layer) --
   -----------------------------------------------

   procedure Handle_Data
     (C : access Channel;
      S : Sockets.Socket_Type);
   --  Interface to lower layer: handle a 'Data present' event.

private

   type Channel is tagged limited record
      Server  : Servers.Server_Access;
      Socket  : Sockets.Socket_Type;
      Session : Protocols.Session_Access;

      Data_Expected : Stream_Element_Count;
      Data_Exact    : Boolean;
      Data_Buffer   : Buffers.Buffer_Access;
      Data_Arrived  : aliased Boolean := False;
      --  XXX Document!
   end record;

end Droopi.Channels;


