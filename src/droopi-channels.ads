--  A communication channel (a transport service protocol entity).

--  $Id$

with Droopi.Buffers;
with Droopi.Protocols;
with Droopi.Sockets;

package Droopi.Channels is

   pragma Elaborate_Body;

   type Channel is tagged limited private;
   type Channel_Access is access all Channel'Class;
   --  One communication channel.

   function Create
     (Socket : Sockets.Socket_Type;
      Session : Protocols.Session_Access)
     return Channel_Access;

   procedure Destroy (C : in out Channel_Access);

   ---------------------------------------------------
   -- Channel primitives (interface to upper layer) --
   ---------------------------------------------------

   procedure Send_Data
     (C : access Channel;
      B : access Buffers.Buffer_Type);
   --  Send data onto the channel.

   procedure Expect_Data (C : access Channel; Size : Natural);
   --  Signal C that data is expected. If Size = 0,
   --  a callback to the attached session will be made as soon
   --  as data is received; if Size > 0, a callback
   --  will be made when exactly Size bytes of data have
   --  been received.

   -----------------------------------------------
   -- Callback point (interface to lower layer) --
   -----------------------------------------------

   procedure Handle_Data
     (C : access Channel;
      S : Sockets.Socket_Type);
   --  Interface to lower layer: handle a 'Data present' event.

private

   type Channel is tagged limited record
      Socket  : Droopi.Sockets.Socket_Type;
      Session : Droopi.Protocols.Session_Access;
   end record;

end Droopi.Channels;


