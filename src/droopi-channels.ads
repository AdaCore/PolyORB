--  A communication channel (a transport service protocol entity).

--  $Id$

with Ada.Streams; use Ada.Streams;

with Droopi.Buffers;
with Droopi.Sockets;

package Droopi.Channels is

   pragma Elaborate_Body;

   type Channel is abstract tagged limited private;
   type Channel_Access is access all Channel'Class;
   --  One communication channel.

   procedure Create
     (C      : access Channel;
      Socket : Sockets.Socket_Type);

   procedure Destroy (C : in out Channel_Access);

   ---------------------------------------------------
   -- Channel primitives (interface to upper layer) --
   ---------------------------------------------------

   procedure Send_Data
     (C : access Channel;
      B : access Buffers.Buffer_Type);
   --  Send data onto the channel.

   procedure Expect_Data
     (C     : access Channel;
      B     : Buffers.Buffer_Access;
      Size  : Stream_Element_Count;
      Exact : Boolean := True);
   --  Signal C that data is to be received into B.
   --  If Exact is False, a call back to the session associated
   --  with C will be performed  as soon as data is received;
   --  if Exact is True, the call back will occur when exactly
   --  Size bytes of data have been received.

   --  Callbacks defined by upper layer:

   procedure Signal_Data (C : access Channel) is abstract;
   --  Signal the upper layer that a 'Data present' event
   --  has occured (called by redispatching from Handle_Data).

   procedure Signal_Connection_Closed (C : access Channel) is abstract;
   --  Signal the upper layer that the underlying transport
   --  service has deteted the termination of the connection.

   -------------------------------
   -- Interface to lower layers --
   -------------------------------

   procedure Handle_Data
     (C : access Channel;
      Connection_Closed : out Boolean);
   --  Interface to lower layer: handle a 'Data present' event.

private

   type Channel is abstract tagged limited record
      Socket  : Sockets.Socket_Type;
      --  The transport service endpoint.

      Data_Expected : Ada.Streams.Stream_Element_Count;
      --  The amount of incoming data expected on this
      --  connection.

      Data_Exact    : Boolean;
      --  Is Data_Expected an exact amount? If not, signal
      --  upper layer as soon as /any/ data is received.

      Data_Buffer   : Buffers.Buffer_Access;
      --  Buffer used to store received data.

      Data_Arrived  : aliased Boolean := False;
      --  Condition variable set to False when more data
      --  is expected, and to True when it has been received.
   end record;

end Droopi.Channels;
