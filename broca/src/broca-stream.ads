with Ada.Unchecked_Deallocation;
with Broca.Buffers; use Broca.Buffers;
with Broca.Locks;
with Interfaces.C;

package Broca.Stream is
   --  A stream is a channel of communication.
   --  It has the same features as a connection.
   --  A stream is bidirectionnal: Data can be sent or received, however, there
   --  is no interractions between the two ways.

   --  When this exception is raised, it means the connection was closed, and
   --  the stream cannot be used anymore.
   Connection_Closed : exception;

   --  The base type.
   type Stream_Type is abstract tagged limited
     record
        Lock_S, Lock_R : Broca.Locks.Mutex_Type;
     end record;

   --  Send a buffer to a stream.
   --  All the buffer (from 0 to BUFFER.POS - 1) is sent.
   --  In case of failure, connection_closed is raised.
   procedure Send (Stream : access Stream_Type;
                   Buffer : in Buffer_Descriptor) is abstract;

   --  Become an exclusif owner for sending data to the stream.
   --  Can queue the task.
   procedure Lock_Send (Stream : access Stream_Type);
   procedure Unlock_Send (Stream : access Stream_Type);

   --  Receive data from a stream.
   --  Exactly stream.Pos bytes are expected.
   --  Can raise connection_closed.
   procedure Receive (Stream : access Stream_Type;
                      Buffer : in out Buffer_Descriptor) is abstract;

   --  Become an exclusif owner for receiving data from the stream.
   --  Can queue the task.
   procedure Lock_Receive (Stream : access Stream_Type);
   procedure Unlock_Receive (Stream : access Stream_Type);

   type Stream_Acc is access all Stream_Type'Class;
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Object => Stream_Type'Class, Name => Stream_Acc);

   type Stream_Acc_Array is array (Natural range <>) of Stream_Acc;

   --  A stream for a socket.
   type Fd_Stream_Type is new Stream_Type with
     record
        Fd : Interfaces.C.int;
     end record;
   procedure Send (Stream : access Fd_Stream_Type;
                   Buffer : in Buffer_Descriptor);
   procedure Receive (Stream : access Fd_Stream_Type;
                      Buffer : in out Buffer_Descriptor);
   function Create_Fd_Stream (Fd : Interfaces.C.int) return Stream_Acc;

end Broca.Stream;
