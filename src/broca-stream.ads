------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         B R O C A . S T R E A M                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Broca.Opaque; use Broca.Opaque;
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
   procedure Send
     (Stream : access Stream_Type;
      Buffer : access Buffer_Type) is abstract;

   --  Become an exclusive owner for sending data to the stream.
   --  Can queue the task.
   procedure Lock_Send (Stream : access Stream_Type);
   procedure Unlock_Send (Stream : access Stream_Type);

   --  Receive data from a stream. Fill exactly buffer.
   --  Can raise Connection_Closed.
   --  procedure Receive
   --    (Stream : access Stream_Type;
   --     Buffer : access Buffer_Type) is abstract;

   --  Receive data from a stream into an octet array.
   function Receive
     (Stream : access Stream_Type;
      Length : Index_Type)
     return Octet_Array is abstract;

   --  Become an exclusive owner for receiving data from the
   --  stream. Can queue the task.
   procedure Lock_Receive (Stream : access Stream_Type);
   procedure Unlock_Receive (Stream : access Stream_Type);

   --  A stream for a socket.
   type Fd_Stream_Type is new Stream_Type with private;

   procedure Send
     (Stream : access Fd_Stream_Type;
      Buffer : access Buffer_Type);

   --  procedure Receive
   --    (Stream : access Fd_Stream_Type;
   --     Buffer : access Buffer_Type);

   function Receive
     (Stream : access Fd_Stream_Type;
      Length : Index_Type)
     return Octet_Array;

   type Stream_Ptr is access all Stream_Type'Class;
   type Stream_Ptr_Array is array (Natural range <>) of Stream_Ptr;

   function Create_Fd_Stream (Fd : Interfaces.C.int) return Stream_Ptr;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Stream_Type'Class, Name => Stream_Ptr);

private

   type Fd_Stream_Type is new Stream_Type with
     record
        Fd : Interfaces.C.int;
     end record;

end Broca.Stream;
