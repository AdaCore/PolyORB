------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . B U F F E R S                       --
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

--  Buffer management

--  $Id: //droopi/main/src/polyorb-buffers.ads#11 $

with System;
--  For bit-order information.

with Ada.Streams;   use Ada.Streams;

with PolyORB.Opaque; use PolyORB.Opaque;
--  General opaque data storage types.

with PolyORB.Opaque.Chunk_Pools;
--  Chunked memory storage.

with PolyORB.Sockets;
--  Low-level IO operations.

package PolyORB.Buffers is

   pragma Elaborate_Body;

   -------------------------
   -- General definitions --
   -------------------------

   type Endianness_Type is (Little_Endian, Big_Endian);

   Host_Order : constant Endianness_Type;
   --  The byte order of this host.

   type Buffer_Type is limited private;
   type Buffer_Access is access all Buffer_Type;
   --  A pointer to a dynamically allocated buffer.

   ------------------------
   -- General operations --
   ------------------------

   function Length (Buffer : access Buffer_Type) return Stream_Element_Count;
   pragma Inline (Length);
   --  Return the length of Buffer.

   procedure Set_Endianness
     (Buffer : access Buffer_Type;
      E      :        Endianness_Type);
   function Endianness (Buffer : Buffer_Type) return Endianness_Type;
   --  Return the endianness of Buffer.
   --  XXX This should be moved to CDR.

   procedure Release_Contents (Buffer : in out Buffer_Type);
   --  Signal that the current contents of a buffer will not be
   --  used anymore. The associated storage will be deallocated.

   procedure Initialize_Buffer
     (Buffer     : access Buffer_Type;
      Size       : Stream_Element_Count;
      Data       : Opaque_Pointer;
      Endianness : Endianness_Type;
      Initial_CDR_Position : Stream_Element_Offset);
   --  Sets the contents of Buffer using data
   --  passed as a pointer Data and a size Size.
   --  Buffer must be a fresh, empty buffer.
   --  The first element of Data corresponds to
   --  the indicated Initial_CDR_Position.
   --  The byte-order of the data is Endianness.
   --  The lifespan of the data designated by Data
   --  must be no less than the lifespan of the
   --  resulting buffer.

   type Reservation is private;
   function Reserve
     (Buffer : access Buffer_Type;
      Amount : Stream_Element_Count)
     return Reservation;
   --  Reserve Amount contiguous bytes in Buffer at the current
   --  position, to be filled later through a call to Copy_Data.
   --  The position of the reservation is the current position
   --  in Buffer before the call. The length of the reservation
   --  is Amount.

   procedure Copy_Data
     (From : in Buffer_Type;
      Into : Reservation);
   --  Fill reservation Into using the data from From.
   --  The position and length of From and Into must match.

   function Copy
     (Buffer : access Buffer_Type)
     return Buffer_Access;
   --  Make a copy of Buffer. The copy's data is allocated
   --  only from its internal storage pool. There is no
   --  constraint on the lifespan of the resulting buffer.
   --  It is the caller's responsibility to call Release
   --  on the returned Buffer_Access to free the associated
   --  resources. The initial and current CDR positions of the
   --  new buffers are set to the initial CDR position of the
   --  source.

   procedure Release
     (A_Buffer : in out Buffer_Access);
   --  Release the contents of A_Buffer and the associated control
   --  structures when they won't be used anymore.
   --  On return, A_Buffer is set to null.

   function To_Stream_Element_Array
     (Buffer   : access Buffer_Type)
     return Stream_Element_Array;
   --  Dump the contents of Buffer into a Stream_Element_Array.
   --  Using this function is dangerous because it may overflow
   --  the stack with the contents of a big buffer. It is therefore
   --  DEPRECATED. Use the following instead.

   function To_Stream_Element_Array
     (Buffer   : access Buffer_Type)
     return Opaque.Zone_Access;
   --  Dump the contents of Buffer into a Stream_Element_Array,
   --  and return a pointer to it. The caller must take care of
   --  deallocating the pointer after use.

   ------------------------------
   -- The CDR view of a buffer --
   ------------------------------

   --  A buffer has a current position index called the current
   --  CDR position. Marshalling data into the buffer and
   --  unmarshalling data from the buffer first advances the
   --  current buffer position according to the alignment
   --  constraints for the data type, then further advance it
   --  by the size of the data effectively marshalled or
   --  unmarshalled.

   procedure Set_Initial_Position
     (Buffer   : access Buffer_Type;
      Position : Stream_Element_Offset);
   --  Sets the initial and current CDR positions
   --  of Buffer to Position. No data must have
   --  been inserted into Buffer yet.

   procedure Pad_Align
     (Buffer    : access Buffer_Type;
      Alignment : Alignment_Type);
   --  Aligns Buffer on specified Alignment before inserting
   --  aligned data. A padding chunk is inserted into Buffer
   --  if necessary.

   procedure Align_Position
     (Buffer    : access Buffer_Type;
      Alignment : Alignment_Type);
   --  Aligns Buffer on specified Alignment before retrieving
   --  aligned data.

   --  After execution of either of the two above operations,
   --  the current CDR position of Buffer is advanced to a
   --  multiple of Alignment.

   --  Inserting data into a buffer

   procedure Insert_Raw_Data
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Data      : Opaque_Pointer);
   --  Inserts data into Buffer by reference at the current
   --  CDR position. This procedure is used to implement
   --  marshalling by reference.

   procedure Allocate_And_Insert_Cooked_Data
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Data      : out Opaque_Pointer);
   --  Allocates Size bytes within Buffer's memory
   --  pool, and inserts this chunk of memory into
   --  Buffer at the current CDR position.
   --  A pointer to the allocated space is returned,
   --  so the caller can copy data into it.
   --  This procedure is used to implement marshalling
   --  by copy. The current position is not changed.

   procedure Unuse_Allocation
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count);
   --  Cancel the allocation of Size bytes at the end
   --  of this Buffer's memory pool. Size must be no greater
   --  than the size of the last chunk inserted, which must
   --  have been allocated using Allocate_And_Insert_Cooked_Data.
   --  XXX Check that this last restriction is enforced.

   --  Retrieving data from a buffer

   procedure Extract_Data
     (Buffer      : access Buffer_Type;
      Data        : out Opaque_Pointer;
      Size        : Stream_Element_Count;
      Use_Current : Boolean := True;
      At_Position : Stream_Element_Offset := 0);
   --  Retrieve Size elements from Buffer. If Use_Current,
   --  the extraction starts at the current position in the
   --  buffer, else it starts at At_Position.

   --  On return, Data contains an access to the retrieved
   --  Data, and if Use_Current, then the CDR current position
   --  is advanced by Size.

   function CDR_Position (Buffer : access Buffer_Type)
     return Stream_Element_Offset;
   --  Return the current CDR position of the buffer
   --  in the marshalling stream.

   function Remaining (Buffer : access Buffer_Type)
     return Stream_Element_Count;
   --  Return the number of bytes available from Buffer,
   --  from the current position to the end of data.

   procedure Rewind (Buffer : access Buffer_Type);
   --  Reset the current position in Buffer to the initial
   --  position.

   ---------------------------------------
   -- The input/output view of a buffer --
   ---------------------------------------

   procedure Send_Buffer
     (Buffer : access Buffer_Type;
      Socket : Sockets.Socket_Type);
   --  Send the contents of Buffer onto Socket.

   procedure Receive_Buffer
     (Buffer   : access Buffer_Type;
      Socket   : Sockets.Socket_Type;
      Max      : Stream_Element_Count;
      Received : out Stream_Element_Count);
   --  Received at most Max octets of data into Buffer at
   --  current position. On return, Received is set to the
   --  effective amount of data received. The current position
   --  is unchanged.

   -------------------------
   -- Utility subprograms --
   -------------------------

   procedure Show (Buffer : in Buffer_Type);
   --  Display the contents of Buffer for debugging
   --  purpose.

private

   ------------------------------------------
   -- Determination of the host byte order --
   ------------------------------------------

   use System;

   Default_Bit_Order_To_Endianness :
     constant array (Bit_Order) of Endianness_Type
     := (High_Order_First => Big_Endian,
         Low_Order_First  => Little_Endian);

   Host_Order : constant Endianness_Type :=
     Default_Bit_Order_To_Endianness (Default_Bit_Order);

   --------------
   -- A Buffer --
   --------------

   type Iovec is record
      Iov_Base : Opaque_Pointer;
      Iov_Len  : Stream_Element_Count;
   end record;
   --  This is modeled after the POSIX iovec, but is not equivalent
   --  (because we cannot depend on being able to manipulate System.Address).

   type Buffer_Chunk_Metadata is record
      --  An Iovec pool manipulates chunks of memory allocated
      --  from a Chunk_Pool. This records holds the metadata
      --  associated by the Iovec pool and the Buffer (below)
      --  with each allocated chunk.

      Last_Used : Stream_Element_Offset := 0;
      --  The index within the chunk of the last
      --  used element.
   end record;

   --  A space pre-reservation within a buffer.

   type Reservation is record
      Location     : Opaque_Pointer;
      Endianness   : Endianness_Type;
      CDR_Position : Stream_Element_Offset;
      Length       : Stream_Element_Count;
   end record;

   Null_Buffer_Chunk_Metadata : constant Buffer_Chunk_Metadata
     := (Last_Used => 0);

   package Buffer_Chunk_Pools is
      new Chunk_Pools
     (Chunk_Metadata => Buffer_Chunk_Metadata,
      Null_Metadata  => Null_Buffer_Chunk_Metadata);

   subtype Chunk_Metadata_Access is
     Buffer_Chunk_Pools.Metadata_Access;

   package Iovec_Pools is

      --  An Iovec_Pool stores a sequence of Iovecs with
      --  corresponding descriptors. An array of pre-allocated
      --  storage is used if the pool contains no more than
      --  Prealloc_Size items, else a dynamically-allocated
      --  array is used.

      Write_Error : exception;
      Read_Error  : exception;

      type Iovec_Pool_Type is private;

      procedure Grow_Shrink
        (Iovec_Pool   : access Iovec_Pool_Type;
         Size         : Stream_Element_Offset;
         Data         : out Opaque_Pointer);
      --  Augment/reduce the length of the last Iovec in
      --  Iovec_Pool by Size elements, if possible.
      --  On success, a pointer to the reserved
      --  space is returned in Data. On failure, a null
      --  pointer is returned.

      procedure Prepend_Pool
        (Prefix     : Iovec_Pool_Type;
         Iovec_Pool : in out Iovec_Pool_Type);
      --  Prepends the contents of Prefix in Iovec_Pool.
      --  Prefix is unchanged.

      procedure Append
        (Iovec_Pool : in out Iovec_Pool_Type;
         An_Iovec   : Iovec;
         A_Chunk    : Buffer_Chunk_Pools.Chunk_Access := null);
      --  Append An_Iovec at the end of Iovec_Pool.
      --  If A_Chunk is not null, then the Iovec points to
      --  data within the designated chunk, and can be
      --  extended towards the end of the chunk if necessary.

      procedure Extract_Data
        (Iovec_Pool : Iovec_Pool_Type;
         Data       : out Opaque_Pointer;
         Offset     : Stream_Element_Offset;
         Size       : Stream_Element_Count);
      --  Retrieve exactly Size octets of data from
      --  Iovec_Pool starting at Offset.
      --  The data must be stored contiguously.
      --  If there are not Size octest of data
      --  contiguously stored in Iovec_Pool at Offset,
      --  then exception Read_Error is raised.

      procedure Release
        (Iovec_Pool : in out Iovec_Pool_Type);
      --  Signals that Iovec_Pool will not be used anymore.
      --  The associated Iovec array storage is returned to
      --  the system.

      procedure Write_To_Socket
        (S          : PolyORB.Sockets.Socket_Type;
         Iovec_Pool : access Iovec_Pool_Type);
      --  Write the contents of Iovec_Pool onto S.

      ---------------------------------------
      -- Low-level interfaces to the octet --
      -- stream of an Iovec_Pool.          --
      ---------------------------------------

      procedure Dump (Iovec_Pool : Iovec_Pool_Type; Into : Opaque_Pointer);
      --  Dump the content of an Iovec_Pool into Into.

      function Dump (Iovec_Pool : Iovec_Pool_Type) return Zone_Access;
      --  Dump the contents of Iovec_Pool into an array of octets. The result
      --  must be deallocated when not used anymore.

   private

      type Iovec_Array is array (Positive range <>) of aliased Iovec;
      type Iovec_Array_Access is access all Iovec_Array;

      Prealloc_Size : constant := 16;
      --  The number of slots in the preallocated iovec array.

      type Iovec_Pool_Type is record

         Prealloc_Array : aliased Iovec_Array (1 .. Prealloc_Size);
         Dynamic_Array  : Iovec_Array_Access := null;
         --  The pre-allocated and dynamically allocated
         --  Iovec_Arrays.

         Length : Natural := Prealloc_Size;
         --  The length of the arrays currently in use.

         Last : Natural := 0;
         --  The number of the last allocated Iovec in the pool.
         --  If Last <= Prealloc_Array'Last then the pool's
         --  Iovecs are stored in Prealloc_Array, else they
         --  are stored in Dynamic_Array.

         Last_Chunk : Buffer_Chunk_Pools.Chunk_Access := null;
         --  If the last Iovec is pointing into user data,
         --  then we cannot assume that addresses beyond the
         --  end of the Iovec's buffer is valid: this
         --  Iovec cannot be grown. In this case,
         --  Last_Chunk is set to null.

         --  If the last Iovec is pointing into a memory
         --  chunk from a Buffer's chunk pool, then we can
         --  grow the Iovec if its last element is also the
         --  last allocated element of the chunk. In this
         --  second case, Last_Chunk is set to an access
         --  that designates the storage chunk.
      end record;

   end Iovec_Pools;

   type Buffer_Type is record
      Endianness : Endianness_Type := Host_Order;
      --  The byte order of the data stored in the
      --  buffer.

      CDR_Position : Stream_Element_Offset := 0;
      --  The current position within the stream for
      --  marshalling and unmarshalling.

      Initial_CDR_Position : Stream_Element_Offset := 0;
      --  The position within the stream of the first
      --  element of Buffer.

      Contents     : aliased Iovec_Pools.Iovec_Pool_Type;
      --  The marshalled data as a pool of Iovecs.

      Storage      : aliased Buffer_Chunk_Pools.Pool_Type;
      --  A set of memory chunks used to store data
      --  marshalled by copy.

      Length       : Stream_Element_Count := 0;
      --  Length of stored data.
   end record;

end PolyORB.Buffers;
