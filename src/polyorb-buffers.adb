------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . B U F F E R S                       --
--                                                                          --
--                                 B o d y                                  --
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

--  $Id: //droopi/main/src/polyorb-buffers.adb#7 $

with Ada.Unchecked_Deallocation;
--  For Iovec_Pools.Free.

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

package body PolyORB.Buffers is

   use PolyORB.Log;
   use Buffer_Chunk_Pools;
   use Iovec_Pools;

   package L is new PolyORB.Log.Facility_Log ("polyorb.buffers");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   subtype Output_Line is String (1 .. 48);

   Hex : constant String      := "0123456789ABCDEF";
   Nil : constant Output_Line := (others => ' ');

   ------------------------
   -- General operations --
   ------------------------

   function Length (Buffer : access Buffer_Type) return Stream_Element_Count is
   begin
      return Buffer.Length;
   end Length;

   procedure Set_Endianness
     (Buffer : access Buffer_Type; E : Endianness_Type)
   is
   begin
      pragma Assert
        (Buffer.CDR_Position = Buffer.Initial_CDR_Position);
      Buffer.Endianness := E;
   end Set_Endianness;

   function Endianness
     (Buffer : Buffer_Type)
     return Endianness_Type is
   begin
      return Buffer.Endianness;
   end Endianness;

   procedure Release_Contents
     (Buffer : in out Buffer_Type) is
   begin
      Release (Buffer.Contents);
      Release (Buffer.Storage'Access);
      Buffer.CDR_Position := 0;
      Buffer.Initial_CDR_Position := 0;
      Buffer.Endianness := Host_Order;
      Buffer.Length := 0;
   end Release_Contents;

   procedure Initialize_Buffer
     (Buffer     : access Buffer_Type;
      Size       : Stream_Element_Count;
      Data       : Opaque_Pointer;
      Endianness : Endianness_Type;
      Initial_CDR_Position : Stream_Element_Offset)
   is
      Data_Iovec : constant Iovec
        := (Iov_Base => Data,
            Iov_Len  => Data.Zone'Length - (Data.Offset - Data.Zone'First));

   begin
      pragma Assert (True
        and then Buffer.CDR_Position = 0
        and then Buffer.Initial_CDR_Position = 0);

      Buffer.Endianness := Endianness;
      Buffer.CDR_Position := Initial_CDR_Position;
      Buffer.Initial_CDR_Position := Initial_CDR_Position;

      Append
        (Iovec_Pool => Buffer.Contents,
         An_Iovec   => Data_Iovec);

      Buffer.Length := Size;
   end Initialize_Buffer;

   function Reserve
     (Buffer : access Buffer_Type;
      Amount : Stream_Element_Count)
     return Reservation
   is
      Copy_Address : Opaque_Pointer;
      Initial_Position : constant Stream_Element_Offset
        := Buffer.CDR_Position;
   begin
      Allocate_And_Insert_Cooked_Data
        (Buffer, Amount, Copy_Address);

      return Reservation'
        (Location     => Copy_Address,
         Endianness   => Buffer.Endianness,
         CDR_Position => Initial_Position,
         Length       => Amount);
   end Reserve;

   procedure Copy_Data
     (From : in Buffer_Type;
      Into : Reservation) is
   begin
      pragma Assert (True
        and then From.Endianness   = Into.Endianness
        and then From.Initial_CDR_Position = Into.CDR_Position
        and then From.Length       = Into.Length);

      Iovec_Pools.Dump (From.Contents, Into.Location);
   end Copy_Data;

   function Copy
     (Buffer : access Buffer_Type)
     return Buffer_Access
   is
      Into         : constant Buffer_Access := new Buffer_Type;
      Copy_Address : Opaque_Pointer;

   begin
      Into.Endianness := Buffer.Endianness;
      Set_Initial_Position (Into, Buffer.Initial_CDR_Position);

      Allocate_And_Insert_Cooked_Data
        (Into,
         Buffer.Length,
         Copy_Address);

      Iovec_Pools.Dump (Buffer.Contents, Copy_Address);

      Into.CDR_Position := Buffer.Initial_CDR_Position;
      return Into;
   end Copy;

   procedure Release
     (A_Buffer : in out Buffer_Access)
   is
      procedure Free is
         new Ada.Unchecked_Deallocation
        (Buffer_Type, Buffer_Access);

   begin
      if A_Buffer /= null then
         Release_Contents (A_Buffer.all);
         Free (A_Buffer);
      end if;
   end Release;

   function To_Stream_Element_Array
     (Buffer   : access Buffer_Type)
     return Opaque.Zone_Access
   is
   begin
      pragma Assert (Buffer.Initial_CDR_Position = 0);
      return Iovec_Pools.Dump (Buffer.Contents);
   end To_Stream_Element_Array;

   function To_Stream_Element_Array
     (Buffer   : access Buffer_Type)
     return Stream_Element_Array
   is
      Contents : Zone_Access := To_Stream_Element_Array (Buffer);
      Result   : constant Stream_Element_Array  := Contents.all;
   begin
      Free (Contents);
      return Result;
   end To_Stream_Element_Array;

   ------------------------------
   -- The CDR view of a buffer --
   ------------------------------

   procedure Set_Initial_Position
     (Buffer   : access Buffer_Type;
      Position : Stream_Element_Offset) is
   begin
      pragma Assert
        (Buffer.Initial_CDR_Position = Buffer.CDR_Position);

      Buffer.Initial_CDR_Position := Position;
      Buffer.CDR_Position := Position;
   end Set_Initial_Position;

   Null_Data : constant Zone_Access
     := new Stream_Element_Array'
     (1 .. Alignment_Type'Last - 1 => 0);
   --  Null data used for padding.

   Null_Data_Address : constant Opaque_Pointer
     := (Zone   => Null_Data,
         Offset => Null_Data'First);

   procedure Align
     (Buffer    : access Buffer_Type;
      Alignment : Alignment_Type)
   is
      Padding : constant Stream_Element_Count
         := (Alignment - Buffer.CDR_Position) mod Alignment;
      Padding_Space : Opaque_Pointer;
   begin

      if Padding = 0 then
         --  Buffer is already aligned.
         return;
      end if;

      Grow_Shrink (Buffer.Contents'Access, Padding, Padding_Space);
      --  Try to extend Buffer.Content's last Iovec
      --  to provide proper alignment.

      if Padding_Space.Zone = null then
         --  Grow was unable to extend the last Iovec:
         --  insert a non-growable iovec corresponding
         --  to static null data.

         declare
            Padding_Iovec : constant Iovec
              := (Iov_Base => Null_Data_Address,
                  Iov_Len  => Padding);
         begin
            Append
              (Iovec_Pool => Buffer.Contents,
               An_Iovec   => Padding_Iovec);
         end;
      end if;

      Buffer.CDR_Position := Buffer.CDR_Position + Padding;
      --  Advance the CDR position to the new alignment.

      Buffer.Length := Buffer.Length + Padding;

      pragma Assert (Buffer.CDR_Position mod Alignment = 0);
      --  Post-condition: the buffer is aligned as requested.
   end Align;

   --  Inserting data into a buffer

   procedure Insert_Raw_Data
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Data      : Opaque_Pointer)
   is
      Data_Iovec : constant Iovec
        := (Iov_Base => Data, Iov_Len  => Size);
   begin
      pragma Assert (Buffer.Endianness = Host_Order);

      Append
        (Iovec_Pool => Buffer.Contents,
         An_Iovec   => Data_Iovec);
      Buffer.CDR_Position := Buffer.CDR_Position + Size;
      Buffer.Length := Buffer.Length + Size;
   end Insert_Raw_Data;

   procedure Allocate_And_Insert_Cooked_Data
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Data      : out Opaque_Pointer)
   is
      A_Data : Opaque_Pointer;
   begin
      Grow_Shrink (Buffer.Contents'Access, Size, A_Data);
      --  First try to grow an existing Iovec.

      if A_Data.Zone = null then
         declare
            A_Chunk : Chunk_Access;
            Data_Iovec : Iovec;
         begin
            Allocate (Buffer.Storage'Access, A_Chunk);
            Data_Iovec := (Iov_Base => Chunk_Storage (A_Chunk),
                           Iov_Len  => Size);

            A_Data := Chunk_Storage (A_Chunk);
            Metadata (A_Chunk).all :=
              (Last_Used             => Size);
            Append
              (Iovec_Pool => Buffer.Contents,
               An_Iovec   => Data_Iovec,
               A_Chunk    => A_Chunk);
            pragma Assert (A_Data.Zone /= null);
         end;
      end if;

      Data := A_Data;
      Buffer.CDR_Position := Buffer.CDR_Position + Size;
      Buffer.Length := Buffer.Length + Size;
   end Allocate_And_Insert_Cooked_Data;

   procedure Unuse_Allocation
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count)
   is
      Data : Opaque_Pointer;
   begin
      if Size /= 0 then
         Grow_Shrink (Buffer.Contents'Access, -Size, Data);
         Buffer.CDR_Position := Buffer.CDR_Position - Size;
         Buffer.Length := Buffer.Length - Size;
      end if;
   end Unuse_Allocation;

   procedure Extract_Data
     (Buffer : access Buffer_Type;
      Data   : out Opaque_Pointer;
      Size   : Stream_Element_Count;
      Use_Current : Boolean := True;
      At_Position : Stream_Element_Offset := 0)
   is
      Start_Position : Stream_Element_Offset;
   begin
      if Use_Current then
         Start_Position := Buffer.CDR_Position;
      else
         Start_Position := At_Position;
      end if;
      Extract_Data
        (Buffer.Contents, Data,
         Start_Position - Buffer.Initial_CDR_Position, Size);
      if Use_Current then
         Buffer.CDR_Position := Buffer.CDR_Position + Size;
      end if;
   end Extract_Data;

   function CDR_Position
     (Buffer : access Buffer_Type)
     return Stream_Element_Offset
   is
   begin
      return Buffer.CDR_Position;
   end CDR_Position;

   ---------------------------------------
   -- The input/output view of a buffer --
   ---------------------------------------

   procedure Send_Buffer
     (Buffer : access Buffer_Type;
      Socket : Sockets.Socket_Type)
   is
   begin
      Iovec_Pools.Write_To_Socket (Socket, Buffer.Contents'Access);
   end Send_Buffer;

   procedure Receive_Buffer
     (Buffer   : access Buffer_Type;
      Socket   : Sockets.Socket_Type;
      Max      : Stream_Element_Count;
      Received : out Stream_Element_Count)
   is
      Data : Opaque_Pointer;
      Last : Stream_Element_Offset;
      Addr : PolyORB.Sockets.Sock_Addr_Type;
      Saved_CDR_Position : constant Stream_Element_Offset
        := Buffer.CDR_Position;
   begin
      Allocate_And_Insert_Cooked_Data (Buffer, Max, Data);
      PolyORB.Sockets.Receive_Socket
        (Socket => Socket,
         Item   => Data.Zone (Data.Offset .. Data.Offset + Max - 1),
         Last   => Last,
         From   => Addr);
      Received := Last - Data.Offset + 1;
      Unuse_Allocation (Buffer, Max - Received);
      Buffer.CDR_Position := Saved_CDR_Position;
   end Receive_Buffer;

   -------------------------
   -- Utility subprograms --
   -------------------------

   procedure Show (Octets : Zone_Access);
   --  Display the contents of Octets for
   --  debugging purposes.

   procedure Show (Octets : Zone_Access)
   is
      Output : Output_Line;
      Index  : Natural := 1;

   begin
      for J in Octets'Range loop
         Output (Index)     := ' ';
         Output (Index + 1) := Hex
           (Natural (Octets (J) / 16) + 1);
         Output (Index + 2) := Hex
           (Natural (Octets (J) mod 16) + 1);
         Index := Index + 3;

         if Index > Output'Length then
            pragma Debug (O (Output));
            Index := 1;
            Output := Nil;
         end if;
      end loop;

      if Index /= 1 then
         pragma Debug (O (Output (1 .. Index - 1)));
         null;
      end if;
   end Show;

   procedure Show
     (Buffer : in Buffer_Type)
   is
   begin
      pragma Debug (O ("Dumping "
                       & Endianness_Type'Image (Buffer.Endianness)
                       & " buffer, CDR position is "
                       & Stream_Element_Offset'Image
                       (Buffer.CDR_Position) & " (length is" &
                       Buffer.Length'Img & ")"));

      declare
         Dumped : Zone_Access := Iovec_Pools.Dump (Buffer.Contents);
      begin
         Show (Dumped);
         Free (Dumped);
      end;
   end Show;

   package body Iovec_Pools is

      procedure Free is new Ada.Unchecked_Deallocation
        (Iovec_Array, Iovec_Array_Access);

      procedure Grow_Shrink
        (Iovec_Pool   : access Iovec_Pool_Type;
         Size         : Stream_Element_Offset;
         Data         : out Opaque_Pointer)
      is

         function First_Address_After (An_Iovec : Iovec)
           return Opaque_Pointer;
         --  Return the address of the storage
         --  element immediately following the
         --  last element of An_Iovec.

         function First_Address_After (An_Iovec : Iovec)
           return Opaque_Pointer is
         begin
            return An_Iovec.Iov_Base + An_Iovec.Iov_Len;
         end First_Address_After;

         procedure Do_Grow
           (Last_Iovec : in out Iovec;
            Last_Chunk : Chunk_Access);

         procedure Do_Grow
           (Last_Iovec : in out Iovec;
            Last_Chunk : Chunk_Access)
         is

         begin
            if Last_Chunk /= null then
               declare
                  Chunk_Metadata : constant Chunk_Metadata_Access
                    := Metadata (Last_Chunk);
               begin
                  if False
                    or else (Size > 0
                             and then Chunk_Metadata.Last_Used + Size
                               <= Last_Chunk.Size)
                    or else (Size < 0
                             and then Chunk_Metadata.Last_Used + Size
                               >= 0
                             and then Last_Iovec.Iov_Len + Size >= 0)
                  then
                     Chunk_Metadata.Last_Used
                       := Chunk_Metadata.Last_Used + Size;
                     Data := First_Address_After (Last_Iovec);
                     Last_Iovec.Iov_Len := Last_Iovec.Iov_Len + Size;
                  else
                     pragma Assert (False);
                     null;
                  end if;
               end;
            end if;
         end Do_Grow;

      begin
         Data.Zone := null;
         if Iovec_Pool.Last = 0 then
            --  Empty Iovec pool.
            return;
         end if;

         if Iovec_Pool.Last <= Iovec_Pool.Prealloc_Array'Last then
            Do_Grow (Iovec_Pool.Prealloc_Array (Iovec_Pool.Last),
                     Iovec_Pool.Last_Chunk);
         else
            Do_Grow (Iovec_Pool.Dynamic_Array (Iovec_Pool.Last),
                     Iovec_Pool.Last_Chunk);
         end if;
      end Grow_Shrink;

      ----------------------------------------
      -- Utility Subprograms (declarations) --
      ----------------------------------------

      function Is_Dynamic
        (Iovec_Pool : Iovec_Pool_Type)
        return Boolean;
      --  True iff Iovec pool uses dynamically allocated
      --  storage for the Iovecs and descriptors.

      function Iovecs
        (Iovec_Pool : Iovec_Pool_Type)
        return Iovec_Array;
      --  Returns the Iovec_Array of Iovec_Pool.

      procedure Extend
        (Iovec_Pool : in out Iovec_Pool_Type;
         Require    : Natural;
         Allocate   : Natural);
      --  Check the number of available Iovecs in
      --  Iovec_Pool and possibly extend it.
      --  If Iovec_Pool's length is at least
      --  Require, then does nothing, else
      --  make it Allocate Iovecs long.

      procedure Dump (Iovecs : Iovec_Array; Into : Opaque_Pointer);
      --  Dump the content of Iovecs into Into.

      function Dump (Iovecs : Iovec_Array) return Zone_Access;
      --  Dump the data designated by an Iovec_Array
      --  into an array of octets.

      ----------------------------------
      -- Utility Subprograms (bodies) --
      ----------------------------------

      function Is_Dynamic
        (Iovec_Pool : Iovec_Pool_Type)
        return Boolean is
      begin
         return Iovec_Pool.Dynamic_Array /= null;
      end Is_Dynamic;

      function Iovecs
        (Iovec_Pool : Iovec_Pool_Type)
        return Iovec_Array is
      begin
         if Is_Dynamic (Iovec_Pool) then
            return Iovec_Pool.Dynamic_Array
              (1 .. Iovec_Pool.Last);
         else
            return Iovec_Pool.Prealloc_Array
              (1 .. Iovec_Pool.Last);
         end if;
      end Iovecs;

      procedure Extend
        (Iovec_Pool : in out Iovec_Pool_Type;
         Require    : Natural;
         Allocate   : Natural) is
      begin
         pragma Assert (Allocate >= Require);

         if Require > Iovec_Pool.Length then
            declare
               New_Array : constant Iovec_Array_Access
                 := new Iovec_Array
                 (1 .. Allocate);

            begin
               New_Array (1 .. Iovec_Pool.Last)
                 := Iovecs (Iovec_Pool);

               if Is_Dynamic (Iovec_Pool) then
                  Free (Iovec_Pool.Dynamic_Array);
               end if;

               Iovec_Pool.Dynamic_Array := New_Array;
               Iovec_Pool.Length := New_Array'Length;
            end;
         end if;
      end Extend;

      procedure Dump (Iovecs : Iovec_Array; Into : Opaque_Pointer)
      is
         Offset : Stream_Element_Offset := Into.Offset;
      begin
         for I in Iovecs'Range loop
            declare
               L : constant Stream_Element_Count := Iovecs (I).Iov_Len;
               S : constant Opaque_Pointer := Iovecs (I).Iov_Base;
            begin
               Into.Zone (Offset .. Offset + L - 1)
                 := S.Zone (S.Offset .. S.Offset + L - 1);
               Offset := Offset + L;
            end;
         end loop;
      end Dump;

      function Dump (Iovecs : Iovec_Array) return Zone_Access
      is
         Result : Zone_Access;
         Length : Stream_Element_Count := 0;
      begin
         for I in Iovecs'Range loop
            Length := Length + Iovecs (I).Iov_Len;
         end loop;
         Result := new Stream_Element_Array (1 .. Length);
         Dump (Iovecs, Opaque_Pointer'
               (Zone => Result,
                Offset => Result'First));
         return Result;
      end Dump;

      -------------------------------------------
      -- Visible subprograms (implementations) --
      -------------------------------------------

      procedure Prepend_Pool
        (Prefix     : Iovec_Pool_Type;
         Iovec_Pool : in out Iovec_Pool_Type) is

         New_Last : constant Natural := Iovec_Pool.Last + Prefix.Last;
      begin
         Extend (Iovec_Pool, New_Last, New_Last + 1);
         --  An Iovec pool that has been prefixed
         --  will likely not be appended to anymore.

         --  Append new Iovec.

         if Is_Dynamic (Iovec_Pool) then
            Iovec_Pool.Dynamic_Array (1 .. New_Last)
              := Iovecs (Prefix) & Iovecs (Iovec_Pool);
         else
            Iovec_Pool.Prealloc_Array (1 .. New_Last)
              := Iovecs (Prefix) & Iovecs (Iovec_Pool);
         end if;

         Iovec_Pool.Last := New_Last;
      end Prepend_Pool;

      procedure Append
        (Iovec_Pool : in out Iovec_Pool_Type;
         An_Iovec   : Iovec;
         A_Chunk    : Buffer_Chunk_Pools.Chunk_Access := null)
      is
         New_Last : constant Natural := Iovec_Pool.Last + 1;
      begin
         Extend (Iovec_Pool, New_Last, 2 * Iovec_Pool.Length);

         --  Append new Iovec.

         Iovec_Pool.Last := New_Last;
         Iovec_Pool.Last_Chunk := A_Chunk;

         if Is_Dynamic (Iovec_Pool) then
            Iovec_Pool.Dynamic_Array (Iovec_Pool.Last)
              := An_Iovec;
         else
            Iovec_Pool.Prealloc_Array (Iovec_Pool.Last)
              := An_Iovec;
         end if;
      end Append;

      procedure Extract_Data
        (Iovec_Pool : Iovec_Pool_Type;
         Data       : out Opaque_Pointer;
         Offset     : Stream_Element_Offset;
         Size       : Stream_Element_Count)
      is
         Vecs             : constant Iovec_Array  := Iovecs (Iovec_Pool);
         Offset_Remainder : Stream_Element_Offset := Offset;
         Index            : Natural               := Vecs'First;
      begin
         while Offset_Remainder >= Vecs (Index).Iov_Len loop
            Offset_Remainder := Offset_Remainder
              - Vecs (Index).Iov_Len;
            Index := Index + 1;
         end loop;
         pragma Assert (Offset_Remainder + Size <= Vecs (Index).Iov_Len);

         Data := Vecs (Index).Iov_Base + Offset_Remainder;
      exception
         when others =>
            raise Read_Error;
      end Extract_Data;

      procedure Release
        (Iovec_Pool : in out Iovec_Pool_Type) is
      begin
         if Is_Dynamic (Iovec_Pool) then
            Free (Iovec_Pool.Dynamic_Array);
         end if;

         Iovec_Pool.Last := 0;
         Iovec_Pool.Length := Iovec_Pool.Prealloc_Array'Length;
      end Release;

      procedure Write_To_Socket
        (S          : PolyORB.Sockets.Socket_Type;
         Iovec_Pool : access Iovec_Pool_Type)
      is
         use PolyORB.Sockets;

         Vecs : Iovec_Array := Iovecs (Iovec_Pool.all);

         Index : Natural := Vecs'First;

         Last  : Stream_Element_Offset;

         Count : Stream_Element_Count;
         --  Number of Stream_Elements written in one
         --  Send_Socket operation.

         Rest  : Stream_Element_Count := 0;
         --  Number of Stream_Elements yet to be written.

      begin
         for I in Vecs'Range loop
            Rest := Rest + Vecs (I).Iov_Len;
         end loop;

         while Rest > 0 loop
            declare
               P : constant Opaque_Pointer := Vecs (Index).Iov_Base;
               L : constant Stream_Element_Count := Vecs (Index).Iov_Len;
            begin

               --  FIXME:
               --  For now we do scatter-gather ourselves for lack of a writev
               --  operation in GNAT.Sockets.

               Send_Socket (S, P.Zone (P.Offset .. P.Offset + L - 1), Last);
               --  May raise Socket_Error.
               Count := Last - P.Offset + 1;

               while Index <= Vecs'Last
                 and then Count >= Vecs (Index).Iov_Len loop
                  Rest  := Rest  - Vecs (Index).Iov_Len;
                  Count := Count - Vecs (Index).Iov_Len;
                  Index := Index + 1;
               end loop;

               if Count > 0 then
                  Vecs (Index).Iov_Base :=
                    Vecs (Index).Iov_Base + Count;
                  Vecs (Index).Iov_Len
                    := Vecs (Index).Iov_Len - Count;
               end if;
            end;
         end loop;
      end Write_To_Socket;

      procedure Dump
        (Iovec_Pool : Iovec_Pool_Type;
         Into       : Opaque_Pointer)
      is
      begin
         Dump (Iovecs (Iovec_Pool), Into);
      end Dump;

      function Dump
        (Iovec_Pool : in Iovec_Pool_Type)
        return Zone_Access is
      begin
         if Is_Dynamic (Iovec_Pool) then
            return Dump (Iovec_Pool.Dynamic_Array
                         (1 .. Iovec_Pool.Last));
         else
            return Dump (Iovec_Pool.Prealloc_Array
                         (1 .. Iovec_Pool.Last));
         end if;
      end Dump;

   end Iovec_Pools;

end PolyORB.Buffers;

