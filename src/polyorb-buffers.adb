------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . B U F F E R S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Utils.Unchecked_Deallocation;

pragma Warnings (Off);
--  Depends on System.Unsigned_Types, an internal GNAT unit
with System.Unsigned_Types;
pragma Warnings (On);

with PolyORB.Log;

package body PolyORB.Buffers is

   use Ada.Streams;
   use System.Storage_Elements;
   use System.Unsigned_Types;
   use PolyORB.Opaque;
   use PolyORB.Log;
   use Buffer_Chunk_Pools;
   use Iovec_Pools;

   package L is new PolyORB.Log.Facility_Log ("polyorb.buffers");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   package L2 is new PolyORB.Log.Facility_Log ("polyorb.buffers_show");
   procedure O2 (Message : String; Level : Log_Level := Debug)
     renames L2.Output;
   function C2 (Level : Log_Level := Debug) return Boolean
     renames L2.Enabled;

   -----------------------
   -- Local subprograms --
   -----------------------

   function To_Stream_Element_Array
     (Buffer : Buffer_Type) return Opaque.Zone_Access;
   --  Dump the contents of Buffer into a Stream_Element_Array, and return a
   --  pointer to it. The caller must take care of deallocating the pointer
   --  after use.

   function Padding_Size
     (Pos       : Stream_Element_Offset;
      Alignment : Alignment_Type) return Stream_Element_Count;
   pragma Inline (Padding_Size);
   --  Return size of padding required to bring buffer position Pos to the
   --  desired alignment.

   procedure Show (Octets : Zone_Access);
   --  Display the contents of Octets for debugging purposes.

   --------------------
   -- Align_Position --
   --------------------

   procedure Align_Position
     (Buffer    : access Buffer_Type;
      Alignment : Alignment_Type)
   is
      Padding : constant Stream_Element_Count :=
        Padding_Size (Buffer.CDR_Position, Alignment);
   begin
      pragma Debug
        (C, O ("Align_Position: pos = "
            & Stream_Element_Offset'Image (Buffer.CDR_Position)
            & ", padding by" & Stream_Element_Count'Image (Padding)
            & " for " & Alignment_Type'Image (Alignment)));

      pragma Assert
        (Buffer.CDR_Position + Padding <=
           Buffer.Initial_CDR_Position + Buffer.Length);

      --  Advance the CDR position to the new aligned position

      Buffer.CDR_Position := Buffer.CDR_Position + Padding;

      pragma Debug
        (C, O ("Align_Position: now at"
                 & Stream_Element_Offset'Image (Buffer.CDR_Position)));
   end Align_Position;

   -------------------------------------
   -- Allocate_And_Insert_Cooked_Data --
   -------------------------------------

   procedure Allocate_And_Insert_Cooked_Data
     (Buffer    : access Buffer_Type;
      Size      :        Stream_Element_Count;
      Data      :    out Opaque_Pointer)
   is
      A_Data : Opaque_Pointer;

   begin
      Grow_Shrink (Buffer.Contents'Access, Size, A_Data);
      --  First try to grow an existing Iovec.

      if Is_Null (A_Data) then
         declare
            A_Chunk : Chunk_Access;
            Data_Iovec : Iovec;
         begin
            Allocate (Buffer.Storage'Access, A_Chunk, Size);
            pragma Assert (A_Chunk /= null and then A_Chunk.Size >= Size);
            Data_Iovec := (Iov_Base => Chunk_Storage (A_Chunk),
                           Iov_Len  => Storage_Offset (Size));

            A_Data := Chunk_Storage (A_Chunk);
            Metadata (A_Chunk).all := (Last_Used => Size);
            Append
              (Iovec_Pool => Buffer.Contents,
               An_Iovec   => Data_Iovec,
               A_Chunk    => A_Chunk);
            pragma Assert (not Is_Null (A_Data));
         end;
      end if;

      Data := A_Data;
      Buffer.CDR_Position := Buffer.CDR_Position + Size;
      Buffer.Length := Buffer.Length + Size;
   end Allocate_And_Insert_Cooked_Data;

   ------------------
   -- CDR_Position --
   ------------------

   function CDR_Position
     (Buffer : access Buffer_Type)
     return Stream_Element_Offset is
   begin
      return Buffer.CDR_Position;
   end CDR_Position;

   ----------
   -- Copy --
   ----------

   function Copy (Buffer : access Buffer_Type) return Buffer_Access is
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

   ---------------
   -- Copy_Data --
   ---------------

   procedure Copy_Data
     (From : Buffer_Type;
      Into :    Reservation) is
   begin
      pragma Assert (True
        and then From.Endianness           = Into.Endianness
        and then From.Initial_CDR_Position = Into.CDR_Position
        and then From.Length               = Into.Length);

      Iovec_Pools.Dump (From.Contents, Into.Location);
   end Copy_Data;

   ----------------
   -- Endianness --
   ----------------

   function Endianness (Buffer : access Buffer_Type) return Endianness_Type is
   begin
      return Buffer.Endianness;
   end Endianness;

   ------------------
   -- Extract_Data --
   ------------------

   procedure Extract_Data
     (Buffer      : access Buffer_Type;
      Data        : out Opaque_Pointer;
      Size        : Stream_Element_Count;
      Use_Current : Boolean := True;
      At_Position : Stream_Element_Offset := 0)
   is
      Extracted_Size : Stream_Element_Count := Size;
   begin
      Partial_Extract_Data (Buffer, Data, Extracted_Size,
        Use_Current, At_Position, Partial => False);
      pragma Assert (Extracted_Size = Size);
   end Extract_Data;

   ------------
   -- Length --
   ------------

   function Length (Buffer : Buffer_Type) return Stream_Element_Count is
   begin
      return Buffer.Length;
   end Length;

   -----------------------
   -- Initialize_Buffer --
   -----------------------

   procedure Initialize_Buffer
     (Buffer               : access Buffer_Type;
      Size                 :        Stream_Element_Count;
      Data                 :        Opaque_Pointer;
      Endianness           :        Endianness_Type;
      Initial_CDR_Position :        Stream_Element_Offset)
   is
      Data_Iovec : constant Iovec := (Iov_Base => Data,
                                      Iov_Len  => Storage_Offset (Size));

   begin
      pragma Assert (True
        and then Buffer.CDR_Position = 0
        and then Buffer.Initial_CDR_Position = 0);

      Buffer.Endianness           := Endianness;
      Buffer.CDR_Position         := Initial_CDR_Position;
      Buffer.Initial_CDR_Position := Initial_CDR_Position;

      Append
        (Iovec_Pool => Buffer.Contents,
         An_Iovec   => Data_Iovec);

      Buffer.Length := Size;
   end Initialize_Buffer;

   ---------------------
   -- Insert_Raw_Data --
   ---------------------

   procedure Insert_Raw_Data
     (Buffer    : access Buffer_Type;
      Size      :        Stream_Element_Count;
      Data      :        Opaque_Pointer)
   is
      Data_Iovec : constant Iovec
        := (Iov_Base => Data, Iov_Len => Storage_Offset (Size));
   begin
      pragma Assert (Buffer.Endianness = Host_Order);

      Append (Iovec_Pool => Buffer.Contents, An_Iovec => Data_Iovec);
      Buffer.CDR_Position := Buffer.CDR_Position + Size;
      Buffer.Length       := Buffer.Length + Size;
   end Insert_Raw_Data;

   ---------------
   -- Pad_Align --
   ---------------

   Null_Data : aliased Stream_Element_Array
     (1 .. 2 ** Alignment_Type'Pos (Alignment_Type'Last)) := (others => 0);
   --  Null data used for padding

   procedure Pad_Align
     (Buffer    : access Buffer_Type;
      Alignment : Alignment_Type)
   is
      Padding : constant Stream_Element_Count :=
        Padding_Size (Buffer.CDR_Position, Alignment);
      Padding_Space : Opaque_Pointer;
   begin
      pragma Debug
        (C, O ("Pad_Align: pos = "
            & Stream_Element_Offset'Image (Buffer.CDR_Position)
            & ", padding by" & Stream_Element_Count'Image (Padding)
            & " for " & Alignment_Type'Image (Alignment)));

      if Padding = 0 then
         --  Buffer is already aligned

         return;
      end if;

      --  Try to extend Buffer.Content's last Iovec to provide proper alignment

      Grow_Shrink (Buffer.Contents'Access, Padding, Padding_Space);

      if Is_Null (Padding_Space) then

         --  Grow_Shrink was unable to extend the last Iovec:
         --  insert a non-growable iovec corresponding to static null data.

         declare
            Padding_Iovec : constant Iovec :=
              (Iov_Base => Null_Data'Address,
               Iov_Len  => Storage_Offset (Padding));
         begin
            Append (Iovec_Pool => Buffer.Contents, An_Iovec => Padding_Iovec);
         end;

      else
         --  Ensure padding space is zeroed out for deterministic behaviour.

         declare
            Z : Stream_Element_Array (1 .. Padding);
            for Z'Address use Padding_Space;
            pragma Import (Ada, Z);
         begin
            Z := (others => 0);
         end;
      end if;

      Buffer.Length := Buffer.Length + Padding;
      Align_Position (Buffer, Alignment);
   end Pad_Align;

   ------------------
   -- Padding_Size --
   ------------------

   function Padding_Size
     (Pos       : Stream_Element_Offset;
      Alignment : Alignment_Type) return Stream_Element_Count
   is
      --  Note: we take advantage of the fact that the representation of
      --  Alignment_Type is such that representation(Align_<n>) = <n>.

      function To_SSU is
        new Ada.Unchecked_Conversion (Alignment_Type, Short_Short_Unsigned);

      Alignment_Mask : constant Long_Unsigned :=
                         Long_Unsigned (To_SSU (Alignment) - 1);

      Padding : constant Long_Unsigned :=
        (-Long_Unsigned (Pos)) and Alignment_Mask;
   begin
      return Stream_Element_Count (Padding);
   end Padding_Size;

   --------------------------
   -- Partial_Extract_Data --
   --------------------------

   procedure Partial_Extract_Data
     (Buffer      : access Buffer_Type;
      Data        :    out Opaque_Pointer;
      Size        : in out Stream_Element_Count;
      Use_Current :        Boolean := True;
      At_Position :        Stream_Element_Offset := 0;
      Partial     :        Boolean := True)
   is
      Start_Position : Stream_Element_Offset;
      Requested_Size : constant Stream_Element_Count := Size;
   begin
      if Use_Current then
         Start_Position := Buffer.CDR_Position;
      else
         Start_Position := At_Position;
      end if;

      Extract_Data
        (Buffer.Contents, Data,
         Start_Position - Buffer.Initial_CDR_Position, Size);

      if Size < Requested_Size and then not Partial then
         raise Constraint_Error with "buffer underflow (requested"
                 & Requested_Size'Img & ", got" & Size'Img & ")";
      end if;

      if Use_Current then
         Buffer.CDR_Position := Buffer.CDR_Position + Size;
      end if;
   end Partial_Extract_Data;

   ----------
   -- Peek --
   ----------

   function Peek
     (Buffer   : access Buffer_Type;
      Position : Ada.Streams.Stream_Element_Offset)
     return Ada.Streams.Stream_Element
   is
   begin
      return Iovec_Pools.Peek
        (Iovec_Pool => Buffer.Contents,
         Offset     => Position - Buffer.Initial_CDR_Position);
   end Peek;

   --------------------
   -- Receive_Buffer --
   --------------------

   procedure Receive_Buffer
     (Buffer   : access Buffer_Type;
      Max      :        Stream_Element_Count;
      Received :    out Stream_Element_Count)
   is
      V                  : aliased Iovec;
      Saved_CDR_Position : constant Stream_Element_Offset :=
        Buffer.CDR_Position;

   begin
      pragma Debug (C, O ("Receive_Buffer: Max =" & Max'Img));

      Allocate_And_Insert_Cooked_Data (Buffer, Max, V.Iov_Base);
      V.Iov_Len := Storage_Offset (Max);
      Lowlevel_Receive (V'Access);
      Received := Stream_Element_Offset (V.Iov_Len);

      pragma Debug (C, O ("Receive_Buffer: Received =" & Received'Img));
      Unuse_Allocation (Buffer, Max - Received);
      Buffer.CDR_Position := Saved_CDR_Position;
   end Receive_Buffer;

   -------------
   -- Release --
   -------------

   procedure Release
     (A_Buffer : in out Buffer_Access)
   is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Buffer_Type,
         Name => Buffer_Access);

   begin
      if A_Buffer /= null then
         Release_Contents (A_Buffer.all);
         Free (A_Buffer);
      end if;
   end Release;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents
     (Buffer : in out Buffer_Type) is
   begin
      Release (Buffer.Contents);
      Buffer_Chunk_Pools.Release (Buffer.Storage'Access);
      Buffer.CDR_Position         := 0;
      Buffer.Initial_CDR_Position := 0;
      Buffer.Endianness           := Host_Order;
      Buffer.Length               := 0;
   end Release_Contents;

   ---------------
   -- Remaining --
   ---------------

   function Remaining
     (Buffer : access Buffer_Type)
     return Stream_Element_Count is
   begin
      return Buffer.Initial_CDR_Position + Buffer.Length
        - Buffer.CDR_Position;
   end Remaining;

   -------------
   -- Reserve --
   -------------

   function Reserve
     (Buffer : access Buffer_Type;
      Amount :        Stream_Element_Count) return Reservation
   is
      Copy_Address     : Opaque_Pointer;
      Initial_Position : constant Stream_Element_Offset :=
        Buffer.CDR_Position;
   begin
      Allocate_And_Insert_Cooked_Data
        (Buffer, Amount, Copy_Address);

      return Reservation'
        (Location     => Copy_Address,
         Endianness   => Buffer.Endianness,
         CDR_Position => Initial_Position,
         Length       => Amount);
   end Reserve;

   ------------
   -- Rewind --
   ------------

   procedure Rewind
     (Buffer : access Buffer_Type) is
   begin
      Buffer.CDR_Position := Buffer.Initial_CDR_Position;
   end Rewind;

   -----------------
   -- Send_Buffer --
   -----------------

   procedure Send_Buffer (Buffer : access Buffer_Type) is
      procedure Send_Iovec_Pool is new Iovec_Pools.Send_Iovec_Pool
        (Lowlevel_Send);
   begin
      Send_Iovec_Pool (Buffer.Contents'Access, Buffer.Length);
   end Send_Buffer;

   ----------------------
   -- Set_CDR_Position --
   ----------------------

   procedure Set_CDR_Position
     (Buffer   : access Buffer_Type;
      Position :        Stream_Element_Offset) is
   begin
      Buffer.CDR_Position := Position;
   end Set_CDR_Position;

   --------------------
   -- Set_Endianness --
   --------------------

   procedure Set_Endianness
     (Buffer : access Buffer_Type;
      E      :        Endianness_Type) is
   begin
      pragma Assert (Buffer.CDR_Position = Buffer.Initial_CDR_Position);
      Buffer.Endianness := E;
   end Set_Endianness;

   --------------------------
   -- Set_Initial_Position --
   --------------------------

   procedure Set_Initial_Position
     (Buffer   : access Buffer_Type;
      Position : Stream_Element_Offset) is
   begin
      pragma Assert
        (Buffer.Initial_CDR_Position = Buffer.CDR_Position);

      Buffer.Initial_CDR_Position := Position;
      Buffer.CDR_Position         := Position;
   end Set_Initial_Position;

   ----------
   -- Show --
   ----------

   procedure Show (Octets : Zone_Access)
   is
      subtype Hexa_Line is String (1 .. 50);
      subtype ASCII_Line is String (1 .. 17);

      Hex : constant String      := "0123456789ABCDEF";
      Nil_Hexa  : constant Hexa_Line := (others => ' ');
      Nil_ASCII : constant ASCII_Line := (others => ' ');

      Hexa  : Hexa_Line  := Nil_Hexa;
      Line  : ASCII_Line := Nil_ASCII;
      Index_Hexa  : Natural := 1;
      Index_ASCII : Natural := 1;
   begin
      for J in Octets'Range loop
         Hexa (Index_Hexa) := ' ';
         Hexa (Index_Hexa + 1) := Hex (Natural (Octets (J) / 16) + 1);
         Hexa (Index_Hexa + 2) := Hex (Natural (Octets (J) mod 16) + 1);

         Index_Hexa := Index_Hexa + 3;

         if Octets (J) < 32 or else Octets (J) > 127 then
            Line (Index_ASCII) := '.';
         else
            Line (Index_ASCII) := Character'Val (Natural (Octets (J)));
         end if;
         Index_ASCII := Index_ASCII + 1;

         if Index_Hexa = 25 then
            Hexa (Index_Hexa) := ' ';
            Hexa (Index_Hexa + 1) := ' ';
            Index_Hexa := Index_Hexa + 2;
            Line (Index_ASCII) := ' ';
            Index_ASCII := Index_ASCII + 1;
         end if;

         if Index_Hexa > Hexa'Length then
            pragma Debug (C2, O2 (Hexa & "   " & Line));
            Index_Hexa := 1;
            Hexa := Nil_Hexa;
            Index_ASCII := 1;
            Line := Nil_ASCII;
         end if;
      end loop;

      if Index_Hexa /= 1 then
         pragma Debug (C2, O2 (Hexa & "   " & Line));
         null;
      end if;
   end Show;

   ----------
   -- Show --
   ----------

   procedure Show (Buffer : access Buffer_Type) is
   begin
      pragma Debug (C2, O2 ("Dumping "
                       & Endianness_Type'Image (Buffer.Endianness)
                       & " buffer, CDR position is "
                       & Stream_Element_Offset'Image
                       (Buffer.CDR_Position) & " (length is" &
                       Buffer.Length'Img & ")"));
      if Buffer.Length = 0 then
         return;
      end if;

      declare
         Dumped : Zone_Access := To_Stream_Element_Array (Buffer.all);
      begin
         Show (Dumped);
         Free (Dumped);
      end;
   end Show;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array
     (Buffer : Buffer_Type) return Opaque.Zone_Access
   is
      Result : Opaque.Zone_Access;
   begin
      pragma Assert (Buffer.Initial_CDR_Position = 0);
      Result := new Stream_Element_Array (1 .. Length (Buffer));
      Iovec_Pools.Dump (Buffer.Contents, Result (Result'First)'Address);
      return Result;
   end To_Stream_Element_Array;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array
     (Buffer : Buffer_Type) return Stream_Element_Array
   is
      Contents : Zone_Access := To_Stream_Element_Array (Buffer);
      Result   : constant Stream_Element_Array := Contents.all;
   begin
      Free (Contents);
      return Result;
   end To_Stream_Element_Array;

   ----------------------
   -- Unuse_Allocation --
   ----------------------

   procedure Unuse_Allocation
     (Buffer    : access Buffer_Type;
      Size      :        Stream_Element_Count)
   is
      Data : Opaque_Pointer;

   begin
      if Size /= 0 then
         Grow_Shrink (Buffer.Contents'Access, -Size, Data);
         Buffer.CDR_Position := Buffer.CDR_Position - Size;
         Buffer.Length := Buffer.Length - Size;
      end if;
   end Unuse_Allocation;

   -----------------
   -- Iovec_Pools --
   -----------------

   package body Iovec_Pools is

      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Iovec_Array,
         Name => Iovec_Array_Access);

      ----------------------------------------
      -- Utility Subprograms (declarations) --
      ----------------------------------------

      function Is_Dynamic (Iovec_Pool : Iovec_Pool_Type) return Boolean;
      pragma Inline (Is_Dynamic);
      --  True iff Iovec pool uses dynamically allocated storage for the Iovecs
      --  and descriptors.

      function Iovecs_Address
        (Iovec_Pool : Iovec_Pool_Type) return System.Address;
      pragma Inline (Iovecs_Address);
      --  Returns the address of the first element of Iovec_Pool's Iovec_Array

      procedure Extend
        (Iovec_Pool : in out Iovec_Pool_Type;
         Require    :        Natural;
         Allocate   :        Natural);
      --  Check the number of available Iovecs in Iovec_Pool and possibly
      --  extend it. If Iovec_Pool's length is at least Require, then do
      --  nothing, else make it Allocate Iovecs long.

      procedure Dump (Iovecs : Iovec_Array; Into : Opaque_Pointer);
      --  Dump the content of Iovecs into Into

      ------------
      -- Append --
      ------------

      procedure Append
        (Iovec_Pool : in out Iovec_Pool_Type;
         An_Iovec   : Iovec;
         A_Chunk    : Buffer_Chunk_Pools.Chunk_Access := null)
      is
         New_Last : constant Natural := Iovec_Pool.Last + 1;
      begin
         Extend (Iovec_Pool, New_Last, 2 * Iovec_Pool.Length);

         --  Append new Iovec

         Iovec_Pool.Last := New_Last;
         Iovec_Pool.Last_Chunk := A_Chunk;

         declare
            Pool_Iovecs_Address : constant System.Address :=
              Iovecs_Address (Iovec_Pool);
            Pool_Iovecs : Iovec_Array (1 .. Iovec_Pool.Length);
            for Pool_Iovecs'Address use Pool_Iovecs_Address;
            pragma Import (Ada, Pool_Iovecs);
         begin
            Pool_Iovecs (Iovec_Pool.Last) := An_Iovec;
         end;
      end Append;

      ----------
      -- Dump --
      ----------

      procedure Dump
        (Iovecs : Iovec_Array;
         Into   : Opaque_Pointer)
      is
         Offset : Storage_Offset := 0;
      begin
         for J in Iovecs'Range loop
            declare
               L : constant Stream_Element_Offset :=
                 Stream_Element_Offset (Iovecs (J).Iov_Len);

               S_Addr : constant System.Address := Iovecs (J).Iov_Base;
               S : Stream_Element_Array (0 .. L - 1);
               for S'Address use S_Addr;
               pragma Import (Ada, S);

               D_Addr : constant System.Address := Into + Offset;
               D : Stream_Element_Array (0 .. L - 1);
               for D'Address use D_Addr;
               pragma Import (Ada, D);

            begin
               D := S;
               Offset := Offset + Storage_Offset (L);
            end;
         end loop;
      end Dump;

      ----------
      -- Dump --
      ----------

      procedure Dump
        (Iovec_Pool : Iovec_Pool_Type;
         Into       : Opaque_Pointer)
      is
         Vecs_Address : constant System.Address := Iovecs_Address (Iovec_Pool);
         Vecs : Iovec_Array (1 .. Iovec_Pool.Last);
         for Vecs'Address use Vecs_Address;
         pragma Import (Ada, Vecs);
      begin
         Dump (Vecs, Into);
      end Dump;

      ------------
      -- Extend --
      ------------

      procedure Extend
        (Iovec_Pool : in out Iovec_Pool_Type;
         Require    :        Natural;
         Allocate   :        Natural) is
      begin
         pragma Assert (Allocate >= Require);

         if Require > Iovec_Pool.Length then
            declare
               New_Array : constant Iovec_Array_Access :=
                 new Iovec_Array (1 .. Allocate);

               Old_Array_Address : constant System.Address :=
                 Iovecs_Address (Iovec_Pool);
               Old_Array : Iovec_Array (1 .. Iovec_Pool.Length);
               for Old_Array'Address use Old_Array_Address;
               pragma Import (Ada, Old_Array);
            begin
               New_Array (1 .. Iovec_Pool.Last) := Old_Array (Old_Array'Range);

               if Is_Dynamic (Iovec_Pool) then
                  Free (Iovec_Pool.Dynamic_Array);
               end if;

               Iovec_Pool.Dynamic_Array := New_Array;
               Iovec_Pool.Length := New_Array'Length;
            end;
         end if;
      end Extend;

      ------------------
      -- Extract_Data --
      ------------------

      procedure Extract_Data
        (Iovec_Pool : in out Iovec_Pool_Type;
         Data       : out Opaque_Pointer;
         Offset     :     Stream_Element_Offset;
         Size       : in out Stream_Element_Count)
      is
         Vecs_Address : constant System.Address := Iovecs_Address (Iovec_Pool);
         Vecs : Iovec_Array (1 .. Iovec_Pool.Last);
         for Vecs'Address use Vecs_Address;
         pragma Import (Ada, Vecs);

         Offset_Remainder : Storage_Offset := Storage_Offset (Offset);
         Last_Index       : Positive renames Iovec_Pool.Last_Extract_Iovec;
         Last_Offset      : Storage_Offset
                              renames Iovec_Pool.Last_Extract_Iovec_Offset;
      begin
         if Offset_Remainder < Last_Offset then
            Last_Index  := 1;
            Last_Offset := 0;
         else
            Offset_Remainder := Offset_Remainder - Last_Offset;
         end if;

         while Last_Index <= Vecs'Last
                 and then
               Offset_Remainder >= Vecs (Last_Index).Iov_Len
         loop
            Offset_Remainder := Offset_Remainder - Vecs (Last_Index).Iov_Len;
            Last_Offset      := Last_Offset      + Vecs (Last_Index).Iov_Len;
            Last_Index       := Last_Index       + 1;
         end loop;

         if Last_Index > Vecs'Last then
            --  Attempt to extract data past end of buffer

            Data := System.Null_Address;
            Size := 0;

         else
            --  Note: bypass contiguous size computation if requested size is
            --  1, as this is a frequent case.

            if Size > 1 then
               declare
                  Contiguous_Size : constant Stream_Element_Count :=
                    Stream_Element_Count (Vecs (Last_Index).Iov_Len
                                            - Offset_Remainder);
               begin
                  if Size > Contiguous_Size then
                     Size := Contiguous_Size;
                  end if;
               end;
            end if;

            Data := Vecs (Last_Index).Iov_Base + Offset_Remainder;
         end if;
      end Extract_Data;

      -----------------
      -- Grow_Shrink --
      -----------------

      procedure Grow_Shrink
        (Iovec_Pool   : access Iovec_Pool_Type;
         Size         : Stream_Element_Offset;
         Data         : out Opaque_Pointer)
      is

         function First_Address_After (An_Iovec : Iovec) return Opaque_Pointer;
         pragma Inline (First_Address_After);
         --  Return the address of the storage element immediately following
         --  the last element of An_Iovec.

         procedure Do_Grow
           (Last_Iovec : in out Iovec;
            Last_Chunk : Chunk_Access);
         pragma Inline (Do_Grow);
         --  ??? comment needed

         -------------
         -- Do_Grow --
         -------------

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
                             and then Chunk_Metadata.Last_Used + Size >= 0
                             and then Last_Iovec.Iov_Len
                               + Storage_Offset (Size) >= 0)
                  then
                     Chunk_Metadata.Last_Used
                       := Chunk_Metadata.Last_Used + Size;
                     Data := First_Address_After (Last_Iovec);
                     Last_Iovec.Iov_Len := Last_Iovec.Iov_Len
                       + Storage_Offset (Size);
                  else
                     --  Cannot grow last chunk: leave Data unchanged.

                     pragma Debug
                       (C, O ("Cannot satisfy growth request of size"
                           & Stream_Element_Offset'Image (Size)));
                     null;
                  end if;
               end;
            end if;
         end Do_Grow;

         -------------------------
         -- First_Address_After --
         -------------------------

         function First_Address_After
           (An_Iovec : Iovec) return Opaque_Pointer
         is
         begin
            return An_Iovec.Iov_Base + An_Iovec.Iov_Len;
         end First_Address_After;

      --  Start of processing for Grow_Shrink

      begin
         Data := System.Null_Address;

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

      --------------------
      -- Iovecs_Address --
      --------------------

      function Iovecs_Address
        (Iovec_Pool : Iovec_Pool_Type) return System.Address
      is
      begin
         if Is_Dynamic (Iovec_Pool) then
            return Iovec_Pool.Dynamic_Array (1)'Address;
         else
            return Iovec_Pool.Prealloc_Array (1)'Address;
         end if;
      end Iovecs_Address;

      ----------------
      -- Is_Dynamic --
      ----------------

      function Is_Dynamic (Iovec_Pool : Iovec_Pool_Type) return Boolean is
      begin
         return Iovec_Pool.Dynamic_Array /= null;
      end Is_Dynamic;

      ----------
      -- Peek --
      ----------

      function Peek
        (Iovec_Pool : Iovec_Pool_Type;
         Offset     : Stream_Element_Offset)
        return Stream_Element
      is
         Vecs_Address : constant System.Address
           := Iovecs_Address (Iovec_Pool);
         Iovecs : Iovec_Array (1 .. Iovec_Pool.Last);
         for Iovecs'Address use Vecs_Address;
         pragma Import (Ada, Iovecs);
         Current_Offset : Stream_Element_Offset := 0;
      begin
         for J in Iovecs'Range loop
            declare
               L : constant Stream_Element_Offset
                 := Stream_Element_Offset (Iovecs (J).Iov_Len);
            begin
               if Offset < L + Current_Offset then
                  declare
                     S_Addr : constant System.Address
                       := Iovecs (J).Iov_Base;
                     S : Stream_Element_Array (0 .. L - 1);
                     for S'Address use S_Addr;
                     pragma Import (Ada, S);
                  begin
                     return S (Offset - Current_Offset);
                  end;
               end if;
               Current_Offset := Current_Offset + L;
            end;
         end loop;

         raise Constraint_Error;
      end Peek;

      ------------------
      -- Prepend_Pool --
      ------------------

      procedure Prepend_Pool
        (Prefix     : Iovec_Pool_Type;
         Iovec_Pool : in out Iovec_Pool_Type)
      is
         New_Last : constant Natural := Iovec_Pool.Last + Prefix.Last;

      begin
         Extend (Iovec_Pool, New_Last, New_Last + 1);
         --  An Iovec pool that has been prefixed will likely not be appended
         --  to anymore.

         declare
            Prefix_Iovecs_Address : constant System.Address :=
              Iovecs_Address (Prefix);
            Prefix_Iovecs : Iovec_Array (1 .. Prefix.Length);
            for Prefix_Iovecs'Address use Prefix_Iovecs_Address;
            pragma Import (Ada, Prefix_Iovecs);

            Pool_Iovecs_Address : constant System.Address
              := Iovecs_Address (Iovec_Pool);
            Pool_Iovecs : Iovec_Array (1 .. Iovec_Pool.Length);
            for Pool_Iovecs'Address use Pool_Iovecs_Address;
            pragma Import (Ada, Pool_Iovecs);

         begin
            --  Append new Iovec

            Pool_Iovecs (1 .. New_Last) :=
              Prefix_Iovecs (Prefix_Iovecs'Range)
                & Pool_Iovecs (1 .. Iovec_Pool.Last);
            Iovec_Pool.Last := New_Last;
         end;
      end Prepend_Pool;

      -------------
      -- Release --
      -------------

      procedure Release
        (Iovec_Pool : in out Iovec_Pool_Type) is
      begin
         if Is_Dynamic (Iovec_Pool) then
            Free (Iovec_Pool.Dynamic_Array);
         end if;

         Iovec_Pool.Last := 0;
         Iovec_Pool.Length := Iovec_Pool.Prealloc_Array'Length;
         Iovec_Pool.Last_Extract_Iovec := 1;
         Iovec_Pool.Last_Extract_Iovec_Offset := 0;
      end Release;

      ---------------------
      -- Send_Iovec_Pool --
      ---------------------

      procedure Send_Iovec_Pool
        (Iovec_Pool : access Iovec_Pool_Type;
         Length     :        Stream_Element_Count)
      is
         Vecs_Address : constant System.Address
           := Iovecs_Address (Iovec_Pool.all);

         Vecs : Iovec_Array (1 .. Iovec_Pool.Last);
         for Vecs'Address use Vecs_Address;
         pragma Import (Ada, Vecs);

         Index : Natural := Vecs'First;

         Count : Storage_Offset;
         Remainder : Storage_Offset := Storage_Offset (Length);
         --  Number of Stream_Elements yet to be written.

      begin
         while Remainder > 0 loop

            Lowlevel_Send (Vecs (1)'Access, Vecs'Last - Index + 1, Count);

            while Index <= Vecs'Last
              and then Count >= Vecs (Index).Iov_Len
            loop
               Remainder := Remainder - Vecs (Index).Iov_Len;
               Count := Count - Vecs (Index).Iov_Len;
               Index := Index + 1;
            end loop;

            if Count > 0 then
               Vecs (Index).Iov_Base := Vecs (Index).Iov_Base + Count;
               Vecs (Index).Iov_Len  := Vecs (Index).Iov_Len  - Count;
               Remainder := Remainder - Count;
            end if;

            --  Abort completion point, in case this is being called
            --  indirectly from an asynchronous transfer of control.
            --  Otherwise, we might loop forever when using the ZCX
            --  runtime.

            delay 0.0;
         end loop;
      end Send_Iovec_Pool;

   end Iovec_Pools;

end PolyORB.Buffers;
