------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        B R O C A . B U F F E R S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.14 $
--                                                                          --
--         Copyright (C) 1999, 2000 ENST Paris University, France.          --
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

with Broca.Debug;
with System; use System;

with Ada.Unchecked_Deallocation;
--  For Iovec_Pools.Free.

with Interfaces.C;
--  For Interfaces.C.int.

with Ada.Unchecked_Conversion;
--  For conversions between Opaque_Pointer and Caddr_T.
--  For conversions between System.Address and Interfaces.C.int.

with System.Address_To_Access_Conversions;
--  For Dump.

with CORBA;
--  For CORBA.Octet and CORBA.Bool.

package body Broca.Buffers is

   use Buffer_Chunk_Pools;
   use Iovec_Pools;

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.buffers");
   procedure O is new Broca.Debug.Output (Flag);

   subtype Output_Line is String (1 .. 48);

   Hex : constant String      := "0123456789ABCDEF";
   Nil : constant Output_Line := (others => ' ');

   ---------------------------
   -- Interface with Iovecs --
   ---------------------------

   subtype C_int is Interfaces.C.int;

   ------------------------
   -- General operations --
   ------------------------

   function Endianness
     (Buffer : Buffer_Type)
     return Endianness_Type is
   begin
      return Buffer.Endianness;
   end Endianness;

   procedure Release
     (Buffer : in out Buffer_Type) is
   begin
      Release (Buffer.Contents);
      Release (Buffer.Storage'Access);
      Buffer.CDR_Position := 0;
      Buffer.Initial_CDR_Position := 0;
      Buffer.Endianness := Host_Order;
   end Release;

   procedure Initialize_Buffer
     (Buffer     : access Buffer_Type;
      Size       : Index_Type;
      Data       : Opaque_Pointer;
      Endianness : Endianness_Type;
      Initial_CDR_Position : Index_Type)
   is
      Data_Iovec : constant Iovec
        := (Iov_Base => Data,
            Iov_Len  => C_int (Size));

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
   end Initialize_Buffer;

   procedure Prepend
     (Prefix : in Buffer_Type;
      Buffer : access Buffer_Type) is
   begin
      pragma Assert (True
        and then Prefix.Endianness = Buffer.Endianness
        and then Prefix.CDR_Position = Buffer.Initial_CDR_Position);

      Iovec_Pools.Prepend_Pool (Prefix.Contents, Buffer.Contents);

      Buffer.Initial_CDR_Position := Prefix.Initial_CDR_Position;
   end Prepend;

   function Copy
     (Buffer : access Buffer_Type)
     return Buffer_Access
   is
      Into : constant Buffer_Access
        := new Buffer_Type;

      Octets : constant Octet_Array
        := Dump (Buffer.Contents);
      Copy_Address : Opaque_Pointer;

      subtype Data is Octet_Array (Octets'Range);

      package Opaque_Pointer_To_Data_Access is
         new System.Address_To_Access_Conversions (Data);

      use Opaque_Pointer_To_Data_Access;

   begin
      Into.Endianness := Buffer.Endianness;
      Set_Initial_Position (Into, Buffer.Initial_CDR_Position);

      Allocate_And_Insert_Cooked_Data
        (Buffer,
         Octets'Length,
         Copy_Address);
      To_Pointer (Copy_Address).all := Octets;

      return Into;
   end Copy;

   procedure Release
     (A_Buffer : in out Buffer_Access)
   is
      procedure Free is
         new Ada.Unchecked_Deallocation
        (Buffer_Type, Buffer_Access);

   begin
      Release (A_Buffer.all);
      Free (A_Buffer);
   end Release;

   ----------------------------------------
   -- The Encapsulation view of a buffer --
   ----------------------------------------

   function Encapsulate
     (Buffer   : access Buffer_Type)
     return Encapsulation is
   begin
      pragma Assert (Buffer.Initial_CDR_Position = 0);

      return Iovec_Pools.Dump (Buffer.Contents);
   end Encapsulate;

   procedure Decapsulate
     (Octets : access Encapsulation;
      Buffer : access Buffer_Type)
   is
      Endianness : Endianness_Type;
   begin
      if CORBA.Boolean'Val
        (CORBA.Octet (Octets (Octets'First))) then
         Endianness := Little_Endian;
      else
         Endianness := Big_Endian;
      end if;

      Initialize_Buffer
        (Buffer     => Buffer,
         Size       => Octets.all'Length - 1,
         Data       => Octets (Octets'First + 1)'Address,
         Endianness => Endianness,
         Initial_CDR_Position => 1);

   end Decapsulate;

   ------------------------------
   -- The CDR view of a buffer --
   ------------------------------

   procedure Set_Initial_Position
     (Buffer   : access Buffer_Type;
      Position : Index_Type) is
   begin
      pragma Assert
        (Buffer.Initial_CDR_Position = Buffer.CDR_Position);

      Buffer.Initial_CDR_Position := Position;
      Buffer.CDR_Position := Position;
   end Set_Initial_Position;

   Null_Data : aliased array (1 .. Alignment_Type'Last - 1)
     of Interfaces.Unsigned_8 := (others => 0);
   pragma Convention (C, Null_Data);
   --  Null data used for padding.
   Null_Data_Address : constant System.Address
     := Null_Data'Address;
   --  The address of the first element of array Null_Data.

   procedure Align
     (Buffer    : access Buffer_Type;
      Alignment : Alignment_Type)
   is
      Padding : constant Index_Type
         := (Alignment - Buffer.CDR_Position) mod Alignment;
      Padding_Space : Opaque_Pointer;
   begin

      if Padding = 0 then
         --  Buffer is already aligned.
         return;
      end if;

      Grow (Buffer.Contents'Access, Padding, Padding_Space);
      --  Try to extend Buffer.Content's last Iovec
      --  to provide proper alignment.

      if Padding_Space = Null_Address then
         --  Grow was unable to extend the last Iovec:
         --  insert a non-growable iovec corresponding
         --  to static null data.

         declare
            Padding_Iovec : constant Iovec
              := (Iov_Base => Null_Data_Address,
                  Iov_Len  => C_int (Padding));
         begin
            Append
              (Iovec_Pool => Buffer.Contents,
               An_Iovec   => Padding_Iovec);
         end;
      end if;

      Buffer.CDR_Position := Buffer.CDR_Position + Padding;
      --  Advance the CDR position to the new alignment.

      pragma Assert (Buffer.CDR_Position mod Alignment = 0);
      --  Post-condition: the buffer is aligned as requested.
   end Align;

   --  Inserting data into a buffer

   procedure Insert_Raw_Data
     (Buffer    : access Buffer_Type;
      Size      : Index_Type;
      Data      : Opaque_Pointer)
   is
      Data_Iovec : constant Iovec
        := (Iov_Base => Data,
            Iov_Len  => C_int (Size));
   begin
      pragma Assert (Buffer.Endianness = Host_Order);

      Append
        (Iovec_Pool => Buffer.Contents,
         An_Iovec   => Data_Iovec);
      Buffer.CDR_Position := Buffer.CDR_Position + Size;
   end Insert_Raw_Data;

   procedure Allocate_And_Insert_Cooked_Data
     (Buffer    : access Buffer_Type;
      Size      : Index_Type;
      Data      : out Opaque_Pointer)
   is
      A_Data : Opaque_Pointer;
   begin
      Grow (Buffer.Contents'Access, Size, A_Data);
      --  First try to grow an existing Iovec.

      if A_Data /= Null_Address then
         Buffer.CDR_Position := Buffer.CDR_Position + Size;
         Data := A_Data;
         return;
      end if;

      declare
         A_Chunk : Chunk_Access;
         Data_Iovec : Iovec;
      begin
         Allocate (Buffer.Storage'Access, A_Chunk);
         Data_Iovec := (Iov_Base => Chunk_Storage (A_Chunk),
                        Iov_Len  => C_int (Size));

         A_Data := Chunk_Storage (A_Chunk);
         pragma Assert (A_Data /= Null_Address);
         Data := A_Data;

         Metadata (A_Chunk).all :=
           (Last_Used             => Size);
         Append
           (Iovec_Pool => Buffer.Contents,
            An_Iovec   => Data_Iovec,
            A_Chunk    => A_Chunk);
         Buffer.CDR_Position := Buffer.CDR_Position + Size;
      end;
   end Allocate_And_Insert_Cooked_Data;

   procedure Extract_Data
     (Buffer : access Buffer_Type;
      Data   : out Opaque_Pointer;
      Size   : Index_Type) is
   begin
      Extract_Data
        (Buffer.Contents,
         Data,
         Buffer.CDR_Position - Buffer.Initial_CDR_Position,
         Size);
      Buffer.CDR_Position := Buffer.CDR_Position + Size;
   end Extract_Data;

   function Length
     (Buffer : access Buffer_Type)
     return Index_Type is
   begin
      return Buffer.CDR_Position - Buffer.Initial_CDR_Position;
   end Length;

   -------------------------
   -- Utility subprograms --
   -------------------------

   procedure Show
     (Octets : Octet_Array);
   --  Display the contents of Octets for
   --  debugging purposes.

   procedure Show
     (Octets : Octet_Array)
   is
      Output : Output_Line;
      Index  : Natural := 1;

      use Interfaces;
      --  For operations on Unsigned_8.

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
                       & Index_Type'Image
                       (Buffer.CDR_Position)));

      Show (Iovec_Pools.Dump (Buffer.Contents));
   end Show;

   package body Iovec_Pools is

      procedure Free is new Ada.Unchecked_Deallocation
        (Iovec_Array, Iovec_Array_Access);

      --  Address <-> Integer conversions

      function To_C_int is
         new Ada.Unchecked_Conversion
        (System.Address, C_int);
      function To_Address is
         new Ada.Unchecked_Conversion
        (C_int, System.Address);

      procedure Grow
        (Iovec_Pool   : access Iovec_Pool_Type;
         Size         : Index_Type;
         Data         : out Opaque_Pointer)
      is

         use Interfaces.C;

         function First_Address_After
           (An_Iovec : Iovec)
           return System.Address;
         --  Return the address of the storage
         --  element immediately following the
         --  last element of An_Iovec.

         function First_Address_After
           (An_Iovec : Iovec)
           return System.Address is
         begin
            return To_Address
              (To_C_int (An_Iovec.Iov_Base) + An_Iovec.Iov_Len);
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
                  if Chunk_Metadata.Last_Used + Size
                    <= Last_Chunk.Size then
                     Chunk_Metadata.Last_Used
                       := Chunk_Metadata.Last_Used + Size;
                     Data := First_Address_After (Last_Iovec);
                     Last_Iovec.Iov_Len := Last_Iovec.Iov_Len
                       + C_int (Size);
                  end if;
               end;
            end if;
         end Do_Grow;

      begin
         Data := Null_Address;
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
      end Grow;

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
         Require    : Positive_Index_Type;
         Allocate   : Positive_Index_Type);
      --  Check the number of available Iovecs in
      --  Iovec_Pool and possibly extend it.
      --  If Iovec_Pool's length is at least
      --  Require, then does nothing, else
      --  make it Allocate Iovecs long.

      function Dump
        (Iovecs : Iovec_Array)
        return Octet_Array;
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

      function Dump
        (Iovecs : Iovec_Array)
        return Octet_Array
      is
         Length : Index_Type := 0;
      begin
         for I in Iovecs'Range loop
            Length := Length + Index_Type (Iovecs (I).Iov_Len);
         end loop;

         declare
            Result  : Octet_Array (1 .. Length);
            Current : Index_Type := Result'First;
         begin
            for I in Iovecs'Range loop
               declare
                  subtype Data is
                    Octet_Array (1 .. Index_Type
                                 (Iovecs (I).Iov_Len));

                  package Opaque_Pointer_To_Data_Access is
                      new System.Address_To_Access_Conversions
                      (Data);

                  use Opaque_Pointer_To_Data_Access;

                  Octets : constant Object_Pointer
                    := To_Pointer (Iovecs (I).Iov_Base);
               begin
                  Result
                    (Current .. Current
                     + Index_Type (Iovecs (I).Iov_Len) - 1)
                    := Octets.all;
                  Current := Current
                    + Index_Type (Iovecs (I).Iov_Len);
               end;
            end loop;

            return Result;

         end;
      end Dump;

      -------------------------------------------
      -- Visible subprograms (implementations) --
      -------------------------------------------

      procedure Extend
        (Iovec_Pool : in out Iovec_Pool_Type;
         Require    : Positive_Index_Type;
         Allocate   : Positive_Index_Type) is
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

      procedure Prepend_Pool
        (Prefix     : Iovec_Pool_Type;
         Iovec_Pool : in out Iovec_Pool_Type) is

         New_Last : constant Index_Type
           := Iovec_Pool.Last + Prefix.Last;
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
         New_Last : constant Index_Type
           := Iovec_Pool.Last + 1;
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
         Offset     : Index_Type;
         Size       : Index_Type)
      is
         use Interfaces.C;

         Vecs : constant Iovec_Array
           := Iovecs (Iovec_Pool);
         Offset_Remainder : C_int
           := C_int (Offset);
         Index : Index_Type
           := Vecs'First;
      begin
         while Offset_Remainder >= Vecs (Index).Iov_Len loop
            Offset_Remainder := Offset_Remainder
              - Vecs (Index).Iov_Len;
            Index := Index + 1;
         end loop;

         pragma Assert (Offset_Remainder + C_int (Size)
                          <= Vecs (Index).Iov_Len);

         Data := To_Address (To_C_int (Vecs (Index).Iov_Base)
                             + Offset_Remainder);
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

      function Dump
        (Iovec_Pool : in Iovec_Pool_Type)
        return Octet_Array is
      begin
         if Is_Dynamic (Iovec_Pool) then
            return Dump (Iovec_Pool.Dynamic_Array
                         (1 .. Iovec_Pool.Last));
         else
            return Dump (Iovec_Pool.Prealloc_Array
                         (1 .. Iovec_Pool.Last));
         end if;
      end Dump;

      procedure Write_To_FD
        (FD : in Interfaces.C.int;
         Iovec_Pool : access Iovec_Pool_Type)
      is
         use Sockets.Thin;
         use Interfaces.C;

         Vecs : Iovec_Array
           := Iovecs (Iovec_Pool.all);

         Index : Index_Type := Vecs'First;

         Count : int;
         Rest  : int := 0;

      begin
         for I in Vecs'Range loop
            Rest := Rest + Vecs (I).Iov_Len;
         end loop;

         while Rest > 0 loop
            Count := C_Writev
              (FD,
               Vecs (Index)'Address,
               C_int (Vecs'Last - Index + 1));

            --  FIXME: Should improve error reporting.
            --  This is initially from sockets.adb.
            --  Thomas.
            if Count < 0 then
               --  Raise_With_Message ("Send failed");
               raise Write_Error;
            elsif Count = 0 then
               raise Write_Error;
            end if;

            while Index <= Vecs'Last
              and then Count >= Vecs (Index).Iov_Len loop
               Rest  := Rest  - Vecs (Index).Iov_Len;
               Count := Count - Vecs (Index).Iov_Len;
               Index := Index + 1;
            end loop;

            if Count > 0 then
               Vecs (Index).Iov_Base := To_Address
                 (To_C_int (Vecs (Index).Iov_Base) + Count);
               Vecs (Index).Iov_Len
                 := Vecs (Index).Iov_Len - Count;
            end if;
         end loop;
      end Write_To_FD;

   end Iovec_Pools;

end Broca.Buffers;

