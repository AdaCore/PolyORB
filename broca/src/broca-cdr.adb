with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;                     use System;

package body Broca.CDR is

   use CORBA;

   procedure Free_It is
      new Ada.Unchecked_Deallocation (Octet_Array, Octet_Array_Ptr);

   Local_Endianess : Endianess_Type;

   function Rev (Octets : Octet_Array) return Octet_Array;

   procedure Marshall_Endian
     (Buffer    : access Buffer_Type;
      Octets    : Octet_Array;
      Alignment : in Alignment_Type);

   function Unmarshall_Endian
     (Buffer    : access Buffer_Type;
      Size      : Index_Type;
      Alignment : Alignment_Type)
     return Octet_Array;

   function To_Long is
      new Ada.Unchecked_Conversion (CORBA.Unsigned_Long, CORBA.Long);
   function To_Unsigned_Long is
      new Ada.Unchecked_Conversion (CORBA.Long, CORBA.Unsigned_Long);
   function To_Short is
      new Ada.Unchecked_Conversion (CORBA.Unsigned_Short, CORBA.Short);
   function To_Unsigned_Short is
      new Ada.Unchecked_Conversion (CORBA.Short, CORBA.Unsigned_Short);

   function Align (Index : Index_Type; Alignment : Alignment_Type)
     return Index_Type;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Buffer : in out Buffer_Type) is
   begin
      Buffer.Content := new Octet_Array'(Buffer.Content.all);
   end Adjust;

   -----------
   -- Align --
   -----------

   function Align (Index : Index_Type; Alignment : Alignment_Type)
     return Index_Type
   is
   begin
      return Index_Type (Alignment) *
        ((Index + Index_Type (Alignment) - 1) / Index_Type (Alignment));
   end Align;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Buffer : in out Buffer_Type) is
   begin
      Free (Buffer.Content);
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (Octets : in out Octet_Array_Ptr) is
   begin
      Free_It (Octets);
   end Free;

   -----------------
   -- Get_Content --
   -----------------

   function Get_Content (Buffer : access Buffer_Type) return Octet_Array is
   begin
      return Buffer.Content.all;
   end Get_Content;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Buffer : in out Buffer_Type) is
   begin
      Initialize (Buffer'Access, Local_Endianess);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Buffer  : access Buffer_Type;
      Content : in Octet_Array)
   is
   begin
      Free (Buffer.Content);
      Buffer.Content     := new Octet_Array (0 .. Content'Length - 1);
      Buffer.Content.all := Content;
      Buffer.Index       := 0;
      if Unmarshall (Buffer) then
         Buffer.Endianess := Little_Endian;
      else
         Buffer.Endianess := Big_Endian;
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Buffer    : access Buffer_Type;
      Endianess : in Endianess_Type)
   is
   begin
      Buffer.Endianess := Endianess;
      Buffer.Content   := new Octet_Array (0 .. -1);
      Buffer.Index     := 0;
      Marshall (Buffer, Buffer.Endianess = Little_Endian);
   end Initialize;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Inner  : in Buffer_Type)
   is
   begin
      Marshall (Buffer, CORBA.Unsigned_Long'(Inner.Content'Length));
      Marshall_Opaque (Buffer, Inner.Content.all);
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Boolean)
   is
   begin
      Marshall (Buffer, CORBA.Octet'(CORBA.Boolean'Pos (Octets)));
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Char)
   is
   begin
      Marshall (Buffer, CORBA.Octet'(CORBA.Char'Pos (Octets)));
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Octet)
   is
   begin
      Marshall_Opaque (Buffer, (1 => Octets));
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Unsigned_Short)
   is
   begin
      Marshall_Endian
        (Buffer,
         (CORBA.Octet (Octets / 256), CORBA.Octet (Octets mod 256)),
         2);
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Unsigned_Long)
   is
   begin
      Marshall_Endian
        (Buffer,
         (CORBA.Octet (Octets / 256**3),
          CORBA.Octet ((Octets / 256**2) mod 256),
          CORBA.Octet ((Octets / 256) mod 256),
          CORBA.Octet (Octets mod 256)),
         4);
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Long)
   is
   begin
      Marshall (Buffer, To_Unsigned_Long (Octets));
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Short)
   is
   begin
      Marshall (Buffer, To_Unsigned_Short (Octets));
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.String)
   is
      Equiv : constant String := To_String (Octets) & Ascii.Nul;
   begin
      Marshall (Buffer, CORBA.Unsigned_Long'(Equiv'Length));
      for I in Equiv'Range loop
         Marshall (Buffer, CORBA.Char'Val (Character'Pos (Equiv (I))));
      end loop;
   end Marshall;

   ---------------------
   -- Marshall_Endian --
   ---------------------

   procedure Marshall_Endian
     (Buffer    : access Buffer_Type;
      Octets    : Octet_Array;
      Alignment : in Alignment_Type)
   is
   begin
      if Buffer.Endianess = Big_Endian then
         Marshall_Opaque (Buffer, Octets, Alignment);
      else
         Marshall_Opaque (Buffer, Rev (Octets), Alignment);
      end if;
   end Marshall_Endian;

   ---------------------
   -- Marshall_Opaque --
   ---------------------

   procedure Marshall_Opaque
     (Buffer    : access Buffer_Type;
      Octets    : in Octet_Array;
      Alignment : in Alignment_Type := 1)
   is
      Pad_Size    : constant Index_Type :=
        Align (Buffer.Content'Last, Alignment) - Buffer.Content'Last;
      Padding     : constant Octet_Array (1 .. Pad_Size) := (others => 0);
      New_Content : constant Octet_Array_Ptr :=
        new Octet_Array'(Buffer.Content.all & Padding & Octets);
   begin
      Free (Buffer.Content);
      Buffer.Content := New_Content;
   end Marshall_Opaque;

   ---------
   -- Rev --
   ---------

   function Rev (Octets : Octet_Array) return Octet_Array is
      Result : Octet_Array (Octets'Range);
   begin
      for I in Octets'Range loop
         Result (Octets'Last - I + Octets'First) := Octets (I);
      end loop;
      return Result;
   end Rev;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall (Buffer : access Buffer_Type) return Buffer_Type is
      Inner  : aliased Buffer_Type;
      Length : constant CORBA.Unsigned_Long := Unmarshall (Buffer);
   begin
      Initialize (Inner'Access, Unmarshall_Opaque (Buffer,
                                                   Index_Type (Length)));
      return Inner;
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall (Buffer : access Buffer_Type) return CORBA.Boolean is
   begin
      return CORBA.Boolean'Val (CORBA.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall (Buffer : access Buffer_Type) return CORBA.Char is
   begin
      return CORBA.Char'Val (CORBA.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall (Buffer : access Buffer_Type) return CORBA.Octet is
      Result : constant Octet_Array := Unmarshall_Opaque (Buffer, 1);
   begin
      return Result (Result'First);
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Unsigned_Short
   is
      Octets : constant Octet_Array :=
        Unmarshall_Endian (Buffer, 2, 2);
   begin
      return CORBA.Unsigned_Short (Octets (Octets'First)) * 256 +
        CORBA.Unsigned_Short (Octets (Octets'First + 1));
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Unsigned_Long
   is
      Octets : constant Octet_Array :=
        Unmarshall_Endian (Buffer, 4, 4);
   begin
      return CORBA.Unsigned_Long (Octets (Octets'First)) * 256**3 +
        CORBA.Unsigned_Long (Octets (Octets'First + 1)) * 256**2 +
        CORBA.Unsigned_Long (Octets (Octets'First + 2)) * 256 +
        CORBA.Unsigned_Long (Octets (Octets'First + 3));
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Long
   is
   begin
      return To_Long (Unmarshall (Buffer));
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Short
   is
   begin
      return To_Short (Unmarshall (Buffer));
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall (Buffer : access Buffer_Type) return CORBA.String is
      Length : constant CORBA.Unsigned_Long := Unmarshall (Buffer);
      Equiv  : String (1 .. Natural (Length));
   begin
      for I in Equiv'Range loop
         Equiv (I) := Character'Val (CORBA.Char'Pos (Unmarshall (Buffer)));
      end loop;
      return To_Unbounded_String (Equiv (1 .. Equiv'Length - 1));
   end Unmarshall;

   -----------------------
   -- Unmarshall_Endian --
   -----------------------

   function Unmarshall_Endian
     (Buffer    : access Buffer_Type;
      Size      : Index_Type;
      Alignment : Alignment_Type)
     return Octet_Array
   is
   begin
      if Buffer.Endianess = Big_Endian then
         return Unmarshall_Opaque (Buffer, Size, Alignment);
      else
         return Rev (Unmarshall_Opaque (Buffer, Size, Alignment));
      end if;
   end Unmarshall_Endian;

   -----------------------
   -- Unmarshall_Opaque --
   -----------------------

   function Unmarshall_Opaque
     (Buffer    : access Buffer_Type;
      Size      : Index_Type;
      Alignment : Alignment_Type := 1)
     return Octet_Array
   is
      Pad_Size    : constant Index_Type :=
        Align (Buffer.Index, Alignment) - Buffer.Index;
      Result   : Octet_Array (0 .. Size - 1);
   begin
      Buffer.Index := Buffer.Index + Pad_Size;
      Result       := Buffer.Content (Buffer.Index ..
                                      Buffer.Index + Buffer.Index + Size - 1);
      Buffer.Index := Buffer.Index + Size;
      return Result;
   end Unmarshall_Opaque;

begin
   if Default_Bit_Order = High_Order_First then
      Local_Endianess := Big_Endian;
   else
      Local_Endianess := Little_Endian;
   end if;
end Broca.CDR;
