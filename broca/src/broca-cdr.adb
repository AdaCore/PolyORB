------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . C D R                             --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

with Broca.Debug;

package body Broca.CDR is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.cdr");
   procedure O is new Broca.Debug.Output (Flag);

   use CORBA;
   --  For operators on CORBA integer types.

   -------------------------
   -- Utility subprograms --
   -------------------------

   subtype BO_Octet is Broca.Opaque.Octet;

   function Rev
     (Octets : Octet_Array)
     return Octet_Array;
   --  Reverse the order of an array of octets.

   procedure Align_Marshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Octet_Array;
      Alignment : Alignment_Type := 1);
   --  Align Buffer on Alignment, then marshall a copy of
   --  Octets into it.
   --  The data in Octets shall be presented in big-endian
   --  byte order.

   function Align_Unmarshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Index_Type;
      Alignment : Alignment_Type := 1)
     return Octet_Array;
   --  Align Buffer on Alignment, then unmarshall a copy of
   --  Size octets from it.
   --  The data is returned in big-endian byte order.

   procedure Align_Marshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Octet_Array;
      Alignment : Alignment_Type := 1);
   --  Align Buffer on Alignment, then marshall a copy of
   --  Octets into it.
   --  The data in Octets shall be presented in the
   --  host's byte order.

   function Align_Unmarshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Index_Type;
      Alignment : Alignment_Type := 1)
     return Octet_Array;
   --  Align Buffer on Alignment, then unmarshall a copy of
   --  Size octets from it.
   --  The data is returned in the host's byte order.

   ------------------------------------------
   -- Conversions between CORBA signed and --
   -- unsigned integer types.              --
   ------------------------------------------

   function To_Long is
      new Ada.Unchecked_Conversion
     (CORBA.Unsigned_Long, CORBA.Long);
   function To_Unsigned_Long is
      new Ada.Unchecked_Conversion
     (CORBA.Long, CORBA.Unsigned_Long);
   function To_Short is
      new Ada.Unchecked_Conversion
     (CORBA.Unsigned_Short, CORBA.Short);
   function To_Unsigned_Short is
      new Ada.Unchecked_Conversion
     (CORBA.Short, CORBA.Unsigned_Short);

   -------------------------------------------
   --  Conversions for floating point types --
   -------------------------------------------

   subtype Double_Buf is Octet_Array (1 .. 8);

   function To_Unsigned_Long is
      new Ada.Unchecked_Conversion
     (CORBA.Float, CORBA.Unsigned_Long);
   function To_Float is
      new Ada.Unchecked_Conversion
     (CORBA.Unsigned_Long, CORBA.Float);
   function To_Double_Buf is
      new Ada.Unchecked_Conversion
     (CORBA.Double, Double_Buf);
   function To_Double is
      new Ada.Unchecked_Conversion
     (Double_Buf, CORBA.Double);

   ----------------------------------
   -- Marshall-by-copy subprograms --
   -- for all elementary types     --
   ----------------------------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.Boolean)
   is
   begin
      Marshall (Buffer, CORBA.Octet'(CORBA.Boolean'Pos (Data)));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Char)
   is
   begin
      Marshall (Buffer, CORBA.Octet'(CORBA.Char'Pos (Data)));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.Octet)
   is
   begin
      Align_Marshall_Copy (Buffer, (1 => BO_Octet (Data)), 1);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.Unsigned_Short)
   is
   begin
      Align_Marshall_Big_Endian_Copy
        (Buffer,
         (BO_Octet (Data / 256),
          BO_Octet (Data mod 256)),
         2);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.Unsigned_Long)
   is
   begin
      Align_Marshall_Big_Endian_Copy
        (Buffer,
         (BO_Octet (Data / 256**3),
          BO_Octet ((Data / 256**2) mod 256),
          BO_Octet ((Data / 256) mod 256),
          BO_Octet (Data mod 256)),
         4);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.Long)
   is
   begin
      Marshall (Buffer, To_Unsigned_Long (Data));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.Short)
   is
   begin
      Marshall (Buffer, To_Unsigned_Short (Data));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.Float)
   is
   begin
      Marshall (Buffer, To_Unsigned_Long (Data));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.Double)
   is
      Buf : Double_Buf
        := To_Double_Buf (Data);
   begin
      Align_Marshall_Host_Endian_Copy
        (Buffer, Buf, 8);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.String)
   is
      Equiv : constant String := CORBA.To_String (Data) & ASCII.Nul;
   begin
      Marshall (Buffer, CORBA.Unsigned_Long'(Equiv'Length));
      for I in Equiv'Range loop
         Marshall (Buffer, CORBA.Char'Val (Character'Pos (Equiv (I))));
      end loop;
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.Any) is
   begin
      null;
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.NamedValue) is
   begin
      Marshall (Buffer, Data.Argument);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Encapsulation) is
   begin
      Marshall (Buffer, CORBA.Unsigned_Long (Data'Length));
      for I in Data'Range loop
         Marshall (Buffer, CORBA.Octet (Data (I)));
      end loop;
   end Marshall;

   ---------------------------------------------------
   -- Marshall-by-reference subprograms             --
   -- (for elementary types, these are placeholders --
   -- that actually perform marshalling by copy.    --
   ---------------------------------------------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Octet) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Char) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Boolean) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Short) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Unsigned_Short) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Long) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Unsigned_Long) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Float) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Double) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.String) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Any) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.NamedValue) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Encapsulation) is
   begin
      Marshall (Buffer, CORBA.Unsigned_Long (Data'Length));
      Insert_Raw_Data (Buffer, Data'Length, Data (Data'First)'Address);
   end Marshall;

   ------------------------------------
   -- Unmarshall-by-copy subprograms --
   ------------------------------------

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Boolean is
   begin
      return CORBA.Boolean'Val (CORBA.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Char is
   begin
      return CORBA.Char'Val (CORBA.Octet'(Unmarshall (Buffer)));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Octet is
      Result : constant Octet_Array
        := Align_Unmarshall_Copy (Buffer, 1, 1);
   begin
      return CORBA.Octet (Result (Result'First));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Unsigned_Short
   is
      Octets : constant Octet_Array :=
        Align_Unmarshall_Big_Endian_Copy (Buffer, 2, 2);


   begin
      return CORBA.Unsigned_Short (Octets (Octets'First)) * 256 +
        CORBA.Unsigned_Short (Octets (Octets'First + 1));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Unsigned_Long
   is
      Octets : constant Octet_Array :=
        Align_Unmarshall_Big_Endian_Copy (Buffer, 4, 4);

   begin
      return CORBA.Unsigned_Long (Octets (Octets'First)) * 256**3
        + CORBA.Unsigned_Long (Octets (Octets'First + 1)) * 256**2
        + CORBA.Unsigned_Long (Octets (Octets'First + 2)) * 256
        + CORBA.Unsigned_Long (Octets (Octets'First + 3));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Long
   is
   begin
      return To_Long (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Short
   is
   begin
      return To_Short (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Float
   is
   begin
      return To_Float (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Double
   is
      Octets : constant Octet_Array :=
        Align_Unmarshall_Host_Endian_Copy (Buffer, 8, 8);

   begin
      return To_Double (Double_Buf (Octets));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.String
   is
      Length : constant CORBA.Unsigned_Long
        := Unmarshall (Buffer);
      Equiv  : String (1 .. Natural (Length));
   begin
      for I in Equiv'Range loop
         Equiv (I) := Character'Val (CORBA.Char'Pos
                                     (Unmarshall (Buffer)));
      end loop;
      return CORBA.To_CORBA_String
        (Equiv (1 .. Equiv'Length - 1));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Any is
   begin
      return CORBA.To_Any (CORBA.Short (0));
   end Unmarshall;

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Data : in out CORBA.NamedValue) is
   begin
      Data.Argument := Unmarshall (Buffer);
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return Encapsulation
   is
      Length : CORBA.Unsigned_Long;
   begin
      Length := Unmarshall (Buffer);
      declare
         E : Encapsulation (1 .. Index_Type (Length));
      begin
         for I in E'Range loop
            E (I) := BO_Octet (CORBA.Octet'(Unmarshall (Buffer)));
         end loop;
         return E;
      end;
   end Unmarshall;

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

   ------------------------------------
   -- Align_Marshall_Big_Endian_Copy --
   ------------------------------------

   procedure Align_Marshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Octet_Array;
      Alignment : Alignment_Type := 1)
   is
   begin
      if Endianness (Buffer.all) = Big_Endian then
         Align_Marshall_Copy (Buffer, Octets, Alignment);
      else
         Align_Marshall_Copy (Buffer, Rev (Octets), Alignment);
      end if;
   end Align_Marshall_Big_Endian_Copy;

   -------------------------------------
   -- Align_Marshall_Host_Endian_Copy --
   -------------------------------------

   procedure Align_Marshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Octet_Array;
      Alignment : Alignment_Type := 1)
   is
   begin
      if Endianness (Buffer.all) = Host_Order then
         Align_Marshall_Copy (Buffer, Octets, Alignment);
      else
         Align_Marshall_Copy (Buffer, Rev (Octets), Alignment);
      end if;
   end Align_Marshall_Host_Endian_Copy;

   -------------------------
   -- Align_Marshall_Copy --
   -------------------------

   procedure Align_Marshall_Copy
     (Buffer    : access Buffer_Type;
      Octets    : in Octet_Array;
      Alignment : Alignment_Type := 1)
   is

      subtype Data is Octet_Array (Octets'Range);

      package Opaque_Pointer_To_Data_Access is
         new System.Address_To_Access_Conversions
        (Data);

      Data_Address : Opaque_Pointer;

   begin
      Align (Buffer, Alignment);
      Allocate_And_Insert_Cooked_Data
        (Buffer,
         Octets'Length,
         Data_Address);

      Opaque_Pointer_To_Data_Access.To_Pointer (Data_Address).all
        := Octets;
   end Align_Marshall_Copy;

   --------------------------------------
   -- Align_Unmarshall_Big_Endian_Copy --
   --------------------------------------

   function Align_Unmarshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Index_Type;
      Alignment : Alignment_Type := 1)
     return Octet_Array
   is
   begin
      if Endianness (Buffer.all) = Big_Endian then
         return Align_Unmarshall_Copy (Buffer, Size, Alignment);
      else
         return Rev (Align_Unmarshall_Copy
                     (Buffer, Size, Alignment));
      end if;
   end Align_Unmarshall_Big_Endian_Copy;

   --------------------------------------
   -- Align_Unmarshall_Host_Endian_Copy --
   --------------------------------------

   function Align_Unmarshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Index_Type;
      Alignment : Alignment_Type := 1)
     return Octet_Array
   is
   begin
      if Endianness (Buffer.all) = Host_Order then
         return Align_Unmarshall_Copy (Buffer, Size, Alignment);
      else
         return Rev (Align_Unmarshall_Copy
                     (Buffer, Size, Alignment));
      end if;
   end Align_Unmarshall_Host_Endian_Copy;

   ---------------------------
   -- Align_Unmarshall_Copy --
   ---------------------------

   function Align_Unmarshall_Copy
     (Buffer    : access Buffer_Type;
      Size      : Index_Type;
      Alignment : Alignment_Type := 1)
     return Octet_Array
   is

      subtype Data is Octet_Array (1 .. Size);


      package Opaque_Pointer_To_Data_Access is
         new System.Address_To_Access_Conversions
        (Data);

      Data_Address : Opaque_Pointer;

   begin
      Align (Buffer, Alignment);
      Extract_Data (Buffer, Data_Address, Size);
      return Opaque_Pointer_To_Data_Access.To_Pointer
        (Data_Address).all;
   end Align_Unmarshall_Copy;

   -------------------------
   -- Start_Encapsulation --
   -------------------------

   procedure Start_Encapsulation
     (Buffer : access Buffer_Type) is
   begin
      Set_Initial_Position (Buffer, 0);
      Marshall (Buffer,
                CORBA.Boolean
                (Endianness (Buffer.all) = Little_Endian));
   end Start_Encapsulation;

end Broca.CDR;
