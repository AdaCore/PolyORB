------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                    B R O C A . M A R S H A L L I N G                     --
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

with Broca.Exceptions;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Marshalling is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.marshalling");
   procedure O is new Broca.Debug.Output (Flag);

   subtype Short_Buffer_Type is Buffer_Type (0 .. S_Size - 1);
   function Buffer_Type_To_Unsigned_Short is new Ada.Unchecked_Conversion
     (Source => Short_Buffer_Type, Target => CORBA.Unsigned_Short);
   function Unsigned_Short_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Unsigned_Short, Target => Short_Buffer_Type);
   function Buffer_Type_To_Short is new Ada.Unchecked_Conversion
     (Source => Short_Buffer_Type, Target => CORBA.Short);
   function Short_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Short, Target => Short_Buffer_Type);

   subtype Long_Buffer_Type is Buffer_Type (0 .. L_Size - 1);
   function Buffer_Type_To_Unsigned_Long is new Ada.Unchecked_Conversion
     (Source => Long_Buffer_Type, Target => CORBA.Unsigned_Long);
   function Unsigned_Long_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Unsigned_Long, Target => Long_Buffer_Type);
   function Buffer_Type_To_Long is new Ada.Unchecked_Conversion
     (Source => Long_Buffer_Type, Target => CORBA.Long);
   function Long_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Long, Target => Long_Buffer_Type);
   function Buffer_Type_To_Float is new Ada.Unchecked_Conversion
     (Source => Long_Buffer_Type, Target => CORBA.Float);
   function Float_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Float, Target => Long_Buffer_Type);

   subtype Very_Long_Buffer_Type is Buffer_Type (0 .. D_Size - 1);
   function Buffer_Type_To_Double is new Ada.Unchecked_Conversion
     (Source => Very_Long_Buffer_Type, Target => CORBA.Double);
   function Double_To_Buffer_Type is new Ada.Unchecked_Conversion
     (Source => CORBA.Double, Target => Very_Long_Buffer_Type);

   procedure Revert (Bytes : in out Buffer_Type);

   ----------------------
   -- Compute_New_Size --
   ----------------------

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Octet) is
   begin
      Compute_New_Size (Buffer, 1, 1);
   end Compute_New_Size;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Char) is
   begin
      Compute_New_Size (Buffer, 1, 1);
   end Compute_New_Size;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Boolean) is
   begin
      Compute_New_Size (Buffer, 1, 1);
   end Compute_New_Size;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Unsigned_Short) is
   begin
      Compute_New_Size (Buffer, 2, 2);
   end Compute_New_Size;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor; Value  : in CORBA.Short) is
   begin
      Compute_New_Size (Buffer, S_Size, S_Size);
   end Compute_New_Size;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Unsigned_Long) is
   begin
      Compute_New_Size (Buffer, 4, 4);
   end Compute_New_Size;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Long) is
   begin
      Compute_New_Size (Buffer, 4, 4);
   end Compute_New_Size;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Float) is
   begin
      Compute_New_Size (Buffer, 4, 4);
   end Compute_New_Size;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Double) is
   begin
      Compute_New_Size (Buffer, 8, 8);
   end Compute_New_Size;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.String) is
   begin
      --  4 for Length, N for String. 1 for \0.
      Compute_New_Size
        (Buffer,
         UL_Size,
         UL_Size + Buffer_Index_Type (CORBA.Length (Value)) + 1);
   end Compute_New_Size;

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in String) is
   begin
      --  4 for Length, N for String. 1 for \0.
      Compute_New_Size
        (Buffer, UL_Size,
         UL_Size + Buffer_Index_Type (Value'Length) + 1);
   end Compute_New_Size;

   ----------------------
   -- Compute_New_Size --
   ----------------------

   procedure Compute_New_Size
     (Buffer        : in out Buffer_Descriptor;
      Length_Size   : in Buffer_Index_Type;
      Element_Size  : in Buffer_Index_Type;
      Array_Length  : in Natural) is
   begin
      Compute_New_Size
        (Buffer,
         Alignment_Type (Length_Size),
         Length_Size);
      Compute_New_Size
        (Buffer,
         Alignment_Type (Element_Size),
         Buffer_Index_Type (Array_Length) * Element_Size);
   end Compute_New_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Octet)
   is
      Bytes : Buffer_Type (0 .. 0);
   begin
      Bytes (0) := Byte (Value);
      Write (Buffer, Bytes);
   end Marshall;

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Char)
   is
      Bytes : Buffer_Type (0 .. 0);
   begin
      Bytes (0) := CORBA.Char'Pos (Value);
      Write (Buffer, Bytes);
   end Marshall;

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Boolean)
   is
      Bytes : Buffer_Type (0 .. 0);
   begin
      Bytes (0) := CORBA.Boolean'Pos (Value);
      Write (Buffer, Bytes);
   end Marshall;

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Unsigned_Short) is
   begin
      Align_Size (Buffer, US_Size);
      Write (Buffer, Unsigned_Short_To_Buffer_Type (Value));
   end Marshall;

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Short) is
   begin
      Align_Size (Buffer, S_Size);
      Write (Buffer, Short_To_Buffer_Type (Value));
   end Marshall;

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Unsigned_Long) is
   begin
      Align_Size (Buffer, UL_Size);
      Write (Buffer, Unsigned_Long_To_Buffer_Type (Value));
   end Marshall;

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Long) is
   begin
      Align_Size (Buffer, L_Size);
      Write (Buffer, Long_To_Buffer_Type (Value));
   end Marshall;

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Float) is
   begin
      Align_Size (Buffer, F_Size);
      Write (Buffer, Float_To_Buffer_Type (Value));
   end Marshall;

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Double) is
   begin
      Align_Size (Buffer, D_Size);
      Write (Buffer, Double_To_Buffer_Type (Value));
   end Marshall;

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.String)
   is
      S : String := CORBA.To_Standard_String (Value);
   begin
      Marshall (Buffer, S);
   end Marshall;

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in String)
   is
      subtype Sub_String is String (Value'Range);
      subtype Sub_Buffer_Type is Buffer_Type (0 .. Value'Length - 1);
      function String_To_Buffer_Type is
        new Ada.Unchecked_Conversion (Sub_String, Sub_Buffer_Type);
   begin
      Marshall (Buffer, CORBA.Unsigned_Long (Value'Length + 1));
      Write    (Buffer, String_To_Buffer_Type (Value));
      Marshall (Buffer, CORBA.Octet (0));
   end Marshall;

   ------------
   -- Revert --
   ------------

   procedure Revert (Bytes : in out Buffer_Type)
   is
      F : constant Buffer_Index_Type := Bytes'First;
      M : constant Buffer_Index_Type := (Bytes'First + Bytes'Last - 1) / 2;
      L : constant Buffer_Index_Type := Bytes'Last;
      B : Byte;
   begin
      for I in F .. M loop
         B := Bytes (I);
         Bytes (I) := Bytes (L - I);
         Bytes (L - I) := B;
      end loop;
   end Revert;

   -----------------
   -- Skip_String --
   -----------------

   procedure Skip_String
     (Buffer : in out Buffer_Descriptor)
   is
      Length : Buffer_Index_Type;
   begin
      Unmarshall (Buffer, CORBA.Unsigned_Long (Length));
      declare
         Bytes : Buffer_Type (0 .. Length - 1);
      begin
         Read (Buffer, Bytes);
      end;
   end Skip_String;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Octet)
   is
      Bytes : Buffer_Type (0 .. 0);
   begin
      Read (Buffer, Bytes);
      Result := CORBA.Octet (Bytes (0));
   end Unmarshall;

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Char)
   is
      Bytes : Buffer_Type (0 .. 0);
   begin
      Read (Buffer, Bytes);
      Result := CORBA.Char'Val (Bytes (0));
   end Unmarshall;

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Boolean)
   is
      Bytes : Buffer_Type (0 .. 0);
   begin
      Read (Buffer, Bytes);
      case Bytes (0) is
         when 0 =>
            Result := False;
         when 1 =>
            Result := True;
         when others =>
            Broca.Exceptions.Raise_Marshal;
      end case;
   end Unmarshall;

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Unsigned_Short)
   is
      Bytes : Short_Buffer_Type;
   begin
      Align_Size (Buffer, US_Size);
      Read (Buffer, Bytes);
      if Get_Endianess (Buffer) /= Is_Little_Endian then
         Revert (Bytes);
      end if;
      Result := Buffer_Type_To_Unsigned_Short (Bytes);
   end Unmarshall;

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Short)
   is
      Bytes : Short_Buffer_Type;
   begin
      Align_Size (Buffer, S_Size);
      Read (Buffer, Bytes);
      if Get_Endianess (Buffer) /= Is_Little_Endian then
         Revert (Bytes);
      end if;
      Result := Buffer_Type_To_Short (Bytes);
   end Unmarshall;

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Unsigned_Long)
   is
      Bytes : Long_Buffer_Type;
   begin
      Align_Size (Buffer, UL_Size);
      Read (Buffer, Bytes);
      if Get_Endianess (Buffer) /= Is_Little_Endian then
         Revert (Bytes);
      end if;
      Result := Buffer_Type_To_Unsigned_Long (Bytes);
   end Unmarshall;

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Long)
   is
      Bytes : Long_Buffer_Type;
   begin
      Align_Size (Buffer, L_Size);
      Read (Buffer, Bytes);
      if Get_Endianess (Buffer) /= Is_Little_Endian then
         Revert (Bytes);
      end if;
      Result := Buffer_Type_To_Long (Bytes);
   end Unmarshall;

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Float)
   is
      Bytes : Long_Buffer_Type;
   begin
      Align_Size (Buffer, F_Size);
      Read (Buffer, Bytes);
      if Get_Endianess (Buffer) /= Is_Little_Endian then
         Revert (Bytes);
      end if;
      Result := Buffer_Type_To_Float (Bytes);
   end Unmarshall;

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Double)
   is
      Bytes : Very_Long_Buffer_Type;
   begin
      Align_Size (Buffer, D_Size);
      Read (Buffer, Bytes);
      if Get_Endianess (Buffer) /= Is_Little_Endian then
         Revert (Bytes);
      end if;
      Result := Buffer_Type_To_Double (Bytes);
   end Unmarshall;

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.String)
   is
      use type CORBA.Octet;
      Length : Buffer_Index_Type;
   begin
      Unmarshall (Buffer, CORBA.Unsigned_Long (Length));
      declare
         subtype Sub_String is String (1 .. Natural (Length - 1));
         subtype Sub_Buffer_Type is Buffer_Type (0 .. Length - 2);
         function Buffer_Type_To_String is
           new Ada.Unchecked_Conversion (Sub_Buffer_Type, Sub_String);
         Bytes : Sub_Buffer_Type;
         Terminator : CORBA.Octet;
      begin
         Read (Buffer, Bytes);
         Unmarshall (Buffer, Terminator);
         if Terminator /= 0 then
            Broca.Exceptions.Raise_Marshal;
         end if;
         Result := CORBA.To_CORBA_String (Buffer_Type_To_String (Bytes));
      end;
   end Unmarshall;

end Broca.Marshalling;
