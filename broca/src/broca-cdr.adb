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
with Interfaces;
with Broca.Debug;
with Broca.Exceptions;
with Broca.Refs;
with Broca.Object;

with CORBA.Impl;

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

   function To_Long_Long is
      new Ada.Unchecked_Conversion
     (CORBA.Unsigned_Long_Long, CORBA.Long_Long);
   function To_Unsigned_Long_Long is
      new Ada.Unchecked_Conversion
     (CORBA.Long_Long, CORBA.Unsigned_Long_Long);
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
   subtype Long_Double_Buf is Octet_Array (1 .. 12);

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
   function To_Long_Double_Buf is
      new Ada.Unchecked_Conversion
     (CORBA.Long_Double, Long_Double_Buf);
   function To_Long_Double is
      new Ada.Unchecked_Conversion
     (Long_Double_Buf, CORBA.Long_Double);

   ----------------------------------
   -- Marshall-by-copy subprograms --
   -- for all elementary types     --
   ----------------------------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Boolean) is
   begin
      Marshall (Buffer, CORBA.Octet'(CORBA.Boolean'Pos (Data)));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Char) is
   begin
      Marshall (Buffer, CORBA.Octet'(CORBA.Char'Pos (Data)));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Wchar) is
   begin
      Align_Marshall_Big_Endian_Copy
        (Buffer,
         (BO_Octet (CORBA.Wchar'Pos (Data) / 256),
          BO_Octet (CORBA.Wchar'Pos (Data) mod 256)),
         2);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Octet) is
   begin
      Align_Marshall_Copy (Buffer, (1 => BO_Octet (Data)), 1);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Unsigned_Short) is
   begin
      Align_Marshall_Big_Endian_Copy
        (Buffer,
         (BO_Octet (Data / 256),
          BO_Octet (Data mod 256)),
         2);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Unsigned_Long) is
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
      Data   : in CORBA.Unsigned_Long_Long) is
   begin
      Align_Marshall_Big_Endian_Copy
        (Buffer,
         (BO_Octet (Data / 256**7),
          BO_Octet ((Data / 256**6) mod 256),
          BO_Octet ((Data / 256**5) mod 256),
          BO_Octet ((Data / 256**4) mod 256),
          BO_Octet ((Data / 256**3) mod 256),
          BO_Octet ((Data / 256**2) mod 256),
          BO_Octet ((Data / 256) mod 256),
          BO_Octet (Data mod 256)),
         8);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Long_Long) is
   begin
      Marshall (Buffer, To_Unsigned_Long_Long (Data));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Long) is
   begin
      Marshall (Buffer, To_Unsigned_Long (Data));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Short) is
   begin
      Marshall (Buffer, To_Unsigned_Short (Data));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Float) is
   begin
      Marshall (Buffer, To_Unsigned_Long (Data));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Double)
   is
      Buf : Double_Buf := To_Double_Buf (Data);

   begin
      Align_Marshall_Host_Endian_Copy (Buffer, Buf, 8);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Long_Double)
   is
      Buf : Long_Double_Buf := To_Long_Double_Buf (Data);

   begin
      Align_Marshall_Host_Endian_Copy (Buffer, Buf, 8);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.String)
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
      Data   : in CORBA.Wide_String)
   is
      Equiv : constant Wide_String :=
        CORBA.To_Wide_String (Data) & Standard.Wide_Character'Val (0);

   begin
      Marshall (Buffer, CORBA.Unsigned_Long'(Equiv'Length));
      for I in Equiv'Range loop
         Marshall (Buffer, CORBA.Wchar'Val (Wide_Character'Pos (Equiv (I))));
      end loop;
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Identifier) is
   begin
      Marshall (Buffer, CORBA.String (Data));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.RepositoryId) is
   begin
      Marshall (Buffer, CORBA.String (Data));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.ValueModifier) is
   begin
      Marshall (Buffer, CORBA.Short (Data));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Visibility) is
   begin
      Marshall (Buffer, CORBA.Short (Data));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Any) is
   begin
      Marshall (Buffer, Get_Type (Data));
      Marshall_From_Any (Buffer, Data);
   end Marshall;

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Any) is
      Data_Type : CORBA.TypeCode.Object := Get_Type (Data);
   begin
      while CORBA.TypeCode.Kind (Data_Type) = Tk_Alias loop
         Data_Type := CORBA.TypeCode.Content_Type (Data_Type);
      end loop;
      case CORBA.TypeCode.Kind (Data_Type) is
         when Tk_Null
           | Tk_Void =>
            null;
         when Tk_Short =>
            declare
               S : CORBA.Short := From_Any (Data);
            begin
               Marshall (Buffer, S);
            end;
         when Tk_Long =>
            declare
               L : CORBA.Long := From_Any (Data);
            begin
               Marshall (Buffer, L);
            end;
         when Tk_Ushort =>
            declare
               Us : CORBA.Unsigned_Short := From_Any (Data);
            begin
               Marshall (Buffer, Us);
            end;
         when Tk_Ulong =>
            declare
               Ul : CORBA.Unsigned_Long := From_Any (Data);
            begin
               Marshall (Buffer, Ul);
            end;
         when Tk_Float =>
            declare
               F : CORBA.Float := From_Any (Data);
            begin
               Marshall (Buffer, F);
            end;
         when Tk_Double =>
            declare
               D : CORBA.Double := From_Any (Data);
            begin
               Marshall (Buffer, D);
            end;
         when Tk_Boolean =>
            declare
               B : CORBA.Boolean := From_Any (Data);
            begin
               Marshall (Buffer, B);
            end;
         when Tk_Char =>
            declare
               C : CORBA.Char := From_Any (Data);
            begin
               Marshall (Buffer, C);
            end;
         when Tk_Octet =>
            declare
               O : CORBA.Octet := From_Any (Data);
            begin
               Marshall (Buffer, O);
            end;
         when Tk_Any =>
            declare
               A : CORBA.Any := From_Any (Data);
            begin
               Marshall (Buffer, A);
            end;
         when Tk_TypeCode =>
            declare
               T : CORBA.TypeCode.Object := From_Any (Data);
            begin
               Marshall (Buffer, T);
            end;
         when Tk_Principal =>
            --  FIXME : to be done
            null;
         when Tk_Objref =>
            Marshall (Buffer, CORBA.Object.From_Any (Data));
         when Tk_Struct =>
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.Get_Aggregate_Count (Data);
               Value : CORBA.Any;
            begin
               for I in 0 .. Nb - 1 loop
                  Value := CORBA.Get_Aggregate_Element
                    (Data,
                     CORBA.TypeCode.Member_Type (Data_Type, I),
                     I);
                  Marshall_From_Any (Buffer, Value);
               end loop;
            end;
         when Tk_Union =>
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.Get_Aggregate_Count (Data);
               Value, Label_Value : CORBA.Any;
            begin
               Label_Value := Get_Aggregate_Element
                 (Data,
                  CORBA.TypeCode.Discriminator_Type (Data_Type),
                  CORBA.Unsigned_Long (0));
               Marshall_From_Any (Buffer, Label_Value);
               for I in 1 .. Nb - 1 loop
                  Value := CORBA.Get_Aggregate_Element
                    (Data,
                     CORBA.TypeCode.Member_Type_With_Label
                     (Data_Type, Label_Value, I),
                     I);
                  Marshall_From_Any (Buffer, Value);
               end loop;
            end;
         when Tk_Enum =>
            declare
               Value : CORBA.Any;
            begin
               Value := CORBA.Get_Aggregate_Element
                 (Data,
                  CORBA.TypeCode.TC_Unsigned_Long,
                  CORBA.Unsigned_Long (0));
               Marshall_From_Any (Buffer, Value);
            end;
         when Tk_String =>
            declare
               S : CORBA.String := From_Any (Data);
            begin
               Marshall (Buffer, S);
            end;
         when Tk_Sequence =>
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.Get_Aggregate_Count (Data);
               Value : CORBA.Any;
            begin
               Value := CORBA.Get_Aggregate_Element
                 (Data,
                  CORBA.TypeCode.TC_Unsigned_Long,
                  CORBA.Unsigned_Long (0));
               Marshall_From_Any (Buffer, Value);
               for I in 1 .. Nb - 1 loop
                  Value := CORBA.Get_Aggregate_Element
                    (Data,
                     CORBA.TypeCode.Content_Type (Data_Type),
                     I);
                  Marshall_From_Any (Buffer, Value);
               end loop;
            end;
         when Tk_Array =>
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.Get_Aggregate_Count (Data);
               Value : CORBA.Any;
            begin
               for I in 0 .. Nb - 1 loop
                  Value := CORBA.Get_Aggregate_Element
                    (Data,
                     CORBA.TypeCode.Content_Type (Data_Type),
                     I);
                  Marshall_From_Any (Buffer, Value);
               end loop;
            end;
         when Tk_Alias =>
            --  we should never reach this point
            raise Program_Error;
         when Tk_Except =>
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.Get_Aggregate_Count (Data);
               Value : CORBA.Any;
            begin
               for I in 0 .. Nb - 1 loop
                  Value := CORBA.Get_Aggregate_Element
                    (Data,
                     CORBA.TypeCode.Member_Type (Data_Type, I),
                     I);
                  Marshall_From_Any (Buffer, Value);
               end loop;
            end;
         when Tk_Longlong =>
            declare
               Ll : CORBA.Long_Long := From_Any (Data);
            begin
               Marshall (Buffer, Ll);
            end;
         when Tk_Ulonglong =>
            declare
               Ull : CORBA.Unsigned_Long_Long := From_Any (Data);
            begin
               Marshall (Buffer, Ull);
            end;
         when Tk_Longdouble =>
            declare
               Ld : CORBA.Long_Double := From_Any (Data);
            begin
               Marshall (Buffer, Ld);
            end;
         when Tk_Widechar =>
            declare
               Wc : CORBA.Wchar := From_Any (Data);
            begin
               Marshall (Buffer, Wc);
            end;
         when Tk_Wstring =>
            declare
               Ws : CORBA.Wide_String := From_Any (Data);
            begin
               Marshall (Buffer, Ws);
            end;
         when Tk_Fixed =>
            --  FIXME : to be done
            null;
         when Tk_Value =>
            --  FIXME : to be done
            null;
         when Tk_Valuebox =>
            --  FIXME : to be done
            null;
         when Tk_Native =>
            --  FIXME : to be done
            null;
         when Tk_Abstract_Interface =>
            --  FIXME : to be done
            null;
      end case;
   end Marshall_From_Any;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.TypeCode.Object) is
      Complex_Buffer : aliased Buffer_Type;
   begin
      case CORBA.TypeCode.Kind (Data) is
         when Tk_Null =>
            Marshall (Buffer, CORBA.Unsigned_Long'(0));
         when Tk_Void =>
            Marshall (Buffer, CORBA.Unsigned_Long'(1));
         when Tk_Short =>
            Marshall (Buffer, CORBA.Unsigned_Long'(2));
         when Tk_Long =>
            Marshall (Buffer, CORBA.Unsigned_Long'(3));
         when Tk_Ushort =>
            Marshall (Buffer, CORBA.Unsigned_Long'(4));
         when Tk_Ulong =>
            Marshall (Buffer, CORBA.Unsigned_Long'(5));
         when Tk_Float =>
            Marshall (Buffer, CORBA.Unsigned_Long'(6));
         when Tk_Double =>
            Marshall (Buffer, CORBA.Unsigned_Long'(7));
         when Tk_Boolean =>
            Marshall (Buffer, CORBA.Unsigned_Long'(8));
         when Tk_Char =>
            Marshall (Buffer, CORBA.Unsigned_Long'(9));
         when Tk_Octet =>
            Marshall (Buffer, CORBA.Unsigned_Long'(10));
         when Tk_Any =>
            Marshall (Buffer, CORBA.Unsigned_Long'(11));
         when Tk_TypeCode =>
            Marshall (Buffer, CORBA.Unsigned_Long'(12));
         when Tk_Principal =>
            Marshall (Buffer, CORBA.Unsigned_Long'(13));
         when Tk_Objref =>
            Marshall (Buffer, CORBA.Unsigned_Long'(14));
            Marshall (Buffer, CORBA.TypeCode.Id (Data));
            Marshall (Buffer, CORBA.TypeCode.Name (Data));
         when Tk_Struct =>
            Marshall (Buffer, CORBA.Unsigned_Long'(15));
            Start_Encapsulation (Complex_Buffer'Access);
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Name (Data));
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.TypeCode.Parameter_Count (Data);
            begin
               Marshall (Complex_Buffer'Access, Nb);
               for I in 0 .. Nb - 1 loop
                  Marshall (Complex_Buffer'Access,
                            CORBA.TypeCode.Member_Name (Data, I));
                  Marshall (Complex_Buffer'Access,
                            CORBA.TypeCode.Member_Type (Data, I));
               end loop;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer'Access));
            Release (Complex_Buffer);
         when Tk_Union =>
            Marshall (Buffer, CORBA.Unsigned_Long'(16));
            Start_Encapsulation (Complex_Buffer'Access);
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Name (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Discriminator_Type (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Default_Index (Data));
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.TypeCode.Parameter_Count (Data);
            begin
               Marshall (Complex_Buffer'Access, Nb);
               for I in 0 .. Nb - 1 loop
                  Marshall_From_Any (Complex_Buffer'Access,
                                     CORBA.TypeCode.Member_Label (Data, I));
                  Marshall (Complex_Buffer'Access,
                            CORBA.TypeCode.Member_Name (Data, I));
                  Marshall (Complex_Buffer'Access,
                            CORBA.TypeCode.Member_Type (Data, I));
               end loop;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer'Access));
            Release (Complex_Buffer);
         when Tk_Enum =>
            Marshall (Buffer, CORBA.Unsigned_Long'(17));
            Start_Encapsulation (Complex_Buffer'Access);
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Name (Data));
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.TypeCode.Parameter_Count (Data);
            begin
               Marshall (Complex_Buffer'Access, Nb);
               for I in 0 .. Nb - 1 loop
                  Marshall (Complex_Buffer'Access,
                            CORBA.TypeCode.Member_Name (Data, I));
               end loop;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer'Access));
            Release (Complex_Buffer);
         when Tk_String =>
            Marshall (Buffer, CORBA.Unsigned_Long'(18));
            Marshall (Buffer, CORBA.TypeCode.Length (Data));
         when Tk_Sequence =>
            Marshall (Buffer, CORBA.Unsigned_Long'(19));
            Start_Encapsulation (Complex_Buffer'Access);
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Content_Type (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Length (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer'Access));
            Release (Complex_Buffer);
         when Tk_Array =>
            Marshall (Buffer, CORBA.Unsigned_Long'(20));
            Start_Encapsulation (Complex_Buffer'Access);
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Content_Type (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Length (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer'Access));
            Release (Complex_Buffer);
         when Tk_Alias =>
            Marshall (Buffer, CORBA.Unsigned_Long'(21));
            Start_Encapsulation (Complex_Buffer'Access);
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Name (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Content_Type (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer'Access));
            Release (Complex_Buffer);
         when Tk_Except =>
            Marshall (Buffer, CORBA.Unsigned_Long'(22));
            Start_Encapsulation (Complex_Buffer'Access);
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Name (Data));
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.TypeCode.Parameter_Count (Data);
            begin
               Marshall (Complex_Buffer'Access, Nb);
               for I in 0 .. Nb - 1 loop
                  Marshall (Complex_Buffer'Access,
                            CORBA.TypeCode.Member_Name (Data, I));
                  Marshall (Complex_Buffer'Access,
                            CORBA.TypeCode.Member_Type (Data, I));
               end loop;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer'Access));
            Release (Complex_Buffer);
         when Tk_Longlong =>
            Marshall (Buffer, CORBA.Unsigned_Long'(23));
         when Tk_Ulonglong =>
            Marshall (Buffer, CORBA.Unsigned_Long'(24));
         when Tk_Longdouble =>
            Marshall (Buffer, CORBA.Unsigned_Long'(25));
         when Tk_Widechar =>
            Marshall (Buffer, CORBA.Unsigned_Long'(26));
         when Tk_Wstring =>
            Marshall (Buffer, CORBA.Unsigned_Long'(27));
            Marshall (Buffer, CORBA.TypeCode.Length (Data));
         when Tk_Fixed =>
            Marshall (Buffer, CORBA.Unsigned_Long'(28));
            Marshall (Buffer, CORBA.TypeCode.Fixed_Digits (Data));
            Marshall (Buffer, CORBA.TypeCode.Fixed_Scale (Data));
         when Tk_Value =>
            Marshall (Buffer, CORBA.Unsigned_Long'(29));
            Start_Encapsulation (Complex_Buffer'Access);
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Name (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Type_Modifier (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Concrete_Base_Type (Data));
            declare
               Nb : CORBA.Unsigned_Long :=
                 CORBA.TypeCode.Parameter_Count (Data);
            begin
               Marshall (Complex_Buffer'Access, Nb);
               for I in 0 .. Nb - 1 loop
                  Marshall (Complex_Buffer'Access,
                            CORBA.TypeCode.Member_Name (Data, I));
                  Marshall (Complex_Buffer'Access,
                            CORBA.TypeCode.Member_Type (Data, I));
                  Marshall (Complex_Buffer'Access,
                            CORBA.TypeCode.Member_Visibility (Data, I));
               end loop;
            end;
            Marshall (Buffer, Encapsulate (Complex_Buffer'Access));
            Release (Complex_Buffer);
         when Tk_Valuebox =>
            Marshall (Buffer, CORBA.Unsigned_Long'(30));
            Start_Encapsulation (Complex_Buffer'Access);
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Name (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Content_Type (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer'Access));
            Release (Complex_Buffer);
         when Tk_Native =>
            Marshall (Buffer, CORBA.Unsigned_Long'(31));
            Start_Encapsulation (Complex_Buffer'Access);
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer'Access));
            Release (Complex_Buffer);
         when Tk_Abstract_Interface =>
            Marshall (Buffer, CORBA.Unsigned_Long'(32));
            Start_Encapsulation (Complex_Buffer'Access);
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Id (Data));
            Marshall (Complex_Buffer'Access,
                      CORBA.TypeCode.Name (Data));
            Marshall (Buffer, Encapsulate (Complex_Buffer'Access));
            Release (Complex_Buffer);
      end case;
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.NamedValue) is
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
      Data   : access CORBA.Wchar) is
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
      Data   : access CORBA.Long_Long) is
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
      Data   : access CORBA.Unsigned_Long_Long) is
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
      Data   : access CORBA.Long_Double) is
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
      Data   : access CORBA.Wide_String) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Identifier) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.RepositoryId) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.ValueModifier) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Visibility) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Any) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall;

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Any) is
   begin
      Marshall (Buffer, Data.all);
   end Marshall_From_Any;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.TypeCode.Object) is
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
     return CORBA.Wchar is
      Octets : constant Octet_Array :=
        Align_Unmarshall_Big_Endian_Copy (Buffer, 2, 2);
   begin
      return CORBA.Wchar'Val
        (CORBA.Unsigned_Long (Octets (Octets'First)) * 256 +
         CORBA.Unsigned_Long (Octets (Octets'First + 1)));
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
     return CORBA.Unsigned_Long_Long is
      Octets : constant Octet_Array :=
        Align_Unmarshall_Big_Endian_Copy (Buffer, 8, 8);
   begin
      return CORBA.Unsigned_Long_Long (Octets (Octets'First)) * 256**7
        + CORBA.Unsigned_Long_Long (Octets (Octets'First + 1)) * 256**6
        + CORBA.Unsigned_Long_Long (Octets (Octets'First + 2)) * 256**5
        + CORBA.Unsigned_Long_Long (Octets (Octets'First + 3)) * 256**4
        + CORBA.Unsigned_Long_Long (Octets (Octets'First + 4)) * 256**3
        + CORBA.Unsigned_Long_Long (Octets (Octets'First + 5)) * 256**2
        + CORBA.Unsigned_Long_Long (Octets (Octets'First + 6)) * 256
        + CORBA.Unsigned_Long_Long (Octets (Octets'First + 7));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Long_Long is
   begin
      return To_Long_Long (Unmarshall (Buffer));
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
     return CORBA.Double is
      Octets : constant Octet_Array :=
        Align_Unmarshall_Host_Endian_Copy (Buffer, 8, 8);
   begin
      return To_Double (Double_Buf (Octets));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Long_Double is
      Octets : constant Octet_Array :=
        Align_Unmarshall_Host_Endian_Copy (Buffer, 12, 8);
   begin
      return To_Long_Double (Long_Double_Buf (Octets));
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
     return CORBA.Wide_String
   is
      Length : constant CORBA.Unsigned_Long
        := Unmarshall (Buffer);
      Equiv  : Wide_String (1 .. Natural (Length));
   begin
      for I in Equiv'Range loop
         Equiv (I) := Wide_Character'Val (CORBA.Wchar'Pos
                                          (Unmarshall (Buffer)));
      end loop;
      return CORBA.To_CORBA_Wide_String
        (Equiv (1 .. Equiv'Length - 1));
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Identifier is
      Result : CORBA.String := Unmarshall (Buffer);
   begin
      return CORBA.Identifier (Result);
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.RepositoryId is
      Result : CORBA.String := Unmarshall (Buffer);
   begin
      return CORBA.RepositoryId (Result);
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.ValueModifier is
      Result : CORBA.Short := Unmarshall (Buffer);
   begin
      return CORBA.ValueModifier (Result);
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Visibility is
      Result : CORBA.Short := Unmarshall (Buffer);
   begin
      return CORBA.Visibility (Result);
   end Unmarshall;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Any is
   begin
      return CORBA.To_Any (CORBA.Short (0));
   end Unmarshall;

   function Unmarshall_To_Any (Buffer : access Buffer_Type;
                               Any_Type : TypeCode.Object)
     return CORBA.Any is
   begin
      return CORBA.To_Any (CORBA.Short (0));
   end Unmarshall_To_Any;

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.TypeCode.Object is
      Nb : CORBA.Unsigned_Long := Unmarshall (Buffer);
      Result : CORBA.TypeCode.Object;
   begin
      case Nb is
         when 0 =>
            Result := CORBA.TypeCode.TC_Null;
         when 1 =>
            Result := CORBA.TypeCode.TC_Void;
         when 2 =>
            Result := CORBA.TypeCode.TC_Short;
         when 3 =>
            Result := CORBA.TypeCode.TC_Long;
         when 4 =>
            Result := CORBA.TypeCode.TC_Unsigned_Short;
         when 5 =>
            Result := CORBA.TypeCode.TC_Unsigned_Long;
         when 6 =>
            Result := CORBA.TypeCode.TC_Float;
         when 7 =>
            Result := CORBA.TypeCode.TC_Double;
         when 8 =>
            Result := CORBA.TypeCode.TC_Boolean;
         when 9 =>
            Result := CORBA.TypeCode.TC_Char;
         when 10 =>
            Result := CORBA.TypeCode.TC_Octet;
         when 11 =>
            Result := CORBA.TypeCode.TC_Any;
         when 12 =>
            Result := CORBA.TypeCode.TC_TypeCode;
         when 13 =>
            Result := CORBA.TypeCode.TC_Principal;
         when 14 =>
            Result := CORBA.TypeCode.TC_ObjRef;
            declare
               Id : CORBA.String := Unmarshall (Buffer);
               Name : CORBA.String := Unmarshall (Buffer);
            begin
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;
         when 15 =>
            Result := CORBA.TypeCode.TC_Struct;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : CORBA.String;
               Nb : CORBA.Unsigned_Long;
               Member_Type : CORBA.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id   := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Nb   := Unmarshall (Complex_Buffer'Access);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               for I in 0 .. Nb - 1 loop
                  Member_Name := Unmarshall (Complex_Buffer'Access);
                  Member_Type := Unmarshall (Complex_Buffer'Access);
                  CORBA.TypeCode.Add_Parameter
                    (Result, To_Any (Member_Name));
                  CORBA.TypeCode.Add_Parameter
                    (Result, To_Any (Member_Type));
               end loop;
            end;
         when 16 =>
            Result := CORBA.TypeCode.TC_Union;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : CORBA.String;
               Nb, Default_Index : CORBA.Unsigned_Long;
               Discriminator_Type, Member_Type : CORBA.TypeCode.Object;
               Member_Label : CORBA.Any;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Discriminator_Type := Unmarshall (Complex_Buffer'Access);
               Default_Index := Unmarshall (Complex_Buffer'Access);
               Nb := Unmarshall (Complex_Buffer'Access);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Discriminator_Type));
               for I in 0 .. Nb - 1 loop
                  Member_Label := Unmarshall_To_Any
                    (Complex_Buffer'Access,
                     Discriminator_Type);
                  Member_Name := Unmarshall (Complex_Buffer'Access);
                  Member_Type := Unmarshall (Complex_Buffer'Access);
                  CORBA.TypeCode.Add_Parameter
                    (Result, Member_Label);
                  CORBA.TypeCode.Add_Parameter
                    (Result, To_Any (Member_Name));
                  CORBA.TypeCode.Add_Parameter
                    (Result, To_Any (Member_Type));
               end loop;
            end;
         when 17 =>
            Result := CORBA.TypeCode.TC_Enum;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : CORBA.String;
               Nb : CORBA.Unsigned_Long;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Nb := Unmarshall (Complex_Buffer'Access);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               for I in 0 .. Nb - 1 loop
                  Member_Name := Unmarshall (Complex_Buffer'Access);
                  CORBA.TypeCode.Add_Parameter
                    (Result, To_Any (Member_Name));
               end loop;
            end;
         when 18 =>
            Result := CORBA.TypeCode.TC_String;
            declare
               Length : CORBA.Unsigned_Long;
            begin
               Length := Unmarshall (Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;
         when 19 =>
            Result := CORBA.TypeCode.TC_Sequence;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Length : CORBA.Unsigned_Long;
               Content_Type : CORBA.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Content_Type := Unmarshall (Complex_Buffer'Access);
               Length := Unmarshall (Complex_Buffer'Access);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;
         when 20 =>
            Result := CORBA.TypeCode.TC_Array;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Length : CORBA.Unsigned_Long;
               Content_Type : CORBA.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Content_Type := Unmarshall (Complex_Buffer'Access);
               Length := Unmarshall (Complex_Buffer'Access);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;
         when 21 =>
            Result := CORBA.TypeCode.TC_Alias;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : CORBA.String;
               Content_Type : CORBA.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Content_Type := Unmarshall (Complex_Buffer'Access);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;
         when 22 =>
            Result := CORBA.TypeCode.TC_Except;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : CORBA.String;
               Nb : CORBA.Unsigned_Long;
               Member_Type : CORBA.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Nb := Unmarshall (Complex_Buffer'Access);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               for I in 0 .. Nb - 1 loop
                  Member_Name := Unmarshall (Complex_Buffer'Access);
                  Member_Type := Unmarshall (Complex_Buffer'Access);
                  CORBA.TypeCode.Add_Parameter
                    (Result, To_Any (Member_Name));
                  CORBA.TypeCode.Add_Parameter
                    (Result, To_Any (Member_Type));
               end loop;
            end;
         when 23 =>
            Result := CORBA.TypeCode.TC_Long_Long;
         when 24 =>
            Result := CORBA.TypeCode.TC_Unsigned_Long_Long;
         when 25 =>
            Result := CORBA.TypeCode.TC_Long_Double;
         when 26 =>
            Result := CORBA.TypeCode.TC_Wchar;
         when 27 =>
            Result := CORBA.TypeCode.TC_Wide_String;
            declare
               Length : CORBA.Unsigned_Long;
            begin
               Length := Unmarshall (Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Length));
            end;
         when 28 =>
            Result := CORBA.TypeCode.TC_Fixed;
            declare
               Fixed_Digits : CORBA.Unsigned_Short;
               Fixed_Scale : CORBA.Short;
            begin
               Fixed_Digits := Unmarshall (Buffer);
               Fixed_Scale := Unmarshall (Buffer);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Fixed_Digits));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Fixed_Scale));
            end;


         when 29 =>
            Result := CORBA.TypeCode.TC_Value;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name, Member_Name : CORBA.String;
               Type_Modifier, Visibility : CORBA.Short;
               Nb : CORBA.Unsigned_Long;
               Concrete_Base_Type, Member_Type : CORBA.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Type_Modifier := Unmarshall (Complex_Buffer'Access);
               Concrete_Base_Type := Unmarshall (Complex_Buffer'Access);
               Nb := Unmarshall (Complex_Buffer'Access);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Type_Modifier));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Concrete_Base_Type));
               for I in 0 .. Nb - 1 loop
                  Member_Name := Unmarshall (Complex_Buffer'Access);
                  Member_Type := Unmarshall (Complex_Buffer'Access);
                  Visibility := Unmarshall (Complex_Buffer'Access);
                  CORBA.TypeCode.Add_Parameter
                    (Result, To_Any (Member_Name));
                  CORBA.TypeCode.Add_Parameter
                    (Result, To_Any (Member_Type));
                  CORBA.TypeCode.Add_Parameter
                    (Result, To_Any (Visibility));
               end loop;
            end;
         when 30 =>
            Result := CORBA.TypeCode.TC_Valuebox;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : CORBA.String;
               Content_Type : CORBA.TypeCode.Object;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               Content_Type := Unmarshall (Complex_Buffer'Access);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Content_Type));
            end;
         when 31 =>
            Result := CORBA.TypeCode.TC_Native;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : CORBA.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;
         when 32 =>
            Result := CORBA.TypeCode.TC_Abstract_Interface;
            declare
               Complex_Encap : aliased Encapsulation
                 := Unmarshall (Buffer);
               Complex_Buffer : aliased Buffer_Type;
               Id, Name : CORBA.String;
            begin
               Decapsulate (Complex_Encap'Access, Complex_Buffer'Access);
               Id := Unmarshall (Complex_Buffer'Access);
               Name := Unmarshall (Complex_Buffer'Access);
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Name));
               CORBA.TypeCode.Add_Parameter
                 (Result, To_Any (Id));
            end;
         when others =>
            raise Constraint_Error;
      end case;
      return Result;
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type;
      Name : CORBA.Identifier;
      Flags : CORBA.Flags)
      return CORBA.NamedValue
   is
      Result : NamedValue;
   begin
      Result.Name := Name;
      Result.Argument := Unmarshall (Buffer);
      Result.Arg_Modes := Flags;
      return Result;
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
     return Octet_Array is
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

   ----------------
   --  Marshall  --
   ----------------
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data : in CORBA.Object.Ref'Class) is
   begin
      if CORBA.Object.Is_Nil (Data) then
         Broca.Exceptions.Raise_Marshal;
      end if;
      Broca.Refs.Marshall
        (Buffer,
         Broca.Refs.Ref_Type'Class (Data.Ptr.all));
   end Marshall;

   -----------------
   --  Unmarshall --
   -----------------
   procedure Unmarshall (Buffer : access Buffer_Type;
                         Data : in out CORBA.Object.Ref'Class) is
      Obj : constant CORBA.Impl.Object_Ptr
        := new Broca.Object.Object_Type;
   begin
      Broca.Object.Unmarshall
        (Buffer, Broca.Object.Object_Type (Obj.all));
      CORBA.Object.Set (Data, Obj);
   end Unmarshall;

   -----------------
   --  Unmarshall --
   -----------------
   function Unmarshall (Buffer : access Buffer_Type)
                        return CORBA.Object.Ref'Class is
      New_Ref : CORBA.Object.Ref;
   begin
      Unmarshall (Buffer, New_Ref);
      return New_Ref;
   end Unmarshall;


   ------------------
   --  Fixed_Point --
   ------------------
   package body Fixed_Point is

      Flag : constant Natural :=
        Broca.Debug.Is_Active ("broca.cdr.fixed_point");
      procedure O is new Broca.Debug.Output (Flag);

      subtype BO_Octet is Broca.Opaque.Octet;

      use Interfaces;

      Fixed_Positive_Zero : constant BO_Octet
        := 16#C#;
      Fixed_Negative : constant BO_Octet
        := 16#D#;

      procedure Marshall
        (Buffer : access Buffer_Type;
         Data   : access F) is
      begin
         Marshall (Buffer, Data.all);
      end Marshall;

      procedure Marshall
        (Buffer : access Buffer_Type;
         Data   : in F)
      is
         N_Digits : Integer
           := 0;

         Val : F := Data;
      begin

         loop
            N_Digits := N_Digits + 1;
            Val := Val * 0.1;
            exit when Val = 0.0;
         end loop;

         declare
            Octets : Octet_Array
              (0 .. Index_Type ((N_Digits + 2) / 2 - 1))
              := (others => 0);
            --  The size of the representation is
            --  at least 1, plus 1 nibble for the sign.

            Offset : Integer;
            Bias : F;
         begin
            if N_Digits mod 2 /= 0 then
               Offset := 0;
            else
               Offset := 1;
            end if;

            Val := Data;
            Bias := F (10.0 ** (N_Digits - F'Scale + 1));

            for I in Offset .. Offset + N_Digits loop
               declare
                  Digit : constant BO_Octet
                    := BO_Octet (Val / Bias);
               begin
                  if I mod 2 = 0 then
                     Octets (Index_Type (I / 2)) := Digit * 16;
                  else
                     Octets (Index_Type (I / 2))
                       := Octets (Index_Type (I / 2)) + Digit;
                  end if;
                  Val := Val - F (Digit) * Bias;
                  Bias := 0.1 * Bias;
               end;
            end loop;
            if Data >= 0.0 then
               Octets (Octets'Last) :=
                 Octets (Octets'Last) + Fixed_Positive_Zero;
            else
               Octets (Octets'Last) :=
                 Octets (Octets'Last) + Fixed_Negative;
            end if;
            Align_Marshall_Copy (Buffer, Octets, 1);
         end;
      end Marshall;

      function Unmarshall
        (Buffer : access Buffer_Type)
         return F
      is
         use CORBA;

         O : CORBA.Octet;
         Result : F := 0.0;
      begin
         loop
            O := Unmarshall (Buffer);
            if O / 16 > 9
              or else
              (O mod 16 > 9
               and then O mod 16 /= CORBA.Octet (Fixed_Positive_Zero)
               and then O mod 16 /= CORBA.Octet (Fixed_Negative))
            then
               Broca.Exceptions.Raise_Marshal;
            end if;

            Result := Result * 10 + F (O / 16) * F'Delta;
            if O mod 16 < 10 then
               Result := Result * 10 + F (O mod 16) * F'Delta;
            else
               if O mod 16 = CORBA.Octet (Fixed_Negative) then
                  Result := -Result;
               end if;
               exit;
            end if;
         end loop;

         return Result;
      end Unmarshall;

   end Fixed_Point;

end Broca.CDR;
