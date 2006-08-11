------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           C O R B A . I D L _ S E Q U E N C E S . H E L P E R            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Initialization;
with PolyORB.Sequences.Unbounded.CORBA_Helper;
with PolyORB.Utils.Strings;

package body CORBA.IDL_SEQUENCES.Helper is

   procedure Deferred_Initialization;

   package IDL_SEQUENCE_Any_Helper is new IDL_SEQUENCE_Any.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Boolean_Helper is new IDL_SEQUENCE_Boolean.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Char_Helper is new IDL_SEQUENCE_Char.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Double_Helper is new IDL_SEQUENCE_Double.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Float_Helper is new IDL_SEQUENCE_Float.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Long_Double_Helper is
     new IDL_SEQUENCE_Long_Double.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Long_Helper is new IDL_SEQUENCE_Long.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Long_Long_Helper is
     new IDL_SEQUENCE_Long_Long.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Octet_Helper is new IDL_SEQUENCE_Octet.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Short_Helper is new IDL_SEQUENCE_Short.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_String_Helper is new IDL_SEQUENCE_String.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Unsigned_Long_Helper is
     new IDL_SEQUENCE_Unsigned_Long.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Unsigned_Long_Long_Helper is
     new IDL_SEQUENCE_Unsigned_Long_Long.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Unsigned_Short_Helper is
     new IDL_SEQUENCE_Unsigned_Short.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Wide_Char_Helper is
     new IDL_SEQUENCE_Wide_Char.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   package IDL_SEQUENCE_Wide_String_Helper is
     new IDL_SEQUENCE_Wide_String.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any,
      Element_Wrap     => CORBA.Wrap);

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      TC_IDL_SEQUENCE_Any :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Any, 0);
      IDL_SEQUENCE_Any_Helper.Initialize
        (Element_TC  => CORBA.TC_Any,
         Sequence_TC => TC_IDL_SEQUENCE_Any);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("AnySeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/AnySeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_AnySeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_AnySeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_AnySeq, CORBA.To_Any (TC_IDL_SEQUENCE_Any));
      end;

      TC_IDL_SEQUENCE_Boolean :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Boolean, 0);
      IDL_SEQUENCE_Boolean_Helper.Initialize
        (Element_TC  => CORBA.TC_Boolean,
         Sequence_TC => TC_IDL_SEQUENCE_Boolean);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("BooleanSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/BooleanSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_BooleanSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_BooleanSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_BooleanSeq, CORBA.To_Any (TC_IDL_SEQUENCE_Boolean));
      end;

      TC_IDL_SEQUENCE_Char :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Char, 0);
      IDL_SEQUENCE_Char_Helper.Initialize
        (Element_TC  => CORBA.TC_Char,
         Sequence_TC => TC_IDL_SEQUENCE_Char);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("CharSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/CharSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CharSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CharSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_CharSeq, CORBA.To_Any (Helper.TC_IDL_SEQUENCE_Char));
      end;

      TC_IDL_SEQUENCE_Wide_Char :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Wchar, 0);
      IDL_SEQUENCE_Wide_Char_Helper.Initialize
        (Element_TC  => CORBA.TC_Wchar,
         Sequence_TC => TC_IDL_SEQUENCE_Wide_Char);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("WCharSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/WCharSeq:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_WCharSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_WCharSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_WCharSeq, CORBA.To_Any (TC_IDL_SEQUENCE_Wide_Char));
      end;

      TC_IDL_SEQUENCE_Octet :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Octet, 0);
      IDL_SEQUENCE_Octet_Helper.Initialize
        (Element_TC  => CORBA.TC_Octet,
         Sequence_TC => TC_IDL_SEQUENCE_Octet);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("OctetSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/OctetSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_OctetSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_OctetSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_OctetSeq, CORBA.To_Any (Helper.TC_IDL_SEQUENCE_Octet));
      end;

      TC_IDL_SEQUENCE_Short :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Short, 0);
      IDL_SEQUENCE_Short_Helper.Initialize
        (Element_TC  => CORBA.TC_Short,
         Sequence_TC => TC_IDL_SEQUENCE_Short);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("ShortSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/ShortSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ShortSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ShortSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ShortSeq, CORBA.To_Any (TC_IDL_SEQUENCE_Short));
      end;

      TC_IDL_SEQUENCE_Unsigned_Short :=
        CORBA.TypeCode.Internals.Build_Sequence_TC
        (CORBA.TC_Unsigned_Short, 0);
      IDL_SEQUENCE_Unsigned_Short_Helper.Initialize
        (Element_TC  => CORBA.TC_Unsigned_Short,
         Sequence_TC => TC_IDL_SEQUENCE_Unsigned_Short);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("UShortSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/UShortSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_UShortSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_UShortSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_UShortSeq, CORBA.To_Any (TC_IDL_SEQUENCE_Unsigned_Short));
      end;

      TC_IDL_SEQUENCE_Long :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Long, 0);
      IDL_SEQUENCE_Long_Helper.Initialize
        (Element_TC  => CORBA.TC_Long,
         Sequence_TC => TC_IDL_SEQUENCE_Long);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("LongSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/LongSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_LongSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_LongSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_LongSeq, CORBA.To_Any (TC_IDL_SEQUENCE_Long));
      end;

      TC_IDL_SEQUENCE_Unsigned_Long :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Unsigned_Long, 0);
      IDL_SEQUENCE_Unsigned_Long_Helper.Initialize
        (Element_TC  => CORBA.TC_Unsigned_Long,
         Sequence_TC => TC_IDL_SEQUENCE_Unsigned_Long);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("ULongSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/ULongSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ULongSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ULongSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ULongSeq, CORBA.To_Any (TC_IDL_SEQUENCE_Unsigned_Long));
      end;

      TC_IDL_SEQUENCE_Long_Long :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Long_Long, 0);
      IDL_SEQUENCE_Long_Long_Helper.Initialize
        (Element_TC  => CORBA.TC_Long_Long,
         Sequence_TC => TC_IDL_SEQUENCE_Long_Long);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("LongLongSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/LongLongSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_LongLongSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_LongLongSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_LongLongSeq, CORBA.To_Any (TC_IDL_SEQUENCE_Long_Long));
      end;

      TC_IDL_SEQUENCE_Unsigned_Long_Long :=
        CORBA.TypeCode.Internals.Build_Sequence_TC
          (CORBA.TC_Unsigned_Long_Long, 0);
      IDL_SEQUENCE_Unsigned_Long_Long_Helper.Initialize
        (Element_TC  => CORBA.TC_Unsigned_Long_Long,
         Sequence_TC => TC_IDL_SEQUENCE_Unsigned_Long_Long);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("ULongLongSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/ULongLongSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ULongLongSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ULongLongSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ULongLongSeq,
            CORBA.To_Any (TC_IDL_SEQUENCE_Unsigned_Long_Long));
      end;

      TC_IDL_SEQUENCE_Float :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Float, 0);
      IDL_SEQUENCE_Float_Helper.Initialize
        (Element_TC  => CORBA.TC_Float,
         Sequence_TC => TC_IDL_SEQUENCE_Float);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("FloatSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/FloatSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_FloatSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_FloatSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_FloatSeq, CORBA.To_Any (TC_IDL_SEQUENCE_Float));
      end;

      TC_IDL_SEQUENCE_Double :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Double, 0);
      IDL_SEQUENCE_Double_Helper.Initialize
        (Element_TC  => CORBA.TC_Double,
         Sequence_TC => TC_IDL_SEQUENCE_Double);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("DoubleSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/DoubleSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_DoubleSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_DoubleSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_DoubleSeq, CORBA.To_Any (TC_IDL_SEQUENCE_Double));
      end;

      TC_IDL_SEQUENCE_Long_Double :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Long_Double, 0);
      IDL_SEQUENCE_Long_Double_Helper.Initialize
        (Element_TC  => CORBA.TC_Long_Double,
         Sequence_TC => TC_IDL_SEQUENCE_Long_Double);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("LongDoubleSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/LongDoubleSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_LongDoubleSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_LongDoubleSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_LongDoubleSeq, CORBA.To_Any (TC_IDL_SEQUENCE_Long_Double));
      end;

      TC_IDL_SEQUENCE_String :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_String, 0);
      IDL_SEQUENCE_String_Helper.Initialize
        (Element_TC  => CORBA.TC_String,
         Sequence_TC => TC_IDL_SEQUENCE_String);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("StringSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/StringSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_StringSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_StringSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_StringSeq, CORBA.To_Any (TC_IDL_SEQUENCE_String));
      end;

      TC_IDL_SEQUENCE_Wide_String :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Wide_String, 0);
      IDL_SEQUENCE_Wide_String_Helper.Initialize
        (Element_TC  => CORBA.TC_Wide_String,
         Sequence_TC => TC_IDL_SEQUENCE_Wide_String);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("WStringSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/WStringSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_WStringSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_WStringSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_WStringSeq, CORBA.To_Any (TC_IDL_SEQUENCE_Wide_String));
      end;
   end Deferred_Initialization;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Any.Sequence
      renames IDL_SEQUENCE_Any_Helper.From_Any;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Boolean.Sequence
      renames IDL_SEQUENCE_Boolean_Helper.From_Any;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Char.Sequence
      renames IDL_SEQUENCE_Char_Helper.From_Any;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Double.Sequence
      renames IDL_SEQUENCE_Double_Helper.From_Any;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Float.Sequence
      renames IDL_SEQUENCE_Float_Helper.From_Any;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Long.Sequence
      renames IDL_SEQUENCE_Long_Helper.From_Any;

   function From_Any
     (Item : CORBA.Any)
      return IDL_SEQUENCE_Long_Double.Sequence
      renames IDL_SEQUENCE_Long_Double_Helper.From_Any;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Long_Long.Sequence
      renames IDL_SEQUENCE_Long_Long_Helper.From_Any;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Octet.Sequence
      renames IDL_SEQUENCE_Octet_Helper.From_Any;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Short.Sequence
      renames IDL_SEQUENCE_Short_Helper.From_Any;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_String.Sequence
      renames IDL_SEQUENCE_String_Helper.From_Any;

   function From_Any
     (Item : CORBA.Any)
      return IDL_SEQUENCE_Unsigned_Long.Sequence
      renames IDL_SEQUENCE_Unsigned_Long_Helper.From_Any;

   function From_Any
     (Item : CORBA.Any)
      return IDL_SEQUENCE_Unsigned_Long_Long.Sequence
      renames IDL_SEQUENCE_Unsigned_Long_Long_Helper.From_Any;

   function From_Any
     (Item : CORBA.Any)
      return IDL_SEQUENCE_Unsigned_Short.Sequence
      renames IDL_SEQUENCE_Unsigned_Short_Helper.From_Any;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Wide_Char.Sequence
      renames IDL_SEQUENCE_Wide_Char_Helper.From_Any;

   function From_Any
     (Item : CORBA.Any)
      return IDL_SEQUENCE_Wide_String.Sequence
      renames IDL_SEQUENCE_Wide_String_Helper.From_Any;

   function From_Any (Item : CORBA.Any) return AnySeq is
      Result : constant IDL_SEQUENCE_Any.Sequence := From_Any (Item);

   begin
      return AnySeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return BooleanSeq is
      Result : constant IDL_SEQUENCE_Boolean.Sequence := From_Any (Item);

   begin
      return BooleanSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return CharSeq is
      Result : constant IDL_SEQUENCE_Char.Sequence := From_Any (Item);

   begin
      return CharSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return DoubleSeq is
      Result : constant IDL_SEQUENCE_Double.Sequence := From_Any (Item);

   begin
      return DoubleSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return FloatSeq is
      Result : constant IDL_SEQUENCE_Float.Sequence := From_Any (Item);

   begin
      return FloatSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return LongDoubleSeq is
      Result : constant IDL_SEQUENCE_Long_Double.Sequence := From_Any (Item);

   begin
      return LongDoubleSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return LongSeq is
      Result : constant IDL_SEQUENCE_Long.Sequence := From_Any (Item);

   begin
      return LongSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return LongLongSeq is
      Result : constant IDL_SEQUENCE_Long_Long.Sequence := From_Any (Item);

   begin
      return LongLongSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return OctetSeq is
      Result : constant IDL_SEQUENCE_Octet.Sequence := From_Any (Item);

   begin
      return OctetSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return ShortSeq is
      Result : constant IDL_SEQUENCE_Short.Sequence := From_Any (Item);

   begin
      return ShortSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return StringSeq is
      Result : constant IDL_SEQUENCE_String.Sequence := From_Any (Item);

   begin
      return StringSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return ULongLongSeq is
      Result : constant IDL_SEQUENCE_Unsigned_Long_Long.Sequence
        := From_Any (Item);

   begin
      return ULongLongSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return ULongSeq is
      Result : constant IDL_SEQUENCE_Unsigned_Long.Sequence := From_Any (Item);

   begin
      return ULongSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return UShortSeq is
      Result : constant IDL_SEQUENCE_Unsigned_Short.Sequence
        := From_Any (Item);

   begin
      return UShortSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return WCharSeq is
      Result : constant IDL_SEQUENCE_Wide_Char.Sequence := From_Any (Item);

   begin
      return WCharSeq (Result);
   end From_Any;

   function From_Any (Item : CORBA.Any) return WStringSeq is
      Result : constant IDL_SEQUENCE_Wide_String.Sequence := From_Any (Item);

   begin
      return WStringSeq (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : IDL_SEQUENCE_Any.Sequence) return CORBA.Any
      renames IDL_SEQUENCE_Any_Helper.To_Any;

   function To_Any (Item : IDL_SEQUENCE_Boolean.Sequence) return CORBA.Any
      renames IDL_SEQUENCE_Boolean_Helper.To_Any;

   function To_Any (Item : IDL_SEQUENCE_Char.Sequence) return CORBA.Any
      renames IDL_SEQUENCE_Char_Helper.To_Any;

   function To_Any (Item : IDL_SEQUENCE_Double.Sequence) return CORBA.Any
    renames IDL_SEQUENCE_Double_Helper.To_Any;

   function To_Any (Item : IDL_SEQUENCE_Float.Sequence) return CORBA.Any
      renames IDL_SEQUENCE_Float_Helper.To_Any;

   function To_Any (Item : IDL_SEQUENCE_Long.Sequence) return CORBA.Any
      renames IDL_SEQUENCE_Long_Helper.To_Any;

   function To_Any (Item : IDL_SEQUENCE_Long_Double.Sequence) return CORBA.Any
      renames IDL_SEQUENCE_Long_Double_Helper.To_Any;

   function To_Any (Item : IDL_SEQUENCE_Long_Long.Sequence) return CORBA.Any
      renames IDL_SEQUENCE_Long_Long_Helper.To_Any;

   function To_Any (Item : IDL_SEQUENCE_Octet.Sequence) return CORBA.Any
      renames IDL_SEQUENCE_Octet_Helper.To_Any;

   function To_Any (Item : IDL_SEQUENCE_Short.Sequence) return CORBA.Any
      renames IDL_SEQUENCE_Short_Helper.To_Any;

   function To_Any (Item : IDL_SEQUENCE_String.Sequence) return CORBA.Any
      renames IDL_SEQUENCE_String_Helper.To_Any;

   function To_Any
     (Item : IDL_SEQUENCE_Unsigned_Long.Sequence)
      return CORBA.Any
      renames IDL_SEQUENCE_Unsigned_Long_Helper.To_Any;

   function To_Any
     (Item : IDL_SEQUENCE_Unsigned_Long_Long.Sequence)
      return CORBA.Any
      renames IDL_SEQUENCE_Unsigned_Long_Long_Helper.To_Any;

   function To_Any
     (Item : IDL_SEQUENCE_Unsigned_Short.Sequence)
      return CORBA.Any
      renames IDL_SEQUENCE_Unsigned_Short_Helper.To_Any;

   function To_Any (Item : IDL_SEQUENCE_Wide_Char.Sequence) return CORBA.Any
      renames IDL_SEQUENCE_Wide_Char_Helper.To_Any;

   function To_Any (Item : IDL_SEQUENCE_Wide_String.Sequence) return CORBA.Any
      renames IDL_SEQUENCE_Wide_String_Helper.To_Any;

   function To_Any (Item : AnySeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Any.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_AnySeq);
      return Result;
   end To_Any;

   function To_Any (Item : BooleanSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Boolean.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_BooleanSeq);
      return Result;
   end To_Any;

   function To_Any (Item : CharSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Char.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_CharSeq);
      return Result;
   end To_Any;

   function To_Any (Item : DoubleSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Double.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_DoubleSeq);
      return Result;
   end To_Any;

   function To_Any (Item : FloatSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Float.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_FloatSeq);
      return Result;
   end To_Any;

   function To_Any (Item : LongDoubleSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Long_Double.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_LongDoubleSeq);
      return Result;
   end To_Any;

   function To_Any (Item : LongSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Long.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_LongSeq);
      return Result;
   end To_Any;

   function To_Any (Item : LongLongSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Long_Long.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_LongLongSeq);
      return Result;
   end To_Any;

   function To_Any (Item : OctetSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Octet.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_OctetSeq);
      return Result;
   end To_Any;

   function To_Any (Item : ShortSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Short.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_ShortSeq);
      return Result;
   end To_Any;

   function To_Any (Item : StringSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_String.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_StringSeq);
      return Result;
   end To_Any;

   function To_Any (Item : ULongLongSeq) return CORBA.Any is
      Result : CORBA.Any
        := To_Any (IDL_SEQUENCE_Unsigned_Long_Long.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_ULongLongSeq);
      return Result;
   end To_Any;

   function To_Any (Item : ULongSeq) return CORBA.Any is
      Result : CORBA.Any
        := To_Any (IDL_SEQUENCE_Unsigned_Long.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_ULongSeq);
      return Result;
   end To_Any;

   function To_Any (Item : UShortSeq) return CORBA.Any is
      Result : CORBA.Any
        := To_Any (IDL_SEQUENCE_Unsigned_Short.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_UShortSeq);
      return Result;
   end To_Any;

   function To_Any (Item : WCharSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Wide_Char.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_WCharSeq);
      return Result;
   end To_Any;

   function To_Any (Item : WStringSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Wide_String.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_WStringSeq);
      return Result;
   end To_Any;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"CORBA.IDL_SEQUENCES.Helper",
          Conflicts => Empty,
          Depends   => +"any",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access,
          Shutdown  => null));
   end;
end CORBA.IDL_SEQUENCES.Helper;
