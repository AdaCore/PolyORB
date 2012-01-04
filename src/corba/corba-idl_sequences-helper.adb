------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           C O R B A . I D L _ S E Q U E N C E S . H E L P E R            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;
with PolyORB.Sequences.Unbounded.CORBA_Helper;
pragma Elaborate_All (PolyORB.Sequences.Unbounded.CORBA_Helper);
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
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Any);

      IDL_SEQUENCE_Any_Helper.Initialize
        (Element_TC  => CORBA.TC_Any,
         Sequence_TC => TC_IDL_SEQUENCE_Any);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("AnySeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/AnySeq:1.0");
      begin
         TC_AnySeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_Any);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_AnySeq);
      end;

      TC_IDL_SEQUENCE_Boolean :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Boolean, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Boolean);

      IDL_SEQUENCE_Boolean_Helper.Initialize
        (Element_TC  => CORBA.TC_Boolean,
         Sequence_TC => TC_IDL_SEQUENCE_Boolean);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("BooleanSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/BooleanSeq:1.0");
      begin
         TC_BooleanSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_Boolean);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_BooleanSeq);
      end;

      TC_IDL_SEQUENCE_Char :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Char, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Char);

      IDL_SEQUENCE_Char_Helper.Initialize
        (Element_TC  => CORBA.TC_Char,
         Sequence_TC => TC_IDL_SEQUENCE_Char);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("CharSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/CharSeq:1.0");
      begin
         TC_CharSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_Char);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_CharSeq);
      end;

      TC_IDL_SEQUENCE_Wide_Char :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Wchar, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Wide_Char);

      IDL_SEQUENCE_Wide_Char_Helper.Initialize
        (Element_TC  => CORBA.TC_Wchar,
         Sequence_TC => TC_IDL_SEQUENCE_Wide_Char);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("WCharSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/WCharSeq:1.0");
      begin
         TC_WCharSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_Wide_Char);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_WCharSeq);
      end;

      TC_IDL_SEQUENCE_Octet :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Octet, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Octet);

      IDL_SEQUENCE_Octet_Helper.Initialize
        (Element_TC  => CORBA.TC_Octet,
         Sequence_TC => TC_IDL_SEQUENCE_Octet);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("OctetSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/OctetSeq:1.0");
      begin
         TC_OctetSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_Octet);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_OctetSeq);
      end;

      TC_IDL_SEQUENCE_Short :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Short, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Short);

      IDL_SEQUENCE_Short_Helper.Initialize
        (Element_TC  => CORBA.TC_Short,
         Sequence_TC => TC_IDL_SEQUENCE_Short);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ShortSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/ShortSeq:1.0");
      begin
         TC_ShortSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_Short);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_ShortSeq);
      end;

      TC_IDL_SEQUENCE_Unsigned_Short :=
        CORBA.TypeCode.Internals.Build_Sequence_TC
        (CORBA.TC_Unsigned_Short, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Unsigned_Short);

      IDL_SEQUENCE_Unsigned_Short_Helper.Initialize
        (Element_TC  => CORBA.TC_Unsigned_Short,
         Sequence_TC => TC_IDL_SEQUENCE_Unsigned_Short);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("UShortSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/UShortSeq:1.0");
      begin
         TC_UShortSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_Unsigned_Short);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_UShortSeq);
      end;

      TC_IDL_SEQUENCE_Long :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Long, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Long);

      IDL_SEQUENCE_Long_Helper.Initialize
        (Element_TC  => CORBA.TC_Long,
         Sequence_TC => TC_IDL_SEQUENCE_Long);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("LongSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/LongSeq:1.0");
      begin
         TC_LongSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_Long);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_LongSeq);
      end;

      TC_IDL_SEQUENCE_Unsigned_Long :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Unsigned_Long, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Unsigned_Long);

      IDL_SEQUENCE_Unsigned_Long_Helper.Initialize
        (Element_TC  => CORBA.TC_Unsigned_Long,
         Sequence_TC => TC_IDL_SEQUENCE_Unsigned_Long);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ULongSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/ULongSeq:1.0");
      begin
         TC_ULongSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_Unsigned_Long);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_ULongSeq);
      end;

      TC_IDL_SEQUENCE_Long_Long :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Long_Long, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Long_Long);

      IDL_SEQUENCE_Long_Long_Helper.Initialize
        (Element_TC  => CORBA.TC_Long_Long,
         Sequence_TC => TC_IDL_SEQUENCE_Long_Long);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("LongLongSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/LongLongSeq:1.0");
      begin
         TC_LongLongSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_Long_Long);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_LongLongSeq);
      end;

      TC_IDL_SEQUENCE_Unsigned_Long_Long :=
        CORBA.TypeCode.Internals.Build_Sequence_TC
          (CORBA.TC_Unsigned_Long_Long, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Unsigned_Long_Long);

      IDL_SEQUENCE_Unsigned_Long_Long_Helper.Initialize
        (Element_TC  => CORBA.TC_Unsigned_Long_Long,
         Sequence_TC => TC_IDL_SEQUENCE_Unsigned_Long_Long);

      declare
         Name : constant CORBA.String :=
                  CORBA.To_CORBA_String ("ULongLongSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/ULongLongSeq:1.0");
      begin
         TC_ULongLongSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id,
            Parent => TC_IDL_SEQUENCE_Unsigned_Long_Long);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_ULongLongSeq);
      end;

      TC_IDL_SEQUENCE_Float :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Float, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Float);

      IDL_SEQUENCE_Float_Helper.Initialize
        (Element_TC  => CORBA.TC_Float,
         Sequence_TC => TC_IDL_SEQUENCE_Float);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("FloatSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/FloatSeq:1.0");
      begin
         TC_FloatSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_Float);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_FloatSeq);
      end;

      TC_IDL_SEQUENCE_Double :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Double, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Double);

      IDL_SEQUENCE_Double_Helper.Initialize
        (Element_TC  => CORBA.TC_Double,
         Sequence_TC => TC_IDL_SEQUENCE_Double);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("DoubleSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/DoubleSeq:1.0");
      begin
         TC_DoubleSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_Double);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_DoubleSeq);
      end;

      TC_IDL_SEQUENCE_Long_Double :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Long_Double, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Long_Double);

      IDL_SEQUENCE_Long_Double_Helper.Initialize
        (Element_TC  => CORBA.TC_Long_Double,
         Sequence_TC => TC_IDL_SEQUENCE_Long_Double);

      declare
         Name : constant CORBA.String :=
                  CORBA.To_CORBA_String ("LongDoubleSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String
                    ("IDL:omg.org/CORBA/LongDoubleSeq:1.0");
      begin
         TC_LongDoubleSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_Long_Double);
         CORBA.TypeCode.Internals.Disable_Reference_Counting
           (TC_LongDoubleSeq);
      end;

      TC_IDL_SEQUENCE_String :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_String, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_String);

      IDL_SEQUENCE_String_Helper.Initialize
        (Element_TC  => CORBA.TC_String,
         Sequence_TC => TC_IDL_SEQUENCE_String);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("StringSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/StringSeq:1.0");
      begin
         TC_StringSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => TC_IDL_SEQUENCE_String);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_StringSeq);
      end;

      TC_IDL_SEQUENCE_Wide_String :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Wide_String, 0);
      CORBA.TypeCode.Internals.Disable_Reference_Counting
        (TC_IDL_SEQUENCE_Wide_String);

      IDL_SEQUENCE_Wide_String_Helper.Initialize
        (Element_TC  => CORBA.TC_Wide_String,
         Sequence_TC => TC_IDL_SEQUENCE_Wide_String);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("WStringSeq");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String ("IDL:omg.org/CORBA/WStringSeq:1.0");
      begin
         TC_WStringSeq := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name, Id, TC_IDL_SEQUENCE_Wide_String);
         CORBA.TypeCode.Internals.Disable_Reference_Counting (TC_WStringSeq);
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
      Internals.Set_Type (Result, TC_AnySeq);
      return Result;
   end To_Any;

   function To_Any (Item : BooleanSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Boolean.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_BooleanSeq);
      return Result;
   end To_Any;

   function To_Any (Item : CharSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Char.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_CharSeq);
      return Result;
   end To_Any;

   function To_Any (Item : DoubleSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Double.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_DoubleSeq);
      return Result;
   end To_Any;

   function To_Any (Item : FloatSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Float.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_FloatSeq);
      return Result;
   end To_Any;

   function To_Any (Item : LongDoubleSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Long_Double.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_LongDoubleSeq);
      return Result;
   end To_Any;

   function To_Any (Item : LongSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Long.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_LongSeq);
      return Result;
   end To_Any;

   function To_Any (Item : LongLongSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Long_Long.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_LongLongSeq);
      return Result;
   end To_Any;

   function To_Any (Item : OctetSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Octet.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_OctetSeq);
      return Result;
   end To_Any;

   function To_Any (Item : ShortSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Short.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_ShortSeq);
      return Result;
   end To_Any;

   function To_Any (Item : StringSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_String.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_StringSeq);
      return Result;
   end To_Any;

   function To_Any (Item : ULongLongSeq) return CORBA.Any is
      Result : CORBA.Any
        := To_Any (IDL_SEQUENCE_Unsigned_Long_Long.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_ULongLongSeq);
      return Result;
   end To_Any;

   function To_Any (Item : ULongSeq) return CORBA.Any is
      Result : CORBA.Any
        := To_Any (IDL_SEQUENCE_Unsigned_Long.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_ULongSeq);
      return Result;
   end To_Any;

   function To_Any (Item : UShortSeq) return CORBA.Any is
      Result : CORBA.Any
        := To_Any (IDL_SEQUENCE_Unsigned_Short.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_UShortSeq);
      return Result;
   end To_Any;

   function To_Any (Item : WCharSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Wide_Char.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_WCharSeq);
      return Result;
   end To_Any;

   function To_Any (Item : WStringSeq) return CORBA.Any is
      Result : CORBA.Any := To_Any (IDL_SEQUENCE_Wide_String.Sequence (Item));

   begin
      Internals.Set_Type (Result, TC_WStringSeq);
      return Result;
   end To_Any;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (X : access IDL_SEQUENCE_Any.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Any_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Boolean.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Boolean_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Char.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Char_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Wide_Char.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Wide_Char_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Octet.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Octet_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Short.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Short_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Unsigned_Short.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Unsigned_Short_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Long.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Long_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Unsigned_Long.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Unsigned_Long_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Long_Long.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Long_Long_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Unsigned_Long_Long.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Unsigned_Long_Long_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Float.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Float_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Double.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Double_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Long_Double.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Long_Double_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_String.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_String_Helper.Wrap;

   function Wrap
     (X : access IDL_SEQUENCE_Wide_String.Sequence)
     return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_Wide_String_Helper.Wrap;

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
