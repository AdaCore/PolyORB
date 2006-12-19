------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . I D L _ S E Q U E N C E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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

with CORBA.Sequences.Unbounded;

package CORBA.IDL_SEQUENCES is

   --  Implementation Note: this package defines all sequences types
   --  in the CORBA module. These definitions are separate to avoid
   --  dragging to much code.

   --  AnySeq sequence

   package IDL_SEQUENCE_Any is new CORBA.Sequences.Unbounded (Any);

   type AnySeq is new IDL_SEQUENCE_Any.Sequence;

   --  BooleanSeq sequence

   package IDL_SEQUENCE_Boolean is
     new CORBA.Sequences.Unbounded (CORBA.Boolean);

   type BooleanSeq is new IDL_SEQUENCE_Boolean.Sequence;

   --  CharSeq sequence

   package IDL_SEQUENCE_Char is new CORBA.Sequences.Unbounded (Char);

   type CharSeq is new IDL_SEQUENCE_Char.Sequence;

   --  WCharSeq sequence

   package IDL_SEQUENCE_Wide_Char is new CORBA.Sequences.Unbounded (Wchar);

   type WCharSeq is new IDL_SEQUENCE_Wide_Char.Sequence;

   --  Octet sequence

   package IDL_SEQUENCE_Octet is new CORBA.Sequences.Unbounded (Octet);

   type OctetSeq is new IDL_SEQUENCE_Octet.Sequence;

   --  ShortSeq sequence

   package IDL_SEQUENCE_Short is new CORBA.Sequences.Unbounded (Short);

   type ShortSeq is new IDL_SEQUENCE_Short.Sequence;

   --  UShortSeq sequence

   package IDL_SEQUENCE_Unsigned_Short is
     new CORBA.Sequences.Unbounded (Unsigned_Short);

   type UShortSeq is new IDL_SEQUENCE_Unsigned_Short.Sequence;

   --  LongSeq sequence

   package IDL_SEQUENCE_Long is new CORBA.Sequences.Unbounded (Long);

   type LongSeq is new IDL_SEQUENCE_Long.Sequence;

   --  ULongSeq sequence

   package IDL_SEQUENCE_Unsigned_Long is
     new CORBA.Sequences.Unbounded (Unsigned_Long);

   type ULongSeq is new IDL_SEQUENCE_Unsigned_Long.Sequence;

   --  LongLongSeq sequence

   package IDL_SEQUENCE_Long_Long is new CORBA.Sequences.Unbounded (Long_Long);

   type LongLongSeq is new IDL_SEQUENCE_Long_Long.Sequence;

   --  LongLongSeq sequence

   package IDL_SEQUENCE_Unsigned_Long_Long is
     new CORBA.Sequences.Unbounded (Unsigned_Long_Long);

   type ULongLongSeq is new IDL_SEQUENCE_Unsigned_Long_Long.Sequence;

   --  FloatSeq sequence

   package IDL_SEQUENCE_Float is new CORBA.Sequences.Unbounded (Float);

   type FloatSeq is new IDL_SEQUENCE_Float.Sequence;

   --  DoubleSeq sequence

   package IDL_SEQUENCE_Double is new CORBA.Sequences.Unbounded (Double);

   type DoubleSeq is new IDL_SEQUENCE_Double.Sequence;

   --  LongDoubleSeq sequence

   package IDL_SEQUENCE_Long_Double is
     new CORBA.Sequences.Unbounded (Long_Double);

   type LongDoubleSeq is new IDL_SEQUENCE_Long_Double.Sequence;

   --  StringSeq sequence

   package IDL_SEQUENCE_String is new CORBA.Sequences.Unbounded (String);

   type StringSeq is new IDL_SEQUENCE_String.Sequence;

   --  WStringSeq sequence

   package IDL_SEQUENCE_Wide_String is
     new CORBA.Sequences.Unbounded (Wide_String);

   type WStringSeq is new IDL_SEQUENCE_Wide_String.Sequence;

   --  Repository Ids

   AnySeq_Repository_Id        : constant Standard.String
     := "IDL:CORBA/AnySeq:1.0";

   BooleanSeq_Repository_Id    : constant Standard.String
     := "IDL:CORBA/BooleanSeq:1.0";

   CharSeq_Repository_Id       : constant Standard.String
     := "IDL:CORBA/CharSeq:1.0";

   DoubleSeq_Repository_Id     : constant Standard.String
     := "IDL:CORBA/DoubleSeq:1.0";

   FloatSeq_Repository_Id      : constant Standard.String
     := "IDL:CORBA/FloatSeq:1.0";

   LongDoubleSeq_Repository_Id : constant Standard.String
     := "IDL:CORBA/LongDoubleSeq:1.0";

   LongLongSeq_Repository_Id   : constant Standard.String
     := "IDL:CORBA/LongLongSeq:1.0";

   LongSeq_Repository_Id       : constant Standard.String
     := "IDL:CORBA/LongSeq:1.0";

   OctetSeq_Repository_Id      : constant Standard.String
     := "IDL:CORBA/OctetSeq:1.0";

   ShortSeq_Repository_Id      : constant Standard.String
     := "IDL:CORBA/ShortSeq:1.0";

   StringSeq_Repository_Id     : constant Standard.String
     := "IDL:CORBA/StringSeq:1.0";

   ULongSeq_Repository_Id      : constant Standard.String
     := "IDL:CORBA/ULongSeq:1.0";

   ULongLongSeq_Repository_Id  : constant Standard.String
     := "IDL:CORBA/ULongLongSeq:1.0";

   UShortSeq_Repository_Id     : constant Standard.String
     := "IDL:CORBA/UShortSeq:1.0";

   WCharSeq_Repository_Id      : constant Standard.String
     := "IDL:CORBA/WCharSeq:1.0";

   WStringSeq_Repository_Id    : constant Standard.String
     := "IDL:CORBA/WStringSeq:1.0";

end CORBA.IDL_SEQUENCES;
