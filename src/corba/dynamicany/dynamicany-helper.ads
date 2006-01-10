------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    D Y N A M I C A N Y . H E L P E R                     --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA.Object;
with PolyORB.Any;

package DynamicAny.Helper is

   --  FieldName type

   TC_FieldName : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return FieldName;

   function To_Any (Item : FieldName) return CORBA.Any;

   --  NameValuePair structure

   TC_NameValuePair : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Struct);

   function From_Any (Item : CORBA.Any) return NameValuePair;

   function To_Any (Item : NameValuePair) return CORBA.Any;

   --  NameValuePair sequence

   TC_IDL_SEQUENCE_DynamicAny_NameValuePair : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Sequence);

   function From_Any
     (Item : CORBA.Any)
      return IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence;

   function To_Any
     (Item : IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence)
      return CORBA.Any;

   TC_NameValuePairSeq : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return NameValuePairSeq;

   function To_Any (Item : NameValuePairSeq) return CORBA.Any;

   --  DynAny interface forward

   function Unchecked_To_Ref
     (The_Ref : CORBA.Object.Ref'Class)
      return DynAny_Forward.Ref;

   function To_Ref
     (The_Ref : CORBA.Object.Ref'Class)
      return DynAny_Forward.Ref;

   TC_DynAny_Forward : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Object);

   --  NameDynAnyPair structure

   TC_NameDynAnyPair : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Struct);

   --  NameDynAnyPairSeq sequence

   TC_IDL_SEQUENCE_DynamicAny_NameDynAnyPair : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Sequence);

   TC_NameDynAnyPairSeq : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   --  AnySeq sequence

   TC_IDL_SEQUENCE_Any : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Sequence);

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Any.Sequence;

   function To_Any (Item : IDL_SEQUENCE_Any.Sequence) return CORBA.Any;

   TC_AnySeq : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return AnySeq;

   function To_Any (Item : AnySeq) return CORBA.Any;

   --  DynAnySeq sequence

   TC_IDL_SEQUENCE_DynamicAny_DynAny_Forward : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Sequence);

   TC_DynAnySeq : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   --  MustTruncate exception

   TC_MustTruncate : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Except);

   function From_Any (Item : CORBA.Any) return MustTruncate_Members;

   function To_Any (Item : MustTruncate_Members) return CORBA.Any;

   procedure Raise_MustTruncate (Members : MustTruncate_Members);
   pragma No_Return (Raise_MustTruncate);

end DynamicAny.Helper;
