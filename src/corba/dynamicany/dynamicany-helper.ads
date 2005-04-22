
with CORBA.Object;
with PolyORB.Any;

package DynamicAny.Helper is

   --  FieldName type

   TC_FieldName : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return FieldName;

   function To_Any (Item : in FieldName) return CORBA.Any;

   --  NameValuePair structure

   TC_NameValuePair : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Struct);

   function From_Any (Item : in CORBA.Any) return NameValuePair;

   function To_Any (Item : in NameValuePair) return CORBA.Any;

   --  NameValuePair sequence

   TC_IDL_SEQUENCE_DynamicAny_NameValuePair : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Sequence);

   function From_Any
     (Item : in CORBA.Any)
      return IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence;

   function To_Any
     (Item : in IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence)
      return CORBA.Any;

   TC_NameValuePairSeq : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return NameValuePairSeq;

   function To_Any (Item : in NameValuePairSeq) return CORBA.Any;

   --  DynAny interface forward

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return DynAny_Forward.Ref;

   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
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

   function From_Any (Item : in CORBA.Any) return IDL_SEQUENCE_Any.Sequence;

   function To_Any (Item : in IDL_SEQUENCE_Any.Sequence) return CORBA.Any;

   TC_AnySeq : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return AnySeq;

   function To_Any (Item : in AnySeq) return CORBA.Any;

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

   function From_Any (Item : in CORBA.Any) return MustTruncate_Members;

   function To_Any (Item : in MustTruncate_Members) return CORBA.Any;

   procedure Raise_MustTruncate (Members : in MustTruncate_Members);
   pragma No_Return (Raise_MustTruncate);

end DynamicAny.Helper;
