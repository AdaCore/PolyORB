
with PolyORB.Exceptions;
with PolyORB.Initialization;
with PolyORB.Sequences.Unbounded.CORBA_Helper;
with PolyORB.Utils.Strings;

with DynamicAny.DynAny.Helper;

package body DynamicAny.Helper is

   package IDL_SEQUENCE_Any_Helper is
     new IDL_SEQUENCE_Any.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any);

   package IDL_SEQUENCE_DynamicAny_NameValuePair_Helper is
     new IDL_SEQUENCE_DynamicAny_NameValuePair.CORBA_Helper
     (Element_To_Any   => To_Any,
      Element_From_Any => From_Any);

   procedure Raise_MustTruncate_From_Any (Item : in PolyORB.Any.Any);
   pragma No_Return (Raise_MustTruncate_From_Any);

   procedure Deferred_Initialization;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("FieldName");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/DynamicAny/FieldName:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_FieldName, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_FieldName, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_FieldName, CORBA.To_Any (CORBA.TC_String));
      end;

      declare
         Name           : constant CORBA.String
           := CORBA.To_CORBA_String ("NameValuePair");
         Id             : constant CORBA.String
           := CORBA.To_CORBA_String
           ("IDL:omg.org/DynamicAny/NameValuePair:1.0");
         Arg_Name_Id    : constant CORBA.String
           := CORBA.To_CORBA_String ("id");
         Arg_Name_Value : constant CORBA.String
           := CORBA.To_CORBA_String ("value");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameValuePair, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameValuePair, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameValuePair, CORBA.To_Any (TC_FieldName));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameValuePair, CORBA.To_Any (Arg_Name_Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameValuePair, CORBA.To_Any (CORBA.TC_Any));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameValuePair, CORBA.To_Any (Arg_Name_Value));
      end;

      TC_IDL_SEQUENCE_DynamicAny_NameValuePair :=
        CORBA.TypeCode.Internals.Build_Sequence_TC
          (DynamicAny.Helper.TC_NameValuePair, 0);
      IDL_SEQUENCE_DynamicAny_NameValuePair_Helper.Initialize
        (Element_TC  => DynamicAny.Helper.TC_NameValuePair,
         Sequence_TC => TC_IDL_SEQUENCE_DynamicAny_NameValuePair);

      declare
         Name : constant CORBA.String
           := CORBA.To_CORBA_String ("NameValuePairSeq");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String
           ("IDL:omg.org/DynamicAny/NameValuePairSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameValuePairSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameValuePairSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameValuePairSeq,
            CORBA.To_Any (TC_IDL_SEQUENCE_DynamicAny_NameValuePair));
      end;

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("DynAny");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/DynamicAny/DynAny:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_DynAny_Forward, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_DynAny_Forward, CORBA.To_Any (Id));
      end;

      declare
         Name           : constant CORBA.String
           := CORBA.To_CORBA_String ("NameDynAnyPair");
         Id             : constant CORBA.String
           := CORBA.To_CORBA_String
           ("IDL:omg.org/DynamicAny/NameDynAnyPair:1.0");
         Arg_Name_Id    : constant CORBA.String
           := CORBA.To_CORBA_String ("id");
         Arg_Name_Value : constant CORBA.String
           := CORBA.To_CORBA_String ("value");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameDynAnyPair, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameDynAnyPair, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameDynAnyPair, CORBA.To_Any (TC_FieldName));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameDynAnyPair, CORBA.To_Any (Arg_Name_Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameDynAnyPair, CORBA.To_Any (DynAny.Helper.TC_DynAny));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameDynAnyPair, CORBA.To_Any (Arg_Name_Value));
      end;

      TC_IDL_SEQUENCE_DynamicAny_NameDynAnyPair :=
        CORBA.TypeCode.Internals.Build_Sequence_TC
          (DynamicAny.Helper.TC_NameDynAnyPair, 0);

      declare
         Name : constant CORBA.String
           := CORBA.To_CORBA_String ("NameDynAnyPairSeq");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String
           ("IDL:omg.org/DynamicAny/NameDynAnyPairSeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameDynAnyPairSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameDynAnyPairSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_NameDynAnyPairSeq,
            CORBA.To_Any (TC_IDL_SEQUENCE_DynamicAny_NameDynAnyPair));
      end;

      TC_IDL_SEQUENCE_Any :=
        CORBA.TypeCode.Internals.Build_Sequence_TC
          (CORBA.TC_Any, 0);
      IDL_SEQUENCE_Any_Helper.Initialize
        (Element_TC  => CORBA.TC_Any,
         Sequence_TC => TC_IDL_SEQUENCE_Any);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("AnySeq");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/DynamicAny/AnySeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_AnySeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_AnySeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_AnySeq, CORBA.To_Any (TC_IDL_SEQUENCE_Any));
      end;

      TC_IDL_SEQUENCE_DynamicAny_DynAny_Forward :=
        CORBA.TypeCode.Internals.Build_Sequence_TC
          (DynamicAny.DynAny.Helper.TC_DynAny, 0);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("DynAnySeq");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/DynamicAny/DynAnySeq:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_DynAnySeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_DynAnySeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_DynAnySeq,
            CORBA.To_Any (TC_IDL_SEQUENCE_DynamicAny_DynAny_Forward));
      end;

      declare
         Name : constant CORBA.String
           := CORBA.To_CORBA_String ("MustTruncate");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String
           ("IDL:omg.org/DynamicAny/MustTruncate:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_MustTruncate, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_MustTruncate, CORBA.To_Any (Id));
      end;
      PolyORB.Exceptions.Register_Exception
        (CORBA.TypeCode.Internals.To_PolyORB_Object (TC_MustTruncate),
         Raise_MustTruncate_From_Any'Access);
   end Deferred_Initialization;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in CORBA.Any) return IDL_SEQUENCE_Any.Sequence
     renames IDL_SEQUENCE_Any_Helper.From_Any;

   function From_Any (Item : in CORBA.Any)
     return IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence
       renames IDL_SEQUENCE_DynamicAny_NameValuePair_Helper.From_Any;

   function From_Any (Item : in CORBA.Any) return AnySeq is
      Result : constant DynamicAny.IDL_SEQUENCE_Any.Sequence
        := DynamicAny.Helper.From_Any (Item);
   begin
      return DynamicAny.AnySeq (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return FieldName is
      Result : constant CORBA.String := CORBA.From_Any (Item);

   begin
      return FieldName (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return MustTruncate_Members is
      pragma Unreferenced (Item);

      Result : MustTruncate_Members;

   begin
      return Result;
   end From_Any;

   function From_Any (Item : in CORBA.Any) return NameValuePair is
      Index        : CORBA.Any;
      Result_Id    : FieldName;
      Result_Value : CORBA.Any;

   begin
      Index :=
        CORBA.Internals.Get_Aggregate_Element
        (Item, TC_FieldName, CORBA.Unsigned_Long (0));
      Result_Id := From_Any (Index);

      Index :=
        CORBA.Internals.Get_Aggregate_Element
        (Item, CORBA.TC_Any, CORBA.Unsigned_Long (1));
      Result_Value := CORBA.From_Any (Index);

      return
         (Id    => Result_Id,
          Value => Result_Value);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return NameValuePairSeq is
      Result : constant IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence
        := From_Any (Item);

   begin
      return NameValuePairSeq (Result);
   end From_Any;

   ------------------------
   -- Raise_MustTruncate --
   ------------------------

   procedure Raise_MustTruncate (Members : in MustTruncate_Members) is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (MustTruncate'Identity,
         Members);
   end Raise_MustTruncate;

   ---------------------------------
   -- Raise_MustTruncate_From_Any --
   ---------------------------------

   procedure Raise_MustTruncate_From_Any (Item : in PolyORB.Any.Any) is
      Members : constant MustTruncate_Members
        := From_Any (CORBA.Internals.To_CORBA_Any (Item));

   begin
      PolyORB.Exceptions.User_Raise_Exception
        (MustTruncate'Identity,
         Members);
   end Raise_MustTruncate_From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in IDL_SEQUENCE_Any.Sequence) return CORBA.Any
     renames IDL_SEQUENCE_Any_Helper.To_Any;

   function To_Any (Item : in IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence)
     return CORBA.Any
       renames IDL_SEQUENCE_DynamicAny_NameValuePair_Helper.To_Any;

   function To_Any (Item : in AnySeq) return CORBA.Any is
      Result : CORBA.Any := Helper.To_Any (IDL_SEQUENCE_Any.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_AnySeq);

      return Result;
   end To_Any;

   function To_Any (Item : in FieldName) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.String (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_FieldName);

      return Result;
   end To_Any;

   function To_Any (Item : in MustTruncate_Members) return CORBA.Any is
      pragma Unreferenced (Item);

      Result : CORBA.Any
        := CORBA.Internals.Get_Empty_Any_Aggregate (TC_MustTruncate);

   begin
      return Result;
   end To_Any;

   function To_Any (Item : in NameValuePair) return CORBA.Any is
      Result : CORBA.Any
        := CORBA.Internals.Get_Empty_Any_Aggregate (TC_NameValuePair);

   begin
      CORBA.Internals.Add_Aggregate_Element (Result, To_Any (Item.Id));
      CORBA.Internals.Add_Aggregate_Element
        (Result, CORBA.To_Any (Item.Value));

      return Result;
   end To_Any;

   function To_Any (Item : in NameValuePairSeq) return CORBA.Any is
      Result : CORBA.Any
        := To_Any (IDL_SEQUENCE_DynamicAny_NameValuePair.Sequence (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_NameValuePairSeq);

      return Result;
   end To_Any;

   ------------
   -- To_Ref --
   ------------

   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return DynAny_Forward.Ref
   is
   begin
      if CORBA.Object.Is_Nil (The_Ref)
        or else CORBA.Object.Is_A
        (The_Ref, "IDL:omg.org/DynamicAny/DynAny:1.0")
      then
         return Unchecked_To_Ref (The_Ref);
      end if;

      CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
   end To_Ref;

   ----------------------
   -- Unchecked_To_Ref --
   ----------------------

   function Unchecked_To_Ref (The_Ref : in CORBA.Object.Ref'Class)
     return DynAny_Forward.Ref
   is
      Result : DynamicAny.DynAny_Forward.Ref;

   begin
      DynAny_Forward.Set (Result, CORBA.Object.Object_Of (The_Ref));

      return Result;
   end Unchecked_To_Ref;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"DynamicAny.Helper",
          Conflicts => Empty,
          Depends   => +"exceptions",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;
end DynamicAny.Helper;
