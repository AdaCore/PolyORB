----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.AbstractBase;
with CORBA.Impl;
with CORBA.ORB.TypeCode;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.FixedDef;
with CORBA.Repository_Root.FixedDef.Impl;
with CORBA.Repository_Root.ArrayDef.Impl;
with CORBA.Repository_Root.ArrayDef;
with CORBA.Repository_Root.SequenceDef;
with CORBA.Repository_Root.SequenceDef.Impl;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.WstringDef;
with CORBA.Repository_Root.WstringDef.Impl;
with CORBA.Repository_Root.StringDef;
with CORBA.Repository_Root.StringDef.Impl;
with CORBA.Repository_Root.PrimitiveDef;
with CORBA.Repository_Root.PrimitiveDef.Impl;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.Repository.Skel;
with CORBA.Repository_Root.IRObject.Impl;

package body CORBA.Repository_Root.Repository.Impl is


   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : Repository_Forward.Ref)
     return Object_Ptr is
   begin
      return Object_Ptr
        (Repository.Object_Of
         (Repository.Convert_Forward.To_Ref
          (Fw_Ref)));
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return Repository_Forward.Ref is
      Ref : Repository.Ref;
   begin
      Set (Ref, CORBA.Impl.Object_Ptr (Obj));
      return Repository.Convert_Forward.To_Forward (Ref);
   end To_Forward;


   function lookup_id
     (Self : access Object;
      search_id : in CORBA.RepositoryId)
     return CORBA.Repository_Root.Contained.Ref
   is
      Result_Object : Contained.Impl.Object_Ptr;
      Result_Ref : Contained.Ref;
      use Contained.Impl;
   begin
      Result_Object := Contained.Impl.Lookup_Id (Get_Contents (Self),
                                                 Search_Id);
      Contained.Set (Result_Ref,
                     CORBA.Impl.Object_Ptr (Result_Object));
      --  it's a nil ref if Result_Object is null
      return Result_Ref;
   end lookup_id;


   function get_canonical_typecode
     (Self : access Object;
      tc : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin

      --  Insert implementation of get_canonical_typecode

      return Result;
   end get_canonical_typecode;


   ---------------------
   --  get_primitive  --
   ---------------------
   function get_primitive
     (Self : access Object;
      kind : in CORBA.Repository_Root.PrimitiveKind)
     return CORBA.Repository_Root.PrimitiveDef.Ref
   is
      Result : CORBA.Repository_Root.PrimitiveDef.Ref;
      Obj : PrimitiveDef.Impl.Object_Ptr := new PrimitiveDef.Impl.Object;
      IDL_Type : CORBA.TypeCode.Object;
   begin
      --  Create the appropriate TypeCode
      case kind is
         when Pk_Null =>
            IDL_Type := TC_Null;
         when Pk_Void =>
            IDL_Type := TC_Void;
         when Pk_Short =>
            IDL_Type := TC_Short;
         when Pk_Long =>
            IDL_Type := TC_Long;
         when Pk_Ushort =>
            IDL_Type := TC_Unsigned_Short;
         when Pk_Ulong =>
            IDL_Type := TC_Unsigned_Long;
         when Pk_Float =>
            IDL_Type := TC_Float;
         when Pk_Double =>
            IDL_Type := TC_Double;
         when Pk_Boolean =>
            IDL_Type := TC_Boolean;
         when Pk_Char =>
            IDL_Type := TC_Char;
         when Pk_Octet =>
            IDL_Type := TC_Octet;
         when Pk_Any =>
            IDL_Type := TC_Any;
         when Pk_TypeCode =>
            IDL_Type := TC_TypeCode;
         when Pk_Principal =>
            IDL_Type := CORBA.TypeCode.TC_Principal;
         when Pk_String =>
            IDL_Type := TC_String;
         when Pk_Objref =>
            IDL_Type := TC_ObjRef;
         when Pk_Longlong =>
            IDL_Type := TC_Long_Long;
         when Pk_Ulonglong =>
            IDL_Type := TC_Unsigned_Long_Long;
         when Pk_Longdouble =>
            IDL_Type := TC_Long_Double;
         when Pk_Wchar =>
            IDL_Type := TC_Wchar;
         when Pk_Wstring =>
            IDL_Type := TC_Wide_String;
         when Pk_Value_Base =>
            IDL_Type := CORBA.TypeCode.TC_Value;
      end case;

      --  initialize the object
      PrimitiveDef.Impl.Init (Obj,
                              IRObject.Impl.Object_Ptr (Obj),
                              Dk_Primitive,
                              IDL_Type,
                              Kind);
      --  create the ref
      PrimitiveDef.Set (Result,
                        CORBA.Impl.Object_Ptr (Obj));
      return Result;
   end get_primitive;


   function create_string
     (Self : access Object;
      bound : in CORBA.Unsigned_Long)
     return CORBA.Repository_Root.StringDef.Ref
   is
      Result : StringDef.Ref;
      Obj : StringDef.Impl.Object_Ptr := new StringDef.Impl.Object;
   begin
      --  initialization of the string
      StringDef.Impl.Init (Obj,
                           IRObject.Impl.Object_Ptr (Obj),
                           Dk_String,
                           CORBA.ORB.TypeCode.Create_String_Tc (Bound),
                           Bound);
      --  create the ref
      StringDef.Set (Result,
                     CORBA.Impl.Object_Ptr (Obj));

      return Result;
   end create_string;


   function create_wstring
     (Self : access Object;
      bound : in CORBA.Unsigned_Long)
     return CORBA.Repository_Root.WstringDef.Ref
   is
      Result : CORBA.Repository_Root.WstringDef.Ref;
      Obj : WstringDef.Impl.Object_Ptr := new WstringDef.Impl.Object;
   begin
      --  initialization of the wstring
      WstringDef.Impl.Init (Obj,
                            IRObject.Impl.Object_Ptr (Obj),
                            Dk_Wstring,
                            CORBA.ORB.TypeCode.Create_Wstring_Tc (Bound),
                            Bound);
      --  create the ref
      WstringDef.Set (Result,
                      CORBA.Impl.Object_Ptr (Obj));

      return Result;
   end create_wstring;


   function create_sequence
     (Self : access Object;
      bound : in CORBA.Unsigned_Long;
      element_type : in CORBA.Repository_Root.IDLType.Ref)
     return CORBA.Repository_Root.SequenceDef.Ref
   is
      Result : CORBA.Repository_Root.SequenceDef.Ref;
      Element : CORBA.TypeCode.Object := IDLType.Impl.Get_Type
        (IDLType.Impl.Object_Ptr (IDLType.Object_Of (Element_Type)));
      Obj : SequenceDef.Impl.Object_Ptr := new SequenceDef.Impl.Object;
   begin
      --  initialization of the Sequence
      SequenceDef.Impl.Init (Obj,
                             IRObject.Impl.Object_Ptr (Obj),
                             Dk_Sequence,
                             CORBA.ORB.TypeCode.Create_Sequence_Tc (Bound,
                                                                    Element),
                             Bound,
                             Element_Type);
      --  create the ref
      SequenceDef.Set (Result,
                       CORBA.Impl.Object_Ptr (Obj));

      return Result;
   end create_sequence;


   function create_array
     (Self : access Object;
      length : in CORBA.Unsigned_Long;
      element_type : in CORBA.Repository_Root.IDLType.Ref)
     return CORBA.Repository_Root.ArrayDef.Ref
   is
      Result : CORBA.Repository_Root.ArrayDef.Ref;
      Element : CORBA.TypeCode.Object := IDLType.Impl.Get_Type
        (IDLType.Impl.Object_Ptr (IDLType.Object_Of (Element_Type)));
      Obj : ArrayDef.Impl.Object_Ptr := new ArrayDef.Impl.Object;
   begin
      --  initialization of the Array
      ArrayDef.Impl.Init (Obj,
                          IRObject.Impl.Object_Ptr (Obj),
                          Dk_Array,
                          CORBA.ORB.TypeCode.Create_Array_Tc (length,
                                                              Element),
                          length,
                          Element_Type);
      --  create the ref
      ArrayDef.Set (Result,
                    CORBA.Impl.Object_Ptr (Obj));

      return Result;
   end create_array;


   function create_fixed
     (Self : access Object;
      IDL_digits : in CORBA.Unsigned_Short;
      scale : in CORBA.Short)
     return CORBA.Repository_Root.FixedDef.Ref
   is
      Result : CORBA.Repository_Root.FixedDef.Ref;
      Obj : FixedDef.Impl.Object_Ptr := new FixedDef.Impl.Object;
   begin
      --  initialization of the Fixed
      FixedDef.Impl.Init (Obj,
                          IRObject.Impl.Object_Ptr (Obj),
                          Dk_Fixed,
                          CORBA.ORB.TypeCode.Create_Fixed_Tc (IDL_digits,
                                                              Scale),
                          IDL_digits,
                          Scale);
      --  create the ref
      FixedDef.Set (Result,
                    CORBA.Impl.Object_Ptr (Obj));

      return Result;
   end create_fixed;

end CORBA.Repository_Root.Repository.Impl;
