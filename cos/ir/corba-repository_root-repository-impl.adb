----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

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
with CORBA.Repository_Root.IRObject.Impl;

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.CORBA_P.Server_Tools;
with PortableServer;

package body CORBA.Repository_Root.Repository.Impl is

   -----------
   -- Debug --
   -----------

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("repository.impl");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   package L2 is new PolyORB.Log.Facility_Log ("repository.impl_method_trace");
   procedure O2 (Message : in Standard.String; Level : Log_Level := Debug)
     renames L2.Output;

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : Repository_Forward.Ref)
     return Object_Ptr is
      Result : PortableServer.Servant;
   begin
      pragma Debug (O2 ("to_object (repository)"));
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
        (Repository.Convert_Forward.To_Ref (Fw_Ref),
         Result);
      return Object_Ptr (Result);
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------

   function To_Forward (Obj : Object_Ptr)
                        return Repository_Forward.Ref is
      Ref : Repository.Ref;
   begin
      pragma Debug (O2 ("to_forward (repository)"));
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Ref);
      return Repository.Convert_Forward.To_Forward (Ref);
   end To_Forward;


   function lookup_id
     (Self : access Object;
      search_id : in CORBA.RepositoryId)
     return CORBA.Repository_Root.Contained.Ref
   is
      Result_Object : Contained.Impl.Object_Ptr;
      Nil_Ref : CORBA.Repository_Root.Contained.Ref;
      pragma Warnings (Off, Nil_Ref);
      --  Not initialized explicitly.
      use Contained.Impl;
   begin
      Result_Object := Contained.Impl.Lookup_Id (Get_Contents (Self),
                                                 search_id);


      --  Return a nil_ref if not found
      if Result_Object = null then
         return Nil_Ref;
      end if;

      return Contained.Convert_Forward.To_Ref
        (Contained.Impl.To_Forward (Result_Object));

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
         when pk_null =>
            IDL_Type := TC_Null;
         when pk_void =>
            IDL_Type := TC_Void;
         when pk_short =>
            IDL_Type := TC_Short;
         when pk_long =>
            IDL_Type := TC_Long;
         when pk_ushort =>
            IDL_Type := TC_Unsigned_Short;
         when pk_ulong =>
            IDL_Type := TC_Unsigned_Long;
         when pk_float =>
            IDL_Type := TC_Float;
         when pk_double =>
            IDL_Type := TC_Double;
         when pk_boolean =>
            IDL_Type := TC_Boolean;
         when pk_char =>
            IDL_Type := TC_Char;
         when pk_octet =>
            IDL_Type := TC_Octet;
         when pk_any =>
            IDL_Type := TC_Any;
         when pk_TypeCode =>
            IDL_Type := TC_TypeCode;
         when pk_Principal =>
            IDL_Type := CORBA.TypeCode.TC_Principal;
         when pk_string =>
            IDL_Type := TC_String;
         when pk_objref =>
            IDL_Type := CORBA.TypeCode.TC_Object;
         when pk_longlong =>
            IDL_Type := TC_Long_Long;
         when pk_ulonglong =>
            IDL_Type := TC_Unsigned_Long_Long;
         when pk_longdouble =>
            IDL_Type := TC_Long_Double;
         when pk_wchar =>
            IDL_Type := TC_Wchar;
         when pk_wstring =>
            IDL_Type := TC_Wide_String;
         when pk_value_base =>
            IDL_Type := CORBA.TypeCode.TC_Value;
      end case;

      --  initialize the object
      PrimitiveDef.Impl.Init (Obj,
                              IRObject.Impl.Object_Ptr (Obj),
                              dk_Primitive,
                              IDL_Type,
                              kind);



      --  activate it
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Result);

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
                           dk_String,
                           bound);

      --  activate it
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Result);

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
                            dk_Wstring,
                            bound);

      --  activate it
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Result);

      return Result;
   end create_wstring;


   function create_sequence
     (Self : access Object;
      bound : in CORBA.Unsigned_Long;
      element_type : in CORBA.Repository_Root.IDLType.Ref)
     return CORBA.Repository_Root.SequenceDef.Ref
   is
      Result : CORBA.Repository_Root.SequenceDef.Ref;
      Elem_Obj : PortableServer.Servant;
      Element : CORBA.TypeCode.Object;
      Obj : SequenceDef.Impl.Object_Ptr := new SequenceDef.Impl.Object;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant (element_type,
                                               Elem_Obj);
      Element := IDLType.Impl.get_type
        (IDLType.Impl.To_IDLType
         (IRObject.Impl.Object_Ptr (Elem_Obj)));
      --  initialization of the Sequence
      SequenceDef.Impl.Init (Obj,
                             IRObject.Impl.Object_Ptr (Obj),
                             dk_Sequence,
                             bound,
                             element_type);

      --  activate it
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Result);

      return Result;
   end create_sequence;


   function create_array
     (Self : access Object;
      length : in CORBA.Unsigned_Long;
      element_type : in CORBA.Repository_Root.IDLType.Ref)
     return CORBA.Repository_Root.ArrayDef.Ref
   is
      Result : CORBA.Repository_Root.ArrayDef.Ref;
      Obj : ArrayDef.Impl.Object_Ptr := new ArrayDef.Impl.Object;
      Elem_Obj : PortableServer.Servant;
      Element : CORBA.TypeCode.Object;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant (element_type,
                                               Elem_Obj);
      Element := IDLType.Impl.get_type
        (IDLType.Impl.To_IDLType
         (IRObject.Impl.Object_Ptr (Elem_Obj)));

      --  initialization of the Array
      ArrayDef.Impl.Init (Obj,
                          IRObject.Impl.Object_Ptr (Obj),
                          dk_Array,
                          length,
                          element_type);

      --  activate it
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Result);

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
                          dk_Fixed,
                          IDL_digits,
                          scale);

      --  Activate it
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Result);

      return Result;
   end create_fixed;

end CORBA.Repository_Root.Repository.Impl;
