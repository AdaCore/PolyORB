----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Impl;
with CORBA.Object;

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

with Broca.Debug;
with Broca.Server_Tools;
with PortableServer;

package body CORBA.Repository_Root.Repository.Impl is


   -----------
   -- Debug --
   -----------

   Flag : constant Natural
     := Broca.Debug.Is_Active ("repository.impl");
   procedure O is new Broca.Debug.Output (Flag);

   Flag2 : constant Natural
     := Broca.Debug.Is_Active ("repository.impl_method_trace");
   procedure O2 is new Broca.Debug.Output (Flag2);

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : Repository_Forward.Ref)
     return Object_Ptr is
      Result : Portableserver.Servant;
   begin
      pragma Debug (O2 ("to_object (repository)"));
      Broca.Server_Tools.Reference_To_Servant
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
      Broca.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Ref);
      return Repository.Convert_Forward.To_Forward (Ref);
   end To_Forward;


   function lookup_id
     (Self : access Object;
      search_id : in CORBA.RepositoryId)
     return CORBA.Repository_Root.Contained.Ref
   is
      Result_Object : Contained.Impl.Object_Ptr;
      use Contained.Impl;
   begin
      Result_Object := Contained.Impl.Lookup_Id (Get_Contents (Self),
                                                 Search_Id);


      --  return a nil_ref if not found
      if Result_Object = null then
         return (CORBA.Object.Nil_Ref with null record);
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



      --  activate it
      Broca.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Result);

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
                           Bound);

      --  activate it
      Broca.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Result);

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
                            Bound);

      --  activate it
      Broca.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Result);

      return Result;
   end create_wstring;


   function create_sequence
     (Self : access Object;
      bound : in CORBA.Unsigned_Long;
      element_type : in CORBA.Repository_Root.IDLType.Ref)
     return CORBA.Repository_Root.SequenceDef.Ref
   is
      Result : CORBA.Repository_Root.SequenceDef.Ref;
      Elem_Obj : Portableserver.Servant;
      Element : CORBA.TypeCode.Object;
      Obj : SequenceDef.Impl.Object_Ptr := new SequenceDef.Impl.Object;
   begin
      Broca.Server_Tools.Reference_To_Servant (Element_Type,
                                               Elem_Obj);
      Element := IDLType.Impl.Get_Type
        (IDLType.Impl.To_IDLType
         (IRObject.Impl.Object_Ptr (Elem_Obj)));
      --  initialization of the Sequence
      SequenceDef.Impl.Init (Obj,
                             IRObject.Impl.Object_Ptr (Obj),
                             Dk_Sequence,
                             Bound,
                             Element_Type);

      --  activate it
      Broca.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Result);

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
      Elem_Obj : Portableserver.Servant;
      Element : CORBA.TypeCode.Object;
   begin
      Broca.Server_Tools.Reference_To_Servant (Element_Type,
                                               Elem_Obj);
      Element := IDLType.Impl.Get_Type
        (IDLType.Impl.To_IDLType
         (IRObject.Impl.Object_Ptr (Elem_Obj)));

      --  initialization of the Array
      ArrayDef.Impl.Init (Obj,
                          IRObject.Impl.Object_Ptr (Obj),
                          Dk_Array,
                          length,
                          Element_Type);

      --  activate it
      Broca.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Result);

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
                          IDL_digits,
                          Scale);

      --  activate it
      Broca.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Result);

      return Result;
   end create_fixed;

end CORBA.Repository_Root.Repository.Impl;
