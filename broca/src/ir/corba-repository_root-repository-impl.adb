----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.AbstractBase;
with CORBA.Impl;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.FixedDef;
with CORBA.Repository_Root.ArrayDef;
with CORBA.Repository_Root.SequenceDef;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.WstringDef;
with CORBA.Repository_Root.StringDef;
with CORBA.Repository_Root.PrimitiveDef;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.Repository.Skel;

package body CORBA.Repository_Root.Repository.Impl is


   function To_Object (Fw_Ref : Repository_Forward.Ref)
     return Repository.Impl.Object_Ptr is
   begin
      return Repository.Impl.Object_Ptr
        (Repository.Object_Of
         (Repository.Convert_Forward.To_Ref
          (Fw_Ref)));
   end To_Object;

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


   function get_primitive
     (Self : access Object;
      kind : in CORBA.Repository_Root.PrimitiveKind)
     return CORBA.Repository_Root.PrimitiveDef.Ref
   is
      Result : CORBA.Repository_Root.PrimitiveDef.Ref;
   begin

      --  Insert implementation of get_primitive

      return Result;
   end get_primitive;


   function create_string
     (Self : access Object;
      bound : in CORBA.Unsigned_Long)
     return CORBA.Repository_Root.StringDef.Ref
   is
      Result : CORBA.Repository_Root.StringDef.Ref;
   begin

      --  Insert implementation of create_string

      return Result;
   end create_string;


   function create_wstring
     (Self : access Object;
      bound : in CORBA.Unsigned_Long)
     return CORBA.Repository_Root.WstringDef.Ref
   is
      Result : CORBA.Repository_Root.WstringDef.Ref;
   begin

      --  Insert implementation of create_wstring

      return Result;
   end create_wstring;


   function create_sequence
     (Self : access Object;
      bound : in CORBA.Unsigned_Long;
      element_type : in CORBA.Repository_Root.IDLType.Ref)
     return CORBA.Repository_Root.SequenceDef.Ref
   is
      Result : CORBA.Repository_Root.SequenceDef.Ref;
   begin

      --  Insert implementation of create_sequence

      return Result;
   end create_sequence;


   function create_array
     (Self : access Object;
      length : in CORBA.Unsigned_Long;
      element_type : in CORBA.Repository_Root.IDLType.Ref)
     return CORBA.Repository_Root.ArrayDef.Ref
   is
      Result : CORBA.Repository_Root.ArrayDef.Ref;
   begin

      --  Insert implementation of create_array

      return Result;
   end create_array;


   function create_fixed
     (Self : access Object;
      IDL_digits : in CORBA.Unsigned_Short;
      scale : in CORBA.Short)
     return CORBA.Repository_Root.FixedDef.Ref
   is
      Result : CORBA.Repository_Root.FixedDef.Ref;
   begin

      --  Insert implementation of create_fixed

      return Result;
   end create_fixed;

end CORBA.Repository_Root.Repository.Impl;
