----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.FixedDef;
with CORBA.Repository_Root.ArrayDef;
with CORBA.Repository_Root.SequenceDef;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.WstringDef;
with CORBA.Repository_Root.StringDef;
with CORBA.Repository_Root.PrimitiveDef;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Container.Impl;

package CORBA.Repository_Root.Repository.Impl is

   type Object is
     new CORBA.Repository_Root.Container.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  Transform the forward to an impl.object.ptr.
   function To_Object (Fw_Ref : Repository_Forward.Ref)
     return Repository.Impl.Object_Ptr;

   --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return Repository_Forward.Ref;

   function lookup_id
     (Self : access Object;
      search_id : in CORBA.RepositoryId)
     return CORBA.Repository_Root.Contained.Ref;

   function get_canonical_typecode
     (Self : access Object;
      tc : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object;

   function get_primitive
     (Self : access Object;
      kind : in CORBA.Repository_Root.PrimitiveKind)
     return CORBA.Repository_Root.PrimitiveDef.Ref;

   function create_string
     (Self : access Object;
      bound : in CORBA.Unsigned_Long)
     return CORBA.Repository_Root.StringDef.Ref;

   function create_wstring
     (Self : access Object;
      bound : in CORBA.Unsigned_Long)
     return CORBA.Repository_Root.WstringDef.Ref;

   function create_sequence
     (Self : access Object;
      bound : in CORBA.Unsigned_Long;
      element_type : in CORBA.Repository_Root.IDLType.Ref)
     return CORBA.Repository_Root.SequenceDef.Ref;

   function create_array
     (Self : access Object;
      length : in CORBA.Unsigned_Long;
      element_type : in CORBA.Repository_Root.IDLType.Ref)
     return CORBA.Repository_Root.ArrayDef.Ref;

   function create_fixed
     (Self : access Object;
      IDL_digits : in CORBA.Unsigned_Short;
      scale : in CORBA.Short)
     return CORBA.Repository_Root.FixedDef.Ref;

private

   type Object is
     new CORBA.Repository_Root.Container.Impl.Object with null record;

end CORBA.Repository_Root.Repository.Impl;

