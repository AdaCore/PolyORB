----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;
pragma Elaborate_All (CORBA.Repository_Root.IDLType.Impl);

package CORBA.Repository_Root.SequenceDef.Impl is

   type Object is
     new CORBA.Repository_Root.IDLType.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   IDL_Type : CORBA.TypeCode.Object;
                   Bound : CORBA.Unsigned_Long;
                   Element_Type_Def : CORBA.Repository_Root.IDLType.Ref);


   function get_bound
     (Self : access Object)
     return CORBA.Unsigned_Long;

   procedure set_bound
     (Self : access Object;
      To : in CORBA.Unsigned_Long);

   function get_element_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

   function get_element_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref;

   procedure set_element_type_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref);

private

   type Object is
     new CORBA.Repository_Root.IDLType.Impl.Object with record
        Bound : CORBA.Unsigned_Long;
        --  the Element_Type field is the one from the IDLType
        Element_Type_Def : CORBA.Repository_Root.IDLType.Ref;
     end record;

end CORBA.Repository_Root.SequenceDef.Impl;
