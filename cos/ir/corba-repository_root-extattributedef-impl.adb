
with CORBA.Repository_Root.ExceptionDef.Impl;
with CORBA.Repository_Root.ExtAttributeDef.Skel;
pragma Elaborate (CORBA.Repository_Root.ExtAttributeDef.Skel);
pragma Warnings (Off, CORBA.Repository_Root.ExtAttributeDef.Skel);

package body CORBA.Repository_Root.ExtAttributeDef.Impl is

   ------------------------
   -- describe_attribute --
   ------------------------

   function describe_attribute
     (Self : access Object)
      return ExtAttributeDescription
   is
   begin
      return
       (name           => get_name (Self),
        id             => get_id (Self),
        defined_in     => get_defined_in (Self),
        version        => get_version (Self),
        IDL_type       => get_type (Self),
        mode           => get_mode (Self),
        get_exceptions => Self.Get_Exceptions,
        put_exceptions => Self.Set_Exceptions);
   end describe_attribute;

   ------------------------
   -- get_get_exceptions --
   ------------------------

   function get_get_exceptions
     (Self : access Object)
      return ExcDescriptionSeq
   is
   begin
      return Self.Get_Exceptions;
   end get_get_exceptions;

   ------------------------
   -- get_set_exceptions --
   ------------------------

   function get_set_exceptions
     (Self : access Object)
      return ExcDescriptionSeq
   is
   begin
      return Self.Set_Exceptions;
   end get_set_exceptions;

   ------------------------
   -- set_get_exceptions --
   ------------------------

   procedure set_get_exceptions
     (Self : access Object;
      To   : in     ExcDescriptionSeq)
   is
   begin
      Self.Get_Exceptions := To;
   end set_get_exceptions;

   ------------------------
   -- set_set_exceptions --
   ------------------------

   procedure set_set_exceptions
     (Self : access Object;
      To   : in     ExcDescriptionSeq)
   is
   begin
      Self.Set_Exceptions := To;
   end set_set_exceptions;

   package body Internals is

      ----------
      -- Init --
      ----------

      procedure Init
        (Self           : access Object'Class;
         Real_Object    : in     IRObject.Impl.Object_Ptr;
         Def_Kind       : in     DefinitionKind;
         Id             : in     RepositoryId;
         Name           : in     Identifier;
         Version        : in     VersionSpec;
         Defined_In     : in     Container_Forward.Ref;
         Type_Def       : in     IDLType.Ref;
         Mode           : in     AttributeMode;
         Get_Exceptions : in     ExceptionDefSeq;
         Set_Exceptions : in     ExceptionDefSeq)
      is
      begin
         AttributeDef.Impl.Init
           (AttributeDef.Impl.Object_Ptr (Self),
            Real_Object,
            Def_Kind,
            Id,
            Name,
            Version,
            Defined_In,
            Type_Def,
            Mode);

         Self.Get_Exceptions :=
           ExceptionDef.Impl.Get_ExcDescriptionSeq (Get_Exceptions);
         Self.Set_Exceptions :=
           ExceptionDef.Impl.Get_ExcDescriptionSeq (Set_Exceptions);
      end Init;

   end Internals;

end CORBA.Repository_Root.ExtAttributeDef.Impl;
