
package body CORBA.Repository_Root.AbstractInterfaceDef.Impl is

   package body Internals is

      function To_InterfaceDefSeq
        (Item : in AbstractInterfaceDefSeq)
         return InterfaceDefSeq;

      ----------
      -- Init --
      ----------

      procedure Init
        (Self            : access Object'Class;
         Real_Object     : in     IRObject.Impl.Object_Ptr;
         Def_Kind        : in     DefinitionKind;
         Id              : in     RepositoryId;
         Name            : in     Identifier;
         Version         : in     VersionSpec;
         Defined_In      : in     Container_Forward.Ref;
         Contents        : in     Contained.Impl.Contained_Seq.Sequence;
         Contained_View  : in     Contained.Impl.Object_Ptr;
         IDLType_View    : in     IDLType.Impl.Object_Ptr;
         Base_Interfaces : in     AbstractInterfaceDefSeq)
      is
      begin
         InterfaceDef.Impl.Init
           (InterfaceDef.Impl.Object_Ptr (Self),
            Real_Object,
            Def_Kind,
            Id,
            Name,
            Version,
            Defined_In,
            Contents,
            Contained_View,
            IDLType_View,
            To_InterfaceDefSeq (Base_Interfaces),
            True);
      end Init;

      ------------------------
      -- To_InterfaceDefSeq --
      ------------------------

      function To_InterfaceDefSeq
        (Item : in AbstractInterfaceDefSeq)
         return InterfaceDefSeq
      is
         Result : InterfaceDefSeq;

      begin
         for J in 1 .. Length (Item) loop
            Append
              (Result,
               InterfaceDef.Convert_Forward.To_Forward
               (InterfaceDef.Ref
                (AbstractInterfaceDef.Convert_Forward.To_Ref
                 (Element_Of (Item, J)))));
         end loop;

         return Result;
      end To_InterfaceDefSeq;

   end Internals;

end CORBA.Repository_Root.AbstractInterfaceDef.Impl;
