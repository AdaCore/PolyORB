----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with PortableServer;

package CORBA.Repository_Root.IRObject.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind);

   function Get_Real_Object (Self : access Object)
     return Object_Ptr;

   function get_def_kind
     (Self : access Object)
     return CORBA.Repository_Root.DefinitionKind;

   procedure destroy
     (Self : access Object);

private

   --  The "real_object" is a pointer to the real object
   --  The Def_Kind is an attribute
   type Object is
     new PortableServer.Servant_Base with record
        Real_Object : Object_Ptr;
        Def_Kind : CORBA.Repository_Root.DefinitionKind;
     end record;

   -------------------
   -- IRObject list --
   -------------------

   type IRObject_List_Cell;
   type IRObject_List is access IRObject_List_Cell;
   type IRObject_List_Cell is record
      Car : Object_Ptr;
      Cdr : IRObject_List;
   end record;

   Nil_List : constant IRObject_List := null;

   type IRObject_Iterator is new IRObject_List;

end CORBA.Repository_Root.IRObject.Impl;
