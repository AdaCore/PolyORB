with CORBA;
with PortableServer;

package CosNaming.NamingContext.Impl is

   Key_Size : constant := 4;
   type Key_Type is new String (1 .. Key_Size);

   type Bound_Object;
   type Bound_Object_Ptr is access Bound_Object;

   type Object;
   type Object_Ptr is access all Object'Class;

   type Bound_Object is
      record
         BN   : NameComponent;
         BT   : BindingType;
         Obj  : CORBA.Object.Ref;
         Prev : Bound_Object_Ptr;
         Next : Bound_Object_Ptr;
         NC   : Object_Ptr;
      end record;

   type Object is
     new PortableServer.Servant_Base with
      record
         Key  : Key_Type;
         Self : Object_Ptr;
         Prev : Object_Ptr;
         Next : Object_Ptr;
         Head : Bound_Object_Ptr;
         Tail : Bound_Object_Ptr;
      end record;

   procedure Bind
     (Self : access Object;
      N    : in CosNaming.Name;
      Obj  : in CORBA.Object.Ref);

   procedure Rebind
     (Self : access Object;
      N    : in CosNaming.Name;
      Obj  : in CORBA.Object.Ref);

   procedure Bind_Context
     (Self : access Object;
      N    : in CosNaming.Name;
      NC   : in CosNaming.NamingContext.Ref);

   procedure Rebind_Context
     (Self : access Object;
      N    : in CosNaming.Name;
      NC   : in CosNaming.NamingContext.Ref);

   function Resolve
     (Self : access Object;
      N    : in CosNaming.Name)
     return CORBA.Object.Ref;

   procedure Unbind
     (Self : access Object;
      N    : in CosNaming.Name);

   function New_Context
     (Self : access Object)
     return CosNaming.NamingContext.Ref;

   function New_Context
     return Object_Ptr;

   function Bind_New_Context
     (Self : access Object;
      N    : in CosNaming.Name)
     return CosNaming.NamingContext.Ref;

   procedure Destroy
     (Self : access Object);

   procedure List
     (Self     : access Object;
      How_Many : in CORBA.Unsigned_Long;
      BL       : out CosNaming.BindingList;
      BI       : out CosNaming.BindingIterator_Forward.Ref);

end CosNaming.NamingContext.Impl;
