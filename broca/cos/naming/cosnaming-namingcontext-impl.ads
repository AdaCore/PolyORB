with CORBA;
with PortableServer;

package CosNaming.NamingContext.Impl is

   NC_Id_Size : constant := 4;
   type NC_Id is new String (1 .. NC_Id_Size);

   type String_Access is access String;

   type Bounded_Object;
   type Bounded_Object_Ptr is access Bounded_Object;

   type Object;
   type NamingContext_Ptr is access all Object'Class;

   type Bounded_Object is
      record
         BN   : NameComponent;
         BT   : BindingType;
         Obj  : CORBA.Object.Ref;
         Prev : Bounded_Object_Ptr;
         Next : Bounded_Object_Ptr;
         NC   : NamingContext_Ptr;
      end record;

   type Object is
     new PortableServer.Servant_Base with
      record
         Id   : NC_Id;
         Self : NamingContext_Ptr;
         Prev : NamingContext_Ptr;
         Next : NamingContext_Ptr;
         Head : Bounded_Object_Ptr;
         Tail : Bounded_Object_Ptr;
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
