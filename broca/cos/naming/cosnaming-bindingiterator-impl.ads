with CORBA;
with PortableServer;

package CosNaming.BindingIterator.Impl is

   package Bindings renames IDL_SEQUENCE_CosNaming_Binding;

   type Binding_Element_Array_Ptr is access Bindings.Element_Array;

   type Object;
   type Object_Ptr is access all Object'Class;
   type Object is
     new PortableServer.Servant_Base with
      record
         Self  : Object_Ptr;
         Index : Natural;
         Table : Binding_Element_Array_Ptr;
      end record;

   procedure Next_One
     (Self    : access Object;
      B       : out CosNaming.Binding;
      Returns : out CORBA.Boolean);

   procedure Next_N
     (Self     : access Object;
      How_Many : in CORBA.Unsigned_Long;
      BL       : out CosNaming.BindingList;
      Returns  : out CORBA.Boolean);

   procedure Destroy
     (Self : access Object);

   function Create return Object_Ptr;

end CosNaming.BindingIterator.Impl;
