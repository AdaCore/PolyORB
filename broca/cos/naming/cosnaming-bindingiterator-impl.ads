with CORBA;
with PortableServer;

package CosNaming.BindingIterator.Impl is

   package Bindings renames IDL_SEQUENCE_CosNaming_Binding;

   type Binding_Element_Array_Ptr is access Bindings.Element_Array;

   type Object is
     new PortableServer.Servant_Base with
      record
         Index : Natural;
         Table : Binding_Element_Array_Ptr;
      end record;
   type BindingIterator_Ptr is access all Object'Class;

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

end CosNaming.BindingIterator.Impl;
