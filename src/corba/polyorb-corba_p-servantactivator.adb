with CORBA.Impl;

with PolyORB.Smart_Pointers;

package body PolyORB.CORBA_P.ServantActivator is

   ------------
   -- Create --
   ------------

   procedure Create
     (Self :    out PPT.ServantActivator_Access;
      SA   : access PortableServer.ServantActivator.Ref'Class) is
   begin
      Self := new CORBA_ServantActivator;

      CORBA_ServantActivator (Self.all).SA
        := PortableServer.ServantActivator.SA_Ptr (SA);
   end Create;

   -------------------------
   -- Get_Servant_Manager --
   -------------------------

   function Get_Servant_Manager
     (Self : CORBA_ServantActivator)
     return PortableServer.ServantActivator.Ref'Class is
   begin
      return Self.SA.all;
   end Get_Servant_Manager;

   ---------------
   -- Incarnate --
   ---------------

   function Incarnate
     (Self    : access CORBA_ServantActivator;
      Oid     : in     PPT.Object_Id;
      Adapter : access PPT.Obj_Adapter'Class)
     return PolyORB.Servants.Servant_Access
   is
      CORBA_POA : PortableServer.POA_Forward.Ref;

      CORBA_Servant : PortableServer.Servant;

   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA,
         PolyORB.Smart_Pointers.Entity_Ptr (Adapter));

      CORBA_Servant := PortableServer.ServantActivator.Incarnate
        (PortableServer.ServantActivator.Ref'Class (Self.SA.all),
         PortableServer.ObjectId (Oid),
         CORBA_POA);

      return PolyORB.Servants.Servant_Access
        (PortableServer.To_PolyORB_Servant (CORBA_Servant));
   end Incarnate;

   -----------------
   -- Etherealize --
   -----------------

   procedure Etherealize
     (Self                  : access CORBA_ServantActivator;
      Oid                   : in     PPT.Object_Id;
      Adapter               : access PPT.Obj_Adapter'Class;
      Serv                  : in     PolyORB.Servants.Servant_Access;
      Cleanup_In_Progress   : in     Boolean;
      Remaining_Activations : in     Boolean)
   is
      CORBA_POA : PortableServer.POA_Forward.Ref;

      POA_Servant : constant PortableServer.Servant :=
        PortableServer.Servant (CORBA.Impl.To_CORBA_Servant (Serv));

   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA,
         PolyORB.Smart_Pointers.Entity_Ptr (Adapter));

      PortableServer.ServantActivator.Etherealize
        (PortableServer.ServantActivator.Ref'Class (Self.SA.all),
         PortableServer.ObjectId (Oid),
         CORBA_POA,
         POA_Servant,
         Cleanup_In_Progress,
         Remaining_Activations);
   end Etherealize;

end PolyORB.CORBA_P.ServantActivator;
