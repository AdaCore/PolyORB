package body PortableServer.ServantActivator is

   ---------------
   -- Incarnate --
   ---------------

   function Incarnate
     (Self    : in Ref;
      Oid     : in ObjectId;
      Adapter : in PortableServer.POA_Forward.Ref)
     return PortableServer.Servant
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Oid);
      pragma Unreferenced (Adapter);
      pragma Warnings (On);

   begin
      return null;
   end Incarnate;

   -----------------
   -- Etherealize --
   -----------------

   procedure Etherealize
     (Self                  : in Ref;
      Oid                   : in PortableServer.ObjectId;
      Adapter               : in PortableServer.POA_Forward.Ref;
      Serv                  : in PortableServer.Servant;
      Cleanup_In_Progress   : in CORBA.Boolean;
      Remaining_Activations : in CORBA.Boolean)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Oid);
      pragma Unreferenced (Adapter);
      pragma Unreferenced (Serv);
      pragma Unreferenced (Cleanup_In_Progress);
      pragma Unreferenced (Remaining_Activations);
      pragma Warnings (On);

   begin
      null;
   end Etherealize;

end PortableServer.ServantActivator;
