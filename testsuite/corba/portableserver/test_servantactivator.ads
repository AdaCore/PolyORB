with CORBA;
with PortableServer.ServantActivator;

package Test_ServantActivator is

   --  Activator that does nothing

   type Null_Activator_Ref is new PortableServer.ServantActivator.Ref
     with null record;

   type Null_Activator_Access is access all Null_Activator_Ref;

   function Incarnate
     (Self    : in Null_Activator_Ref;
      Oid     : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA_Forward.Ref)
     return PortableServer.Servant;

   procedure Etherealize
     (Self                  : in Null_Activator_Ref;
      Oid                   : in PortableServer.ObjectId;
      Adapter               : in PortableServer.POA_Forward.Ref;
      Serv                  : in PortableServer.Servant;
      Cleanup_In_Progress   : in CORBA.Boolean;
      Remaining_Activations : in CORBA.Boolean);

   --  Simple activator that creates a servant on demand

   type Simple_Activator_Ref is new PortableServer.ServantActivator.Ref
     with null record;

   type Simple_Activator_Access is access all Simple_Activator_Ref;

   function Incarnate
     (Self    : in Simple_Activator_Ref;
      Oid     : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA_Forward.Ref)
     return PortableServer.Servant;

   procedure Etherealize
     (Self                  : in Simple_Activator_Ref;
      Oid                   : in PortableServer.ObjectId;
      Adapter               : in PortableServer.POA_Forward.Ref;
      Serv                  : in PortableServer.Servant;
      Cleanup_In_Progress   : in CORBA.Boolean;
      Remaining_Activations : in CORBA.Boolean);

   procedure Run_Test_ServantActivator;

end Test_ServantActivator;
