--  This package provides glue codee between PolyORB's
--  ServantActivator and CORBA specific ServantActivator.

with PortableServer.ServantActivator;

with PolyORB.POA_Types;
with PolyORB.Servants;

package PolyORB.CORBA_P.ServantActivator is

   package PPT renames PolyORB.POA_Types;

   type CORBA_ServantActivator is new PPT.ServantActivator with private;

   procedure Create
     (Self :    out PPT.ServantActivator_Access;
      SA   : access PortableServer.ServantActivator.Ref'Class);

   function Get_Servant_Manager
     (Self : CORBA_ServantActivator)
     return PortableServer.ServantActivator.Ref'Class;

   function Incarnate
     (Self    : access CORBA_ServantActivator;
      Oid     : in     PPT.Object_Id;
      Adapter : access PPT.Obj_Adapter'Class)
     return PolyORB.Servants.Servant_Access;

   procedure Etherealize
     (Self                  : access CORBA_ServantActivator;
      Oid                   : in     PPT.Object_Id;
      Adapter               : access PPT.Obj_Adapter'Class;
      Serv                  : in     PolyORB.Servants.Servant_Access;
      Cleanup_In_Progress   : in     Boolean;
      Remaining_Activations : in     Boolean);

private

   type CORBA_ServantActivator is new PPT.ServantActivator with record
      SA : PortableServer.ServantActivator.SA_Ptr;
   end record;

end PolyORB.CORBA_P.ServantActivator;
