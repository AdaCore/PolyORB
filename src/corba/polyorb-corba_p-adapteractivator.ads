
--  This package provides glue codee between PolyORB's
--  AdapterActivator and CORBA specific AdapterActivator.

with PortableServer.AdapterActivator;

with PolyORB.Exceptions;
with PolyORB.POA_Types;

package PolyORB.CORBA_P.AdapterActivator is

   package PPT renames PolyORB.POA_Types;

   type CORBA_AdapterActivator is new PPT.AdapterActivator with private;

   procedure Create
     (Self :    out PPT.AdapterActivator_Access;
      AA   : access PortableServer.AdapterActivator.Ref'Class);

   function Get_Adapter_Activator
     (Self : CORBA_AdapterActivator)
     return PortableServer.AdapterActivator.Ref'Class;

   procedure Unknown_Adapter
     (Self   : access CORBA_AdapterActivator;
      Parent : access PPT.Obj_Adapter'Class;
      Name   : in     String;
      Result :    out Boolean;
      Error  : in out PolyORB.Exceptions.Error_Container);

private

   type CORBA_AdapterActivator is new PPT.AdapterActivator with record
      AA : PortableServer.AdapterActivator.AA_Ptr;
   end record;

end PolyORB.CORBA_P.AdapterActivator;
