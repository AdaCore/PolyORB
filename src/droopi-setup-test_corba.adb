--  A variant of the test setup that uses a POA instead of an SOA.

--  $Id$

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with CORBA;
with CORBA.Test_Object; use CORBA.Test_Object;

with CORBA.ServerRequest;
pragma Warnings (Off, CORBA.ServerRequest);
--  XXX Not used for now.

with PortableServer.POA;
pragma Warnings (Off, PortableServer.POA);
--  XXX Not used for now.

with PolyORB.POA_Types;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.ORB; use PolyORB.ORB;
with PolyORB.POA;
with PolyORB.POA.Basic_POA; use PolyORB.POA.Basic_POA;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Setup.Test; use PolyORB.Setup.Test;

with PolyORB.POA_Config;
with PolyORB.POA_Config.Minimum;
with PolyORB.POA_Manager;

package body PolyORB.Setup.Test_CORBA is

   My_Servant : PolyORB.POA_Types.Servant_Access;
   Obj_Adapter : PolyORB.POA_Types.Obj_Adapter_Access;

   procedure Initialize_CORBA_Test_Object is
   begin
      Put ("Initializing OA confiuration... ");
      PolyORB.POA_Config.Set_Configuration
        (new PolyORB.POA_Config.Minimum.Minimum_Configuration);
      Put ("Creating object adapter... ");
      Obj_Adapter := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
      PolyORB.POA.Basic_POA.Create (Basic_Obj_Adapter (Obj_Adapter.all)'Access);
      --  Create object adapter

      Set_Object_Adapter
        (The_ORB, PolyORB.Obj_Adapters.Obj_Adapter_Access (Obj_Adapter));
      --  Link object adapter with ORB.

      My_Servant := new CORBA.Test_Object.My_Object;
      --  Create application server object.
      CORBA.Test_Object.Create (My_Object (My_Servant.all));

      PolyORB.POA_Manager.Activate
        (PolyORB.POA_Manager.POAManager_Access
         (PolyORB.POA_Manager.Entity_Of
          (POA.Obj_Adapter (Obj_Adapter.all).POA_Manager)));

      declare
         My_Id : constant Objects.Object_Id_Access
           := new Objects.Object_Id'
           (PolyORB.Obj_Adapters.Export
            (PolyORB.Obj_Adapters.Obj_Adapter_Access (Obj_Adapter),
             PolyORB.Objects.Servant_Access (My_Servant)));
         --  XXX memory leak
         --  Register it with the SOA.

      begin
         Create_Reference (The_ORB, My_Id, My_Ref);
         --  Obtain object reference.

         Put_Line ("Registered object: " & PolyORB.Objects.Image (My_Id.all));
         Put_Line ("Reference is     : " & References.Image (My_Ref));
         begin
            Put_Line ("IOR is           : "
                      & CORBA.To_Standard_String
                      (References.IOR.Object_To_String
                       ((Ref => My_Ref,
                         Type_Id => CORBA.To_CORBA_String
                         ("IDL:Echo:1.0")))));
         exception
            when E : others =>
               Put_Line ("Warning: Object_To_String raised:");
               Put_Line (Ada.Exceptions.Exception_Information (E));
         end;

      end;
      Put_Line (" done.");
   end Initialize_CORBA_Test_Object;

end PolyORB.Setup.Test_CORBA;
