--  A variant of the test setup that uses a POA instead of an SOA.

--  $Id$

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with CORBA;
with CORBA.Test_Object; use CORBA.Test_Object;

with Droopi.POA_Types;
with Droopi.Obj_Adapters;
with Droopi.Objects;
with Droopi.ORB; use Droopi.ORB;
with Droopi.POA;
with Droopi.POA.Basic_POA; use Droopi.POA.Basic_POA;
with Droopi.References;
with Droopi.References.IOR;
with Droopi.Setup.Test; use Droopi.Setup.Test;

with Droopi.POA_Config;
with Droopi.POA_Config.Minimum;

package body Droopi.Setup.Test_CORBA is

   My_Servant : Droopi.POA_Types.Servant_Access;
   Obj_Adapter : Droopi.POA_Types.Obj_Adapter_Access;

   procedure Initialize_CORBA_Test_Object is
   begin
      Put ("Initializing OA confiuration");
      Droopi.POA_Config.Set_Configuration
        (new Droopi.POA_Config.Minimum.Minimum_Configuration);
      Put ("Creating object adapter...");
      Obj_Adapter := new Droopi.POA.Basic_POA.Basic_Obj_Adapter;
      Droopi.POA.Basic_POA.Create (Basic_Obj_Adapter (Obj_Adapter.all)'Access);
      --  Create object adapter

      Set_Object_Adapter
        (The_ORB, Droopi.Obj_Adapters.Obj_Adapter_Access (Obj_Adapter));
      --  Link object adapter with ORB.

      My_Servant := new CORBA.Test_Object.My_Object;
      --  Create application server object.
      CORBA.Test_Object.Create (My_Object (My_Servant.all));

      declare
         My_Id : aliased Droopi.Objects.Object_Id
           := Droopi.Obj_Adapters.Export
           (Droopi.Obj_Adapters.Obj_Adapter_Access (Obj_Adapter),
            Droopi.Objects.Servant_Access (My_Servant));
         --  Register it with the SOA.

      begin
         Create_Reference (The_ORB, My_Id'Unchecked_Access, My_Ref);
         --  Obtain object reference.

         Put_Line ("Registered object: " & Droopi.Objects.Image (My_Id));
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

end Droopi.Setup.Test_CORBA;
