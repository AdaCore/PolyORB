with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.ORB;
pragma Elaborate_All (PolyORB.ORB);

with PolyORB.POA;
with PolyORB.POA.Basic_POA; use PolyORB.POA.Basic_POA;
with PolyORB.POA_Types;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.POA_Config;
with PolyORB.POA_Config.Minimum;
with PolyORB.POA_Manager;
with PolyORB.Setup;
with PolyORB.Types;

with PolyORB.Setup.No_Tasking_Server;
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with MOMA.Message_Pool; use MOMA.Message_Pool;

procedure Server is

   use PolyORB.Objects;
   use PolyORB.ORB;
   use PolyORB.Setup;
   use PolyORB.POA;

   Obj_Adapter : PolyORB.POA_Types.Obj_Adapter_Access;

   MOMA_Obj : constant MOMA.Message_Pool.Object_Acc :=
    new MOMA.Message_Pool.Object;
   MOMA_Servant : PolyORB.Objects.Servant_Access
     := To_PolyORB_Servant (MOMA_Obj);
   MOMA_Ref : PolyORB.References.Ref;

begin

   --
   --  Initialize Object Adapter
   --

   Put_Line ("Initializing OA configuration... ");
   PolyORB.POA_Config.Set_Configuration
     (new PolyORB.POA_Config.Minimum.Minimum_Configuration);

   Put_Line ("Creating object adapter... ");
   Obj_Adapter := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
   PolyORB.POA.Basic_POA.Create (Basic_Obj_Adapter (Obj_Adapter.all)'Access);
   --  Create object adapter

   Set_Object_Adapter
     (The_ORB, PolyORB.Obj_Adapters.Obj_Adapter_Access (Obj_Adapter));
   --  Link object adapter with ORB.

   PolyORB.POA_Manager.Activate
     (PolyORB.POA_Manager.POAManager_Access
      (PolyORB.POA_Manager.Entity_Of
       (PolyORB.POA.Obj_Adapter (Obj_Adapter.all).POA_Manager)));

   --
   --  Register the Object to the ORB
   --

   declare
      MOMA_Servant_Id : aliased PolyORB.Objects.Object_Id
        := PolyORB.Obj_Adapters.Export
        (PolyORB.Obj_Adapters.Obj_Adapter_Access (Obj_Adapter), MOMA_Servant);
      --  Register it with the OA.

   begin
      Put_Line ("Registered object: " &
                PolyORB.Objects.Image (MOMA_Servant_Id));
      Create_Reference (The_ORB, MOMA_Servant_Id'Access, "MOMA", MOMA_Ref);

      Put_Line (PolyORB.Types.To_Standard_String
                (PolyORB.References.IOR.Object_To_String (MOMA_Ref)));

   end;

   --
   --  Run the ORB
   --

   Put_Line ("Run the ORB !");
   Run (The_ORB, May_Poll => True);

exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Information (E));

end Server;
