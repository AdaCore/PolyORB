with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

with PolyORB.Obj_Adapters.Simple;
with PolyORB.Objects;
with PolyORB.ORB;
pragma Elaborate_All (PolyORB.ORB);

with PolyORB.References;
with PolyORB.References.IOR;
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

   Obj_Adapter : PolyORB.Obj_Adapters.Obj_Adapter_Access;

   MOMA_Obj : constant MOMA.Message_Pool.Object_Acc
     := new MOMA.Message_Pool.Object;
   MOMA_Servant : PolyORB.Objects.Servant_Access
     := To_PolyORB_Servant (MOMA_Obj);
   MOMA_Ref : PolyORB.References.Ref;

begin

   --
   --  Initialize Object Adapter
   --

   Put ("Creating object adapter...");
   Obj_Adapter := new PolyORB.Obj_Adapters.Simple.Simple_Obj_Adapter;
   PolyORB.Obj_Adapters.Create (Obj_Adapter);
   --  Create object adapter

   Set_Object_Adapter (The_ORB, Obj_Adapter);
   --  Link object adapter with ORB.

   --
   --  Register the Object to the ORB
   --

   declare
      MOMA_Servant_Id : constant Object_Id_Access
        := new Object_Id'(PolyORB.Obj_Adapters.Export
                          (Obj_Adapter, MOMA_Servant));
      --  Register it with the SOA.

   begin
      PolyORB.Obj_Adapters.Simple.Set_Interface_Description
        (PolyORB.Obj_Adapters.Simple.Simple_Obj_Adapter (Obj_Adapter.all),
         MOMA_Servant_Id, MOMA.Message_Pool.If_Desc);
      --  Set object description.

      Put_Line ("Registered object: " &
                PolyORB.Objects.Image (MOMA_Servant_Id.all));
      Create_Reference (The_ORB, MOMA_Servant_Id, "MOMA", MOMA_Ref);

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
