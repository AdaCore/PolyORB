--  Set up a test ORB.

--  $Id$

with Ada.Text_IO; use Ada.Text_IO;

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

with PolyORB.Binding_Data.IIOP;
pragma Elaborate_All (PolyORB.Binding_Data.IIOP);

with PolyORB.Protocols.GIOP;
pragma Elaborate_All (PolyORB.Protocols.GIOP);

with PolyORB.Smart_Pointers;
pragma Elaborate_All (PolyORB.Smart_Pointers);

with PolyORB.No_Tasking;
with PolyORB.ORB.Task_Policies;

with PolyORB.ORB;
pragma Elaborate_All (PolyORB.ORB);

package body PolyORB.Setup.CORBA_Client is

   use PolyORB.ORB;

   procedure Initialize_CORBA_Client
     (SL_Init : Parameterless_Procedure;
      TP : ORB.Tasking_Policy_Access) is
   begin

      -------------------------------
      -- Initialize all subsystems --
      -------------------------------

      Put ("Initializing subsystems...");

      PolyORB.Log.Initialize;
      Put (" logging");
      --  Logging subsystem. Start this one first so we can debug
      --  problems in others.

      SL_Init.all;
      Put (" soft-links");
      --  Setup soft links.

      PolyORB.Smart_Pointers.Initialize;
      Put (" smart-pointers");
      --  Depends on Soft_Links.

      -------------------------------------------
      -- Initialize personality-specific stuff --
      -------------------------------------------

      PolyORB.Binding_Data.IIOP.Initialize;
      Put (" binding-iiop");

      PolyORB.Protocols.GIOP.Initialize;
      Put (" protocols-giop");

      --------------------------
      -- Create ORB singleton --
      --------------------------

      Setup.The_ORB := new ORB.ORB_Type (TP);

      PolyORB.ORB.Create (Setup.The_ORB.all);
      Put (" ORB");

      Put_Line (" done");
   end Initialize_CORBA_Client;

begin

   PolyORB.Setup.CORBA_Client.Initialize_CORBA_Client
     (PolyORB.No_Tasking.Initialize'Access,
      new PolyORB.ORB.Task_Policies.No_Tasking);

end PolyORB.Setup.CORBA_Client;
