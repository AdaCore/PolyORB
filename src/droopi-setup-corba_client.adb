--  Set up a test ORB.

--  $Id$

with Ada.Text_IO; use Ada.Text_IO;

with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

with Droopi.Binding_Data.IIOP;
pragma Elaborate_All (Droopi.Binding_Data.IIOP);

with Droopi.Smart_Pointers;
pragma Elaborate_All (Droopi.Smart_Pointers);

with Droopi.No_Tasking;
with Droopi.ORB.Task_Policies;

with Droopi.ORB;
pragma Elaborate_All (Droopi.ORB);

package body Droopi.Setup.CORBA_Client is

   use Droopi.ORB;

   procedure Initialize_CORBA_Client
     (SL_Init : Parameterless_Procedure;
      TP : ORB.Tasking_Policy_Access) is
   begin

      -------------------------------
      -- Initialize all subsystems --
      -------------------------------

      Put ("Initializing subsystems...");

      Droopi.Log.Initialize;
      Put (" logging");
      --  Logging subsystem. Start this one first so we can debug
      --  problems in others.

      SL_Init.all;
      Put (" soft-links");
      --  Setup soft links.

      Droopi.Smart_Pointers.Initialize;
      Put (" smart-pointers");
      --  Depends on Soft_Links.

      -------------------------------------------
      -- Initialize personality-specific stuff --
      -------------------------------------------

      Droopi.Binding_Data.IIOP.Initialize;
      Put (" binding-iiop");

      --------------------------
      -- Create ORB singleton --
      --------------------------

      Setup.The_ORB := new ORB.ORB_Type (TP);

      Droopi.ORB.Create (Setup.The_ORB.all);
      Put (" ORB");

      Put_Line (" done");
   end Initialize_CORBA_Client;

begin

   Droopi.Setup.CORBA_Client.Initialize_CORBA_Client
     (Droopi.No_Tasking.Initialize'Access,
      new Droopi.ORB.Task_Policies.No_Tasking);

end Droopi.Setup.CORBA_Client;
