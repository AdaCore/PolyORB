--  Set up a test ORB.

--  $Id$

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Droopi.Any;
with Droopi.Any.NVList;
with Droopi.Filters;
with Droopi.Filters.Slicers;
with Droopi.Log;
with Droopi.Obj_Adapters.Simple;
with Droopi.Objects;
with Droopi.ORB.Interface;

with Droopi.Binding_Data.Test;
with Droopi.Binding_Data.IIOP;
with Droopi.Binding_Data.SRP;

with Droopi.Components;

with Droopi.Protocols;
with Droopi.Protocols.Echo;
with Droopi.Protocols.GIOP;
with Droopi.Protocols.SRP;

with Droopi.References;
with Droopi.References.IOR;

with Droopi.Requests;

with Droopi.Smart_Pointers;
with Droopi.Sockets;
with Droopi.Test_Object;
with Droopi.Transport.Sockets;
with Droopi.Types;

with Droopi.No_Tasking;
with Droopi.ORB.Task_Policies;

package body Droopi.Setup.CORBA_Client is

   use Droopi.Binding_Data;
   use Droopi.Filters;
   use Droopi.Objects;
   use Droopi.ORB;
   use Droopi.Sockets;
   use Droopi.Transport;
   use Droopi.Transport.Sockets;


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
