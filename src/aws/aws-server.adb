------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

with Ada.Calendar;
with Ada.Exceptions;

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with AWS.Config.Set;
with AWS.Dispatchers.Callback;
with AWS.Log;
with AWS.Messages;
with AWS.MIME;
with AWS.OS_Lib;
with AWS.Session.Control;

with AWS.Status;
with AWS.Status.Translate_Table;
with AWS.Templates;
with AWS.Utils;
with AWS.Object_Adapter;

with PolyORB.ORB;
with PolyORB.Setup;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;
with PolyORB.Exceptions;
with PolyORB.References;
with PolyORB.Obj_Adapters;
with PolyORB.POA_Config;
with PolyORB.POA_Config.Root_POA;
with PolyORB.POA_Types;

with PolyORB.Types;
with PolyORB.Log;

with PolyORB.POA;
with PolyORB.POA.Basic_POA;
with PolyORB.POA_Policies;
with PolyORB.POA_Policies.Request_Processing_Policy.Use_Default_Servant;
with PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple;
with PolyORB.POA_Policies.Lifespan_Policy.Persistent;
with PolyORB.POA_Policies.Implicit_Activation_Policy.No_Activation;
with PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain;
with PolyORB.POA_Manager.Basic_Manager;

package body AWS.Server is

   use Ada;

   use PolyORB.Log;
   package L is
     new PolyORB.Log.Facility_Log ("aws.server");
   procedure O (Message : in Standard.String; Level : Log_Level := Critical)
     renames L.Output;
   --  the polyorb logging facility

   Security_Initialized : Boolean := False;

   procedure Free is new Ada.Unchecked_Deallocation
     (Dispatchers.Handler'Class, Dispatchers.Handler_Class_Access);

   --     protected File_Upload_UID is

   --        procedure Get (ID : out Natural);
   --        --  returns a UID for file upload. This is to ensure that files
   --        --  coming from clients will always have different name.
   --     private
   --        UID : Natural := 0;
   --     end File_Upload_UID;

   procedure Start
     (The_Server : in out HTTP'Class;
      Dispatcher : in     Dispatchers.Handler'Class);
   --  Start web server with current configuration.

   procedure Init_AWS;
   --  this procedure is called when the personality is started

   --------------------
   -- Initialization --
   --------------------

   procedure Initialization is
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
      use PolyORB.Log.Internals;
   begin
      pragma Debug (O ("AWS.initialization: initializing PolyORB"));
      O ("AWS.initialization: initializing PolyORB");

      if not Is_Initialized then
         Initialize_World;
      end if;
      --  We initialize PolyORB

   end Initialization;


   ---------
   -- Run --
   ---------

   procedure Run is
      use Text_IO;
   begin
      pragma Debug (O ("AWS.Server.Run"));
      PolyORB.ORB.Run (PolyORB.Setup.The_ORB, May_Poll => True);
   end Run;

   --------------
   -- Init_AWS --
   --------------

   procedure Init_AWS is
      use PolyORB.POA;
      use PolyORB.POA.Basic_POA;
      use PolyORB.POA_Policies;
      use PolyORB.POA_Policies.Request_Processing_Policy.
        Use_Default_Servant;
      use PolyORB.Setup;
      use PolyORB.Obj_Adapters;

      Root_Poa : PolyORB.POA.Obj_Adapter_Access
        := PolyORB.POA.Obj_Adapter_Access
        (PolyORB.ORB.Object_Adapter (PolyORB.Setup.The_ORB));
   begin
      if Root_Poa = null then
         pragma Debug (O ("AWS_Init: no root POA found: "
                          & "attempting to create one"));
         declare
            Basic_Root_Poa : constant PolyORB.POA.Basic_POA.
              Basic_Obj_Adapter_Access :=
              new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
            Root_Config : constant PolyORB.POA_Config.Configuration_Access :=
              new PolyORB.POA_Config.Root_POA.Root_POA_Configuration;
         begin
            PolyORB.POA_Config.Root_POA.Initialize
              (PolyORB.POA_Config.Root_POA.Root_POA_Configuration
               (Root_Config.all));
            PolyORB.POA_Config.Set_Configuration (Root_Config);
            pragma Debug (O ("AWS_Init: root POA configuration set"));

            PolyORB.POA.Basic_POA.Create (Basic_Root_Poa);
            pragma Debug (O ("AWS_Init: root POA created"));

            Root_Poa := PolyORB.POA.Obj_Adapter
              (Basic_Root_Poa.all)'Access;
            PolyORB.ORB.Set_Object_Adapter
              (PolyORB.Setup.The_ORB,
               PolyORB.Obj_Adapters.Obj_Adapter_Access (Root_Poa));
            pragma Debug (O ("AWS_Init: root POA attached to the ORB"));

         end;
      else
         pragma Debug (O ("AWS_Init: the root POA already exists: "
                          & "assuming that everything is correct"));
         null;
      end if;

   end Init_AWS;


   protected Counter is

      procedure Add;
      --  Add one to the server counter.

      procedure Remove;
      --  Removes one to the server counter.

      entry Zero;
      --  Accepted only when counter is equal to 0 (no more active server)

   private

      C : Natural := 0;

   end Counter;

   ------------
   -- Config --
   ------------

   function Config (The_Server : in HTTP'Class) return AWS.Config.Object is
   begin
      return The_Server.Properties;
   end Config;

   -------------
   -- Counter --
   -------------

   protected body Counter is

      ---------
      -- Add --
      ---------

      procedure Add is
      begin
         C := C + 1;
      end Add;

      ------------
      -- Remove --
      ------------

      procedure Remove is
      begin
         C := C - 1;
      end Remove;

      ----------
      -- Zero --
      ----------

      entry Zero when C = 0 is
      begin
         null;
      end Zero;

   end Counter;

   ------------------------------------------
   -- Default_Unexpected_Exception_Handler --
   ------------------------------------------

   procedure Default_Unexpected_Exception_Handler
     (E      : in     Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : in     Exceptions.Data;
      Answer : in out Response.Data)
   is
      use Ada.Exceptions;
      use type Templates.Translate_Table;
      pragma Unreferenced (Log);

      Fatal_Error_Template  : constant String := "500.tmplt";
   begin
      if Error.Fatal then
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Fatal error, slot"
            & Positive'Image (Error.Slot) & " is dead now.");
         Text_IO.New_Line (Text_IO.Current_Error);

         Text_IO.Put_Line
           (Text_IO.Current_Error, Exception_Information (E));

      else
         if AWS.OS_Lib.Is_Regular_File (Fatal_Error_Template) then
            Answer := Response.Build
              (MIME.Text_HTML,
               String'(Templates.Parse
                       (Fatal_Error_Template,
                        Status.Translate_Table (Error.Request)
                        & Templates.Assoc
                        ("EXCEPTION", Exception_Information (E)))),
               Messages.S500);
         else
            Answer := Response.Build
              (MIME.Text_HTML,
               "Internal Server Error.<br>"
               & "Please, send the following information to the Web "
               & "Master, thanks.<br><hr><br>"
               & "<pre>" & Exception_Information (E) & "</pre>"
               & "<br><hr>",
               Messages.S500);
         end if;
      end if;
   end Default_Unexpected_Exception_Handler;

   ---------------------
   -- File_Upload_UID --
   ---------------------

   --     protected body File_Upload_UID is

   --        ---------
   --        -- Get --
   --        ---------

   --        procedure Get (ID : out Natural) is
   --        begin
   --           ID  := UID;
   --           UID := UID + 1;
   --        end Get;

   --     end File_Upload_UID;

   ---------
   -- Set --
   ---------

   procedure Set
     (The_Server : in out HTTP'Class;
      Dispatcher : in     Dispatchers.Handler'Class)
   is
      Old : Dispatchers.Handler_Class_Access := The_Server.Dispatcher;
   begin
      The_Server.Dispatcher_Sem.Write;

      The_Server.Dispatcher :=
       new Dispatchers.Handler'Class'(Dispatcher);

      The_Server.Dispatcher_Sem.Release_Write;

      Free (Old);
   end Set;

   ------------------
   -- Set_Security --
   ------------------

   procedure Set_Security (Certificate_Filename : in String) is

      pragma Warnings (Off);
      pragma Unreferenced (Certificate_Filename);
      pragma Warnings (On);

   begin
      Security_Initialized := True;
      --  Net.SSL.Initialize (Certificate_Filename);
   end Set_Security;

   --------------------------------------
   -- Set_Unexpected_Exception_Handler --
   --------------------------------------

   procedure Set_Unexpected_Exception_Handler
     (The_Server : in out HTTP'Class;
      Handler    : in     Exceptions.Unexpected_Exception_Handler) is
   begin
      if The_Server.Shutdown then
         The_Server.Exception_Handler := Handler;
      else
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Could not change exception handler on the active server.");
      end if;
   end Set_Unexpected_Exception_Handler;

   --------------------------
   -- Get_Server_Reference --
   --------------------------

   function Get_Server_Reference
     (The_Server : HTTP'Class)
     return PolyORB.References.Ref
   is
   begin
      return The_Server.Reference;
   end Get_Server_Reference;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (The_Server : in out HTTP'Class)
   is
   begin

      The_Server.Shutdown := True;

      if CNF.Session (The_Server.Properties) then
         Session.Control.Shutdown;
      end if;

      --  Close logs, this ensure that all data will be written to the file.

      Stop_Log (The_Server);

      Stop_Error_Log (The_Server);

      --  Server removed

      Counter.Remove;
   end Shutdown;

   -----------
   -- Start --
   -----------

   procedure Start
     (The_Server                : in out HTTP'Class;
      Name                      : in     String;
      Callback                  : in     Response.Callback;
      Max_Connection            : in     Positive  := Default.Max_Connection;
      Admin_URI                 : in     String    := Default.Admin_URI;
      Port                      : in     Positive  := Default.Server_Port;
      Security                  : in     Boolean   := False;
      Session                   : in     Boolean   := False;
      Case_Sensitive_Parameters : in     Boolean   := True;
      Upload_Directory          : in     String    := Default.Upload_Directory;
      Line_Stack_Size           : in     Positive  := Default.Line_Stack_Size)
   is
   begin
      CNF.Set.Server_Name      (The_Server.Properties, Name);
      CNF.Set.Admin_URI        (The_Server.Properties, Admin_URI);
      CNF.Set.Server_Port      (The_Server.Properties, Port);
      CNF.Set.Security         (The_Server.Properties, Security);
      CNF.Set.Session          (The_Server.Properties, Session);
      CNF.Set.Upload_Directory (The_Server.Properties, Upload_Directory);
      CNF.Set.Max_Connection   (The_Server.Properties, Max_Connection);
      CNF.Set.Line_Stack_Size  (The_Server.Properties, Line_Stack_Size);

      CNF.Set.Case_Sensitive_Parameters
        (The_Server.Properties, Case_Sensitive_Parameters);

      Start (The_Server, Dispatchers.Callback.Create (Callback));
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start
     (The_Server : in out HTTP'Class;
      Callback   : in     Response.Callback;
      Config     : in     AWS.Config.Object) is
   begin
      The_Server.Properties := Config;
      Start (The_Server, Dispatchers.Callback.Create (Callback));
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start
     (The_Server : in out HTTP'Class;
      Dispatcher : in     Dispatchers.Handler'Class;
      Config     : in     AWS.Config.Object) is
   begin
      The_Server.Properties := Config;
      Start (The_Server, Dispatcher);
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start
     (The_Server : in out HTTP'Class;
      Dispatcher : in     Dispatchers.Handler'Class)
   is
   begin

      --  If it is an SSL connection, initialize the SSL library

      if not Security_Initialized
        and then CNF.Security (The_Server.Properties)
      then
         Security_Initialized := True;
         --  Net.SSL.Initialize (CNF.Certificate);
      end if;

      The_Server.Dispatcher := new Dispatchers.Handler'Class'(Dispatcher);

      --  Started time

      The_Server.Start_Time := Calendar.Clock;

      --  Initialize session server.

      if AWS.Config.Session (The_Server.Properties) then
         AWS.Session.Control.Start
           (Session_Check_Interval => CNF.Session_Cleanup_Interval,
            Session_Lifetime       => CNF.Session_Lifetime);
      end if;

      Counter.Add;

      pragma Debug (O ("Start: attempting to create a new POA"));
      declare
         use PolyORB.POA.Basic_POA;
         use PolyORB.POA_Policies;
         use PolyORB.POA_Manager;
         use PolyORB.POA_Manager.Basic_Manager;
         use PolyORB.Setup;
         use PolyORB.Obj_Adapters;
         The_Poa : PolyORB.POA.Obj_Adapter_Access;
         Root_Poa : constant PolyORB.Obj_Adapters.Obj_Adapter_Access :=
           PolyORB.ORB.Object_Adapter (PolyORB.Setup.The_ORB);
         Error : PolyORB.Exceptions.Error_Container;
         Policies : PolicyList;
         The_Poa_Manager : constant Basic_POA_Manager_Access :=
           new Basic_POA_Manager;
      begin
         pragma Assert (Root_Poa /= null);

         PolyORB.POA_Policies.Policy_Sequences.Append
           (Policies, PolyORB.POA_Policies.Request_Processing_Policy.
            Use_Default_Servant.Create.all'Access);
         --  This is what we need

         PolyORB.POA_Policies.Policy_Sequences.Append
           (Policies, PolyORB.POA_Policies.Id_Uniqueness_Policy.
            Multiple.Create.all'Access);
         --  This is required by Use_Default_Servant

         PolyORB.POA_Policies.Policy_Sequences.Append
           (Policies, PolyORB.POA_Policies.Lifespan_Policy.
            Persistent.Create.all'Access);
         --  To get rid of the ";pf=..." in URIs

         PolyORB.POA_Policies.Policy_Sequences.Append
           (Policies, PolyORB.POA_Policies.Servant_Retention_Policy.
            Non_Retain.Create.all'Access);
         --  To get rid of the ";sys" in URIs

         PolyORB.POA_Policies.Policy_Sequences.Append
           (Policies, PolyORB.POA_Policies.Implicit_Activation_Policy.
            No_Activation.Create.all'Access);
         --  Activation policy is incompatible with Non_Retain, so we
         --  use No_Activation.

         pragma Debug (O ("Start: set POA policies"));

         Create (The_Poa_Manager);
         pragma Debug (O ("Start: Created a new POA Manager"));

         PolyORB.POA.Basic_POA.Create_POA
           (PolyORB.POA.Basic_POA.Basic_Obj_Adapter
            (Root_Poa.all)'Access,
            PolyORB.Types.To_PolyORB_String
            (CNF.Server_Name
             (The_Server.Properties)),
            POAManager_Access (The_Poa_Manager),
            Policies,
            The_Poa,
            Error);

         The_Poa.Adapter_Activator := new Object_Adapter.AWS_AdapterActivator;
         --  We set an Adapter Activator which will allow to bypass
         --  subpath errors when searching the right POA

         if PolyORB.Exceptions.Found (Error) then
            pragma Debug (O ("Start: unable to create a new POA", Critical));
            null;
         else
            pragma Debug (O ("Start: a new POA has been created"));


            PolyORB.POA_Manager.Basic_Manager.Activate
              (The_Poa_Manager,
               Error);

            if PolyORB.Exceptions.Found (Error) then
               pragma Debug (O ("AWS_Init: "
                                & "unable to activate the POA Manager",
                                Critical));
               null;
            end if;

            PolyORB.POA.Basic_POA.Set_Servant
              (PolyORB.POA.Basic_POA.Basic_Obj_Adapter (The_Poa.all)'Access,
               The_Server'Unchecked_Access,
               Error);

            The_Poa.Default_Servant := The_Server'Unchecked_Access;

            if PolyORB.Exceptions.Found (Error) then
               pragma Debug (O ("Start: unable to register the servant"));
               return;
            else
               declare
                  use PolyORB.Types;
                  use PolyORB.ORB;
                  use PolyORB.Setup;
                  use PolyORB.POA.Basic_POA;
                  use PolyORB.POA_Types;
                  use PolyORB.Obj_Adapters;
                  Servant_Id : Object_Id_Access;
               begin
                  Servant_To_Id
                    (PolyORB.POA.Basic_POA.Basic_Obj_Adapter
                     (The_Poa.all)'Access,
                     The_Server'Unchecked_Access,
                     Servant_Id,
                     Error);

                  if PolyORB.Exceptions.Found (Error) then
                     pragma Debug
                       (O ("Start: unable to register the servant"));
                     null;
                  else
                     Create_Reference (The_ORB,
                                       Servant_Id,
                                       "aws",
                                       The_Server.Reference);
                  end if;
               end;
            end if;
         end if;
      end;
   end Start;

   ---------------------
   -- Start_Error_Log --
   ---------------------

   procedure Start_Error_Log
     (The_Server        : in out HTTP'Class;
      Split_Mode        : in     Log.Split_Mode := Log.None;
      Filename_Prefix   : in     String         := "")
   is
      use type AWS.Log.Split_Mode;
   begin
      if Split_Mode /= Log.None then
         CNF.Set.Error_Log_Split_Mode
           (The_Server.Properties, Log.Split_Mode'Image (Split_Mode));
      end if;

      if Filename_Prefix /= "" then
         CNF.Set.Error_Log_Filename_Prefix
           (The_Server.Properties, Filename_Prefix);
      end if;

      Log.Start
        (The_Server.Error_Log,
         Log.Split_Mode'Value
         (CNF.Error_Log_Split_Mode (The_Server.Properties)),
         CNF.Log_File_Directory (The_Server.Properties),
         CNF.Error_Log_Filename_Prefix (The_Server.Properties));
   end Start_Error_Log;

   ---------------
   -- Start_Log --
   ---------------

   procedure Start_Log
     (The_Server        : in out HTTP'Class;
      Split_Mode        : in     Log.Split_Mode := Log.None;
      Filename_Prefix   : in     String         := "")
   is
      use type AWS.Log.Split_Mode;
   begin
      if Split_Mode /= Log.None then
         CNF.Set.Log_Split_Mode
           (The_Server.Properties, Log.Split_Mode'Image (Split_Mode));
      end if;

      if Filename_Prefix /= "" then
         CNF.Set.Log_Filename_Prefix
           (The_Server.Properties, Filename_Prefix);
      end if;

      Log.Start
        (The_Server.Log,
         Log.Split_Mode'Value (CNF.Log_Split_Mode (The_Server.Properties)),
         CNF.Log_File_Directory (The_Server.Properties),
         CNF.Log_Filename_Prefix (The_Server.Properties));
   end Start_Log;

   --------------------
   -- Stop_Error_Log --
   --------------------

   procedure Stop_Error_Log (The_Server : in out HTTP'Class) is
   begin
      Log.Stop (The_Server.Error_Log);
   end Stop_Error_Log;

   --------------
   -- Stop_Log --
   --------------

   procedure Stop_Log (The_Server : in out HTTP'Class) is
   begin
      Log.Stop (The_Server.Log);
   end Stop_Log;

   ----------
   -- Wait --
   ----------

   procedure Wait (Mode : in Termination := No_Server) is
   begin
      case Mode is
         when No_Server =>
            Counter.Zero;

         when Q_Key_Pressed =>
            declare
               K : Character;
            begin
               loop
                  Text_IO.Get_Immediate (K);
                  exit when K = 'q' or else K = 'Q';
               end loop;
            end;

         when Forever =>
            loop
               delay Duration'Last;
            end loop;
      end case;
   end Wait;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin

   Register_Module
     (Module_Info'
     (Name => +"aws",
      Conflicts => Empty,
      Depends => Empty,
      Provides => Empty,
      Init => Init_AWS'Access));
   --  we register the aws personality

end AWS.Server;
