------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . C O R B A _ P . T S S _ S T A T E _ M A C H I N E     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Annotations;
with PolyORB.ASN1;
with PolyORB.Binding_Data;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Security_Current;
with PolyORB.CORBA_P.TSS_State_Machine_Actions;
with PolyORB.Errors;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Obj_Adapter_QoS;
with PolyORB.Parameters;
with PolyORB.POA;
with PolyORB.QoS.Security_Contexts;
with PolyORB.QoS.Targets_Security;
with PolyORB.QoS.Transport_Contexts;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Request_QoS;
with PolyORB.Security.Authentication_Mechanisms;
with PolyORB.Security.Authority_Mechanisms;
with PolyORB.Security.Backward_Trust_Evaluators;
with PolyORB.Security.Forward_Trust_Evaluators;
with PolyORB.Security.Credentials;
with PolyORB.Security.Security_Manager;
with PolyORB.Security.Transport_Mechanisms;
with PolyORB.Security.Types;
with PolyORB.Tasking.Threads.Annotations;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package body PolyORB.CORBA_P.TSS_State_Machine is

   use PolyORB.CORBA_P.Interceptors_Hooks;
   use PolyORB.Log;
   use PolyORB.QoS;
   use PolyORB.QoS.Transport_Contexts;

   package L is
     new PolyORB.Log.Facility_Log ("polyorb.corba_p.tss_state_machine");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   procedure POA_Create
     (POA   :        PolyORB.POA.Obj_Adapter_Access;
      Error : in out PolyORB.Errors.Error_Container);

   procedure Server_Invoke
     (Self    : access PSPCE.Entity'Class;
      Request : access PolyORB.Requests.Request;
      Profile : PolyORB.Binding_Data.Profile_Access);

   procedure Throw_SAS_No_Permission
     (Request     : access PolyORB.Requests.Request;
      Context_Id  : PolyORB.Security.Types.Context_Id;
      Major       : PolyORB.Types.Long;
      Minor       : PolyORB.Types.Long;
      Error_Token : PolyORB.Security.Types.Stream_Element_Array_Access;
      Reply_QoS   : out
        PolyORB.QoS.Security_Contexts.QoS_Security_Context_Parameter_Access);
   --  Issue NO_PERMISSION system exception with SAS ContextError service
   --  context as result of execution of request

   procedure Initialize;

   Legacy_POA_Create    :
     PolyORB.CORBA_P.Interceptors_Hooks.POA_Create_Handler;
   Legacy_Server_Invoke :
     PolyORB.CORBA_P.Interceptors_Hooks.Server_Invoke_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Legacy_POA_Create := PolyORB.CORBA_P.Interceptors_Hooks.POA_Create;
      PolyORB.CORBA_P.Interceptors_Hooks.POA_Create :=
        POA_Create'Access;

      Legacy_Server_Invoke := PolyORB.CORBA_P.Interceptors_Hooks.Server_Invoke;
      PolyORB.CORBA_P.Interceptors_Hooks.Server_Invoke :=
        Server_Invoke'Access;
   end Initialize;

   ----------------
   -- POA_Create --
   ----------------

   procedure POA_Create
     (POA   :        PolyORB.POA.Obj_Adapter_Access;
      Error : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Obj_Adapter_QoS;
      use PolyORB.Parameters;
      use PolyORB.QoS.Targets_Security;
      use PolyORB.QoS.Targets_Security.Target_Mechanism_Lists;
      use PolyORB.Security.Authentication_Mechanisms;
      use PolyORB.Security.Transport_Mechanisms;
      use PolyORB.Security.Types;

      procedure Free is
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => QoS_Target_Security_Parameter,


         Name   => QoS_Target_Security_Parameter_Access);

      Creds : constant PolyORB.Security.Credentials.Credentials_List
        := PolyORB.Security.Security_Manager.Own_Credentials;

   begin
      declare
         use PolyORB.Security.Authority_Mechanisms;
         use PolyORB.Security.Backward_Trust_Evaluators;
         use PolyORB.Security.Forward_Trust_Evaluators;
         use PolyORB.Security.Types.OID_Lists;
         use PolyORB.ASN1;

         Disable_Unprotected           : constant Boolean
           := Get_Conf (POA.Name.all, "disable_unprotected", False);
         Transport_Mechanism_Name      : constant String
           := Get_Conf (POA.Name.all, "transport_mechanism", "");
         Authentication_Mechanism_Name : constant String
           := Get_Conf (POA.Name.all, "authentication_mechanism", "none");
         Authentication_Required       : constant Boolean
           := Get_Conf (POA.Name.all, "authentication_required", False);
         Backward_Trust_Rules_File     : constant String
           := Get_Conf (POA.Name.all, "backward_trust_rules_file", "");
         Privilege_Authorities         : constant String
           := Get_Conf (POA.Name.all, "privilege_authorities", "");

         QoS               : QoS_Target_Security_Parameter_Access
           := new QoS_Target_Security_Parameter;
         CM                : constant Target_Mechanism_Access
           := new Target_Mechanism;
         Identity_Types    : Identity_Token_Type := 0;
         Naming_Mechanisms : OID_Lists.List;

         procedure Copy_Naming_Mechanisms (NM : OID_Lists.List);

         ----------------------------
         -- Copy_Naming_Mechanisms --
         ----------------------------

         procedure Copy_Naming_Mechanisms (NM : OID_Lists.List) is
            Iter : OID_Lists.Iterator := First (NM);

         begin
            while not Last (Iter) loop
               Append (Naming_Mechanisms, Duplicate (Value (Iter).all));
               Next (Iter);
            end loop;
         end Copy_Naming_Mechanisms;

      begin
         QoS.Stateful            := False;
         QoS.Disable_Unprotected := Disable_Unprotected;

         --  Transport Mechanism

         if Transport_Mechanism_Name /= "" then
            CM.Transport :=
              PolyORB.Security.Security_Manager.Get_Transport_Mechanism
              (Transport_Mechanism_Name);

            if CM.Transport = null then
               raise Program_Error;
            end if;

            Identity_Types := Supported_Identity_Types (CM.Transport);
            Copy_Naming_Mechanisms
              (Supported_Naming_Mechanisms (CM.Transport));

         else
            CM.Transport := null;
         end if;

         --  Authentication layer

         if Authentication_Mechanism_Name /= "none" then
            CM.Authentication_Mechanism :=
              PolyORB.Security.Authentication_Mechanisms.
                Create_Target_Mechanism
                (Authentication_Mechanism_Name);
            CM.Authentication_Required  := Authentication_Required;

            Identity_Types :=
              Identity_Types
              or Get_Supported_Identity_Types (CM.Authentication_Mechanism);
            Copy_Naming_Mechanisms
              (Get_Supported_Naming_Mechanisms (CM.Authentication_Mechanism));

         else
            CM.Authentication_Mechanism := null;
            CM.Authentication_Required  := False;
         end if;

         --  Privilege authorities

         if Privilege_Authorities /= "" then
            Target_Authority_Mechanism_Lists.Append
              (CM.Authorities,
               Create_Target_Authority_Mechanism (Privilege_Authorities));
         end if;

         --  Backward trust evaluation

         if Backward_Trust_Rules_File /= "" then
            CM.Backward_Trust_Evaluator :=
              Create_Backward_Trust_Evaluator (Backward_Trust_Rules_File);

         else
            CM.Backward_Trust_Evaluator := null;
         end if;

         --  Forward trust evaluation

         CM.Forward_Trust_Evaluator := null;
         CM.Delegation_Required := False;
         --  Not supported yet

         --  Supported identity types and naming mechanisms

         if CM.Backward_Trust_Evaluator /= null
           or else CM.Forward_Trust_Evaluator /= null
         then
            if Identity_Types = 0 then
               raise Program_Error;
            end if;

            CM.Naming_Mechanisms := Naming_Mechanisms;
            CM.Identity_Types := Identity_Types;

         else
            CM.Identity_Types := 0;
         end if;

         --  Credentials

         if Creds'Length /= 0 then
            --  XXX Here should be more useful way to select accepting
            --  credentials
            Set_Accepting_Credentials (CM.all, Creds (Creds'First));
         end if;

         Append (QoS.Mechanisms, CM);

         if Target_Supports (CM.all) /= 0 then
            Set_Object_Adapter_QoS
              (POA, Compound_Security, QoS_Parameter_Access (QoS));

         else
            Release_Contents (QoS);
            Free (QoS);
         end if;
      end;

      if Legacy_POA_Create /= null then
         Legacy_POA_Create (POA, Error);
      end if;
   end POA_Create;

   -------------------
   -- Server_Invoke --
   -------------------

   procedure Server_Invoke
     (Self    : access PSPCE.Entity'Class;
      Request : access PolyORB.Requests.Request;
      Profile : PolyORB.Binding_Data.Profile_Access)
   is
      use PolyORB.CORBA_P.Security_Current;
      use PolyORB.CORBA_P.TSS_State_Machine_Actions;
      use PolyORB.QoS.Security_Contexts;
      use PolyORB.Request_QoS;
      use PolyORB.Security.Types;

      Request_QoS      : constant QoS_Security_Context_Parameter_Access
        := QoS_Security_Context_Parameter_Access
        (Extract_Request_Parameter (Compound_Security, Request.all));
      Transport_QoS    : constant QoS_Transport_Context_Parameter_Access
        := QoS_Transport_Context_Parameter_Access
        (Extract_Request_Parameter (Transport_Security, Request.all));
      Reply_QoS        : QoS_Security_Context_Parameter_Access;
      Stateful         : Boolean;
      Context_Accepted : Boolean := False;
      Token            : Stream_Element_Array_Access;
      Save_Note        : Security_Current_Note;
      Aux_Note         : Security_Current_Note;

   begin
      --  Store Security Current note, it will be replaced inside
      --  Accept_Transport_Context or Accept_Context iff context accepted

      PolyORB.Annotations.Get_Note
        (PolyORB.Tasking.Threads.Annotations.Get_Current_Thread_Notepad.all,
         Save_Note,
         Empty_Security_Current_Note);

      --  Check is SAS Service Context present

      if Request_QoS = null then
         if not Accept_Transport_Context (Profile, Transport_QoS) then
            declare
               use PolyORB.Errors;

               Error : Error_Container;

            begin
               pragma Debug
                (C, O ("Transport context without SAS message not accepted"));

               Throw
                 (Error,
                  No_Permission_E,
                  System_Exception_Members'
                  (Minor => 0, Completed => Completed_No));

               PolyORB.Requests.Set_Exception (Request.all, Error);
            end;

         else
            Context_Accepted := True;
         end if;

      else
         case Request_QoS.Context_Kind is
            when Establish_Context =>
               declare
                  Status    : Accept_Context_Status;
                  Reference : PolyORB.References.Ref;

               begin
                  Accept_Context
                    (Profile,
                     Transport_QoS,
                     Request_QoS.Client_Context_Id,
                     Request_QoS.Client_Authentication_Token,
                     Request_QoS.Identity_Token,
                     Request_QoS.Authorization_Token,
                     Status,
                     Stateful,
                     Reference,
                     Token);

                  case Status is
                     when Success =>
                        Context_Accepted := True;

                     when Invalid_Evidence =>
                        Throw_SAS_No_Permission
                          (Request,
                           Request_QoS.Client_Context_Id,
                           1,
                           1,
                           Token,
                           Reply_QoS);

                     when Invalid_Mechanism =>
                        Throw_SAS_No_Permission
                          (Request,
                           Request_QoS.Client_Context_Id,
                           2,
                           1,
                           Token,
                           Reply_QoS);

                     when Policy_Change =>
                        raise Program_Error;

                     when Conflicting_Evidence =>
                        Throw_SAS_No_Permission
                          (Request,
                           Request_QoS.Client_Context_Id,
                           3,
                           1,
                           Token,
                           Reply_QoS);
                  end case;
               end;

            when Complete_Establish_Context =>
               raise Program_Error;

            when Context_Error =>
               raise Program_Error;

            when Message_In_Context =>
               declare
                  Success : Boolean;

               begin
                  Reference_Context
                    (Transport_QoS,
                     Request_QoS.Client_Context_Id,
                     Success,
                     Token);

                  if not Success then
                     Throw_SAS_No_Permission
                       (Request,
                        Request_QoS.Client_Context_Id,
                        4,
                        1,
                        Token,
                        Reply_QoS);

                  else
                     --  XXX Should be called after complete of request
                     --  processing
                     Discard_Context
                       (Transport_QoS, Request_QoS.Client_Context_Id);
                  end if;
               end;

         end case;
      end if;

      if Context_Accepted then
         Legacy_Server_Invoke (Self, Request, Profile);

         if Request_QoS /= null then
            Reply_QoS :=
              new QoS_Security_Context_Parameter (Complete_Establish_Context);
            Reply_QoS.Client_Context_Id   := Request_QoS.Client_Context_Id;
            Reply_QoS.Context_Stateful    := Stateful;
            Reply_QoS.Final_Context_Token := Token;
         end if;
      end if;

      Add_Reply_QoS
        (Request.all, Compound_Security, QoS_Parameter_Access (Reply_QoS));

      --  Destroy current Security Current note

      if Context_Accepted then
         PolyORB.Annotations.Get_Note
           (PolyORB.Tasking.Threads.Annotations.Get_Current_Thread_Notepad.all,
            Aux_Note);

         Destroy (Aux_Note);
      end if;

      --  Restore original Security Current note

      PolyORB.Annotations.Set_Note
        (PolyORB.Tasking.Threads.Annotations.Get_Current_Thread_Notepad.all,
         Save_Note);
   end Server_Invoke;

   -----------------------------
   -- Throw_SAS_No_Permission --
   -----------------------------

   procedure Throw_SAS_No_Permission
     (Request     : access PolyORB.Requests.Request;
      Context_Id  : PolyORB.Security.Types.Context_Id;
      Major       : PolyORB.Types.Long;
      Minor       : PolyORB.Types.Long;
      Error_Token : PolyORB.Security.Types.Stream_Element_Array_Access;
      Reply_QoS   : out
        PolyORB.QoS.Security_Contexts.QoS_Security_Context_Parameter_Access)
   is
      use PolyORB.Errors;
      use PolyORB.QoS.Security_Contexts;

      Error : Error_Container;

   begin
      Throw
        (Error,
         No_Permission_E,
         System_Exception_Members'(Minor => 0, Completed => Completed_No));

      PolyORB.Requests.Set_Exception (Request.all, Error);

      Reply_QoS := new QoS_Security_Context_Parameter (Context_Error);
      Reply_QoS.Client_Context_Id := Context_Id;
      Reply_QoS.Major_Status      := Major;
      Reply_QoS.Minor_Status      := Minor;
      Reply_QoS.Error_Token       := Error_Token;
   end Throw_SAS_No_Permission;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"polyorb.corba_p.security.tss_state_machine",
          Conflicts => Empty,
          Depends   => +"corba.request"
          & "portableserver"
          & "polyorb.corba_p.interceptors?",
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.CORBA_P.TSS_State_Machine;
