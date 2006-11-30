------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . C O R B A _ P . C S S _ S T A T E _ M A C H I N E     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Binding_Data_QoS;
with PolyORB.CORBA_P.CSS_State_Machine_Actions;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Security_Policy;
with PolyORB.Errors;
with PolyORB.Initialization;
with PolyORB.QoS;
with PolyORB.QoS.Clients_Security;
with PolyORB.QoS.Security_Contexts;
with PolyORB.QoS.Transport_Contexts;
with PolyORB.References.Binding;
with PolyORB.Requests;
with PolyORB.Request_QoS;
with PolyORB.Security.Connections;
with PolyORB.Security.Credentials;
with PolyORB.Utils.Strings;

package body PolyORB.CORBA_P.CSS_State_Machine is

   use PolyORB.Binding_Data_QoS;
   use PolyORB.CORBA_P.Exceptions;
   use PolyORB.CORBA_P.Interceptors_Hooks;
   use PolyORB.CORBA_P.CSS_State_Machine_Actions;
   use PolyORB.QoS;
   use PolyORB.QoS.Clients_Security;
   use PolyORB.QoS.Security_Contexts;
   use PolyORB.QoS.Transport_Contexts;
   use PolyORB.References.Binding;
   use PolyORB.Request_QoS;
   use PolyORB.Security.Credentials;

   Legacy_Client_Invoke : Client_Invoke_Handler;

   procedure Initialize;

   procedure Security_Client_Invoke
     (Request : in PolyORB.Requests.Request_Access;
      Flags   : in PolyORB.Requests.Flags);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Legacy_Client_Invoke := Client_Invoke;
      Client_Invoke        := Security_Client_Invoke'Access;
   end Initialize;

   ----------------------------
   -- Security_Client_Invoke --
   ----------------------------

   procedure Security_Client_Invoke
     (Request : in PolyORB.Requests.Request_Access;
      Flags   : in PolyORB.Requests.Flags)
   is
      use PolyORB.CORBA_P.Security_Policy;
      use PolyORB.QoS.Clients_Security.Client_Mechanism_Lists;

      Configuration_QoS : constant QoS_Client_Security_Parameter_Access
        := QoS_Client_Security_Parameter_Access
        (Get_Profile_QoS
         (Get_Preferred_Profile (Request.Target, True), Compound_Security));
      Policy            : constant Client_Policy
        := Get_Client_Policy (Request.Target);
      Mechanism         : PolyORB.QoS.Clients_Security.Client_Mechanism_Access;
      Credentials       : PolyORB.Security.Credentials.Credentials_Ref;
      Connection        : PolyORB.Security.Connections.Connection_Access;
      Request_QoS       :
        PolyORB.QoS.Security_Contexts.QoS_Security_Context_Parameter_Access;
      Reply_QoS         :
        PolyORB.QoS.Security_Contexts.QoS_Security_Context_Parameter_Access;
      Transport_QoS     :
        PolyORB.QoS.Transport_Contexts.QoS_Transport_Context_Parameter_Access;
      Success           : Boolean;

   begin
      --  Getting preferred compound mechanism

      if Configuration_QoS /= null then
         Get_Mechanism
           (Policy,
            Configuration_QoS.Mechanisms,
            Mechanism,
            Success);

      else
         Get_Mechanism (Policy, Empty, Mechanism, Success);
      end if;

      if not Success then
         declare
            Error : PolyORB.Errors.Error_Container;

         begin
            PolyORB.Errors.Throw
              (Error,
               PolyORB.Errors.Inv_Policy_E,
               PolyORB.Errors.System_Exception_Members'
               (Minor     => 0,
                Completed => PolyORB.Errors.Completed_No));

            PolyORB.Requests.Set_Exception (Request, Error);

            return;
         end;
      end if;

      --  If no preferred compound mechanism or preferred compound mechanism
      --  is unprotected then use legacy invocation handler

      if Mechanism = null
        or else not Is_Protected (Mechanism.all)
      then
         Legacy_Client_Invoke (Request, Flags);

         return;
      end if;

      --  Getting credentials

      Credentials := Get_Client_Credentials (Policy, Mechanism);

      if Is_Nil (Credentials) then
         declare
            Error : PolyORB.Errors.Error_Container;

         begin
            PolyORB.Errors.Throw
              (Error,
               PolyORB.Errors.No_Resources_E,
               PolyORB.Errors.System_Exception_Members'
               (Minor     => 0,
                Completed => PolyORB.Errors.Completed_No));

            PolyORB.Requests.Set_Exception (Request, Error);

            return;
         end;
      end if;

      --  Configure transport mechanism

      Transport_QoS := new QoS_Transport_Context_Parameter;
      Transport_QoS.Selected               := Mechanism;
      Transport_QoS.Invocation_Credentials := Credentials;

      Add_Request_QoS
        (Request, Transport_Security, QoS_Parameter_Access (Transport_QoS));

      --  Bind Object Reference with selected transport mechanism and obtained
      --  credentials

      --  XXX Not yet impelemnted, assume unprotected transport mechanism

      Connection := null;

      --  Create SAS context

      declare
         Element : constant Context_Element
           := Get_Context_Element (Policy, Mechanism, Credentials, Connection);

      begin
         case Element.Kind is
            when No_Element =>
               Request_QoS := null;

            when Establish_Context =>
               Request_QoS :=
                 new QoS_Security_Context_Parameter (Establish_Context);

               Request_QoS.Client_Context_Id   := Element.Client_Context_Id;
               Request_QoS.Client_Authentication_Token :=
                 Element.Client_Authentication_Token;
               Request_QoS.Identity_Token      := Element.Identity_Token;
               Request_QoS.Authorization_Token := Element.Authorization_Token;

            when Message_In_Context =>
               Request_QoS :=
                 new QoS_Security_Context_Parameter (Message_In_Context);

               Request_QoS.Client_Context_Id := Element.Client_Context_Id;
               Request_QoS.Discard_Context   := Element.Discard_Context;
         end case;
      end;

      Add_Request_QoS
        (Request, Compound_Security, QoS_Parameter_Access (Request_QoS));

      Legacy_Client_Invoke (Request, Flags);

      --  If no request security context then return

      if Request_QoS = null then
         return;
      end if;

      --  Otherwise, if location forwarding received or no empty SAS context
      --  then invalidate context and return

      Reply_QoS :=
        QoS_Security_Context_Parameter_Access
        (Extract_Reply_Parameter (Compound_Security, Request));

      if Is_Forward_Request (Request.Exception_Info)
        or else Reply_QoS = null
      then
         Invalidate_Context
           (Connection,
            Request_QoS.Client_Context_Id);

         return;
      end if;

      --  If reply security context received then analize it

      case Reply_QoS.Context_Kind is
         when Establish_Context =>
            raise Program_Error;
            --  Never be happen

         when Complete_Establish_Context =>
            Complete_Context
              (Connection,
               Reply_QoS.Client_Context_Id,
               Reply_QoS.Context_Stateful,
               Reply_QoS.Final_Context_Token);

         when Context_Error =>
            case Reply_QoS.Major_Status is
               when 1 =>
                  --  Invalid Evidience

                  null;

               when 2 =>
                  --  Invalid Mechanism

                  null;

               when 3 =>
                  --  Conflicting Evidience

                  Invalidate_Context
                    (Connection,
                     Reply_QoS.Client_Context_Id,
                     Reply_QoS.Error_Token);

               when 4 =>
                  --  No Context

                  Invalidate_Context
                    (Connection,
                     Reply_QoS.Client_Context_Id,
                     Reply_QoS.Error_Token);

                  raise Program_Error;
                  --  XXX Should repeat request with new context element,
                  --  not yet implemented.

               when others =>
                  raise Program_Error;
                  --  Never be happen
            end case;

         when Message_In_Context =>
            raise Program_Error;
            --  Never be happen
      end case;
   end Security_Client_Invoke;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"polyorb.corba_p.security.css_state_machine",
          Conflicts => Empty,
          Depends   => +"corba.request"
          & "polyorb.corba_p.interceptors?",
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.CORBA_P.CSS_State_Machine;
