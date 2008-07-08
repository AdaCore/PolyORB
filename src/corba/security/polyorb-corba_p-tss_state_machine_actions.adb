------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.CORBA_P.TSS_STATE_MACHINE_ACTIONS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
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

with PolyORB.Annotations;
with PolyORB.CORBA_P.Security_Current;
with PolyORB.Errors;
with PolyORB.Log;
with PolyORB.Obj_Adapter_QoS;
with PolyORB.POA;
with PolyORB.POA_Types;
with PolyORB.QoS.Targets_Security;
with PolyORB.Security.Authentication_Mechanisms;
with PolyORB.Security.Authority_Mechanisms;
with PolyORB.Security.Backward_Trust_Evaluators;
with PolyORB.Security.Credentials.Compound;
with PolyORB.Security.Forward_Trust_Evaluators;
with PolyORB.Security.Identities.Anonymous;
with PolyORB.Security.Transport_Mechanisms;
with PolyORB.Tasking.Threads.Annotations;
with PolyORB.Types;

package body PolyORB.CORBA_P.TSS_State_Machine_Actions is

   use PolyORB.CORBA_P.Security_Current;
   use PolyORB.Log;
   use PolyORB.Security.Backward_Trust_Evaluators;
   use PolyORB.Security.Credentials;
   use PolyORB.Security.Credentials.Compound;
   use PolyORB.Security.Forward_Trust_Evaluators;
   use PolyORB.Security.Identities.Anonymous;
   use PolyORB.Security.Transport_Mechanisms;

   package L is
     new PolyORB.Log.Facility_Log
     ("polyorb.corba_p.tss_state_machine_actions");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   function Get_Object_Adapter_Security_Configuration
     (Profile : PolyORB.Binding_Data.Profile_Access)
      return QoS.Targets_Security.QoS_Target_Security_Parameter_Access;
   --  Retrieve Profile's Object Adapter Security Configuration

   --------------------
   -- Accept_Context --
   --------------------

   procedure Accept_Context
     (Profile                     : PolyORB.Binding_Data.Profile_Access;
      Transport                   :
        PolyORB.QoS.Transport_Contexts.QoS_Transport_Context_Parameter_Access;
      Client_Context_Id           :
        PolyORB.Security.Types.Context_Id;
      Client_Authentication_Token :
        PolyORB.Security.Types.Stream_Element_Array_Access;
      Identity_Token              :
        PolyORB.Security.Identities.Identity_Access;
      Authorization_Token         :
        PS.Authorization_Elements.Authorization_Element_Lists.List;
      Status                      : out Accept_Context_Status;
      Stateful                    : out Boolean;
      Reference                   : out PolyORB.References.Ref;
      Final_Token                 : out
        PolyORB.Security.Types.Stream_Element_Array_Access)
   is
      pragma Unreferenced (Client_Context_Id);
      pragma Unreferenced (Reference);

      use PolyORB.QoS.Targets_Security;
      use PolyORB.QoS.Transport_Contexts;
      use PolyORB.Security.Authorization_Elements;
      use PolyORB.Security.Authorization_Elements.Authorization_Element_Lists;
      use PolyORB.Security.Authentication_Mechanisms;
      use PolyORB.Security.Identities;
      use PolyORB.Security.Authority_Mechanisms;
      use PolyORB.Security.Authority_Mechanisms.
            Target_Authority_Mechanism_Lists;
      use PolyORB.Security.Types;

      QoS             : QoS_Target_Security_Parameter_Access;
      Mech            : Target_Mechanism_Access;
      Client_Identity : Identity_Access := null;
      Target_Identity : constant Identity_Access := null;
      Current_Note    : Security_Current_Note;
      Forward_Done    : Boolean         := False;

   begin
      --  Extract Object's POA security configuration

      QoS := Get_Object_Adapter_Security_Configuration (Profile);

      --  Iff security support for object adapter not configured (and we
      --  have request with SAS message) then reject request

      if QoS = null
        or else Target_Mechanism_Lists.Length (QoS.Mechanisms) = 0
      then
         Status := Invalid_Mechanism;

         pragma Warnings (Off);  --  "Stateful" not set before return
         return;
         pragma Warnings (On);
      end if;

      Mech := Target_Mechanism_Lists.Element (QoS.Mechanisms, 0).all;

      --  Initial values

      Status      := Success;
      Stateful    := False;
      Final_Token := null;

      --  Accepting transport context

      if Transport = null then
         if Mech.Transport /= null then
            Status := Invalid_Mechanism;

            return;
         end if;

      elsif Mech.Transport /= Transport.Transport then
         Status := Invalid_Mechanism;

         return;
      end if;

      --  Authenticate client

      --  If target support client authentication and request contains
      --  client authentication token, then authenticate cleint

      if Mech.Authentication_Mechanism /= null
        and then Client_Authentication_Token /= null
      then
         declare
            Success : Boolean;

         begin
            Accept_Security_Context
              (Mech.Authentication_Mechanism,
               Client_Authentication_Token,
               Success,
               Final_Token,
               Client_Identity);

            if not Success then
               Status := Invalid_Evidence;

               return;
            end if;
         end;

      --  If request contains client authentication token, or target
      --  requires client authentication and request not contains client
      --  authentication token then report client authentication failure

      elsif Mech.Authentication_Required
        or else Client_Authentication_Token /= null
      then
         Status := Invalid_Evidence;

         return;
      end if;

      --  If no authentication layer client identity, then use transport layer
      --  client identity

      if Client_Identity = null
        and then Transport /= null
      then
         Client_Identity :=
           Get_Transport_Identity
           (Compound_Credentials_Access
            (Entity_Of (Transport.Invocation_Credentials)));
      end if;

      --  If target not supports identity assertion but identity token present
      --  then reject request

      if Identity_Token /= null
        and then Mech.Forward_Trust_Evaluator = null
        and then Mech.Backward_Trust_Evaluator = null
      then
         Status := Invalid_Evidence;

         return;
      end if;

      --  If target supports authorization token delivery and request contains
      --  authorization token, then check:
      --   - is authorization token elements signed by trusted authority
      --   - is authorization token elements identity same with identity token
      --     or authentication identity

      if Mech.Authorities /= Target_Authority_Mechanism_Lists.Empty
        and then Authorization_Token /= Authorization_Element_Lists.Empty
      then
         declare
            Element_Iter : Authorization_Element_Lists.Iterator
              := First (Authorization_Token);
            Signed       : Boolean;

         begin
            while not Last (Element_Iter) loop
               Signed := False;

               --  Check authorization element signed by privilege authority

               declare
                  Authority_Iter : Target_Authority_Mechanism_Lists.Iterator
                    := First (Mech.Authorities);

               begin
                  while not Last (Authority_Iter) loop
                     if Verify
                       (Value (Authority_Iter).all, Value (Element_Iter).all)
                     then
                        Signed := True;

                        exit;
                     end if;

                     Next (Authority_Iter);
                  end loop;
               end;

               if not Signed then
                  Status := Invalid_Evidence;

                  return;
               end if;

               --  Check identity in identity token same as authorization
               --  element identity

               if Identity_Token /= null then
                  if
                    not Is_Holder (Value (Element_Iter).all, Identity_Token)
                  then
                     Status := Invalid_Evidence;

                     return;
                  end if;

               --  Or, check authentication identity same as authorization
               --  element identity

               elsif Client_Identity /= null then
                  if
                    not Is_Holder (Value (Element_Iter).all, Client_Identity)
                  then
                     Status := Invalid_Evidence;

                     return;
                  end if;
               end if;

               Next (Element_Iter);
            end loop;
         end;

      --  If target not supports authorization token delivery and authorization
      --  token present or target supports authorization token delivery and
      --  no authorization token present, then reject request

      elsif Mech.Authorities /= Target_Authority_Mechanism_Lists.Empty
        or else Authorization_Token /= Authorization_Element_Lists.Empty
      then
         Status := Invalid_Evidence;

         return;
      end if;

      --  If asserted identity present, authentication identity present and
      --  authorization token also present, then do forward trust evaluation

      if Client_Identity /= null
        and then Identity_Token /= null
        and then Authorization_Token /= Authorization_Element_Lists.Empty
        and then Mech.Forward_Trust_Evaluator /= null
      then
         declare
            Trusted : Boolean;

         begin
            Status := Invalid_Evidence;

            Evaluate_Trust
              (Mech.Forward_Trust_Evaluator,
               Target_Identity,
               Client_Identity,
               Authorization_Token,
               Mech.Authorities /= Target_Authority_Mechanism_Lists.Empty,
               Forward_Done,
               Trusted);

            --  If authorization elements don't have information for forward
            --  trust evaluation, then process backward trust evaluation

            if not Forward_Done then
               Status := Success;

            elsif Trusted then
               Status := Success;

            else
               Status := Invalid_Evidence;
            end if;
         end;
      end if;

      --  If itendity token present, authentication identity present and
      --  no authorization token or authorization token don't have proxy
      --  info attributes, then proceed backward trust evaluation

      if Client_Identity /= null
        and then Identity_Token /= null
        and then not Forward_Done
      then
         declare
            Trusted : Boolean := False;

         begin
            if Mech.Backward_Trust_Evaluator /= null then
               Evaluate_Trust
                 (Mech.Backward_Trust_Evaluator,
                  Client_Identity,
                  Identity_Token,
                  Trusted);
            end if;

            if not Trusted then
               Status := Invalid_Evidence;
               Destroy (Client_Identity);

               return;
            end if;
         end;
      end if;

      --  All checks done, prepare Security Current note

      if Identity_Token /= null then
         Current_Note.Access_Identity := Duplicate (Identity_Token);
         Destroy (Client_Identity);

      else
         Current_Note.Access_Identity := Client_Identity;
      end if;

      PolyORB.Annotations.Set_Note
        (PolyORB.Tasking.Threads.Annotations.Get_Current_Thread_Notepad.all,
         Current_Note);
   end Accept_Context;

   ------------------------------
   -- Accept_Transport_Context --
   ------------------------------

   function Accept_Transport_Context
     (Profile   : PolyORB.Binding_Data.Profile_Access;
      Transport :
        PolyORB.QoS.Transport_Contexts.QoS_Transport_Context_Parameter_Access)
      return Boolean
   is
      use PolyORB.Binding_Data;
      use PolyORB.QoS.Targets_Security;
      use PolyORB.QoS.Transport_Contexts;
      use PolyORB.Security.Types;

      QoS          : QoS_Target_Security_Parameter_Access;
      Current_Note : Security_Current_Note;

   begin
      --  Extract Object's POA security configuration

      QoS := Get_Object_Adapter_Security_Configuration (Profile);

      if QoS = null then
         --  Unprotected POA

         if Transport = null then
            Current_Note.Access_Identity := Create_Anonymous_Identity;

            PolyORB.Annotations.Set_Note
              (PolyORB.Tasking.Threads.Annotations.
                 Get_Current_Thread_Notepad.all,
               Current_Note);

            return True;

         else
            pragma Debug (O ("Unprotected POA, secure transport"));

            return False;
         end if;
      end if;

      if Transport = null then
         --  Unprotected transport

         if not QoS.Disable_Unprotected then
            Current_Note.Access_Identity := Create_Anonymous_Identity;

            PolyORB.Annotations.Set_Note
              (PolyORB.Tasking.Threads.Annotations.
                 Get_Current_Thread_Notepad.all,
               Current_Note);

            return True;

         else
            pragma Debug
              (O ("Unprotected transport, POA require protection"));

            return False;
         end if;
      end if;

      declare
         use Target_Mechanism_Lists;

         Iter : Target_Mechanism_Lists.Iterator := First (QoS.Mechanisms);

      begin
         --  Always accept transport context for local profiles

         if Transport.Transport = null then
            Current_Note.Access_Identity :=
              Get_Transport_Identity
              (Compound_Credentials_Access
               (Entity_Of (Transport.Invocation_Credentials)));

            PolyORB.Annotations.Set_Note
              (PolyORB.Tasking.Threads.Annotations.
                Get_Current_Thread_Notepad.all,
               Current_Note);

            return True;
         end if;

         while not Last (Iter) loop
            if Value (Iter).all.Transport = Transport.Transport then
               if not Value (Iter).all.Authentication_Required
                 and then not Value (Iter).all.Delegation_Required
               then
                  Current_Note.Access_Identity :=
                    Get_Transport_Identity
                    (Compound_Credentials_Access
                     (Entity_Of (Transport.Invocation_Credentials)));

                  PolyORB.Annotations.Set_Note
                  (PolyORB.Tasking.Threads.Annotations.
                     Get_Current_Thread_Notepad.all,
                   Current_Note);

                  return True;

               else
                  pragma Debug
                    (O ("Transport mechanism match,"
                     & " POA requies authentication or delegation"));

                  return False;
               end if;
            end if;

            Next (Iter);
         end loop;

         pragma Debug (O ("Transport mechanism not matched"));

         return False;
      end;
   end Accept_Transport_Context;

   ---------------------
   -- Discard_Context --
   ---------------------

   procedure Discard_Context
     (Transport         :
        PolyORB.QoS.Transport_Contexts.QoS_Transport_Context_Parameter_Access;
      Client_Context_Id :
        PolyORB.Security.Types.Context_Id)
   is
      pragma Unreferenced (Transport);
      pragma Unreferenced (Client_Context_Id);

   begin
      null;
   end Discard_Context;

   -----------------------------------------------
   -- Get_Object_Adapter_Security_Configuration --
   -----------------------------------------------

   function Get_Object_Adapter_Security_Configuration
     (Profile : PolyORB.Binding_Data.Profile_Access)
      return QoS.Targets_Security.QoS_Target_Security_Parameter_Access
   is
      use PolyORB.Binding_Data;
      use PolyORB.Errors;
      use PolyORB.Obj_Adapter_QoS;
      use PolyORB.POA;
      use PolyORB.POA_Types;
      use PolyORB.QoS;
      use PolyORB.QoS.Targets_Security;
      use PolyORB.Types;

      U_Oid  : Unmarshalled_Oid;
      Obj_OA : PolyORB.POA.Obj_Adapter_Access;
      Error  : Error_Container;

   begin
      --  Extract Object's POA security configuration

      Oid_To_U_Oid (Get_Object_Key (Profile.all).all, U_Oid, Error);

      if Found (Error) then
         raise Program_Error;
      end if;

      Find_POA
        (PolyORB.POA.Obj_Adapter_Access (Get_OA (Profile.all)),
         To_Standard_String (U_Oid.Creator),
         True,
         Obj_OA,
         Error);

      if Found (Error) then
         raise Program_Error;
      end if;

      return
        QoS_Target_Security_Parameter_Access
        (Get_Object_Adapter_QoS (Obj_OA, Compound_Security));
   end Get_Object_Adapter_Security_Configuration;

   -----------------------
   -- Reference_Context --
   -----------------------

   procedure Reference_Context
     (Transport         :
        PolyORB.QoS.Transport_Contexts.QoS_Transport_Context_Parameter_Access;
      Client_Context_Id :
        PolyORB.Security.Types.Context_Id;
      Status            : out Boolean;
      Final_Token       : out
        PolyORB.Security.Types.Stream_Element_Array_Access)
   is
      pragma Unreferenced (Transport);
      pragma Unreferenced (Client_Context_Id);

   begin
      Status := False;
      Final_Token := null;
   end Reference_Context;

end PolyORB.CORBA_P.TSS_State_Machine_Actions;
