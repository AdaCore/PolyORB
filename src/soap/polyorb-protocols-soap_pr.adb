------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . P R O T O C O L S . S O A P _ P R             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Strings.Unbounded;

with AWS.Response;

with SOAP.Message;
with SOAP.Message.XML;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.Parameters;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Binding_Data;
with PolyORB.Binding_Data.Local;
with PolyORB.Binding_Data.SOAP;
with PolyORB.Buffer_Sources;
with PolyORB.Filters.AWS_Interface;
with PolyORB.Filters.Interface;
with PolyORB.HTTP_Methods;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Objects;
with PolyORB.Objects.Interface;
with PolyORB.ORB.Interface;
with PolyORB.References;

package body PolyORB.Protocols.SOAP_Pr is

   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.ORB;
   use Standard.SOAP;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.soap_pr");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --------------------
   -- Implementation --
   --------------------

   procedure Create
     (Proto   : access SOAP_Protocol;
      Session : out Filter_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Proto);
      pragma Warnings (On);

      Result : constant Filter_Access
        := new SOAP_Session;
   begin
      SOAP_Session (Result.all).In_Buf
        := new Buffers.Buffer_Type;
      Session := Result;
   end Create;

   procedure Invoke_Request
     (S   : access SOAP_Session;
      R   : Requests.Request_Access;
      Pro : access Binding_Data.Profile_Type'Class)
   is
      P : SOAP.Message.Payload.Object;
      SPro : Binding_Data.SOAP.SOAP_Profile_Type'Class
        renames Binding_Data.SOAP.SOAP_Profile_Type'Class (Pro.all);
   begin
      pragma Assert (S.Pending_Rq = null);
      S.Pending_Rq := R;
      --  Actually should support concurrent calls to invoke_request
      --  with a mutex on Session.Pending_Request that would be taken
      --  here in Invoke_Request and released when the answer is
      --  received.

      begin
         P := SOAP.Message.Payload.Build
           (Types.To_Standard_String (R.Operation),
            SOAP.Parameters.List'(R.Args with null record));

         pragma Debug (O ("SOAP message constructed: "));
         pragma Debug (O (SOAP.Message.XML.Image (P)));
      exception
         when others =>
            --  Cleanup before propagating exception to caller.
            S.Pending_Rq := null;
            raise;
      end;

      --  RD := (R_Headers, R_Body => SOAP.Message.XML.Image (P));
      Components.Emit_No_Reply
        (Lower (S),
         Filters.AWS_Interface.AWS_Request_Out'
         (Request_Method => HTTP_Methods.POST,
          Relative_URI => Binding_Data.SOAP.Get_URI_Path (SPro),
          Data => Types.String
          (Ada.Strings.Unbounded.Unbounded_String'
           (SOAP.Message.XML.Image (P))),
          SOAP_Action => Types.String (R.Operation)));
   end Invoke_Request;

   procedure Abort_Request
     (S : access SOAP_Session;
      R : Requests.Request_Access)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Abort_Request;

   procedure Send_Reply
      (S : access SOAP_Session;
       R : Requests.Request_Access)
   is
   begin
      declare
         use PolyORB.Any;
         use PolyORB.Any.NVList;
         use PolyORB.Any.NVList.Internals;
         use PolyORB.Any.NVList.Internals.NV_Sequence;

         use SOAP.Parameters;

         RO : SOAP.Message.Response.Object
           := SOAP.Message.Response.From
           (SOAP.Message.Payload.Object (S.Current_SOAP_Req.all));
         RP : SOAP.Parameters.List;

         Args : constant NV_Sequence_Access
           := List_Of (R.Args);
         A : Any.NamedValue;
      begin
         SOAP.Message.Payload.Free (S.Current_SOAP_Req);
         RP := +R.Result;
         for I in 1 .. Get_Count (R.Args) loop
            A :=  NV_Sequence.Element_Of (Args.all, Positive (I));
            if False
              or else A.Arg_Modes = ARG_INOUT
              or else A.Arg_Modes = ARG_OUT
            then
               RP := RP & A;
            end if;
         end loop;

         SOAP.Message.Set_Parameters (RO, RP);
         declare
            RD : AWS.Response.Data
              := SOAP.Message.Response.Build (RO);

            --  Here we depend on a violation of abstraction:
            --  we construct an /AWS/ response object, and
            --  AWS is HTTP-specific. This is a shortcoming
            --  of the AWS SOAP engine. It is unknown yet whether
            --  this violation can be easily removed.
         begin
            Components.Emit_No_Reply
              (Lower (S),
               Filters.AWS_Interface.AWS_Response_Out'
               (Data => RD));
         end;
      end;
   end Send_Reply;

   procedure Process_Reply (S   : access SOAP_Session);

   procedure Process_Reply (S   : access SOAP_Session)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Sequence;

      R : constant Requests.Request_Access := S.Pending_Rq;

      Return_Args : PolyORB.Any.NVList.Ref := R.Args;
      Src : aliased Buffer_Sources.Input_Source;

   begin
      if R = null then
         raise Protocol_Error;
         --  Received a reply with no pending request.
      end if;
      R.Result.Arg_Modes := ARG_OUT;
      --  Ensure proper mode for Result.
      List_Of (Return_Args).all := R.Result & List_Of (Return_Args).all;

      Buffer_Sources.Set_Buffer (Src, S.In_Buf);
      declare
         M : SOAP.Message.Response.Object_Access
           := Standard.SOAP.Message.XML.Load_Response
           (Src'Access, Return_Args);
      begin
         SOAP.Message.Response.Free (M);
      end;
      --  Only evaluate the side effects of Load_Response.

      --  XXX BAD BAD this subprogram does not take into account
      --  the case where a FAULT or EXCEPTION has been received
      --  instead of a normal reply!!

      S.Pending_Rq := null;

      Buffers.Release_Contents (S.In_Buf.all);

      PolyORB.Requests.Pump_Up_Arguments
        (Dst_Args => R.Args, Src_Args => Return_Args,
         Direction => ARG_OUT);
      --  XXX Ignore_Src_Mode => True!

      Components.Emit_No_Reply
        (S.Server,
         Objects.Interface.Executed_Request'(Req => R));
   end Process_Reply;

   procedure Handle_Unmarshall_Arguments
     (S : access SOAP_Session;
      Args : in out PolyORB.Any.NVList.Ref)
   is
      Src : aliased Buffer_Sources.Input_Source;
   begin
      Buffer_Sources.Set_Buffer (Src, S.In_Buf);
      S.Current_SOAP_Req := Message.XML.Load_Payload
        (Src'Access, Args);
      Buffers.Release_Contents (S.In_Buf.all);
   end Handle_Unmarshall_Arguments;

   procedure Handle_Data_Indication
     (S : access SOAP_Session;
      Data_Amount : Ada.Streams.Stream_Element_Count)
   is
   begin
      if S.Role = Server then
         declare
            use Ada.Streams;
            use PolyORB.Binding_Data.Local;
            use PolyORB.Types;

            Req : Request_Access;

            Result : Any.NamedValue;
            --  Dummy NamedValue for Create_Request; the actual Result
            --  is set by the called method.

            ORB : constant ORB_Access := ORB_Access (S.Server);

            Target : References.Ref;
            Target_Profile : Binding_Data.Profile_Access
              := new Local_Profile_Type;
            --  Should be free'd when Target is finalized.

            function Path_To_Oid (Path : Types.String)
              return Objects.Object_Id_Access;

            function Path_To_Oid (Path : Types.String)
              return Objects.Object_Id_Access
            is
               M : constant Components.Message'Class
                 := Components.Emit
                 (S.Server, PolyORB.ORB.Interface.URI_Translate'
                  (Path => Path));
               TM : PolyORB.ORB.Interface.Oid_Translate
                 renames PolyORB.ORB.Interface.Oid_Translate (M);
            begin
               pragma Debug
                 (O ("Path_To_Oid: " & To_Standard_String (Path)));
               return TM.Oid;
            end Path_To_Oid;

            The_Oid : Objects.Object_Id_Access := Path_To_Oid (S.Target);

            SOAP_Action_Msg : constant AWS_Interface.AWS_SOAP_Action
              := AWS_Interface.AWS_SOAP_Action
              (Components.Emit
               (Lower (S), AWS_Interface.AWS_Get_SOAP_Action'
                (null record)));

            Args : Any.NVList.Ref;
            --  Nil (not initialised).

         begin
            Create_Local_Profile
              (The_Oid.all, Local_Profile_Type (Target_Profile.all));
            Objects.Free (The_Oid);
            References.Create_Reference
              ((1 => Target_Profile),
               Type_Id => "", R => Target);
            --  Create a temporary, typeless reference for this object.

            Result.Name := To_PolyORB_String ("Result");
            Create_Request
              (Target    => Target,
               Operation => To_Standard_String
               (SOAP_Action_Msg.SOAP_Action),
               Arg_List  => Args,
               Result    => Result,
               Deferred_Arguments_Session =>
                 Components.Component_Access (S),
               Req       => Req);
            S.Target := Types.To_PolyORB_String ("");
            S.Entity_Length := Data_Amount;

            PolyORB.ORB.Queue_Request_To_Handler
              (ORB.Tasking_Policy, ORB,
               PolyORB.ORB.Interface.Queue_Request'
               (Request => Req,
                Requestor => Components.Component_Access (S)));
         end;
      else
         Process_Reply (S);
      end if;
   end Handle_Data_Indication;

   procedure Handle_Connect_Indication
     (S : access SOAP_Session)
   is
   begin
      S.Role := Server;
      Expect_Data (S, S.In_Buf, 0);
      --  Buffer used to receive request from the client.
   end Handle_Connect_Indication;

   procedure Handle_Connect_Confirmation
     (S : access SOAP_Session)
   is
   begin
      S.Role := Client;
      Expect_Data (S, S.In_Buf, 0);
      --  Buffer used to receive reply from the server.
   end Handle_Connect_Confirmation;

   procedure Handle_Disconnect
     (S : access SOAP_Session)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Handle_Disconnect;

   function Handle_Message
     (Sess : access SOAP_Session;
      S : Components.Message'Class)
     return Components.Message'Class
   is
      use PolyORB.Protocols;

      Result : Components.Null_Message;
   begin
      if S in Set_Target_Object then
         Sess.Target := Set_Target_Object (S).Target;
         return Result;
      else
         return PolyORB.Protocols.Handle_Message
           (Session (Sess.all)'Access, S);
         --  Call ancestor method.
      end if;
   end Handle_Message;

end PolyORB.Protocols.SOAP_Pr;
