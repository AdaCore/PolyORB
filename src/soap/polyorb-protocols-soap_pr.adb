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

with PolyORB.Annotations;
with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Binding_Data;
with PolyORB.Binding_Data.Local;
with PolyORB.Binding_Data.SOAP;
with PolyORB.Filters.AWS_Interface;
with PolyORB.Filters.Interface;
with PolyORB.HTTP_Methods;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Objects;
with PolyORB.Objects.Interface;
with PolyORB.ORB.Interface;
with PolyORB.References;
with PolyORB.Utils.Text_Buffers;

package body PolyORB.Protocols.SOAP_Pr is

   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.ORB;
   use Standard.SOAP;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.soap_pr");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   type Request_Note is new Annotations.Note with record
      SOAP_Req : SOAP.Message.Payload.Object;
   end record;

   --------------------
   -- Implementation --
   --------------------

   procedure Create
     (Proto   : access SOAP_Protocol;
      Session : out Filter_Access)
   is
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
      P := SOAP.Message.Payload.Build
        (Types.To_Standard_String (R.Operation),
         SOAP.Parameters.List'(R.Args with null record));

      pragma Debug (O ("SOAP message constructed: "));
      pragma Debug (O (SOAP.Message.XML.Image (P)));
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
      N : Request_Note;
   begin
      Annotations.Get_Note (R.Notepad, N);
      declare
         use PolyORB.Any;
         use PolyORB.Any.NVList;
         use PolyORB.Any.NVList.Internals;
         use PolyORB.Any.NVList.Internals.NV_Sequence;

         use SOAP.Parameters;

         RO : SOAP.Message.Response.Object
           := SOAP.Message.Response.From (N.SOAP_Req);
         RP : SOAP.Parameters.List;

         Args : constant NV_Sequence_Access
           := List_Of (R.Args);
         A : Any.NamedValue;
      begin
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

   procedure Process_Reply
     (S : access SOAP_Session;
      Entity : String);

   procedure Process_Reply
     (S : access SOAP_Session;
      Entity : String)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Sequence;

      M : constant Standard.SOAP.Message.Response.Object'Class
        := Standard.SOAP.Message.XML.Load_Response (Entity);
      Return_Args : constant Any.NVList.Ref
        := Any.NVList.Ref (Message.Parameters (M));

      R : constant Requests.Request_Access := S.Pending_Rq;

   begin
      if R = null then
         raise Protocol_Error;
         --  Received a reply with no pending request.
      end if;

      --  XXX BAD BAD this subprogram does not take into account
      --  the case where a FAULT or EXCEPTION has been received
      --  instead of a normal reply!!

      S.Pending_Rq := null;
      declare
         Result_Any : Any.Any
           := Element_Of (List_Of (Return_Args).all, 1).Argument;
      begin
         Copy_Any_Value (R.Result.Argument, Result_Any);
      end;
      Delete (List_Of (Return_Args).all, 1, 1);
      --  XXX assumes that method result is arg #1.

      PolyORB.Requests.Pump_Up_Arguments
        (A_Args => R.Args, P_Args => Return_Args,
         Direction => ARG_OUT);

      Components.Emit_No_Reply
        (S.Server,
         Objects.Interface.Executed_Request'(Req => R));

   end Process_Reply;

   procedure Handle_Data_Indication
     (S : access SOAP_Session;
      Data_Amount : Ada.Streams.Stream_Element_Count)
   is
      Entity : String (1 .. Integer (Data_Amount));
      --  XXX BAD BAD should be a Types.String so as not to
      --  overflow the stack.
   begin
      Utils.Text_Buffers.Unmarshall_String (S.In_Buf, Entity);
      pragma Debug (O ("SOAP entity received: " & Entity));
      if S.Role = Server then
         declare
            use Ada.Streams;
            use PolyORB.Binding_Data.Local;
            use PolyORB.Types;

            M : constant Message.Payload.Object
              := Message.XML.Load_Payload (Entity);
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

         begin
            Create_Local_Profile
              (The_Oid.all, Local_Profile_Type (Target_Profile.all));
            Objects.Free (The_Oid);
            References.Create_Reference
              ((1 => Target_Profile),
               Type_Id => "", R => Target);
            --  Create a temporary, typeless reference for this object.

            Create_Request
              (Target    => Target,
               Operation => Standard.SOAP.Message.Payload.Procedure_Name (M),
               Arg_List  => Any.NVList.Ref
               (Standard.SOAP.Message.Parameters (M)),
               Result    => Result,
               Deferred_Arguments_Session => null,
               Req       => Req);
            S.Target := Types.To_PolyORB_String ("");

            Annotations.Set_Note
              (Req.Notepad,
               Request_Note'(Annotations.Note
                             with SOAP_Req => M));

            PolyORB.ORB.Queue_Request_To_Handler
              (ORB.Tasking_Policy, ORB,
               PolyORB.ORB.Interface.Queue_Request'
               (Request => Req,
                Requestor => Components.Component_Access (S)));
         end;
      else
         Process_Reply (S, Entity);
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
         return Handle_Message (Session (Sess.all)'Access, S);
         --  Call ancestor method.
      end if;
   end Handle_Message;

end PolyORB.Protocols.SOAP_Pr;
