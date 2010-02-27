------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . P R O T O C O L S . S O A P _ P R             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2010, Free Software Foundation, Inc.          --
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

with Ada.Strings.Unbounded;
with Ada.Exceptions;

with PolyORB.SOAP_P.Response;
with PolyORB.SOAP_P.Message;
with PolyORB.SOAP_P.Message.XML;
with PolyORB.SOAP_P.Message.Response;
with PolyORB.SOAP_P.Parameters;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Binding_Data;
with PolyORB.Binding_Data.Local;
with PolyORB.Binding_Data.SOAP;
with PolyORB.Buffer_Sources;
with PolyORB.Filters.AWS_Interface;
with PolyORB.Filters.Iface;
with PolyORB.HTTP_Methods;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Objects;
with PolyORB.ORB.Iface;
with PolyORB.Obj_Adapters;
with PolyORB.References;
with PolyORB.References.Binding;
with PolyORB.Servants.Iface;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package body PolyORB.Protocols.SOAP_Pr is

   use PolyORB.Filters.Iface;
   use PolyORB.Log;
   use PolyORB.ORB;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.soap_pr");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   procedure Initialize;

   procedure Process_Reply (S : access SOAP_Session);
   --  ??? comment needed

   -------------------
   -- Abort_Request --
   -------------------

   procedure Abort_Request
     (S : access SOAP_Session;
      R : Requests.Request_Access)
   is
   begin
      raise Program_Error;
   end Abort_Request;

   ------------
   -- Create --
   ------------

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

   ----------------------------
   -- Handle_Data_Indication --
   ----------------------------

   procedure Handle_Data_Indication
     (S           : access SOAP_Session;
      Data_Amount : Ada.Streams.Stream_Element_Count;
      Error       : in out Errors.Error_Container)
   is
      pragma Unreferenced (Error);
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

            The_ORB : constant ORB_Access := ORB_Access (S.Server);

            Target : References.Ref;
            Target_Profile : constant Binding_Data.Profile_Access
              := new Local_Profile_Type;
            --  Should be free'd when Target is finalized.

            function Path_To_Oid (Path : Types.String)
              return Objects.Object_Id_Access;

            -----------------
            -- Path_To_Oid --
            -----------------

            function Path_To_Oid (Path : Types.String)
              return Objects.Object_Id_Access
            is
            begin
               pragma Debug
                 (C, O ("Path_To_Oid: " & To_Standard_String (Path)));

               return PolyORB.Obj_Adapters.Rel_URI_To_Oid
                 (PolyORB.ORB.Object_Adapter (The_ORB),
                  PolyORB.Types.To_Standard_String (Path));
            end Path_To_Oid;

            The_Oid : Objects.Object_Id_Access := Path_To_Oid (S.Target);

            SOAP_Action_Msg : constant AWS_Interface.AWS_SOAP_Action
              := AWS_Interface.AWS_SOAP_Action
              (Components.Emit
               (Lower (S), AWS_Interface.AWS_Get_SOAP_Action'
                (null record)));

            Args : Any.NVList.Ref;
            --  Nil (not initialised).

            Unmarshall_Error : PolyORB.Errors.Error_Container;

            use PolyORB.Errors;

         begin
            Create_Local_Profile
              (The_Oid.all, Local_Profile_Type (Target_Profile.all));
            Objects.Free (The_Oid);
            References.Create_Reference
              ((1 => Target_Profile),
               Type_Id => "", R => Target);
            --  Create a temporary, typeless reference for this object.

            Result.Name := To_PolyORB_String ("Result");

            Handle_Unmarshall_Arguments (S, Args, Unmarshall_Error);

            --  As SOAP is a self-described protocol, we can set the argument
            --  list without waiting for the someone to tell us how to do it.
            --  If an error occurred, we need to note it now.

            Create_Request
              (Target    => Target,
               Operation => To_Standard_String
               (SOAP_Action_Msg.SOAP_Action),
               Arg_List  => Args,
               Result    => Result,
               Deferred_Arguments_Session => null,
               Req       => Req,
               Dependent_Binding_Object =>
                 Smart_Pointers.Entity_Ptr (S.Dependent_Binding_Object));

            if Found (Unmarshall_Error) then
               System_Exception_Members
                 (Unmarshall_Error.Member.all).Completed := Completed_No;
               Set_Exception (Req, Unmarshall_Error);
               Req.Completed := True;
               Catch (Unmarshall_Error);
            end if;

            S.Target := Types.To_PolyORB_String ("");
            S.Entity_Length := Data_Amount;

            ORB.Queue_Request_To_Handler (The_ORB,
              ORB.Iface.Queue_Request'
                (Request   => Req,
                 Requestor => Components.Component_Access (S)));
         end;
      else
         Process_Reply (S);
      end if;
   end Handle_Data_Indication;

   -------------------------------
   -- Handle_Connect_Indication --
   -------------------------------

   procedure Handle_Connect_Indication (S : access SOAP_Session) is
   begin
      S.Role := Server;
      Expect_Data (S, S.In_Buf, 0);
      --  Buffer used to receive request from the client
   end Handle_Connect_Indication;

   ---------------------------------
   -- Handle_Connect_Confirmation --
   ---------------------------------

   procedure Handle_Connect_Confirmation (S : access SOAP_Session) is
   begin
      S.Role := Client;
      Expect_Data (S, S.In_Buf, 0);
      --  Buffer used to receive reply from the server
   end Handle_Connect_Confirmation;

   -----------------------
   -- Handle_Disconnect --
   -----------------------

   procedure Handle_Disconnect
     (S : access SOAP_Session; Error : Errors.Error_Container)
   is
      use type Buffers.Buffer_Access;
      use SOAP_P.Message.Payload;

      P   : Requests.Request_Access;
      ORB : constant ORB_Access := ORB_Access (S.Server);

   begin
      if S.In_Buf /= null then
         Buffers.Release (S.In_Buf);
      end if;

      if S.Current_SOAP_Req /= null then
         Free (S.Current_SOAP_Req);
      end if;

      if S.Pending_Rq /= null then
         P := S.Pending_Rq;
         S.Pending_Rq := null;
         Set_Exception (P, Error);

         --  After the following call, S may become invalid

         References.Binding.Unbind (P.Target);

         --  After the following call, P is destroyed

         Components.Emit_No_Reply
           (Components.Component_Access (ORB),
            Servants.Iface.Executed_Request'(Req => P));
      end if;
   end Handle_Disconnect;

   ------------------
   -- Handle_Flush --
   ------------------

   procedure Handle_Flush (S : access SOAP_Session) is
   begin
      raise Program_Error;
   end Handle_Flush;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (Sess : access SOAP_Session;
      S    : Components.Message'Class) return Components.Message'Class
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

   ---------------------------------
   -- Handle_Unmarshall_Arguments --
   ---------------------------------

   procedure Handle_Unmarshall_Arguments
     (S     : access SOAP_Session;
      Args  : in out PolyORB.Any.NVList.Ref;
      Error : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Errors;
      Src : aliased Buffer_Sources.Input_Source;
   begin
      Buffer_Sources.Set_Buffer (Src, S.In_Buf);
      begin
         PolyORB.SOAP_P.Message.XML.Load_Payload
           (Src'Access, Args, S.Current_SOAP_Req);
      exception
         when others =>
            Throw (Error, Marshal_E, System_Exception_Members'
                 (Minor     => 1,
                  Completed => Completed_No));
      end;
      Buffers.Release_Contents (S.In_Buf.all);
   end Handle_Unmarshall_Arguments;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  No initialization necessary for this module
      null;
   end Initialize;

   --------------------
   -- Invoke_Request --
   --------------------

   procedure Invoke_Request
     (S   : access SOAP_Session;
      R   : Requests.Request_Access;
      Pro : access Binding_Data.Profile_Type'Class)
   is
      P    : PolyORB.SOAP_P.Message.Payload.Object;
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
         P := PolyORB.SOAP_P.Message.Payload.Build
           (R.Operation.all,
            PolyORB.SOAP_P.Parameters.List'(R.Args with null record));

      exception
         when E : others =>
            pragma Debug (C, O ("SOAP message: exception in Image:"));
            pragma Debug (C, O (Ada.Exceptions.Exception_Information (E)));

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
           (PolyORB.SOAP_P.Message.XML.Image (P))),
          SOAP_Action => Types.To_PolyORB_String (R.Operation.all)));
   end Invoke_Request;

   -------------------
   -- Process_Reply --
   -------------------

   procedure Process_Reply (S : access SOAP_Session)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      R           : constant Requests.Request_Access := S.Pending_Rq;
      Return_Args : PolyORB.Any.NVList.Ref;
      --  This is an empty NVList, since SOAP is a self-described
      --  protocol. Thus it can fill the returned arguments by itself

      Src : aliased Buffer_Sources.Input_Source;

   begin
      if R = null then
         raise PolyORB.SOAP_P.SOAP_Error;
         --  Received a reply with no pending request.
      end if;
      R.Result.Arg_Modes := ARG_OUT;
      --  Ensure proper mode for Result.

      Buffer_Sources.Set_Buffer (Src, S.In_Buf);
      PolyORB.SOAP_P.Message.XML.Load_Response (Src'Access, Return_Args);
      pragma Debug (C, O ("Process_Reply: processed "
                       & PolyORB.Types.Long'Image
                       (PolyORB.Any.NVList.Get_Count
                        (Return_Args))
                       & " arguments"));

      --  XXX BAD BAD this subprogram does not take into account
      --  the case where a FAULT or EXCEPTION has been received
      --  instead of a normal reply!!

      declare
         Res : NamedValue;

      begin
         Extract_First (List_Of (Return_Args).all, Res);

         if TypeCode.Kind (Get_Type (R.Result.Argument)) = Tk_Void then
            R.Result :=
              (Name      => PolyORB.Types.To_PolyORB_String ("result"),
               Argument  => Res.Argument,
               Arg_Modes => ARG_OUT);
         else
            Move_Any_Value (R.Result.Argument, Res.Argument);
         end if;
      end;
      --  Some applicative personnalities, like AWS, do not specify
      --  the type of the result they are expecting; other do, like
      --  CORBA. So we either copy the any data if the type of the
      --  namedvalue is specified, or simply set the namedvalue if its
      --  type is not specified.

      --  XXX We should consider changing this, by moving this kind of
      --  mechanism into the neutral layer. Thus, protocol
      --  personalities would send data to the neutral layer, like
      --  applicative personalities do for incoming arguments.

      S.Pending_Rq := null;
      Buffers.Release_Contents (S.In_Buf.all);
      Components.Emit_No_Reply
        (R.Requesting_Component,
         Servants.Iface.Executed_Request'(Req => R));
   end Process_Reply;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
      (S : access SOAP_Session;
       R : Requests.Request_Access)
   is
      use PolyORB.Components;
      use type PolyORB.SOAP_P.Message.Payload.Object_Access;
   begin
      if S.Current_SOAP_Req = null then
         --  Fatal error, no known current request
         --  ??? we should send some feedback to the client. For now we just
         --  give up and close the connection.

         Emit_No_Reply (Component_Access (S),
           Disconnect_Request'(null record));
         return;
      end if;

      declare
         use PolyORB.Any;
         use PolyORB.Any.NVList;
         use PolyORB.Any.NVList.Internals;
         use PolyORB.Any.NVList.Internals.NV_Lists;

         use PolyORB.SOAP_P.Parameters;

         RO : PolyORB.SOAP_P.Message.Response.Object
           := PolyORB.SOAP_P.Message.Response.From
           (PolyORB.SOAP_P.Message.Payload.Object (S.Current_SOAP_Req.all));
         RP : PolyORB.SOAP_P.Parameters.List;

         It  : Iterator := First (List_Of (R.Args).all);
         Arg : Element_Access;
      begin
         PolyORB.SOAP_P.Message.Payload.Free (S.Current_SOAP_Req);
         RP := +R.Result;
         while not Last (It) loop
            Arg := Value (It);
            if False
              or else Arg.Arg_Modes = ARG_INOUT
              or else Arg.Arg_Modes = ARG_OUT
            then
               RP := RP & Arg.all;
            end if;
            Next (It);
         end loop;

         PolyORB.SOAP_P.Message.Set_Parameters (RO, RP);
         declare
            RD : constant PolyORB.SOAP_P.Response.Data :=
                   PolyORB.SOAP_P.Message.Response.Build (RO);
            --  Here we depend on a violation of abstraction: we construct an
            --  AWS response object, and AWS is HTTP-specific. This is a
            --  shortcoming of the AWS SOAP engine. It is unknown yet whether
            --  this violation can be easily removed.

         begin
            Components.Emit_No_Reply
              (Lower (S),
               Filters.AWS_Interface.AWS_Response_Out'
               (Data => RD));
         end;
      end;
   end Send_Reply;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"protocols.soap",
       Conflicts => Empty,
       Depends   => +"http_methods" & "http_headers",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Protocols.SOAP_Pr;
