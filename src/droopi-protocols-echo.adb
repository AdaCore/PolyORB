--  A dummy protocol, just for testing.

--  $Id$

with Ada.Exceptions;

with CORBA;
with CORBA.NVList;

with Droopi.Binding_Data.Local;
with Droopi.Buffers;
with Droopi.Filters;
with Droopi.Filters.Interface;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

with Droopi.Obj_Adapters;
with Droopi.Objects;
with Droopi.ORB;
with Droopi.ORB.Interface;
with Droopi.References;
with Droopi.Requests; use Droopi.Requests;

with Droopi.Representations.Test; use Droopi.Representations.Test;

package body Droopi.Protocols.Echo is

   use Droopi.Components;
   use Droopi.Filters;
   use Droopi.Filters.Interface;
   use Droopi.Log;
   use Droopi.ORB;
   use Droopi.ORB.Interface;

   package L is new Droopi.Log.Facility_Log ("droopi.protocols.echo");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Rep : constant Rep_Test_Access := new Rep_Test;

   procedure Create
     (Proto   : access Echo_Protocol;
      Session : out Filter_Access)
   is
   begin

      --  This should be factored in Droopi.Protocols.

      Session := new Echo_Session;

      --  That is Echo-specific. Or is it?

      Echo_Session (Session.all).Buffer := new Buffers.Buffer_Type;
      Echo_Session (Session.all).Out_Buffer := new Buffers.Buffer_Type;

   end Create;

   procedure Invoke_Request (S : access Echo_Session; R : Request) is
   begin
      null;
   end Invoke_Request;

   procedure Abort_Request (S : access Echo_Session; R : Request) is
   begin
      null;
   end Abort_Request;

   procedure Send_Reply (S : access Echo_Session; R : Request)
   is
      use Buffers;
      use Representations.Test;

      B : Buffer_Access renames S.Out_Buffer;

   begin
      Release_Contents (B.all);
      Marshall_String (Rep, B, "200 OK" & ASCII.CR & ASCII.LF);
      Marshall_String (Rep, B, "Request: "
                       & Image (R) & ASCII.CR & ASCII.LF);
      Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => B));
   end Send_Reply;

   procedure Handle_Connect_Indication (S : access Echo_Session) is
   begin
      pragma Debug (O ("Received new connection to echo service..."));

      --  1. Send greetings to client.

      --  Send_String ("Hello, please type data." & ASCII.LF);

      --  2. Notify transport layer that more data is expected.

      Expect_Data (S, S.Buffer, 1024);
      --  Exact => False

      --  Note that there is no race condition here. One might
      --  expect the following unfortunate sequence of events:
      --    10. Greetings sent to client
      --    11. Client answers
      --    20. Expect_Data
      --  (in 11: transport gets unexpected data).
      --  This does not actually happen because the TE is not
      --  being monitored while Send_Greetings and Expect_Data
      --  are done; it becomes monitored again /after/ the
      --  Connect_Indication has been processed.
      --
      --  The same goes for the handling of a Data_Indication.

   end Handle_Connect_Indication;

   procedure Handle_Connect_Confirmation (S : access Echo_Session) is
   begin
      null;
      --  No setup is necessary for newly-created client connections.
   end Handle_Connect_Confirmation;

   type String_Array is array (Integer range <>) of String_Ptr;

   function Split (S : String) return String_Array;
   function Split (S : String) return String_Array
   is
      Result : String_Array (1 .. S'Length);
      Last : Integer := Result'First - 1;
      Word_First : Integer := S'First;
      Word_Last : Integer;
   begin
      while Word_First <= S'Last loop
         Word_Last := Word_First - 1;
         Last := Last + 1;
         while Word_Last < S'Last and then S (Word_Last + 1) /= ' ' loop
            Word_Last := Word_Last + 1;
         end loop;
         Result (Last) := new String'(S (Word_First .. Word_Last));
         Word_First := Word_Last + 1;
         while Word_First <= S'Last and then S (Word_First) = ' ' loop
            Word_First := Word_First + 1;
         end loop;
      end loop;

      return Result (Result'First .. Last);
   end Split;

   procedure Free (SA : in out String_Array);
   procedure Free (SA : in out String_Array) is
   begin
      for I in SA'Range loop
         Free (SA (I));
      end loop;
   end Free;

   procedure Handle_Data_Indication (S : access Echo_Session)
   is
      use CORBA;
      use CORBA.NVList;

      use Binding_Data.Local;
      use Objects;
      use References;

   begin
      pragma Debug (O ("Received data on echo service..."));
      pragma Debug (Buffers.Show (S.Buffer.all));

      declare
         Argv : String_Array
           := Split (Unmarshall_String (Rep, S.Buffer));

         Method     : constant String := Argv (1).all;
         Oid        : constant Object_Id := To_Oid (Argv (2).all);
         Arg_String : constant String := Argv (3).all;

         Req : Request_Access := null;
         Args   : CORBA.NVList.Ref;
         Result : CORBA.NamedValue;

         Target_Profile : Binding_Data.Profile_Access
           := new Local_Profile_Type;
         Target : References.Ref;

         ORB : constant ORB_Access := ORB_Access (S.Server);

      begin
         Buffers.Release_Contents (S.Buffer.all);
         --  Clear buffer

         begin
            pragma Debug (O ("Received request " & Method
                             & " on object " & Image (Oid)
                             & " with args " & Arg_String));

            Args   := Obj_Adapters.Get_Empty_Arg_List
              (Object_Adapter (ORB).all, Oid, Method);
            Result :=
              (Name     => To_CORBA_String ("Result"),
               Argument => Obj_Adapters.Get_Empty_Result
               (Object_Adapter (ORB).all, Oid, Method),
               Arg_Modes => 0);

            Create_Local_Profile
              (Oid, Local_Profile_Type (Target_Profile.all));
            Create_Reference ((1 => Target_Profile), Target);

            Create_Request
              (Target    => Target,
               Operation => Method,
               Arg_List  => Args,
               Result    => Result,
               Req       => Req);

            Emit_No_Reply
              (Component_Access (ORB),
               Queue_Request'(Request   => Req,
                              Requestor => Component_Access (S),
                              Requesting_Task => null));
            --  This request is submitted to the ORB by internal
            --  activity, not by a transient task lent by the
            --  application. Requesting_Task is therefore null.

         exception
            when E : others =>
               O ("Got exception: "
                  & Ada.Exceptions.Exception_Information (E));
         end;
         Free (Argv);
      end;

      Expect_Data (S, S.Buffer, 1024);
      --  XXX Not exact amount.

      --  Prepare to receive next message.

   end Handle_Data_Indication;

   procedure Handle_Disconnect (S : access Echo_Session) is
   begin
      pragma Debug (O ("Received disconnect."));

      --  Cleanup protocol.

      Buffers.Release (S.Buffer);

   end Handle_Disconnect;

end Droopi.Protocols.Echo;

