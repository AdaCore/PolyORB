with Ada.Text_IO; use Ada.Text_IO;

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

with Droopi.Representations.SRP; use Droopi.Representations.SRP;

with GNAT.Regpat;

package body Droopi.Protocols.SRP is

   use Droopi.Components;
   use Droopi.Filters;
   use Droopi.Filters.Interface;
   use Droopi.Log;
   use Droopi.ORB;
   use Droopi.ORB.Interface;

   package L is new Droopi.Log.Facility_Log ("droopi.protocols.srp");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Rep : constant Rep_SRP_Access := new Rep_SRP;

   procedure Create
     (Proto   : access SRP_Protocol;
      Session : out Filter_Access)
   is
   begin
      --  This should be factored in Droopi.Protocols.

      Session := new SRP_Session;

      SRP_Session (Session.all).Buffer := new Buffers.Buffer_Type;
      SRP_Session (Session.all).Out_Buffer := new Buffers.Buffer_Type;

   end Create;

   procedure Connect (S : access SRP_Session) is
   begin
      null;
   end Connect;

   procedure Invoke_Request (S : access SRP_Session; R : Request) is
   begin
      null;
   end Invoke_Request;

   procedure Abort_Request (S : access SRP_Session; R : Request) is
   begin
      null;
   end Abort_Request;

   procedure Send_Reply (S : access SRP_Session; R : Request)
   is
      use Buffers;
      use Representations.SRP;

      B : Buffer_Access renames S.Out_Buffer;
      Data : CORBA.Any;
   begin
      Release_Contents (B.all);
      Data := CORBA.To_Any
        (CORBA.To_CORBA_String ("200 OK" & ASCII.CR & ASCII.LF &
                               Image (R) & ASCII.CR & ASCII.LF));
      Marshall_From_Any (Rep.all, B, Data);

      Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => B));
   end Send_Reply;

   procedure Handle_Connect_Indication (S : access SRP_Session) is
   begin
      pragma Debug (O ("Received new connection to SRP service..."));

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

   procedure Handle_Connect_Confirmation (S : access SRP_Session) is
   begin
      null;
      --  No setup is necessary for newly-created client connections.
   end Handle_Connect_Confirmation;

   --  type String_Array is array (Integer range <>) of String_Ptr;

   -----------
   -- Split --
   -----------

   --  Split the incoming string in according to the SRP protocol
   --  ??? TO MODIFY
   function Split (S : String) return Split_SRP;
   function Split (S : String) return Split_SRP
   is
      use GNAT.Regpat;
      use Objects;
      type String_Ptr is access String;

      Result  : Split_SRP;
      Args    : Arg_Info_Ptr := new Arg_Info;
      Current : Arg_Info_Ptr := Args;
      Last    : Arg_Info_Ptr;
      --  Result : String_Array (1 .. S'Length);
      --  Last : Integer := Result'First - 1;
      --  Word_First : Integer := S'First;
      --  Word_Last : Integer;

      --  ???
      --  WARNING : we consider a restrictive form of the Object_Id
      --  Should be changed
      Matches         : Match_Array (1 .. 255);
      Regexp_Req_OID  : String := "(\w+) (\d+)\?(.*)";
      Regexp_Args     : String := "(\w+)=(\w+)&?(.*)";
      --  Index : Integer := Result'First;
      Args_Ptr : String_Ptr;
   begin
      Match (Compile (Regexp_Req_OID), S, Matches);
      --  Stores the name of the function/procedure called
      Result.Method := new String'(S (Matches (1).First .. Matches (1).Last));
      Put_Line (Result.Method.all);

      --  Stores the Object Id
      --  Result.Oid := To_Oid (String'(S (Matches (2).First ..
      --                                   Matches (2).Last)));
      Result.Oid := new Object_Id'(To_Oid (String'(S (Matches (2).First ..
                                                      Matches (2).Last))));
      Put_Line (To_String (Result.Oid.all));

      --  Stores the last string containing the arguments
      Args_Ptr := new String'(S (Matches (3).First ..
                                 Matches (3).Last));
      pragma Warnings (Off, Args_Ptr);
      --  We want Args_Ptr to be able to be null

      --  ??? Could be optimized
      while Args_Ptr.all /= "" loop
         Match (Compile (Regexp_Args), Args_Ptr.all, Matches);
         Current.Name := new String'(Args_Ptr.all (Matches (1).First ..
                                                Matches (1).Last));
         Current.Value := new String'(Args_Ptr.all (Matches (2).First ..
                                                 Matches (2).Last));

         --  Create a new String with the remaining arguments
         Args_Ptr := new String'(Args_Ptr.all (Matches (3).First ..
                                               Matches (3).Last));
         Current.Next := new Arg_Info;
         Last := Current;
         Current := Current.Next;
      end loop;

      Last.Next := null;
      --  Destroy the last record (is empty)
      Free_Arg_Info (Current);

      Result.Args := Args;
      return Result;
   end Split;

   --  Same as above, but takes a CORBA.Any as an input parameter
   function Split (Data : CORBA.Any) return Split_SRP;
   function Split (Data : CORBA.Any) return Split_SRP
   is
   begin
      return Split (CORBA.To_Standard_String
                    (CORBA.From_Any (Data)));
   end Split;

   --  procedure Free (SA : in out String_Array);
   --  procedure Free (SA : in out String_Array) is
   --  begin
   --     for I in SA'Range loop
   --        Free (SA (I));
   --     end loop;
   --  end Free;

   procedure Handle_Data_Indication (S : access SRP_Session)
   is
      use CORBA;
      use CORBA.NVList;

      use Binding_Data.Local;
      use Objects;
      use References;

   begin
      pragma Debug (O ("Received data on SRP service..."));
      pragma Debug (Buffers.Show (S.Buffer.all));

      declare
         --  Argv : Split_SRP
         --    := Split (Unmarshall_String (Rep.all, S.Buffer));

         --  Argv : Split_SRP
         --    := Split (CORBA.To_Standard_String
         --         (CORBA.From_Any (Unmarshall_To_Any (Rep.all, S.Buffer))));

         Argv : Split_SRP
           := Split (Unmarshall_To_Any (Rep.all, S.Buffer));


         Method     : constant String := Argv.Method.all;
         Oid        : constant Object_Id := Argv.Oid.all;
         Args_Array : constant Arg_Info_Ptr := Argv.Args;
         Current    : Arg_Info_Ptr := Args_Array;

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
                             & " on object " & To_String (Oid)
                             & " with args "));

            declare
               procedure Print_Val (Current : Arg_Info_Ptr);
               procedure Print_Val (Current : Arg_Info_Ptr) is
                  Pointer : Arg_Info_Ptr := Current;
               begin
                  while Pointer /= null loop
                     Put_Line (Pointer.Name.all & " = " & Pointer.Value.all);
                     Pointer := Pointer.Next;
                  end loop;
               end Print_Val;
            begin
               --  while Current /= null loop
               --  pragma Debug (0 (Current.Name.all & " = " &
               --  Current.Value.all));
               --   Current := Current.Next;
               --  end loop;
               pragma Debug (Print_Val (Current));
               null;
            end;
            Current := Args_Array;

            --  Args   := Obj_Adapters.Get_Empty_Arg_List
            --    (Object_Adapter (ORB).all, Oid, Method);

            --  Stores the arguments in a NVList before creating the request
            CORBA.NVList.Create (Args);
            declare
               Simple_Arg : CORBA.NamedValue;
               Arg_Any : CORBA.Any;
            begin
               while Current /= null loop
                  Arg_Any := To_Any (To_CORBA_String (Current.Value.all));
                  Simple_Arg := (Name => To_CORBA_String (Current.Name.all),
                                 Argument => Arg_Any,
                                 Arg_Modes => CORBA.ARG_IN);
                  CORBA.NVList.Add_Item (Args, Simple_Arg);
                  Current := Current.Next;
               end loop;
            end;

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

         exception
            when E : others =>
               O ("Got exception: "
                  & Ada.Exceptions.Exception_Information (E));
         end;
      end;

      Expect_Data (S, S.Buffer, 1024);
      --  XXX Not exact amount.

      --  Prepare to receive next message.

   end Handle_Data_Indication;

   procedure Handle_Disconnect (S : access SRP_Session) is
   begin
      pragma Debug (O ("Received disconnect."));

      --  Cleanup protocol.

      Buffers.Release (S.Buffer);

   end Handle_Disconnect;

end Droopi.Protocols.SRP;
