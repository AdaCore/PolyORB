--  A dummy protocol, just for testing.

--  $Id$

with Ada.Exceptions;

with CORBA;
with CORBA.NVList;

with Droopi.Binding_Data.Local;
with Droopi.Buffers;
with Droopi.Log;
with Droopi.Objects;
with Droopi.References;
with Droopi.Requests; use Droopi.Requests;
with Droopi.Schedulers;

with Droopi.Representations.Test; use Droopi.Representations.Test;

package body Droopi.Protocols.Echo is

   use Droopi.Log;

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

   end Create;

   procedure Invoke_Request (S : access Echo_Session; R : Request) is
   begin
      null;
   end Invoke_Request;

   procedure Abort_Request (S : access Echo_Session; R : Request) is
   begin
      null;
   end Abort_Request;

   procedure Handle_Connect (S : access Echo_Session) is
   begin
      --  Send_String ("Hello, please type data." & ASCII.LF);
      pragma Debug (O ("Received new connection to echo service..."));
      Expect_Data (S, S.Buffer, 1024
      --  Exact => False
                   );
   end Handle_Connect;

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
      begin
         Buffers.Release_Contents (S.Buffer.all);
         --  Clear buffer

         begin
            pragma Debug (O ("Received request " & Method
                             & " on object " & Image (Oid)
                             & " with args " & Arg_String));

            --  Args := Get_Empty_Arg_List (OA, Oid, Method);
            --  Result := Get_Empty_Result (OA, Oid, Method);

            Create_Local_Profile
              (Oid, Local_Profile_Type (Target_Profile.all));
            Create_Reference ((1 => Target_Profile), Target);

            Create_Request
              (Target    => Target,
               Operation => Method,
               Arg_List  => Args,
               Result    => Result,
               Req       => Req);

            Schedulers.Queue_Request (S.Server, Req);

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

