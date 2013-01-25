------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . P R O T O C O L S . E C H O                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

--  A dummy protocol, just for testing.

with Ada.Exceptions;

with PolyORB.Any.NVList;

with PolyORB.Binding_Data.Local;
with PolyORB.Filters;
with PolyORB.Filters.Iface;
with PolyORB.Log;

with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.ORB;
with PolyORB.ORB.Iface;
with PolyORB.References;

with PolyORB.Representations.Test; use PolyORB.Representations.Test;
with PolyORB.Types; use PolyORB.Types;
with PolyORB.Utils.Strings; use PolyORB.Utils.Strings;

package body PolyORB.Protocols.Echo is

   use PolyORB.Components;
   use PolyORB.Filters.Iface;
   use PolyORB.Log;
   use PolyORB.ORB;
   use PolyORB.ORB.Iface;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.echo");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   Rep : Rep_Test;

   overriding procedure Create
     (Proto   : access Echo_Protocol;
      Session : out Filter_Access)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Proto);
      pragma Warnings (On);

      --  This should be factored in PolyORB.Protocols.

      Session := new Echo_Session;

      --  That is Echo-specific. Or is it?

      Echo_Session (Session.all).Buffer := new Buffers.Buffer_Type;
      Echo_Session (Session.all).Out_Buffer := new Buffers.Buffer_Type;

   end Create;

   overriding procedure Invoke_Request
     (S : access Echo_Session;
      R : Request_Access;
      P : access Binding_Data.Profile_Type'Class) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Unreferenced (R);
      pragma Unreferenced (P);
      pragma Warnings (On);
      null;
   end Invoke_Request;

   overriding procedure Abort_Request
     (S : access Echo_Session;
      R : Request_Access)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Unreferenced (R);
      pragma Warnings (On);
      null;
   end Abort_Request;

   overriding procedure Send_Reply
     (S : access Echo_Session;
      R : Request_Access)
   is
      use Buffers;

      B : Buffer_Access renames S.Out_Buffer;

   begin
      Release_Contents (B.all);
      Marshall_String (Rep, B, "200 OK" & ASCII.CR & ASCII.LF);
      Marshall_String (Rep, B, "Request: "
                       & Image (R.all) & ASCII.CR & ASCII.LF);
      Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => B));
   end Send_Reply;

   overriding procedure Handle_Connect_Indication (S : access Echo_Session) is
   begin
      pragma Debug (C, O ("Received new connection to echo service..."));

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

   overriding procedure Handle_Connect_Confirmation
     (S : access Echo_Session)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Warnings (On);
      null;
      --  No setup is necessary for newly-created client connections.
   end Handle_Connect_Confirmation;

   type String_Array is array (Integer range <>) of Utils.Strings.String_Ptr;

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

   overriding procedure Handle_Data_Indication
     (S           : access Echo_Session;
      Data_Amount : Ada.Streams.Stream_Element_Count;
      Error       : in out Errors.Error_Container)
   is
      use Binding_Data.Local;
      use Objects;
      use References;

   begin
      pragma Warnings (Off);
      pragma Unreferenced (Data_Amount, Error);
      pragma Warnings (On);
      pragma Debug (C, O ("Received data on echo service..."));
      pragma Debug (Buffers.Show (S.Buffer));

      declare
         Argv : String_Array
           := Split (Unmarshall_String (Rep, S.Buffer));

         Method     : constant String   := Argv (1).all;
         Oid        : aliased Object_Id := Hex_String_To_Oid (Argv (2).all);
         Arg_String : constant String   := Argv (3).all;

         Req : Request_Access := null;
         Args   : Any.NVList.Ref;
         Result : Any.NamedValue;

         Target_Profile : constant Binding_Data.Profile_Access
           := new Local_Profile_Type;
         Target : References.Ref;

         ORB : constant ORB_Access := ORB_Access (S.Server);

      begin
         Buffers.Release_Contents (S.Buffer.all);
         --  Clear buffer

         begin
            pragma Debug (C, O ("Received request " & Method
                             & " on object " & Image (Oid)
                             & " with args " & Arg_String));

            Args   := Obj_Adapters.Get_Empty_Arg_List
              (Object_Adapter (ORB), Oid'Access, Method);
            Result :=
              (Name     => To_PolyORB_String ("Result"),
               Argument => Obj_Adapters.Get_Empty_Result
               (Object_Adapter (ORB), Oid'Access, Method),
               Arg_Modes => 0);

            Create_Local_Profile
              (Oid, Local_Profile_Type (Target_Profile.all));
            Create_Reference
              (Profiles => (1 => Target_Profile),
               Type_Id  => "",
               --  Unknown (not carried by this protocol)
               R        => Target);

            Create_Request
              (Target    => Target,
               Operation => Method,
               Arg_List  => Args,
               Result    => Result,
               Req       => Req);

            --  This request is submitted to the ORB by internal activity,
            --  not by a transient task lent by the application:
            --  set Requesting_Task to null.

            Queue_Request_To_Handler (ORB,
              Queue_Request'(Request   => Req,
                             Requestor => Component_Access (S)));

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

   overriding procedure Handle_Disconnect
     (S : access Echo_Session; Error : Errors.Error_Container)
   is
      pragma Unreferenced (Error);
   begin
      pragma Debug (C, O ("Received disconnect."));

      --  Cleanup protocol

      Buffers.Release (S.Buffer);
   end Handle_Disconnect;

   overriding procedure Handle_Flush (S : access Echo_Session) is
   begin
      raise Program_Error;
   end Handle_Flush;

end PolyORB.Protocols.Echo;
