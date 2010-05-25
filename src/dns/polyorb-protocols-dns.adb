------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . P R O T O C O L S . D N S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010, Free Software Foundation, Inc.          --
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
with  PolyORB.Log;
with PolyORB.Servants.Iface;
with PolyORB.Annotations;
with PolyORB.References.Binding;
with PolyORB.Any.NVList;
with PolyORB.Binding_Data;
with PolyORB.Errors;
with PolyORB.References;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;
with Ada.Text_IO;
with PolyORB.Representations.DNS;
with PolyORB.Utils;
with PolyORB.Objects;
with PolyORB.Binding_Data.Local;
with PolyORB.Obj_Adapters;
with PolyORB.Smart_Pointers;
with PolyORB.ORB.Iface;
with PolyORB.POA;
with PolyORB.Binding_Objects;
with PolyORB.Any;
with PolyORB.POA_Types;
--  with PolyORB.Transport;
--  with PolyORB.Sockets;
--  with PolyORB.Transport.Datagram.Sockets_Out;
--  with PolyORB.Parameters;
--  with PolyORB.Utils.Sockets;
--  with PolyORB.Obj_Adapters.Simple;
--  with PolyORB.POA_Manager;
--  with PolyORB.POA_Config;
package body PolyORB.Protocols.DNS is
--   use PolyORB.Transport.Datagram.Sockets_Out;
   use PolyORB.Representations.DNS;
   use PolyORB.Binding_Objects;
   use PolyORB.Annotations;
   use PolyORB.Components;
   use PolyORB.Log;
   use PolyORB.References.Binding;
   use PolyORB.ORB;
   use PolyORB.Tasking;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Servants.Iface;
   use PolyORB.Types;
   use Ada.Text_IO;
   use PolyORB.Filters.Iface;
   use PolyORB.Utils;
--   use PolyORB.Sockets;
--   use PolyORB.Parameters;
--    use PolyORB.Utils.Sockets;
--   use PolyORB.POA_Manager;
--   use PolyORB.POA_Config;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.dns");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;

   function C (Level : Log_Level := Debug) return Boolean
               renames L.Enabled;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (S : in out DNS_Session) is
   begin
      pragma Debug (C, O ("Initializing DNS session"));
      --  we need to create a mutex to deal with pending requests
      Tasking.Mutexes.Create (S.Mutex);
      S.Buffer_In := new Buffer_Type;
   end Initialize;

   ------------
   -- Create --
   ------------

   procedure Create
     (Proto   : access DNS_Protocol;
      Session :    out Filter_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Proto);
      pragma Warnings (On);
   begin
      Session := new DNS_Session;
      pragma Debug (C, O ("Creating DNS Session"));
      Initialize (DNS_Session (Session.all));
   end Create;

   --------------------
   -- Invoke_Request --
   --------------------

   procedure Invoke_Request
     (Sess : access DNS_Session;
      R    :        Requests.Request_Access;
      Pro  : access Binding_Data.Profile_Type'Class)
   is
      use PolyORB.Binding_Data;
      use PolyORB.Errors;
      use Unsigned_Long_Flags;

      New_Pending_Req    : Pending_Request_Access;
      New_Pending_Req_Id : Types.Unsigned_Long;
      Error              : Errors.Error_Container;
      Success            : Boolean;
   begin

      New_Pending_Req := new Pending_Request;
      New_Pending_Req.Req := R;
      New_Pending_Req.Target_Profile := Profile_Access (Pro);

      Enter (Sess.Mutex);
      if Is_Set (Sync_None, R.Req_Flags)
        or else Is_Set (Sync_With_Transport, R.Req_Flags)
      then

         --  Oneway call: we won't see any reply for this request, so we need
         --  to destroy the pending request information now.

         New_Pending_Req.Request_Id := Get_Request_Id (Sess);
         Leave (Sess.Mutex);
         pragma Debug (C, O ("One way call : No reply expected"));
         Send_Request (Sess, New_Pending_Req, Error);
         Free (New_Pending_Req);

         if Found (Error) then
            Set_Exception (R, Error);
            Catch (Error);
         end if;
         return;
      end if;

         --  Two-way call: a reply is expected, we store the pending request
         Add_Pending_Request (Sess, New_Pending_Req);
         New_Pending_Req_Id := New_Pending_Req.Request_Id;
         Leave (Sess.Mutex);

         pragma Debug (C, O ("Two way call : a reply is expected"));
         Send_Request (Sess, New_Pending_Req, Error);

      if Found (Error) then
         pragma Debug (C, O ("An error is found after Send_Request"));
         Remove_Pending_Request (Sess, New_Pending_Req_Id, Success);
         if Success then
            Set_Exception (R, Error);

         else
            pragma Assert (Sess.State = Not_Initialized);
            null;
         end if;

         Catch (Error);

         declare
            ORB : constant ORB_Access := ORB_Access (Sess.Server);
         begin
            Emit_No_Reply
              (Component_Access (ORB),
               Servants.Iface.Executed_Request'(Req => R));
         end;
      end if;
      pragma Debug (C, O ("Invoke_Request : leaving"));
   end Invoke_Request;

   -------------------
   -- Abort_Request --
   -------------------

   procedure Abort_Request
     (S : access DNS_Session;
      R    :        Requests.Request_Access) is
   begin
      null; --  Process_Abort_Request (S.Implem, S, R);
   end Abort_Request;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (S       : access DNS_Session;
      Request :        Requests.Request_Access)
   is

      use PolyORB.Errors;

      Sess  : DNS_Session renames DNS_Session (S.all);
      Error : Errors.Error_Container;
   begin
      if Sess.Role = Client then
         raise DNS_Error;
      end if;

      Common_Send_Reply
        (Sess'Access,
         Request,
         Error);
      --  If an error is found, we send back an exception
      if Found (Error) then
         Set_Exception (Request, Error);
         Catch (Error);

         Common_Send_Reply
           (Sess'Access,
            Request,
            Error);

         if Found (Error) then
            Catch (Error);
            raise DNS_Error;
         end if;
      end if;
   end Send_Reply;
      -----------------------
   -- Common_Send_Reply --
   -----------------------

   procedure Common_Send_Reply
     (Sess           : access DNS_Session;
      Request        :        Requests.Request_Access;
      Error          : in out Errors.Error_Container)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Errors;
      use type PolyORB.Any.TypeCode.Local_Ref;

      Buffer_Out      : Buffer_Access := new Buffer_Type;
      Header_Buffer   : Buffer_Access := new Buffer_Type;
      Header_Space    : Reservation;
      It : Iterator;
      Arg : Element_Access;
   begin

      if PolyORB.Any.Is_Empty (Request.Exception_Info) then
         Sess.MCtx.Rcode_Flag  := No_Error;
      end if;

      case Sess.MCtx.Rcode_Flag is

         when No_Error =>
            pragma Debug (C, O ("Using Any to send reply data"));
            null;
         when others =>
            null;
      end case;

      Set_Endianness (Buffer_Out, Big_Endian);
      Set_Endianness (Header_Buffer, Big_Endian);
      Header_Space := Reserve (Buffer_Out, DNS_Header_Size);

      --  Retrieving the number of answers here
      --  XXX temporary fix
      It := First (List_Of (Request.Out_Args).all);
      Next (It);
      Next (It);
      pragma Debug (C, O ("Out_Args : " &
        Length (List_Of (Request.Out_Args).all)'Img));
      Arg := Value (It);
      Sess.MCtx.Nb_Answers := Types.Unsigned_Short
        (Get_Aggregate_Count
        (Aggregate_Content'Class (Get_Value (Get_Container
            (Arg.Argument).all).all)) - 1);

      Marshall_Latin_1_String (Buffer_Out, Sess.MCtx.Request_Name);
      Marshall (Buffer_Out, Types.Unsigned_Short (12));
      Marshall (Buffer_Out, Types.Unsigned_Short (1));

      --  this is a response , we add the ttl field
      --  XXX temporary fix for Unsigned_Long marshalling
      Marshall (Buffer_Out, Types.Unsigned_Short (0));
      Marshall (Buffer_Out, Types.Unsigned_Short (240));

      Marshall_From_Any (Buffer_Out,
                          Get_Container (Arg.Argument).all, True, Error);
      --  Copy Header
      Marshall_DNS_Header_Reply
        (Header_Buffer, Request, Sess.MCtx);
      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
      --  Emit reply
      Show (Buffer_Out);
      Emit_Message (Sess, Buffer_Out, Error);
      Release (Buffer_Out);
      pragma Debug (C, O ("Reply sent"));
   end Common_Send_Reply;
   -------------------------------
   -- Handle_Connect_Indication --
   -------------------------------

   procedure Handle_Connect_Indication
     (S : access DNS_Session) is
   begin
      pragma Debug (C, O ("Handle_Connect_Indication"));
      pragma Assert (S.State = Not_Initialized);
      S.Role := Server;
      Show  (S.Buffer_In);
      Initialize_Session (S);
      Expect_DNS_Header (S);
   end Handle_Connect_Indication;

   ---------------------------------
   -- Handle_Connect_Confirmation --
   ---------------------------------

   procedure Handle_Connect_Confirmation (S : access DNS_Session) is
   begin
      pragma Debug (C, O ("Handle_Connect_Confirmation"));
      pragma Assert (S.State = Not_Initialized);
      S.Role := Client;
      Initialize_Session (S);
      Expect_DNS_Header (S);
   end Handle_Connect_Confirmation;

--  ----------------------------
--     -- Handle_Data_Indication --
--     ----------------------------

   procedure Handle_Data_Indication
     (Sess       : access DNS_Session;
      Data_Amount : Ada.Streams.Stream_Element_Count;
      Error       : in out Errors.Error_Container)
   is
      use Errors;
      Label_Size : Types.Octet;

   begin
      pragma Debug (C, O ("Handle_Data_Indication : Enter"));
      pragma Debug (C, O ("Received data in state " & Sess.State'Img));
      pragma Assert (Sess.State /= Not_Initialized);
      case Sess.State is

         when Expect_Header =>
            pragma Debug (C, O ("Received Header Size " & Data_Amount'Img));
            Unmarshall_DNS_Header (Sess.MCtx, Sess.Buffer_In);
            if Sess.Role = Client then
               Reply_Received (Sess, Request_Id => 1,
                               Rcode      => Sess.MCtx.Rcode_Flag);
            else
            --  At this point we have all header fields stored in Sess.MCtx
            --  If the message is a question
               if Sess.MCtx.Message_Type = Request then
                  for J in 1 .. Sess.MCtx.Nb_Questions loop
                     Sess.State := Expect_Name;
                     --  We need to receive the length of the question name
                     Emit_No_Reply
                      (Port => Lower (Sess),
                       Msg  => DNS_Data_Expected'
                       (In_Buf => Sess.Buffer_In,
                        Max    => Stream_Element_Count
                       (1),
                       State  => Sess.State));
                  end loop;
               end if;
            end if;

         --  at this point we have received the request name size
         when Expect_Name =>
            pragma Debug (C, O ("Received DNS message body"));
            pragma Debug (C, Show (Sess.Buffer_In));

            --  we unmarshall the request name size

            Label_Size := Unmarshall (Sess.Buffer_In);
            Sess.State := Expect_Body;

            --  if it is 0 then we've already received the name, do nothing
            if Label_Size /= Types.Octet (0) then
               pragma Debug (C, O ("Label is of size " & Label_Size'Img));

               Sess.MCtx.Request_Name_Length :=
                 Types.Unsigned_Short (Label_Size);

               --  we now need to receive the rest of the message
               --  size = request's name size + 5 bytes for the rest
               --  of the message + 1 byte of data alingment,if needed
               Emit_No_Reply
                 (Port => Lower (Sess),
                  Msg  => DNS_Data_Expected'
                    (In_Buf => Sess.Buffer_In,
                     Max    => Stream_Element_Count
                       (Label_Size + 5 + (Label_Size mod 2)),
                       State  => Sess.State));
            end if;

         when Expect_Body =>
            Process_Request (Sess);
            Expect_DNS_Header (Sess);
         when others =>

            Throw
              (Error,
               Comm_Failure_E,
               System_Exception_Members'(0, Completed_Maybe));

      end case;
      pragma Debug (C, O ("Handle_Data_Indication : Leave"));
   exception
      when others =>
         Throw
           (Error,
            Comm_Failure_E,
            System_Exception_Members'(0, Completed_Maybe));
   end Handle_Data_Indication;

   -----------------------
   -- Handle_Disconnect --
   -----------------------

   procedure Handle_Disconnect
     (Sess : access DNS_Session; Error : Errors.Error_Container)
   is
      use Pend_Req_Tables;
      P     : Pending_Request_Access;
      ORB   : constant ORB_Access := ORB_Access (Sess.Server);

   begin
      pragma Debug (C, O ("Handle_Disconnect: enter"));

      Enter (Sess.Mutex);

      Sess.State := Not_Initialized;

      if Sess.Buffer_In /= null then
         Release (Sess.Buffer_In);
      end if;

      for J in First (Sess.Pending_Reqs) .. Last (Sess.Pending_Reqs) loop
         if Sess.Pending_Reqs.Table /= null
           and then Sess.Pending_Reqs.Table (J) /= null
         then
            P := Sess.Pending_Reqs.Table (J);
            Sess.Pending_Reqs.Table (J) := null;
            Set_Exception (P.Req, Error);
            References.Binding.Unbind (P.Req.Target);

            --  After the following call, P.Req is destroyed

            Emit_No_Reply (Component_Access (ORB),
                           Servants.Iface.Executed_Request'(Req => P.Req));

            Free (P);
         end if;
      end loop;

      --  All pending request entries have been cleared: reset table

      Set_Last (Sess.Pending_Reqs, First (Sess.Pending_Reqs) - 1);

      Leave (Sess.Mutex);
      pragma Debug (C, O ("Handle_Disconnect: leave"));
   end Handle_Disconnect;

   ------------------
   -- Handle_Flush --
   ------------------

   procedure Handle_Flush (S : access DNS_Session)
     renames Expect_DNS_Header;

   ------------------------
   -- Expect_DNS_Header --
   ------------------------

   --  called to wait another DNS message
   procedure Expect_DNS_Header
     (Sess : access DNS_Session) is
   begin

      --  Check if buffer has been totally read

      if Remaining (Sess.Buffer_In) /= 0 then
         pragma Debug (C, O ("Remaining data in buffer :"
                          & Remaining (Sess.Buffer_In)'Img
                          & " bytes"));
         null;
      end if;

      pragma Debug (C, O ("Waiting for next message"));

      Buffers.Release_Contents (Sess.Buffer_In.all);

      pragma Debug (C, O ("Expect Header : Here Buffer_In is empty"));
      Set_Endianness (Sess.Buffer_In, Big_Endian);
      Sess.State := Expect_Header;
      if Sess.Role = Server then
         Emit_No_Reply
           (Port => Lower (Sess),
            Msg  => DNS_Data_Expected'
             (In_Buf => Sess.Buffer_In,
              Max    => DNS_Header_Size,
              State  => Sess.State));
      else
         Emit_No_Reply
           (Port => Lower (Sess),
            Msg  => Data_Expected'
             (In_Buf => Sess.Buffer_In,
              Max    => DNS_Header_Size));
      end if;
   end Expect_DNS_Header;

   --------------------
   -- Get_Request_Id --
   --------------------

   function Get_Request_Id
     (Sess : access DNS_Session) return Types.Unsigned_Long
   is
      R : constant Types.Unsigned_Long := Sess.Req_Index;
   begin
      Sess.Req_Index := Sess.Req_Index + 1;
      return R;
   end Get_Request_Id;

   procedure Add_Pending_Request
     (Sess     : access DNS_Session;
      Pend_Req : Pending_Request_Access)
   is
      use Pend_Req_Tables;
      Request_Id : Types.Unsigned_Long;
   begin
      Request_Id := Get_Request_Id (Sess);
      pragma Debug (C, O ("Adding pending request with id" & Request_Id'Img));
      Set_Note
        (Pend_Req.Req.Notepad,
         Request_Note'(Annotations.Note with Id => Request_Id));
      Pend_Req.Request_Id := Request_Id;

      for J in First (Sess.Pending_Reqs) .. Last (Sess.Pending_Reqs) loop
         if Sess.Pending_Reqs.Table (J) = null then
            Sess.Pending_Reqs.Table (J) := Pend_Req;
            return;
         end if;
      end loop;

      Increment_Last (Sess.Pending_Reqs);
      Sess.Pending_Reqs.Table (Last (Sess.Pending_Reqs)) := Pend_Req;
   end Add_Pending_Request;

   procedure Get_Pending_Request
     (Sess    : access DNS_Session;
      Id      :        Types.Unsigned_Long;
      Req     :    out Pending_Request;
      Success :    out Boolean;
      Remove  :        Boolean := True)
   is
      use Pend_Req_Tables;

      PRA : Pending_Request_Access;
   begin
      pragma Debug (C, O ("Retrieving pending request with id"
                       & Types.Unsigned_Long'Image (Id)));

      Success := False;
      Enter (Sess.Mutex);

      for J in First (Sess.Pending_Reqs) .. Last (Sess.Pending_Reqs) loop
         if Sess.Pending_Reqs.Table (J) /= null
           and then Sess.Pending_Reqs.Table (J).Request_Id = Id
         then
            PRA := Sess.Pending_Reqs.Table (J);
            if Remove then
               Sess.Pending_Reqs.Table (J) := null;
            end if;
            Req := PRA.all;
            if Remove then
               Free (PRA);
            end if;
            Success := True;
            exit;
         end if;
      end loop;

      Leave (Sess.Mutex);
   end Get_Pending_Request;

   procedure Remove_Pending_Request
     (Sess    : access DNS_Session;
      Id      : Types.Unsigned_Long;
      Success : out Boolean)
   is
      use Pend_Req_Tables;

      PRA : Pending_Request_Access;
   begin
      pragma Debug (C, O ("Retrieving pending request with id"
                       & Types.Unsigned_Long'Image (Id)));

      Enter (Sess.Mutex);
      Success := False;

      for J in First (Sess.Pending_Reqs) .. Last (Sess.Pending_Reqs) loop
         if Sess.Pending_Reqs.Table (J) /= null
           and then Sess.Pending_Reqs.Table (J).Request_Id = Id
         then
            PRA := Sess.Pending_Reqs.Table (J);
            Sess.Pending_Reqs.Table (J) := null;
            Free (PRA);
            Success := True;
            exit;
         end if;
      end loop;

      Leave (Sess.Mutex);
   end Remove_Pending_Request;

   ------------------
   -- Emit Message --
   ------------------

   procedure Emit_Message
     (S      : access Session'Class;
      Buffer : Buffer_Access;
      Error  : in out Errors.Error_Container)
   is
      M : constant Message'Class :=
        Emit (Lower (S), Data_Out'(Out_Buf => Buffer));
   begin
      if M in Filter_Error'Class then
         Error := Filter_Error (M).Error;
      else
         pragma Assert (M in Null_Message'Class);
         null;
      end if;
   end Emit_Message;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (S      : access Session'Class;
      R      : Pending_Request_Access;
      Error  : in out Errors.Error_Container)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Errors;
      Sess          : DNS_Session renames DNS_Session (S.all);
      Buffer        : Buffer_Access;
      Header_Buffer : Buffer_Access;
      Header_Space  : Reservation;
      It : Iterator;
      Arg : Element_Access;
   begin
      pragma Debug (C, O ("Send_Request enter"));
      Buffer := new Buffer_Type;
      Set_Endianness (Buffer, Big_Endian);
      Header_Buffer := new Buffer_Type;
      Set_Endianness (Header_Buffer, Big_Endian);
      Header_Space := Reserve (Buffer, DNS_Header_Size);

      pragma Debug (C, O ("Marshalling request body"));

      It := First (List_Of (R.Req.Args).all);
      Next (It);
      --  Retrieving the number of questions field here, so that
      --  we could marshall them in the dns header
      Arg := Value (It);
      Sess.MCtx.Nb_Questions    := Types.Unsigned_Short
        (Get_Aggregate_Count
        (Aggregate_Content'Class (Get_Value (Get_Container
            (Arg.Argument).all).all)) - 1);

      --  Marshalling the header
      Sess.MCtx.Message_Type := Request;
      Marshall_DNS_Header (Header_Buffer, R, Sess.MCtx);

      --  Marshalling the IN argument : question RR sequence
      if False
         or else Arg.Arg_Modes = ARG_IN
         or else Arg.Arg_Modes = ARG_INOUT
      then
         pragma Debug (C, O ("Marshalling argument "
                             & Types.To_Standard_String (Arg.Name)
                             & " = " & Image (Arg.Argument)));

         Marshall_From_Any (Buffer,
                        Get_Container (Arg.Argument).all, False, Error);
      end if;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

      Emit_Message (Sess'Access, Buffer, Error);
      pragma Debug (C, O ("Send_Request : leave"));
      Release (Buffer);
   end Send_Request;

   procedure Process_Message
     (S          : access Session'Class)
   is
      pragma Warnings (Off);
      Sess : DNS_Session renames DNS_Session (S.all);
      MCtx : DNS_Message_Context
               renames DNS_Message_Context (Sess.MCtx.all);
   begin
      pragma Debug (C, O ("Processing message of type :" &
        MCtx.Message_Type'Img));

      case MCtx.Message_Type is
         when Request =>
            if Sess.Role /= Server then
               raise DNS_Error;
            end if;
            Process_Request (Sess'Access);

         when Reply =>
            if Sess.Role /= Client then
               raise DNS_Error;
            end if;

            declare
               Request_Id : Types.Unsigned_Long;
               Rcode : Rcode_Type;
            begin
               null;
--                 Unmarshall_Service_Context_List
--                   (Sess.Buffer_In, Service_Contexts);
--
--                 Request_Id := Unmarshall (Sess.Buffer_In);
--                 Reply_Status := Unmarshall (Sess.Buffer_In);
--
--                 Common_Reply_Received
--                   (Sess'Access, Request_Id, Reply_Status, Service_Contexts);
            end;
         when others =>
            raise Program_Error;
      end case;
      pragma Debug (C, O ("Processed message : leaving"));
   end Process_Message;
      ---------------------
   -- Process_Request --
      ---------------------
   procedure Process_Request
     (S : access DNS_Session)
   is
      use PolyORB.Any.NVList;
      use PolyORB.Any.TypeCode;
      use PolyORB.Binding_Data;
      use PolyORB.Errors;
      use PolyORB.References;
      use PolyORB.Filters.Iface;
      use PolyORB.Objects;
      use PolyORB.Binding_Data.Local;
      use PolyORB.Obj_Adapters;
      use PolyORB.ORB.Iface;
      use PolyORB.Smart_Pointers;
      use PolyORB.POA;
      use PolyORB.Any;
      use PolyORB.Servants;
      use PolyORB.POA_Types;
--      use PolyORB.Transport;
--      use PolyORB.Obj_Adapters.Simple;

      ORB              : ORB_Access;
      Arg_Name_Auth : constant PolyORB.Types.Identifier :=
        PolyORB.Types.To_PolyORB_String ("authoritative");
      Arg_Name_Question : constant PolyORB.Types.Identifier :=
        PolyORB.Types.To_PolyORB_String ("question");
      Arg_Name_Answer : constant PolyORB.Types.Identifier :=
        PolyORB.Types.To_PolyORB_String ("answer");
      Arg_Name_Au : constant PolyORB.Types.Identifier :=
        PolyORB.Types.To_PolyORB_String ("authority");
      Arg_Name_Add : constant PolyORB.Types.Identifier :=
        PolyORB.Types.To_PolyORB_String ("additional");

      Argument_Auth : PolyORB.Any.Any;
      Argument_Question : PolyORB.Any.Any;
      Argument_Answer : PolyORB.Any.Any;
      Argument_Additional : PolyORB.Any.Any;
      Argument_Authority : PolyORB.Any.Any;
      Sequence_Length : Integer;
      Q_sequence : rrSequence;
      A_sequence : rrSequence;
      Auth_sequence : rrSequence;
      Add_sequence : rrSequence;

      myRR : RR;
      TC  : constant TypeCode.Local_Ref
        := TypeCode.TC_Sequence;
      TCK : constant TCKind := TypeCode.Kind (TC);
      OA               :  Obj_Adapters.Obj_Adapter_Access;
      POA              : PolyORB.POA.Obj_Adapter_Access;
      BO_List          : BO_Ref_List;
      Req_Flags        : Requests.Flags := 0;
      Object_Key       : PolyORB.Objects.Object_Id_Access;
      Target_Profile : Binding_Data.Profile_Access;
      Target           : References.Ref;
      Result           : Any.NamedValue;
      Req              : Request_Access;
      Args             : Any.NVList.Ref;
      Def_Args         : Component_Access;
      BO : Binding_Object_Access;
      Servant    : Components.Component_Access;
      It :  BO_Ref_Lists.Iterator;
      Error : Errors.Error_Container;
      Root_POA : PolyORB.POA.Obj_Adapter_Access;
      Child_POA : PolyORB.POA.Obj_Adapter_Access;
      sm : Servants.Servant_Access;
      serv : Servants.Servant_Access;
      BO_Ref : Smart_Pointers.Ref;

   begin
      if S.Role /= Server then
         raise DNS_Error;
      end if;
      ORB := ORB_Access (S.Server);
      pragma Debug (C, O ("Request_Received: entering"));
      S.MCtx.Request_Name := Types.To_PolyORB_String
        (Unmarshall_DNS_String (S.Buffer_In, S.MCtx.Request_Name_Length));

      pragma Debug (C, O ("Request name : " &
        Types.To_Standard_String (S.MCtx.Request_Name)));
--  Creating an empty args list
--      Object_Id := Get_Object_Key (Selected_Profile.all);

      Root_POA :=  PolyORB.POA.Obj_Adapter_Access
        (Object_Adapter (ORB));

      Find_POA (Self        => Root_POA,
                   Name        => "DNS_POA",
                   Activate_It => False,
                   POA         => Child_POA,
                   Error       => Error);

      pragma Debug (C, O ("Found POA : "
        & Child_POA.Name.all));

      --  Retrieving the default servant for DNS_POA
      Get_Servant (Child_POA, sm, Error);

      --  Retrieving the ObjectId associated to the servant
      Servant_To_Id (Child_POA,
                      P_Servant => sm,
                      Oid       => Object_Key,
                      Error     => Error);

      pragma Debug (C, O ("OBJ KEY : " & Image (Object_Key.all)));

--        Args := Get_Empty_Arg_List
--        (Object_Adapter (ORB),
--         Object_Key,
--         "Query");
      Any.NVList.Create (Args);

      TC_RR := PolyORB.Any.TypeCode.TC_Struct;
      Any.TypeCode.Add_Parameter (TC_RR, Any.To_Any ("RR"));
      Any.TypeCode.Add_Parameter (TC_RR, Any.To_Any ("IDL:DNS/RR:1.0"));
      Any.TypeCode.Add_Parameter (TC_RR, Any.To_Any
                                  (Any.TypeCode.TC_String));
      Any.TypeCode.Add_Parameter (TC_RR, Any.To_Any ("rr_name"));
      --  initialize RR_Type
      TC_RR_Type := PolyORB.Any.TypeCode.TC_Enum;
      Any.TypeCode.Add_Parameter
        (TC_RR_Type, Any.To_Any ("RR_Type"));
      Any.TypeCode.Add_Parameter
        (TC_RR_Type, Any.To_Any ("IDL:DNS/RR_Type:1.0"));
      Any.TypeCode.Add_Parameter
        (TC_RR_Type, Any.To_Any ("PTR"));
      Any.TypeCode.Add_Parameter
        (TC_RR_Type, Any.To_Any ("NS"));

      Any.TypeCode.Add_Parameter
         (TC_RR, Any.To_Any (TC_RR_Type));
      Any.TypeCode.Add_Parameter
         (TC_RR, Any.To_Any ("rr_type"));
      Any.TypeCode.Disable_Reference_Counting
        (Any.TypeCode.Object_Of (TC_RR).all);
      --  Initialize rrSequence
      TC_SEQUENCE_RR := Any.TypeCode.Build_Sequence_TC
                 (TC_RR, 0);
      Any.TypeCode.Disable_Reference_Counting
        (Any.TypeCode.Object_Of (TC_SEQUENCE_RR).all);
      SEQUENCE_RR_Helper.Initialize
              (Element_TC => TC_RR,
               Sequence_TC => TC_SEQUENCE_RR);

      --  Assigning the in out authoritative argument
      Argument_Auth := Any.To_Any (S.MCtx.AA_Flag);
      Add_Item (Args, Arg_Name_Auth, Argument_Auth, Any.ARG_INOUT);
      --  Assigning the question rrSequence
      myRR.rr_name := S.MCtx.Request_Name;
      myRR.rr_type := PTR;
      Sequence_Length := Integer (S.MCtx.Nb_Questions);
      Q_sequence := To_Sequence (Sequence_Length);
      for J in 1 .. Sequence_Length loop
         Replace_Element (Q_sequence, J, myRR);
      end loop;
      Argument_Question := To_Any (Q_sequence);
      Add_Item (Args, Arg_Name_Question, Argument_Question, Any.ARG_IN);
      --  initializing the out Answer rr sequence
      A_sequence := To_Sequence (Sequence_Length);
      Argument_Answer := To_Any (A_sequence);
      Add_Item (Args, Arg_Name_Answer, Argument_Answer, Any.ARG_OUT);

      --  initializing the out Authority rr sequence
      Auth_sequence := To_Sequence (Sequence_Length);
      Argument_Authority := To_Any (Auth_sequence);
      Add_Item (Args, Arg_Name_Au, Argument_Authority, Any.ARG_OUT);

      --  initializing the out Authority rr sequence
      Add_sequence := To_Sequence (Sequence_Length);
      Argument_Additional := To_Any (Add_sequence);
      Add_Item (Args, Arg_Name_Add, Argument_Additional, Any.ARG_OUT);

--        pragma Debug (C, O ("Args are null? :" & Args.Is_Null'Img));
--        Unmarshall_Argument_List (S.Buffer_In, Args, Error);
      Target_Profile := new Local_Profile_Type;
      Create_Local_Profile
           (Object_Key.all,
            Local_Profile_Type (Target_Profile.all));
      pragma Debug (C, O ("Local Profile created"));

      Create_Reference ((1 => Target_Profile), "", Target);
      pragma Debug (C, O ("Reference created"));

      Req_Flags := Sync_Call_Back;

      Create_Request
        (Target    => Target,
         Operation => "Query",
         Arg_List  => Args,
         Result    => Result,
         Deferred_Arguments_Session => Def_Args,
         Req       => Req,
         Req_Flags => Req_Flags,
         Dependent_Binding_Object =>
           Smart_Pointers.Entity_Ptr
             (S.Dependent_Binding_Object));

      Queue_Request_To_Handler (ORB,
        Queue_Request'
          (Request   => Req,
           Requestor => Component_Access (S)));
      PolyORB.Objects.Free (Object_Key);
      pragma Debug (C, O ("Process_Request: leaving"));
   end Process_Request;

   procedure Initialize_Session
     (S      : access Session'Class)
   is

      Sess : DNS_Session renames DNS_Session (S.all);
   begin
      pragma Debug (C, O ("Initialize context for DNS session"));
      Sess.MCtx  := new DNS_Message_Ctx;
   end Initialize_Session;

   ----------------------
   -- Finalize_Session --
   ----------------------
   procedure Finalize_Session
     (S      : access Session'Class)
   is
      pragma Warnings (Off);
      Sess : DNS_Session renames DNS_Session (S.all);
   begin
      null;  --  Free (Sess.MCtx);
      pragma Debug (C, O ("Finalize context for DNS session"));
   end Finalize_Session;

   procedure Initialize is
   begin
      Put_Line ("Initializing DNS Protocol...");
   end Initialize;

   procedure Marshall_DNS_Header
     (Header_Buffer  : access Buffers.Buffer_Type;
      R      :  Pending_Request_Access;
      MCtx : access DNS_Message_Context'Class)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      Arg : Element_Access;
      Test_Request_Id : Types.Unsigned_Short;
      Header_Flags : Flags;
   begin
         --  Marshall DNS request header
      pragma Debug (C, O ("Marshalling DNS request header"));
      --  Marshalling the transaction number
      Marshall (Header_Buffer, Types.Unsigned_Short (0));
      Header_Flags := 0;
      --  Marshalling the DNS header flags
      --  message is a request
      MCtx.QR_Flag := (MCtx.Message_Type = Reply);
      Unsigned_Short_Flags.Set (Header_Flags, QR_Flag_Pos, MCtx.QR_Flag);

      if R.Req.Operation.all = "Query" then
         --  message is a standard query
         pragma Debug (C, O ("request is a sstandard query"));
         MCtx.Opcode_Flag := Query;
         for J in Opcode_Flag_Pos .. QR_Flag_Pos - 1 loop
            Unsigned_Short_Flags.Set (Header_Flags, J, False);
         end loop;
      end if;

      --  Marshalling the authoritative flag
      --  : retrieve it from the request's arguments list
      Arg := Value (First (List_Of (R.Req.Args).all));
      Unsigned_Short_Flags.Set (Header_Flags,  AA_Flag_Pos,
                        PolyORB.Types.Boolean'(From_Any (Arg.Argument)));
      pragma Debug (C, O ("retrieved authoritative flag"));

      --  TODO : if message size > max_message_size then tc=1
      MCtx.TC_Flag := False;
      Unsigned_Short_Flags.Set (Header_Flags, TC_Flag_Pos, MCtx.TC_Flag);
      pragma Debug (C, O ("TC_Flag is set"));
      MCtx.Rec_Flag := False;
      Unsigned_Short_Flags.Set (Header_Flags, Rec_Flag_Pos, MCtx.Rec_Flag);
      pragma Debug (C, O ("Rec_Flag is set"));
      MCtx.Rec_Disp_Flag := False;
      Unsigned_Short_Flags.Set
        (Header_Flags, Rec_Disp_Flag_Pos, MCtx.Rec_Disp_Flag);
      pragma Debug (C, O ("Rec_Disp_Flag is set"));

      --  As this is a query,not a response, Rcode = No_Error
      MCtx.Rcode_Flag := No_Error;
      for J in  Rcode_Flag_Pos  .. Res_Flag_Pos - 1 loop
         Unsigned_Short_Flags.Set (Header_Flags, J, False);
         pragma Debug (C, O ("Setting Rcode flag bit nb:" & J'Img));
      end loop;
      pragma Debug (C, O ("marshalling flags"));
      Marshall (Header_Buffer, Types.Unsigned_Short (Header_Flags));

      --  Number of questions being sent
      pragma Debug (C, O ("marsh. of questions : " & MCtx.Nb_Questions'Img));
      Marshall (Header_Buffer, MCtx.Nb_Questions);
      --  By default, for a query, next fields = 0
      MCtx.Nb_Answers := 0;
      Marshall (Header_Buffer, MCtx.Nb_Answers);
      MCtx.Nb_Auth_Servers := 0;
      Marshall (Header_Buffer, MCtx.Nb_Auth_Servers);
      MCtx.Nb_Add_Infos := 0;
      Marshall (Header_Buffer, MCtx.Nb_Add_Infos);

   end Marshall_DNS_Header;
   ----------------------------
   -- Unmarshall_DNS_Header --
   ----------------------------

   procedure Unmarshall_DNS_Header
     (MCtx_Acc    : access DNS_Message_Context'Class;
      Buffer  : access Buffers.Buffer_Type)
   is
      Header_Flags : Flags;
      Flags_Buffer : Types.Unsigned_Short;
      MCtx :  DNS_Message_Context
                   renames DNS_Message_Context (MCtx_Acc.all);

      Test_Request_Id : Types.Unsigned_Short;

   begin
      pragma Debug (C, O ("Unmarshalling DNS header"));
      Show (Buffer);
      --  Extract Request_Id
      Test_Request_Id := Unmarshall (Buffer);
      pragma Debug (C, O ("Request ID :" & Test_Request_Id'Img));
      MCtx.Request_Id := Types.Unsigned_Long (Test_Request_Id);

      --  Extract the DNS header flags
      Flags_Buffer := Unmarshall (Buffer);
      Header_Flags := Flags (Flags_Buffer);

      if Is_Set (QR_Flag_Pos, Header_Flags) then
         MCtx.Message_Type := Reply;
      else
         MCtx.Message_Type := Request;
      end if;
      pragma Debug (C, O ("Message is a : " & MCtx.Message_Type'Img));

      --  XXX todo: case on Opcode for IQuery and Status
      MCtx.Opcode_Flag := Query;

      pragma Debug (C, O ("Opcode :" & MCtx.Opcode_Flag'Img));

      MCtx.AA_Flag := Is_Set (AA_Flag_Pos, Header_Flags);
      pragma Debug (C, O ("AA_Flag : " & MCtx.AA_Flag'Img));

      MCtx.TC_Flag :=  Is_Set (TC_Flag_Pos, Header_Flags);
      pragma Debug (C, O ("TC_Flag : " & MCtx.TC_Flag'Img));

      MCtx.Rec_Flag :=  Is_Set (Rec_Flag_Pos, Header_Flags);
      pragma Debug (C, O ("Rec_Flag : " & MCtx.Rec_Flag'Img));

      MCtx.Rec_Disp_Flag :=  Is_Set (Rec_Disp_Flag_Pos, Header_Flags);
      pragma Debug (C, O ("Rec_Disp_Flag : " & MCtx.Rec_Disp_Flag'Img));

      --  XXX TODO  : case on Rcode
      MCtx.Rcode_Flag := No_Error;
      pragma Debug (C, O ("Rcode : No_Error"));

      MCtx.Nb_Questions := Unmarshall (Buffer);
      pragma Debug (C, O ("NB Questions :" & MCtx.Nb_Questions'Img));

      MCtx.Nb_Answers := Unmarshall (Buffer);
      pragma Debug (C, O ("NB Resp :" & MCtx.Nb_Answers'Img));

      MCtx.Nb_Auth_Servers := Unmarshall (Buffer);
      pragma Debug (C, O ("NB Auth :" & MCtx.Nb_Auth_Servers'Img));

      MCtx.Nb_Add_Infos := Unmarshall (Buffer);
      pragma Debug (C, O ("NB Add Inf :" & MCtx.Nb_Auth_Servers'Img));

      pragma Debug (C, O ("Leaving Unmarshall_DNS_Header"));
   end Unmarshall_DNS_Header;

   procedure Marshall_DNS_Header_Reply
      (Header_Buffer  : access Buffers.Buffer_Type;
      R      :  Requests.Request_Access;
      MCtx : access DNS_Message_Context'Class)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      Arg : Element_Access;
      Test_Request_Id : Types.Unsigned_Short;
      Header_Flags : Flags;
   begin
         --  Marshall DNS request header
      pragma Debug (C, O ("Marshalling DNS header reply"));

      Marshall (Header_Buffer, Types.Unsigned_Short
                (0));
      --  Marshalling the DNS header flags;
      Header_Flags := 0;
      --  message is a reply
      MCtx.Message_Type := Reply;
      MCtx.QR_Flag := (MCtx.Message_Type = Reply);
      Unsigned_Short_Flags.Set (Header_Flags, QR_Flag_Pos, MCtx.QR_Flag);

      if R.Operation.all = "Query" then
         --  message is a standard query
         pragma Debug (C, O ("request is a sstandard query"));
         MCtx.Opcode_Flag := Query;
         for J in Opcode_Flag_Pos .. QR_Flag_Pos - 1 loop
            Unsigned_Short_Flags.Set (Header_Flags, J, False);
         end loop;
      end if;

      --  Marshalling the authoritative flag
      --  : retrieve it from the request's arguments list
      Arg := Value (First (List_Of (R.Args).all));
      Unsigned_Short_Flags.Set (Header_Flags,  AA_Flag_Pos,
                        PolyORB.Types.Boolean'(From_Any (Arg.Argument)));
      pragma Debug (C, O ("Authoritative flag : "));
      --  TODO : if message size > max_message_size then tc=1
      MCtx.TC_Flag := False;
      Unsigned_Short_Flags.Set (Header_Flags, TC_Flag_Pos, MCtx.TC_Flag);

      MCtx.Rec_Flag := False;
      Unsigned_Short_Flags.Set (Header_Flags, Rec_Flag_Pos, MCtx.Rec_Flag);
      MCtx.Rec_Disp_Flag := False;
      Unsigned_Short_Flags.Set
        (Header_Flags, Rec_Disp_Flag_Pos, MCtx.Rec_Disp_Flag);
      --  three reserved bits
--        for J in Res_Flag_Pos .. Rcode_Flag_Pos - 1 loop
--           Unsigned_Short_Flags.Set (Header_Flags, J, False);
--        end loop;

      --  As this is a query,not a response, Rcode = No_Error
      MCtx.Rcode_Flag := No_Error;
      for J in Rcode_Flag_Pos .. Res_Flag_Pos - 1 loop
         Unsigned_Short_Flags.Set (Header_Flags, J, False);
      end loop;

      pragma Debug (C, O ("Flags have been set "));
      Marshall (Header_Buffer, Types.Unsigned_Short (Header_Flags));
      --  Number of questions being sent
      MCtx.Nb_Questions := 0;
      Marshall (Header_Buffer, MCtx.Nb_Questions);

      --  XXX TODO: multiple answers, auth servers, additional infos
      MCtx.Nb_Answers := 1;
      Marshall (Header_Buffer, MCtx.Nb_Answers);
      MCtx.Nb_Auth_Servers := 0;
      Marshall (Header_Buffer, MCtx.Nb_Auth_Servers);

      MCtx.Nb_Add_Infos := 0;
      Marshall (Header_Buffer, MCtx.Nb_Add_Infos);
      Show (Header_Buffer);
   end Marshall_DNS_Header_Reply;

   --------------
   -- From_Any --
   --------------
   function From_Any
     (C : PolyORB.Any.Any_Container'Class) return RR_Type
   is
   begin
      return RR_Type'Val
        (PolyORB.Types.Unsigned_Long'
           (PolyORB.Any.Get_Aggregate_Element (C, 0)));
   end From_Any;
   function From_Any
     (Item : PolyORB.Any.Any)
     return RR_Type
   is
   begin
      return From_Any
        (PolyORB.Any.Get_Container
           (Item).all);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : RR_Type)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
        PolyORB.Any.Get_Empty_Any_Aggregate
           (TC_RR_Type);
   begin
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.To_Any
           (PolyORB.Types.Unsigned_Long
              (RR_Type'Pos
                 (Item))));
      return Result;
   end To_Any;

   function Wrap
        (X : access RR_Type)
        return PolyORB.Any.Content'Class
   is
   begin
      return Content_RR_Type'(PolyORB.Any.Aggregate_Content with
            V => Ptr_RR_Type (X), Repr_Cache => 0);
   end Wrap;
   ---------------------------
   -- Get_Aggregate_Element --
   ---------------------------

   function Get_Aggregate_Element
        (Acc : not null access Content_RR_Type;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class
    is
         use type PolyORB.Types.Unsigned_Long;
         use type PolyORB.Any.Mechanism;
         pragma Suppress (Validity_Check);
         pragma Unreferenced (Tc, Index);
   begin
      Acc.Repr_Cache := DNS.RR_Type'Pos (Acc.V.all);
      Mech.all := PolyORB.Any.By_Value;
      return PolyORB.Any.Wrap (Acc.Repr_Cache'Unrestricted_Access);
   end Get_Aggregate_Element;

      ---------------------------
      -- Set_Aggregate_Element --
      ---------------------------

   procedure Set_Aggregate_Element
        (Acc : in out Content_RR_Type;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         From_C : in out PolyORB.Any.Any_Container'Class)
      is
         use type PolyORB.Types.Unsigned_Long;
         pragma Assert ((Index = 0));
         pragma Unreferenced (Tc);
   begin
      Acc.V.all := RR_Type'Val (PolyORB.Types.Unsigned_Long'
                 (PolyORB.Any.From_Any (From_C)));
   end Set_Aggregate_Element;

      -------------------------
      -- Get_Aggregate_Count --
      -------------------------

   function Get_Aggregate_Count
        (Acc : Content_RR_Type)
        return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
   begin
      return 1;
   end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

   procedure Set_Aggregate_Count
        (Acc : in out Content_RR_Type;
         Count : PolyORB.Types.Unsigned_Long)
   is
   begin
      null;
   end Set_Aggregate_Count;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_RR_Type;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr
      is
         use type PolyORB.Any.Content_Ptr;
         Target : PolyORB.Any.Content_Ptr;
      begin
         if (Into /= null) then
            if (Into.all not in Content_RR_Type) then
               return null;
            end if;
            Target := Into;
            Content_RR_Type
              (Target.all).V.all := Acc.V.all;
         else
            Target := new Content_RR_Type;
            Content_RR_Type (Target.all).V := new RR_Type'(Acc.V.all);
         end if;
         Content_RR_Type (Target.all).Repr_Cache := Acc.Repr_Cache;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

   procedure Finalize_Value
     (Acc : in out Content_RR_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
              (RR_Type, Ptr_RR_Type);
   begin
      Free (Acc.V);
   end Finalize_Value;

   --  Utilities for the RR type
   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : PolyORB.Any.Any)
     return RR
   is
   begin
      return (rr_name => PolyORB.Any.From_Any
        (PolyORB.Any.Get_Aggregate_Element
           (Item,
            PolyORB.Any.TypeCode.TC_String,
            0)),
      rr_type => From_Any
        (PolyORB.Any.Get_Aggregate_Element
           (Item,
            TC_RR_Type,
            1)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : RR)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
        PolyORB.Any.Get_Empty_Any_Aggregate
           (TC_RR);
   begin
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.To_Any
           (Item.rr_name));
      PolyORB.Any.Add_Aggregate_Element
        (Result, To_Any (Item.rr_type));
      return Result;
   end To_Any;

         -----------
      -- Clone --
      -----------

   function Clone
     (Acc : Content_RR;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr
   is
      use type PolyORB.Any.Content_Ptr;
      Target : PolyORB.Any.Content_Ptr;
   begin
      if (Into /= null) then
         if (Into.all not in Content_RR) then
            return null;
         end if;
         Target := Into;
         Content_RR (Target.all).V.all := Acc.V.all;
      else
         Target := new Content_RR;
         Content_RR (Target.all).V := new RR'(Acc.V.all);
      end if;
      return Target;
   end Clone;
   procedure Finalize_Value
        (Acc : in out Content_RR)
   is
      procedure Free is new Ada.Unchecked_Deallocation (RR, Ptr_RR);
   begin
         Free (Acc.V);
   end Finalize_Value;

         ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

   function Get_Aggregate_Element
     (Acc : not null access Content_RR;
      Tc : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech : not null access PolyORB.Any.Mechanism)
      return PolyORB.Any.Content'Class
   is
      use type PolyORB.Types.Unsigned_Long;
      use type PolyORB.Any.Mechanism;
      pragma Suppress (Validity_Check);
      pragma Unreferenced (Tc);
   begin
      Mech.all := PolyORB.Any.By_Reference;
      case Index is
         when 0 =>
            return PolyORB.Any.Wrap (Acc.V.rr_name'Unrestricted_Access);
         when 1 =>
            return Wrap (Acc.V.rr_type'Unrestricted_Access);
         pragma Warnings (Off);
         when others =>
            raise Constraint_Error;
         pragma Warnings (On);
      end case;
   end Get_Aggregate_Element;

   function Get_Aggregate_Count
      (Acc : Content_RR)
      return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (Acc);
   begin
      return 2;
   end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

   procedure Set_Aggregate_Count
        (Acc : in out Content_RR;
         Count : PolyORB.Types.Unsigned_Long)
   is
   begin
         null;
   end Set_Aggregate_Count;

   function Wrap (X : access RR)
        return PolyORB.Any.Content'Class
   is
   begin
      return Content_RR'(PolyORB.Any.Aggregate_Content with
            V => Ptr_RR (X));
   end Wrap;

   function SEQUENCE_RR_Element_Wrap
        (X : access RR)
         return PolyORB.Any.Content'Class
   is
   begin
      return Wrap (X.all'Unrestricted_Access);
   end SEQUENCE_RR_Element_Wrap;

   function Wrap
        (X : access SEQUENCE_RR.Sequence)
         return PolyORB.Any.Content'Class
     renames SEQUENCE_RR_Helper.Wrap;

   function From_Any
     (Item : PolyORB.Any.Any)
     return SEQUENCE_RR.Sequence renames
      SEQUENCE_RR_Helper.From_Any;

   function To_Any
     (Item : SEQUENCE_RR.Sequence)
      return PolyORB.Any.Any renames
       SEQUENCE_RR_Helper.To_Any;

   function From_Any
     (Item : PolyORB.Any.Any)
      return rrSequence
   is
      Result : constant SEQUENCE_RR.Sequence := From_Any (Item);
   begin
      return rrSequence (Result);
   end From_Any;

   function To_Any
     (Item : rrSequence)
      return PolyORB.Any.Any
   is
      Result : constant PolyORB.Any.Any :=
        To_Any (SEQUENCE_RR.Sequence (Item));
   begin
      --  PolyORB.Any.Set_Type (Result, TC_rrSequence);
      return Result;
   end To_Any;
   procedure Unmarshall_Argument_List
     (Buffer              :        Buffer_Access;
      Args                : in out Any.NVList.Ref;
      Error               : in out Errors.Error_Container)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Errors;

      It  : Iterator;
      Arg : Element_Access;
   begin
      pragma Debug (C, O ("Unmarshall Argument List"));
      It := First (List_Of (Args).all);
      while not Last (It) loop
         Arg := Value (It);
         pragma Debug (C, O ("First Argument unmarshall"));
         Unmarshall_To_Any (Buffer, Get_Container (Arg.Argument).all, Error);
         if Found (Error) then
            return;
         end if;
         Next (It);
      end loop;
   end Unmarshall_Argument_List;

   procedure Reply_Received
     (Sess             : access DNS_Session;
      Request_Id       : Types.Unsigned_Long;
      Rcode     : Rcode_Type)
   is
      use PolyORB.Any;
      use PolyORB.Errors;

      Current_Req  : Pending_Request;
      Success      : Boolean;

--      ORB          : constant ORB_Access := ORB_Access (Sess.Server);

--      Error        : Errors.Error_Container;
   begin
      pragma Debug (C, O ("Reply received: status = "
                       & Rcode_Type'Image (Rcode)
                       & ", id ="
                       & Types.Unsigned_Long'Image (Request_Id)));

      Get_Pending_Request (Sess, Request_Id, Current_Req, Success);

      if not Success then
         raise DNS_Error;
      end if;

      case Rcode is
         when No_Error =>
            --  Unmarshall reply body.
               pragma Debug (C, O ("Use Anys"));

--                 Unmarshall_To_Any
--                   (Sess.Buffer_In,
--                    Get_Container (Current_Req.Req.Result.Argument).all,
--                    Error);

               --  UNMARSHALL ARGUMENTS HERE

--            Expect_DNS_Header (Sess);
            Emit_No_Reply
              (Current_Req.Req.Requesting_Component,
               Servants.Iface.Executed_Request'
               (Req => Current_Req.Req));

         when others =>
            null;
      end case;
   end Reply_Received;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   pragma Debug (C, O ("Registering Module PROTOCOLS.DNS"));
   Register_Module
     (Module_Info'
      (Name      => +"protocols.dns",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Protocols.DNS;
