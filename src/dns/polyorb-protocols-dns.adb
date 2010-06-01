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
with PolyORB.Smart_Pointers;
with PolyORB.ORB.Iface;
with PolyORB.POA;
with PolyORB.Binding_Objects;
with PolyORB.Any;
with PolyORB.POA_Types;

package body PolyORB.Protocols.DNS is

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

      Sess  : DNS_Session renames DNS_Session (S.all);
      Error : Errors.Error_Container;
   begin
      if Sess.Role = Client then
         raise DNS_Error;
      end if;

      --  XXX: TODO : Manage eventual Exceptions and other Rcodes
      if PolyORB.Any.Is_Empty (Request.Exception_Info) then
         Sess.MCtx.Rcode_Flag  := From_Any (Request.Result.Argument);
      end if;

      Set_Endianness (Buffer_Out, Big_Endian);
      Set_Endianness (Header_Buffer, Big_Endian);

      Header_Space := Reserve (Buffer_Out, DNS_Header_Size);

      --  find and marshall the answer sequence
      It := First (List_Of (Request.Out_Args).all);
      Next (It);
      Next (It);
      Arg := Value (It);
      Marshall_From_Any (Buffer_Out, Arg.Argument, True);
      --  XXX: TODO : find and marshall auth and additional server RRs
      --  Copy Header
      Marshall_DNS_Header_Reply
        (Header_Buffer, Request, Sess.MCtx);
      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

      --  Emit reply
      Show (Buffer_Out);
      Emit_Message (Sess'Access, Buffer_Out, Error);
      Release (Buffer_Out);
      pragma Debug (C, O ("Reply sent"));

   end Send_Reply;

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

   ----------------------------
   -- Handle_Data_Indication --
   ----------------------------

   procedure Handle_Data_Indication
     (Sess       : access DNS_Session;
      Data_Amount : Ada.Streams.Stream_Element_Count;
      Error       : in out Errors.Error_Container)
   is
      use PolyORB.Any.NVList;
      use PolyORB.Any.TypeCode;
      use Errors;
      Label_Size : Types.Octet;
      newRR : RR;
   begin
      pragma Debug (C, O ("Handle_Data_Indication : Enter"));
      pragma Debug (C, O ("Received data in state " & Sess.State'Img));
      pragma Assert (Sess.State /= Not_Initialized);
      case Sess.State is

         when Expect_Header =>
            pragma Debug (C, O ("Received Header Size " & Data_Amount'Img));
            Unmarshall_DNS_Header (Sess.MCtx, Sess.Buffer_In);

            if Sess.Role = Client then
               Process_Message (Sess);
            else
               Sess.MCtx.Q_sequence :=
                 To_Sequence (Integer (Sess.MCtx.Nb_Questions));
               Any.NVList.Create (Sess.MCtx.New_Args);
               --  At this point we have all header fields stored in Sess.MCtx
               --  If the message is a question
               if Sess.MCtx.Message_Type = Request then
                  Sess.State := Expect_Name;
                  --  We need to receive the length of the first question name
                  Emit_No_Reply
                      (Port => Lower (Sess),
                       Msg  => DNS_Data_Expected'
                       (In_Buf => Sess.Buffer_In,
                        Max    => Stream_Element_Count
                       (1),
                       State  => Sess.State));
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
            --  XXX : TODO : move this code to representations.dns
            Sess.MCtx.Request_Name := Types.To_PolyORB_String (
              Unmarshall_DNS_String (Sess.Buffer_In,
                Sess.MCtx.Request_Name_Length));
            Sess.MCtx.Request_Type_Code := Unmarshall (Sess.Buffer_In);
            Sess.MCtx.Request_Class := Unmarshall (Sess.Buffer_In);

            case Sess.MCtx.Request_Type_Code is
               when A_Code =>
                  Sess.MCtx.Request_Type := A;
               when NS_Code =>
                  Sess.MCtx.Request_Type := NS;
               when SOA_Code =>
                  Sess.MCtx.Request_Type := SOA;
               when CNAME_Code =>
                  Sess.MCtx.Request_Type := CNAME;
               when PTR_Code =>
                  Sess.MCtx.Request_Type := PTR;
               when TXT_Code =>
                  Sess.MCtx.Request_Type := TXT;
               when SRV_Code =>
                  Sess.MCtx.Request_Type := SRV;
               when others =>
                  null;
            end case;

            --  Assigning the question rrSequence
            newRR.rr_name := Sess.MCtx.Request_Name;
            newRR.rr_type := Sess.MCtx.Request_Type;
            Replace_Element (Sess.MCtx.Q_sequence,
                             Integer (Sess.MCtx.Nb_Questions), newRR);
            Sess.MCtx.Nb_Questions := Sess.MCtx.Nb_Questions - 1;

            if Sess.MCtx.Nb_Questions > 0 then
                  Sess.State := Expect_Name;
                  Emit_No_Reply
                      (Port => Lower (Sess),
                       Msg  => DNS_Data_Expected'
                       (In_Buf => Sess.Buffer_In,
                        Max    => Stream_Element_Count
                       (1),
                        State  => Sess.State));
            end if;

            if Sess.MCtx.Nb_Questions = 0 then
               Process_Message (Sess);
            end if;
            --            Expect_DNS_Header (Sess);
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
      Marshall_From_Any (Buffer, Arg.Argument, False);

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

            Reply_Received
                (Sess'Access, Sess.MCtx.Request_Id, Sess.MCtx.Rcode_Flag);
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
      use PolyORB.ORB.Iface;
      use PolyORB.Smart_Pointers;
      use PolyORB.POA;
      use PolyORB.Any;
      use PolyORB.Servants;
      use PolyORB.POA_Types;

      ORB              : ORB_Access;
      Req_Flags        : Requests.Flags := 0;
      Object_Key       : PolyORB.Objects.Object_Id_Access;
      Target_Profile : Binding_Data.Profile_Access;
      Target           : References.Ref;
      Result           : Any.NamedValue;
      Req              : Request_Access;
      Args             : Any.NVList.Ref;
      Def_Args         : Component_Access;

      Error : Errors.Error_Container;
      Root_POA : PolyORB.POA.Obj_Adapter_Access;
      Child_POA : PolyORB.POA.Obj_Adapter_Access;
      Servant : Servants.Servant_Access;
      Return_Code : Types.Unsigned_Short;
   begin
      if S.Role /= Server then
         raise DNS_Error;
      end if;
      ORB := ORB_Access (S.Server);
      pragma Debug (C, O ("Request_Received: entering"));

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
      Get_Servant (Child_POA, Servant, Error);

      --  Retrieving the ObjectId associated to the servant
      Servant_To_Id (Child_POA,
                      P_Servant => Servant,
                      Oid       => Object_Key,
                      Error     => Error);

      pragma Debug (C, O ("Object key found : " & Image (Object_Key.all)));

      --  Assigning the in out authoritative argument
      Add_Item (S.MCtx.New_Args,
                Arg_Name_Auth, To_Any (S.MCtx.AA_Flag), Any.ARG_INOUT);
      --  Assigning the question rrSequence
      Add_Item (S.MCtx.New_Args,
                Arg_Name_Question, To_Any (S.MCtx.Q_sequence), Any.ARG_IN);
      --  initializing the out Answer rr sequence
      Add_Item (S.MCtx.New_Args,
                Arg_Name_Answer, To_Any (S.MCtx.A_sequence), Any.ARG_OUT);
      --  initializing the out Authority rr sequence
      Add_Item (S.MCtx.New_Args,
                Arg_Name_Au, To_Any (S.MCtx.Auth_sequence), Any.ARG_OUT);
      --  initializing the out Additional infos rr sequence
      Add_Item (S.MCtx.New_Args,
                Arg_Name_Add, To_Any (S.MCtx.Add_sequence), Any.ARG_OUT);

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
         Operation => To_Standard_String (S.MCtx.Request_Opcode),
         Arg_List  => S.MCtx.New_Args,
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
      MCtx.Request_Id := R.Request_Id;
      Marshall (Header_Buffer, Types.Unsigned_Short (MCtx.Request_Id));
      Header_Flags := 0;
      --  Marshalling the DNS header flags
      --  message is a request
      MCtx.QR_Flag := (MCtx.Message_Type = Reply);
      Unsigned_Short_Flags.Set (Header_Flags, QR_Flag_Pos, MCtx.QR_Flag);

      if R.Req.Operation.all = Query_Name then
         --  message is a standard query (0) - no flags to set
         pragma Debug (C, O ("request is a standard Query"));
         MCtx.Opcode_Flag := Query;

      elsif R.Req.Operation.all = IQuery_Name then
         pragma Debug (C, O ("request is an IQuery"));
         MCtx.Opcode_Flag := IQuery;
         Unsigned_Short_Flags.Set (Header_Flags,  Opcode_Flag_Pos + 3, True);

      elsif R.Req.Operation.all = Status_Name then
         pragma Debug (C, O ("request is a Status Query"));
         MCtx.Opcode_Flag := Status;
         Unsigned_Short_Flags.Set (Header_Flags, Opcode_Flag_Pos + 2, True);
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

      if Is_Set (Opcode_Flag_Pos + 3, Header_Flags) then
         MCtx.Opcode_Flag := IQuery;
         MCtx.Request_Opcode := To_PolyORB_String (IQuery_Name);
      elsif Is_Set (Opcode_Flag_Pos + 2, Header_Flags) then
         MCtx.Opcode_Flag := Status;
         MCtx.Request_Opcode := To_PolyORB_String (Status_Name);
      else
         MCtx.Opcode_Flag := Query;
         MCtx.Request_Opcode := To_PolyORB_String (Query_Name);
      end if;

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
      Show (Buffer);
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
      Header_Flags : Flags;
      It : Iterator;
   begin
      --  Marshall DNS request header
      pragma Debug (C, O ("Marshall_DNS_Header_Reply: enter"));

      Marshall (Header_Buffer, Types.Unsigned_Short
                (MCtx.Request_Id));
      --  Marshalling the DNS header flags;
      Header_Flags := 0;
      --  message is a reply
      MCtx.Message_Type := Reply;
      MCtx.QR_Flag := (MCtx.Message_Type = Reply);
      Unsigned_Short_Flags.Set (Header_Flags, QR_Flag_Pos, MCtx.QR_Flag);

      if R.Operation.all = Query_Name then
         --  message is a standard query (0) - no flags to set
         pragma Debug (C, O ("request is a standard Query"));
         MCtx.Opcode_Flag := Query;

      elsif R.Operation.all = IQuery_Name then
         pragma Debug (C, O ("request is an IQuery"));
         MCtx.Opcode_Flag := IQuery;
         Unsigned_Short_Flags.Set (Header_Flags,  Opcode_Flag_Pos + 3, True);

      elsif R.Operation.all = Status_Name then
         pragma Debug (C, O ("request is a Status Query"));
         MCtx.Opcode_Flag := Status;
         Unsigned_Short_Flags.Set (Header_Flags, QR_Flag_Pos + 2, True);
      end if;

      --  Marshalling the authoritative flag
      --  : retrieve it from the request's arguments list
      It := First (List_Of (R.Args).all);
      Arg := Value (It);
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

      case MCtx.Rcode_Flag is
         --  No Error : 0x0000
         when No_Error =>
            null;

         --  Format Error : 0x0001
         when Format_Error =>
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos, True);

         --  Server Failure  : 0x0010
         when Server_Failure =>
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos + 1, True);

         --  Name Error : 0x0011
         when PolyORB.DNS.Helper.Name_Error =>
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos, True);
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos + 1, True);

         --  Not Implemented : 0x0100
         when Not_Implemented =>
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos + 2, True);

         --  Refused : 0x0101
         when Refused =>
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos, True);
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos + 2, True);

         --  YX Domain - name exists: 0x0110
         when YX_Domain =>
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos + 1, True);
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos + 2, True);

         --  YX RR set exists : 0x0111
         when YX_RRSet =>
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos, True);
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos + 1, True);
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos + 2, True);

         --  NX RR set  does not exist : 0x1000
         when NX_RRSet =>
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos + 3, True);

         --  Not Authoritative : 0x1001
         when Not_Auth =>
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos, True);
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos + 3, True);

         --  Name is out of zone : 0x1010
         when Not_Zone =>
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos + 3, True);
            Unsigned_Short_Flags.Set
              (Header_Flags, Rcode_Flag_Pos + 1, True);
      end case;

      pragma Debug (C, O ("Flags have been set "));
      Marshall (Header_Buffer, Types.Unsigned_Short (Header_Flags));

      Next (It);
      --  Number of questions being sent
      --  XXX : Should nb questions be always 0 for an answer?
      MCtx.Nb_Questions := 0;

      Marshall (Header_Buffer, MCtx.Nb_Questions);

      Next (It);
      --  Retrieving the number of  answers field here, so that
      --  we could marshall them in the dns header
      Arg := Value (It);
      MCtx.Nb_Answers := Types.Unsigned_Short
        (Get_Aggregate_Count
        (Aggregate_Content'Class (Get_Value (Get_Container
            (Arg.Argument).all).all)) - 1);
      Marshall (Header_Buffer, MCtx.Nb_Answers);

      --  retrieve and marshall nb of authority servers
      Next (It);
      Arg := Value (It);
      MCtx.Nb_Auth_Servers := Types.Unsigned_Short
        (Get_Aggregate_Count
        (Aggregate_Content'Class (Get_Value (Get_Container
            (Arg.Argument).all).all)) - 1);
      Marshall (Header_Buffer, MCtx.Nb_Auth_Servers);

      --  retrieve and marshall nb of additionnal infos
      Next (It);
      Arg := Value (It);
      MCtx.Nb_Add_Infos := Types.Unsigned_Short
        (Get_Aggregate_Count
        (Aggregate_Content'Class (Get_Value (Get_Container
            (Arg.Argument).all).all)) - 1);
      Marshall (Header_Buffer, MCtx.Nb_Add_Infos);

      pragma Debug (C, O ("Marshall_DNS_Header_Reply: leave"));
   end Marshall_DNS_Header_Reply;

   procedure Unmarshall_Argument_List
     (Sess             : access DNS_Session;
      Args                : in out Any.NVList.Ref;
      Direction           :        Any.Flags;
      Error               : in out Errors.Error_Container)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Errors;

      It  : Iterator := First (List_Of (Args).all);
      Arg : Element_Access;
      Empty : Types.Unsigned_Short;
      R_Type : Types.Unsigned_Short;
      TTL : Types.Unsigned_Short;
      Name_Length : Types.Octet;
      Data_Length : Types.Unsigned_Short;
      Answer_Length : Types.Octet;
      Answer : Types.String;
      Argument_Answer : PolyORB.Any.Any;
      A_sequence : rrSequence;
      answerRR : RR;
      pragma Unreferenced (Empty, Error);
   begin

      Any.NVList.Create (Args);
      Name_Length := Unmarshall (Sess.Buffer_In);
      Sess.MCtx.Request_Name := Types.To_PolyORB_String
        (Unmarshall_DNS_String
           (Sess.Buffer_In, Types.Unsigned_Short (Name_Length)));
      R_Type := Unmarshall (Sess.Buffer_In);

      case R_Type is
         when A_Code =>
            Sess.MCtx.Request_Type := A;
         when NS_Code =>
            Sess.MCtx.Request_Type := NS;
         when SOA_Code =>
            Sess.MCtx.Request_Type := SOA;
         when CNAME_Code =>
            Sess.MCtx.Request_Type := CNAME;
         when PTR_Code =>
            Sess.MCtx.Request_Type := PTR;
         when TXT_Code =>
            Sess.MCtx.Request_Type := TXT;
         when SRV_Code =>
            Sess.MCtx.Request_Type := SRV;
         when others =>
            null;
      end case;

      Sess.MCtx.Request_Class := Unmarshall (Sess.Buffer_In);
      Empty := Unmarshall (Sess.Buffer_In);
      TTL := Unmarshall (Sess.Buffer_In);
      pragma Debug (C, O ("TTL : " & TTL'Img));

      --  XXX TODO : case on Request type for other cases ->
      --  the structure of the dns answer may change for different rr types
      if Sess.MCtx.Request_Type = PTR then
         --  Retrieve data length
         Data_Length := Unmarshall (Sess.Buffer_In);
         pragma Debug (C, O ("Data Length : " & Data_Length'Img));
         Answer_Length := Unmarshall (Sess.Buffer_In);
         Answer := Types.To_PolyORB_String
           (Unmarshall_DNS_String
              (Sess.Buffer_In, Types.Unsigned_Short (Answer_Length)));
         pragma Debug (C, O ("Answer: " & Types.To_Standard_String (Answer)));
      end if;

      pragma Assert (Direction = ARG_IN or else Direction = ARG_OUT);
      --  We know in advance the different types of the arguments
      --  First one is the authoritative flags - direction in out
      Arg := Value (It);
      if Arg.Arg_Modes = ARG_INOUT then
         pragma Debug (C, O ("First arg is:  inout"));
         Set_Any_Value (Sess.MCtx.AA_Flag, Get_Container (Arg.Argument).all);
      end if;

      --  question rr sequence
      --  XXX : TODO : Fill the loop to unmashall question sequences
      Next (It);
      Arg := Value (It);
      if Arg.Arg_Modes = ARG_IN then
         for J in 1 .. Sess.MCtx.Nb_Questions loop
            null;
         end loop;
      end if;
      Next (It);
      Arg := Value (It);

      --  anwer rr sequence
      if Arg.Arg_Modes = ARG_OUT then
         pragma Debug (C, O ("Third arg is:  out"));
         --  initializing the out Answer rr sequence
         A_sequence := To_Sequence (Integer (Sess.MCtx.Nb_Answers));
         --  XXX TODO : case of multiple answers -  to be fixed
         for J in 1 .. Integer (Sess.MCtx.Nb_Answers) loop
            answerRR.rr_name := Answer;
            answerRR.rr_type := Sess.MCtx.Request_Type;
            Replace_Element (A_sequence, J, answerRR);
         end loop;

         Argument_Answer := To_Any (A_sequence);
         Copy_Any_Value (Arg.Argument, Argument_Answer);
      end if;

      --  authority rr sequence
      --  XXX : TODO : Fill the loop to unmashall authority sequence
      Next (It);
      Arg := Value (It);
      if Arg.Arg_Modes = ARG_OUT then
         null;
      end if;
      --  additionnal info rr sequence
      --  XXX : TODO : Fill the loop to unmashall add. infos sequence
      Next (It);
      Arg := Value (It);
      if Arg.Arg_Modes = ARG_OUT then
         null;
      end if;
      pragma Debug (C, O ("Leaving Unmarshall_Argument_List"));
   end Unmarshall_Argument_List;

   procedure Reply_Received
     (Sess             : access DNS_Session;
      Request_Id       : Types.Unsigned_Long;
      RC     : Rcode)
   is
      use PolyORB.Any;
      use PolyORB.Errors;

      Current_Req  : Pending_Request;
      Success      : Boolean;

--      ORB          : constant ORB_Access := ORB_Access (Sess.Server);
      Error        : Errors.Error_Container;
   begin
      pragma Debug (C, O ("Reply received: status = "
                       & Rcode'Image (RC)
                       & ", id ="
                       & Types.Unsigned_Long'Image (Request_Id)));

      Get_Pending_Request (Sess, Request_Id, Current_Req, Success);

      if not Success then
         raise DNS_Error;
      end if;

      case RC is
         when No_Error =>

               pragma Debug (C, O ("No_Error : Unmarshall Reply Body"));

               --  Unmarshall reply body.
               Unmarshall_Argument_List (Sess,
                       Current_Req.Req.Args, PolyORB.Any.ARG_OUT, Error);

--               Expect_DNS_Header (Sess);
               Emit_No_Reply
                 (Current_Req.Req.Requesting_Component,
                  Servants.Iface.Executed_Request'
                    (Req => Current_Req.Req));

         --  XXX tbd : manage other response codes cases
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