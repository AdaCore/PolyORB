------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . P R O T O C O L S . G I O P . G I O P _ 1 _ 2       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2020, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

with Ada.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Binding_Data.GIOP;
with PolyORB.Binding_Data.Local;
with PolyORB.Buffers;
with PolyORB.Components;
with PolyORB.GIOP_P.Code_Sets.Converters;
with PolyORB.GIOP_P.Service_Contexts;
with PolyORB.GIOP_P.Tagged_Components.Code_Sets;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Obj_Adapters;
with PolyORB.Obj_Adapters.Group_Object_Adapter;
with PolyORB.Opaque;
with PolyORB.Parameters;
with PolyORB.Protocols.GIOP.Common;
pragma Elaborate_All (PolyORB.Protocols.GIOP.Common);
with PolyORB.QoS.Addressing_Modes;
with PolyORB.QoS.Service_Contexts;
with PolyORB.QoS.Static_Buffers;
with PolyORB.References.Binding;
with PolyORB.References.IOR;
with PolyORB.Representations.CDR.Common;
with PolyORB.Representations.CDR.GIOP_1_2;
with PolyORB.Request_QoS;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package body PolyORB.Protocols.GIOP.GIOP_1_2 is

   use PolyORB.Buffers;
   use PolyORB.Components;
   use PolyORB.Errors;
   use PolyORB.GIOP_P.Code_Sets;
   use PolyORB.GIOP_P.Code_Sets.Converters;
   use PolyORB.GIOP_P.Service_Contexts;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.Protocols.GIOP.Common;
   use PolyORB.QoS;
   use PolyORB.QoS.Code_Sets;
   use PolyORB.QoS.Service_Contexts;
   use PolyORB.QoS.Static_Buffers;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Representations.CDR.GIOP_1_2;
   use PolyORB.Request_QoS;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.protocols.giop.giop_1_2");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   Permitted_Sync_Scopes : constant PolyORB.Requests.Flags :=
     Sync_None or Sync_With_Transport or Sync_With_Server or Sync_With_Target;

   procedure Free is new Ada.Unchecked_Deallocation
     (GIOP_1_2_CDR_Representation, GIOP_1_2_CDR_Representation_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Target_Address, Target_Address_Access);

   --  Msg_Type

   function Unmarshall is new Generic_Unmarshall
     (Msg_Type, Types.Octet, Unmarshall);

   procedure Marshall is new Generic_Marshall
     (Msg_Type, Types.Octet, Marshall);

   --  Addressing_Dispostion

   function Unmarshall is new Generic_Unmarshall
     (Addressing_Disposition, Types.Unsigned_Short, Unmarshall);

   procedure Marshall is new Generic_Marshall
     (Addressing_Disposition, Types.Unsigned_Short, Marshall);

   --  Helpers

   -----------------------------
   -- Marshall_Locate_Request --
   -----------------------------

   procedure Marshall_Locate_Request
     (Buffer     : Buffer_Access;
      Request_Id : Types.Unsigned_Long;
      Target_Ref : Target_Address);

   procedure Unmarshall_Request_Message
     (Buffer           : access Buffers.Buffer_Type;
      MCtx             : access GIOP_Message_Context_1_2;
      Sync             :    out Sync_Scope;
      Target_Ref       :    out Target_Address_Access;
      Operation        :    out Types.String;
      Service_Contexts :    out QoS_GIOP_Service_Contexts_Parameter_Access);

   procedure Negotiate_Code_Set_And_Update_Session
     (Profile        : Binding_Data.Profile_Access;
      S              : access Session'Class;
      Error          : in out Errors.Error_Container);

   -----------------------------------
   -- Internal function declaration --
   -----------------------------------

   procedure Process_Request (S : in out GIOP_Session'Class);

   procedure Process_Locate_Request (S : in out Session'Class);

   -----------------------
   -- Initialize_Implem --
   -----------------------

   overriding procedure Initialize_Implem (Implem : access GIOP_Implem_1_2) is
      use PolyORB.Parameters;

      Max : constant Types.Unsigned_Long :=
        Types.Unsigned_Long (Get_Conf
          (To_Standard_String (Implem.Section),
           Get_Conf_Chain (Implem) & ".max_message_size",
           Default_Max_GIOP_Message_Size_1_2));
   begin
      Implem.Data_Alignment        := Data_Alignment_1_2;
      Implem.Max_GIOP_Message_Size := Max - (Max mod 8);
      Implem.Max_Body              :=
        Implem.Max_GIOP_Message_Size - Types.Unsigned_Long (GIOP_Header_Size);
      Implem.Permitted_Sync_Scopes := Permitted_Sync_Scopes;
   end Initialize_Implem;

   ------------------------
   -- Initialize_Session --
   ------------------------

   overriding procedure Initialize_Session
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);
   begin
      pragma Debug (C, O ("Initializing GIOP session for version 1.2"));
      declare
         Sess : GIOP_Session renames GIOP_Session (S.all);
      begin
         Sess.MCtx := new GIOP_Message_Context_1_2;
         Sess.SCtx := new GIOP_Session_Context_1_2;
         Sess.Repr := new GIOP_1_2_CDR_Representation;
      end;
      pragma Debug (C, O ("... done"));
   end Initialize_Session;

   ----------------------
   -- Finalize_Session --
   ----------------------

   overriding procedure Finalize_Session
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      Sess : GIOP_Session renames GIOP_Session (S.all);
      MCtx : GIOP_Message_Context_1_2
               renames GIOP_Message_Context_1_2 (Sess.MCtx.all);
   begin
      if MCtx.Frag_Buf /= null then
         Release (MCtx.Frag_Buf);
      end if;
      Free (Sess.MCtx);

      Release
        (QoS_Parameter_Access
           (GIOP_Session_Context_1_2 (Sess.SCtx.all).CS_Context));
      Free (Sess.SCtx);

      Release (GIOP_1_2_CDR_Representation (Sess.Repr.all));
      Free (GIOP_1_2_CDR_Representation_Access (Sess.Repr));

      pragma Debug (C, O ("Finalize context for GIOP session 1.2"));
   end Finalize_Session;

   ---------------------
   -- Process_Message --
   ---------------------

   overriding procedure Process_Message
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class)
   is
      use PolyORB.ORB;

      Sess : GIOP_Session renames GIOP_Session (S.all);
      SCtx : GIOP_Session_Context_1_2
               renames GIOP_Session_Context_1_2 (Sess.SCtx.all);
      MCtx : GIOP_Message_Context_1_2
               renames GIOP_Message_Context_1_2 (Sess.MCtx.all);
   begin
      case MCtx.Message_Type is
         when Request =>
            if Sess.Role /= Server then
               raise Bidirectionnal_GIOP_Not_Implemented;
            end if;

            Process_Request (Sess);

         when Cancel_Request =>
            if Sess.Role /= Server then
               raise Bidirectionnal_GIOP_Not_Implemented;
            end if;
            Common_Process_Cancel_Request
              (Sess'Access, Request_Id => Unmarshall (Sess.Buffer_In));

         when Reply =>
            if Sess.Role /= Client then
               raise Bidirectionnal_GIOP_Not_Implemented;
            end if;

            declare
               Request_Id       : Types.Unsigned_Long;
               Reply_Status     : Reply_Status_Type;
               Service_Contexts : QoS_GIOP_Service_Contexts_Parameter_Access;
            begin
               if CDR_Position (Sess.Buffer_In) = GIOP_Header_Size then
                  Request_Id := Unmarshall (Sess.Buffer_In);
               else
                  --  Request Id has been read before in fragmenting packet

                  Request_Id := MCtx.Request_Id;
               end if;

               Reply_Status := Unmarshall (Sess.Buffer_In);
               Unmarshall_Service_Context_List
                 (Sess.Buffer_In, Service_Contexts);
               pragma Debug (C, O (Request_Id'Img));

               Common_Reply_Received
                 (Sess'Access, Request_Id, Reply_Status, Service_Contexts);
            end;

         when Close_Connection =>
            if Sess.Role /= Server then
               raise Bidirectionnal_GIOP_Not_Implemented;
            end if;
            Expect_GIOP_Header (Sess'Access);

         when Fragment =>

            --  Process_Message is called twice for a Fragment message:
            --  once for the decoding of the fragment header (in state First
            --  or Req), another time for reception of the payload (in state
            --  Fragment). In the Fragment case, the request id has already
            --  been set in the message context.

            if CDR_Position (Sess.Buffer_In) = GIOP_Header_Size then
               MCtx.Request_Id := Unmarshall (Sess.Buffer_In);
            end if;

            declare
               U_MCtx : GIOP_Message_Context_Access :=
                 Get_Reassembly_Context (SCtx'Access, MCtx.Request_Id);
               subtype GMC_1_2 is GIOP_Message_Context_1_2;

               procedure Swap_Bufs;
               --  Exchange the values of the session buffer and the fragment
               --  reassembly buffer.

               procedure Reassembly_Completed;
               --  After receiving the last fragment of a GIOP message,
               --  delete stored context and process reassembled message
               --  through the normal circuitry.

               procedure Swap_Bufs is
                  B : constant Buffer_Access := Sess.Buffer_In;
               begin
                  Sess.Buffer_In := GMC_1_2 (U_MCtx.all).Frag_Buf;
                  GMC_1_2 (U_MCtx.all).Frag_Buf := B;
               end Swap_Bufs;

               --------------------------
               -- Reassembly_Completed --
               --------------------------

               procedure Reassembly_Completed is
               begin
                  Swap_Bufs;
                  Release (GMC_1_2 (U_MCtx.all).Frag_Buf);
                  GMC_1_2 (U_MCtx.all).Message_Type :=
                    GMC_1_2 (U_MCtx.all).Frag_Type;
                  Sess.MCtx.all := U_MCtx.all;
                  Remove_Reassembly_Context (SCtx'Access, U_MCtx);
                  Process_Message (Implem, S);
               end Reassembly_Completed;

            begin
               if U_MCtx = null then
                  pragma Assert (MCtx.Fragmented);
                  U_MCtx := new GIOP_Message_Context_1_2'(MCtx);
                  GMC_1_2 (U_MCtx.all).Message_Type := MCtx.Frag_Type;
                  GMC_1_2 (U_MCtx.all).Frag_State   := First;
                  Store_Reassembly_Context (SCtx'Access, U_MCtx);
               end if;

               if GMC_1_2 (U_MCtx.all).Frag_State = First then
                  pragma Debug (C, O ("First fragment received"));

                  GMC_1_2 (U_MCtx.all).Frag_Buf := new Buffer_Type;
                  Swap_Bufs;
                  --  Steal session buffer to serve as reassembly buffer for
                  --  this message.

                  GMC_1_2 (U_MCtx.all).Frag_State  := Req;
               end if;

               if GMC_1_2 (U_MCtx.all).Frag_State = Req then
                  pragma Debug (C, O ("Fragment header received"));
                  pragma Debug (C, O ("Request ID :" & MCtx.Request_Id'Img));
                  pragma Debug (C, O ("Frag Size  :" & MCtx.Frag_Size'Img));

                  if MCtx.Frag_Size > 0 then
                     --  Receive fragment body into reassembly buffer

                     GMC_1_2 (U_MCtx.all).Frag_State := Fragment;
                     Emit_No_Reply
                       (Port => Lower (S),
                        Msg  => GIOP_Data_Expected'
                          (In_Buf => GMC_1_2 (U_MCtx.all).Frag_Buf,
                           Max    => Stream_Element_Count (MCtx.Frag_Size),
                           State  => Sess.State));
                  else
                     Reassembly_Completed;
                  end if;
               else
                  pragma Assert (GMC_1_2 (U_MCtx.all).Frag_State = Fragment);
                  pragma Debug (C, O ("Fragment received, size:"
                                   & MCtx.Frag_Size'Img));

                  GMC_1_2 (U_MCtx.all).Message_Size :=
                    GMC_1_2 (U_MCtx.all).Message_Size + MCtx.Frag_Size;

                  if MCtx.Fragmented then
                     --  More fragments to come

                     GMC_1_2 (U_MCtx.all).Frag_State := Req;
                     Expect_GIOP_Header (Sess'Access);

                  else
                     --  Last fragment

                     pragma Debug (C, O ("Last fragment, total size:"
                       & GMC_1_2 (U_MCtx.all).Message_Size'Img));
                     Reassembly_Completed;
                  end if;
               end if;
            end;

         when Locate_Reply =>
            if Sess.Role /= Client then
               raise Bidirectionnal_GIOP_Not_Implemented;
            end if;

            --  Exec request if request id is found in pending req list

            declare
               Request_Id   : Types.Unsigned_Long;
               Locate_Reply : Locate_Reply_Type;
            begin
               if CDR_Position (Sess.Buffer_In) = GIOP_Header_Size then
                  Request_Id := Unmarshall (Sess.Buffer_In);
               else
                  Request_Id := MCtx.Request_Id;
               end if;

               Locate_Reply := Unmarshall (Sess.Buffer_In);
               Common_Process_Locate_Reply
                 (Sess'Access,
                  Request_Id,
                  Locate_Reply);
            end;

         when Locate_Request =>
            if Sess.Role /= Server then
               raise Bidirectionnal_GIOP_Not_Implemented;
            end if;

            Process_Locate_Request (Sess);

         when Message_Error =>
            raise GIOP_Error;

      end case;
   end Process_Message;

   ---------------------
   -- Process_Request --
   ---------------------

   procedure Process_Request (S : in out GIOP_Session'Class) is
      use PolyORB.Any.NVList;
      use PolyORB.Binding_Data;
      use PolyORB.Binding_Data.Local;
      use PolyORB.Obj_Adapters;
      use PolyORB.ORB;
      use PolyORB.QoS.Addressing_Modes;
      use PolyORB.References;

      MCtx  : GIOP_Message_Context_1_2
                renames GIOP_Message_Context_1_2 (S.MCtx.all);
      SCtx  : GIOP_Session_Context_1_2
                renames GIOP_Session_Context_1_2 (S.SCtx.all);

      Sync             : Sync_Scope;
      Target_Addr      : Target_Address_Access;
      Operation        : Types.String;
      Req_Flags        : Flags := 0;
      Args             : Any.NVList.Ref;
      Def_Args         : Component_Access;
      Target           : References.Ref;
      Req              : Request_Access;
      CSP              : QoS_GIOP_Code_Sets_Parameter_Access;
      AM               : Addressing_Mode;
      Service_Contexts : QoS_GIOP_Service_Contexts_Parameter_Access;
      Error            : Errors.Error_Container;
      Result           : Any.NamedValue;
      --  Dummy NamedValue for Create_Request;
      --  the actual Result is set by the called method.
   begin
      if S.Role /= Server then
         raise Bidirectionnal_GIOP_Not_Implemented;
      end if;

      pragma Debug (C, O ("Request_Received: entering"));

      if CDR_Position (S.Buffer_In) = GIOP_Header_Size then
         MCtx.Request_Id := Unmarshall (S.Buffer_In);
      end if;

      Unmarshall_Request_Message
        (S.Buffer_In,
         MCtx'Access,
         Sync,
         Target_Addr,
         Operation,
         Service_Contexts);

      case Sync is
         when WITH_TARGET =>
            Req_Flags := Sync_With_Target;

         when WITH_TRANSPORT =>
            Req_Flags := Sync_With_Transport;

         when WITH_SERVER =>
            Req_Flags := Sync_With_Server;

         when others =>
            null;
      end case;

      S.State := Waiting_Unmarshalling;

      case Target_Addr.Address_Type is
         when Key_Addr =>
            AM := Key;

            pragma Debug (C, O ("Object Key : "
                             & Oid_To_Hex_String (
                                 Target_Addr.Object_Key.all)));

            Args := Get_Empty_Arg_List
              (Object_Adapter (ORB_Access (S.Server)),
               Target_Addr.Object_Key,
               To_Standard_String (Operation));

            if not Is_Nil (Args) then
               pragma Debug (C, O ("Immediate arguments unmarshalling"));
               S.State := Waiting_Unmarshalling;
               --  XXX change state name. We are not waiting for
               --  unmarshalling: we do it now. See next line.

               Handle_Unmarshall_Arguments (S'Unchecked_Access, Args, Error);

               if Found (Error) then
                  Catch (Error);
                  raise Program_Error;
                  --  XXX We cannot silently ignore any error. For now,
                  --  we raise this exception. To be investigated.
               end if;

            else
               pragma Debug (C, O ("Unmarshalling of arguments deferred"));
               Def_Args := S'Unchecked_Access;

            end if;

            declare
               Target_Profile : constant Binding_Data.Profile_Access
                 := new Local_Profile_Type;
            begin
               Create_Local_Profile
                (Target_Addr.Object_Key.all,
                 Local_Profile_Type (Target_Profile.all));

               Create_Reference ((1 => Target_Profile), "", Target);
               --  Create a temporary, typeless reference for this object.
               --  If we wanted to have proper type information, we would
               --  have to resolve the (local) object id through the object
               --  adapter, and query the target object for its most derived
               --  type.

               Free (Target_Addr.Object_Key);
            end;

         when Profile_Addr =>
            AM := Profile;

            Create_Reference ((1 => Target_Addr.Profile), "", Target);

            Def_Args := S'Unchecked_Access;
            --  XXX By default, we do deferred unmarshalling, we
            --  have no way to get servant signature.

         when Reference_Addr =>
            AM := Reference;

            Target := Target_Addr.Ref.IOR;

            Def_Args := S'Unchecked_Access;
            --  XXX By default, we do deferred unmarshalling, we
            --  have no way to get servant signature.
      end case;

      Create_Request
        (Target                     => Target,
         Operation                  => To_Standard_String (Operation),
         Arg_List                   => Args,
         Result                     => Result,
         Deferred_Arguments_Session => Def_Args,
         Req                        => Req,
         Req_Flags                  => Req_Flags,
         Dependent_Binding_Object   =>
           Smart_Pointers.Entity_Ptr (S.Dependent_Binding_Object));

      Add_Request_QoS
        (Req.all,
         GIOP_Addressing_Mode,
         new QoS_GIOP_Addressing_Mode_Parameter'
               (Kind => GIOP_Addressing_Mode,
                Mode => AM));

      Add_Request_QoS
        (Req.all,
         GIOP_Service_Contexts,
         QoS_Parameter_Access (Service_Contexts));
      Rebuild_Request_QoS_Parameters (Req.all);

      if Fetch_Secure_Transport_QoS /= null then
         Add_Request_QoS
         (Req.all,
          Transport_Security,
          Fetch_Secure_Transport_QoS
            (PolyORB.Transport.Transport_Endpoint_Access
              (Lower (Filter_Access (Lower (S'Access))))));
         --  XXX Should be reimplemented!
      end if;

      if not SCtx.CSN_Complete then
         CSP :=
           QoS_GIOP_Code_Sets_Parameter_Access
             (Extract_Request_Parameter (GIOP_Code_Sets, Req.all));
         SCtx.CS_Context   := null;
         SCtx.CSN_Complete := True;

         if CSP /= null then
            SCtx.CS_Context := new QoS_GIOP_Code_Sets_Parameter'(CSP.all);
            Set_Converters
              (GIOP_1_2_CDR_Representation (S.Repr.all),
               Get_Converter (Native_Char_Code_Set,  CSP.Char_Data),
               Get_Converter (Native_Wchar_Code_Set, CSP.Wchar_Data));
         end if;
      end if;

      Queue_Request (S'Unchecked_Access, Req, MCtx.Request_Id);
      Free (Target_Addr);
      pragma Debug (C, O ("Request queued."));
   end Process_Request;

   ----------------
   -- Send_Reply --
   ----------------

   overriding procedure Send_Reply
     (Implem  : access GIOP_Implem_1_2;
      S       : access Session'Class;
      Request :        Requests.Request_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.ORB;

      Sess  : GIOP_Session renames GIOP_Session (S.all);
      MCtx  : aliased GIOP_Message_Context_1_2;
      Error : Errors.Error_Container;
   begin
      if Sess.Role = Client then
         raise Bidirectionnal_GIOP_Not_Implemented;
      end if;

      MCtx.Fragmented := False;
      MCtx.Message_Type := Reply;
      Common_Send_Reply
        (Sess'Access,
         Request,
         MCtx'Access,
         Error);

      if Found (Error) then
         Set_Exception (Request.all, Error);
         Catch (Error);

         Common_Send_Reply
           (Sess'Access,
            Request,
            MCtx'Access,
            Error,
            Recovery => True);

         if Found (Error) then
            Catch (Error);

            --  Double error: bail out

            raise GIOP_Error;
         end if;
      end if;
   end Send_Reply;

   ------------------
   -- Emit_Message --
   ------------------

   overriding procedure Emit_Message
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer :        Buffers.Buffer_Access;
      Error  : in out Errors.Error_Container)
   is
      Sess : GIOP_Session renames GIOP_Session (S.all);

      MCtx_1_2  : GIOP_Message_Context_1_2
                    renames GIOP_Message_Context_1_2 (MCtx.all);
      Frag_MCtx : aliased GIOP_Message_Context_1_2;
      --  Context for fragments

      Message_Size : Types.Unsigned_Long :=
        Types.Unsigned_Long (Length (Buffer.all));
   begin
      if Message_Size > Implem.Max_GIOP_Message_Size then

         --  Message is too large, it must be fragmented.

         --  Message is divided into small slices. Each piece is
         --  copied in Out_Buf buffer, correct headers are added.

         declare
            Out_Buf        : Buffer_Access := new Buffer_Type;
            Emit_Size      : Types.Unsigned_Long;
            Request_Id     : Types.Unsigned_Long;
            Version        : GIOP_Version;

         begin
            pragma Debug (C, O ("Fragmenting message, size :"
                             & Message_Size'Img));

            Set_Endianness (Out_Buf, Endianness (Buffer));

            --  Unmarshall headers of input buffer

            Rewind (Buffer);
            Unmarshall_Global_GIOP_Header (GIOP_Session (S.all)'Access,
                                           Buffer, Version);
            --  XXX shouldn't we check that version = GIOP 1.2 ?

            Unmarshall_GIOP_Header (Sess.Implem, MCtx, Buffer);

            --  Check whether fragmentation is allowed for this message type

            if False
              or else MCtx_1_2.Message_Type = Request
              or else MCtx_1_2.Message_Type = Reply
              or else MCtx_1_2.Message_Type = Locate_Request
              or else MCtx_1_2.Message_Type = Locate_Reply
            then
               null;
            else
               --  Fragmentation not allowed for this message type
               raise GIOP_Error;
            end if;

            --  Check if message_size correspond to buffer size

            pragma Assert (MCtx_1_2.Message_Size
                             + Types.Unsigned_Long (GIOP_Header_Size)
                           = Message_Size);

            --  Get request id

            Request_Id := Unmarshall (Buffer);

            pragma Debug (C, O ("Request Id :" & Request_Id'Img));

            Frag_MCtx.Message_Size := Implem.Max_Body;
            Frag_MCtx.Fragmented   := True;
            Frag_MCtx.Message_Type := MCtx_1_2.Message_Type;
            Marshall_Global_GIOP_Header
              (Sess'Access, Frag_MCtx'Access, Out_Buf);

            --  Marshall first fragment

            Marshall (Out_Buf, Request_Id);
            Copy (Buffer, Out_Buf, Implem.Max_Body - Frag_Header_Size);
            GIOP.Emit_Message (GIOP_Implem (Implem.all)'Access, S,
                               Frag_MCtx'Access, Out_Buf, Error);
            Release_Contents (Out_Buf.all);
            if Found (Error) then
               return;
            end if;
            pragma Debug (C, O ("First fragment sent, size :"
                             & Implem.Max_Body'Img));

            --  Create subsequent fragments

            Frag_MCtx.Message_Type := Fragment;
            Message_Size := MCtx_1_2.Message_Size - Implem.Max_Body;

            loop

               --  Last fragment?

               if Message_Size <= Implem.Max_Body - Frag_Header_Size then
                  --  This is the last fragment

                  Frag_MCtx.Fragmented := False;
                  Emit_Size := Message_Size;
               else
                  --  More fragments to come

                  Frag_MCtx.Fragmented := True;
                  Emit_Size := Implem.Max_Body - Frag_Header_Size;
               end if;

               Frag_MCtx.Message_Size := Emit_Size + Frag_Header_Size;

               Marshall_Global_GIOP_Header (Sess'Access,
                                            Frag_MCtx'Access, Out_Buf);
               Marshall (Out_Buf, Request_Id);

               --  if needed, copy data

               if Emit_Size > 0 then
                  Copy (Buffer, Out_Buf, Emit_Size);
               end if;

               pragma Debug (C, O ("Fragment sent, size :" & Emit_Size'Img));

               GIOP.Emit_Message
                 (GIOP_Implem (Implem.all)'Access, S,
                  Frag_MCtx'Access, Out_Buf, Error);
               Release_Contents (Out_Buf.all);
               if Found (Error) then
                  return;
               end if;

               exit when not Frag_MCtx.Fragmented;

               --  Prepare for next fragment

               Message_Size := Message_Size - Emit_Size;
            end loop;

            --  free buffer

            Release (Out_Buf);
         end;
      else
         pragma Debug (C, O ("Emit message, size :" & Message_Size'Img));
         GIOP.Emit_Message (GIOP_Implem (Implem.all)'Access, S,
                            MCtx, Buffer, Error);
      end if;
   end Emit_Message;

   ----------------------------
   -- Process_Locate_Request --
   ----------------------------

   procedure Process_Locate_Request
     (S : in out Session'Class)
   is
      use PolyORB.Binding_Data;
      use PolyORB.Binding_Data.Local;
      use PolyORB.References;

      Sess    : GIOP_Session renames GIOP_Session (S);
      MCtx : GIOP_Message_Context_1_2
               renames GIOP_Message_Context_1_2 (Sess.MCtx.all);
      Reply_MCtx    : aliased GIOP_Message_Context_1_2;
      Buffer  : Buffer_Access renames Sess.Buffer_In;

      Request_Id   : Types.Unsigned_Long;
      Target       : References.Ref;
      Address_Disp : Addressing_Disposition;
      Result       : Locate_Reply_Type;
      Error        : Errors.Error_Container;

   begin
      if CDR_Position (Buffer) = GIOP_Header_Size then
         Request_Id := Unmarshall (Buffer);
      else
         Request_Id := MCtx.Request_Id;
      end if;

      pragma Debug (C, O ("Locate_Request, Request_Id :" & Request_Id'Img));

      --  Target Ref

      Address_Disp := Unmarshall (Buffer);

      pragma Debug (C, O ("Addr_Type  : "
                       & Addressing_Disposition'Image (Address_Disp)));

      case Address_Disp is
         when Key_Addr  =>
            declare
               Obj : constant Stream_Element_Array := Unmarshall (Buffer);

               Obj_Id : Object_Id_Access := new Object_Id'(Object_Id (Obj));

               Target_Profile : constant Binding_Data.Profile_Access :=
                 new Local_Profile_Type;
            begin
               Create_Local_Profile
                 (Obj_Id.all,
                  Local_Profile_Type (Target_Profile.all));

               Create_Reference ((1 => Target_Profile), "", Target);

               Free (Obj_Id);
            end;

         when Profile_Addr  =>
            declare
               use PolyORB.References.IOR;

               Pro : constant Binding_Data.Profile_Access  :=
                 Unmarshall_Profile (Buffer);

            begin
               if Pro = null then
                  pragma Debug (C, O ("Incorrect profile"));
                  raise GIOP_Error;
               end if;

               Create_Reference ((1 => Pro), "", Target);
            end;

         when Reference_Addr  =>
            declare
               Ref : constant IOR_Addressing_Info_Access :=
                 new IOR_Addressing_Info;
            begin
               Ref.Selected_Profile_Index := Unmarshall (Buffer);
               Ref.IOR := Unmarshall (Buffer);

               Target := Ref.IOR;
            end;
      end case;

      --  Check if object is on this node

      declare
         ORB  : constant PolyORB.ORB.ORB_Access
           := PolyORB.ORB.ORB_Access (S.Server);

         Component : PolyORB.Components.Component_Access;
         Profile : PolyORB.Binding_Data.Profile_Access;

         Error : PolyORB.Errors.Error_Container;
      begin
         PolyORB.References.Binding.Bind
           (Target,
            ORB,
            (others => null),
            Component,
            Profile,
            True,
            Error);

         if PolyORB.Errors.Found (Error) then
            if Error.Kind = ForwardRequest_E then
               Result := Object_Forward;
               Set
                 (Target,
                  PolyORB.Smart_Pointers.Entity_Of
                    (ForwardRequest_Members
                       (Error.Member.all).Forward_Reference));

            elsif Error.Kind = ForwardRequestPerm_E then
               Result := Object_Forward_Perm;
               Set
                 (Target,
                  PolyORB.Smart_Pointers.Entity_Of
                    (ForwardRequestPerm_Members
                       (Error.Member.all).Forward_Reference));

            else
               Result := Unknown_Object;
            end if;

            PolyORB.Errors.Catch (Error);

         else
            Result := Object_Here;
         end if;

      end;
      pragma Debug (C, O ("Locate_Request: result is "
                       & Locate_Reply_Type'Image (Result)));

      Reply_MCtx.Fragmented   := False;
      Reply_MCtx.Message_Type := Locate_Reply;
      Reply_MCtx.Request_Id   := Request_Id;
      Common_Locate_Reply (Sess'Access, Reply_MCtx'Access,
                           Result, Target, Error);
      if Found (Error) then
         Catch (Error);
         raise GIOP_Error;
      end if;
      Expect_GIOP_Header (Sess'Access);
   end Process_Locate_Request;

   -------------------
   -- Locate_Object --
   -------------------

   overriding procedure Locate_Object
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      R      : Pending_Request_Access;
      Error  : in out Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.ORB;
      use PolyORB.Binding_Data;

      Sess          : GIOP_Session renames GIOP_Session (S.all);
      MCtx          : aliased GIOP_Message_Context_1_2;
      Buffer        : Buffer_Access;
      Header_Buffer : Buffer_Access;
      Header_Space  : Reservation;
   begin
      if Sess.Role /= Client then
         raise Bidirectionnal_GIOP_Not_Implemented;
      end if;

      Negotiate_Code_Set_And_Update_Session (R.Target_Profile, S, Error);
      if Found (Error) then
         return;
      end if;

      pragma Debug (C, O ("Send locate request to find target object"));
      pragma Debug (C, O ("Locate Request Id :" & R.Locate_Req_Id'Img));
      pragma Debug (C, O ("Request Id :" & R.Request_Id'Img));

      Buffer := new Buffer_Type;
      Header_Buffer := new Buffer_Type;
      Header_Space := Reserve (Buffer, GIOP_Header_Size);

      Marshall_Locate_Request
        (Buffer,
         R.Locate_Req_Id,
         Target_Address'
         (Address_Type => Key_Addr,
          Object_Key   => Get_Object_Key (R.Target_Profile.all)));

      MCtx.Fragmented := False;
      MCtx.Message_Type := Locate_Request;
      MCtx.Message_Size :=
        Types.Unsigned_Long (Length (Buffer.all) - GIOP_Header_Size);
      Marshall_Global_GIOP_Header (Sess'Access, MCtx'Access, Header_Buffer);
      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
      Emit_Message (Sess.Implem, S, MCtx'Access, Buffer, Error);
      Release (Buffer);
   end Locate_Object;

   ------------------
   -- Send_Request --
   ------------------

   overriding procedure Send_Request
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      R      : Pending_Request_Access;
      Error  : in out Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.Requests.Unsigned_Long_Flags;

      Sess          : GIOP_Session renames GIOP_Session (S.all);
      MCtx          : aliased GIOP_Message_Context_1_2;
      SCtx          : GIOP_Session_Context_1_2
                        renames GIOP_Session_Context_1_2 (Sess.SCtx.all);
      Buffer        : Buffer_Access;
      Header_Buffer : Buffer_Access;
      Header_Space  : Reservation;
      Static_Buffer : constant QoS_GIOP_Static_Buffer_Parameter_Access :=
        QoS_GIOP_Static_Buffer_Parameter_Access
          (Extract_Request_Parameter (QoS.GIOP_Static_Buffer, R.Req.all));
   begin
      pragma Debug (C, O ("Sending request, Id :" & R.Request_Id'Img));

      Negotiate_Code_Set_And_Update_Session (R.Target_Profile, S, Error);
      if Found (Error) then
         return;
      end if;

      if SCtx.CS_Context /= null then
         Add_Request_QoS
           (R.Req.all,
            GIOP_Code_Sets,
            new QoS_GIOP_Code_Sets_Parameter'(SCtx.CS_Context.all));
      end if;

      Buffer := new Buffer_Type;
      Header_Buffer := new Buffer_Type;
      Header_Space := Reserve (Buffer, GIOP_Header_Size);
      Marshall (Buffer, R.Request_Id);

      --  Marshalling synchronization scope

      if Is_Set (Sync_With_Target, R.Req.Req_Flags)
        or else Is_Set (Sync_Call_Back, R.Req.Req_Flags)
      then
         --  WITH_TARGET

         Marshall (Buffer, Types.Octet (3));

      elsif Is_Set (Sync_None, R.Req.Req_Flags) then
         --  NONE

         Marshall (Buffer, Types.Octet (0));

      elsif Is_Set (Sync_With_Transport, R.Req.Req_Flags) then
         --  WITH_TRANSPORT

         Marshall (Buffer, Types.Octet (0));

      elsif Is_Set (Sync_With_Server, R.Req.Req_Flags) then
         --  WITH_SERVER

         Marshall (Buffer, Types.Octet (1));

      end if;

      --  Reserved

      for J in 1 .. 3 loop
         Marshall (Buffer, Types.Octet (0));
      end loop;

      --  Target Reference

      declare
         use PolyORB.Binding_Data;
         use PolyORB.Obj_Adapters.Group_Object_Adapter;
         use PolyORB.QoS.Addressing_Modes;
         use PolyORB.Smart_Pointers;

         OA_Entity : constant PolyORB.Smart_Pointers.Entity_Ptr
           := Get_OA (R.Target_Profile.all);
         QoS       : constant QoS_GIOP_Addressing_Mode_Parameter_Access :=
           QoS_GIOP_Addressing_Mode_Parameter_Access
             (Extract_Request_Parameter (GIOP_Addressing_Mode, R.Req.all));
         Mode      : Addressing_Disposition := Key_Addr;

      begin
         if QoS /= null then
            case QoS.Mode is
               when Key =>
                  Mode := Key_Addr;

               when Profile =>
                  Mode := Profile_Addr;

               when Reference =>
                  Mode := Reference_Addr;
            end case;
         end if;

         if Mode < Profile_Addr
           and then OA_Entity /= null
           and then OA_Entity.all in Group_Object_Adapter'Class
         then
            Mode := Profile_Addr;
         end if;

         Marshall (Buffer, Mode);

         case Mode is
            when Key_Addr =>
               declare
                  Oid : constant Object_Id_Access
                    := Binding_Data.Get_Object_Key (R.Target_Profile.all);

               begin
                  Marshall
                    (Buffer,
                     Stream_Element_Array (Oid.all));
               end;

            when Profile_Addr =>
               declare
                  Success : Boolean;

               begin
                  References.IOR.Marshall_Profile
                    (Buffer, R.Target_Profile, Success);

                  if not Success then
                     pragma Debug (C, O ("Incorrect profile"));
                     raise GIOP_Error;
                  end if;
               end;

            when Reference_Addr =>
               declare
                  use PolyORB.References;

                  P : constant Profile_Array := Profiles_Of (R.Req.Target);
                  S : Unsigned_Long          := 0;

               begin
                  for J in P'Range loop
                     if P (J) = R.Target_Profile then
                        S := Unsigned_Long (J - P'First);
                     end if;
                  end loop;

                  Marshall (Buffer, S);
                  References.IOR.Marshall_IOR (Buffer, R.Req.Target);
               end;
         end case;
      end;

      --  Operation

      pragma Debug (C, O ("Operation : " & R.Req.Operation.all));

      Marshall_Latin_1_String (Buffer, R.Req.Operation.all);

      --  Service context

      Rebuild_Request_Service_Contexts (R.Req.all);
      Marshall_Service_Context_List
        (Buffer,
         QoS_GIOP_Service_Contexts_Parameter_Access
           (Extract_Request_Parameter (GIOP_Service_Contexts, R.Req.all)));

      --  Arguments

      if Static_Buffer /= null
        and then Length (Static_Buffer.Buffer.all) /= 0
      then
         --  The arguments were marshalled and stored in the request
         --  QoS attribute. We insert the data contained in the
         --  request QoS in the buffer.

         pragma Debug (C, O ("Using static buffer"));

         Pad_Align (Buffer, Sess.Implem.Data_Alignment);

         declare
            Data            : PolyORB.Opaque.Opaque_Pointer;
            Data_To_Process : Stream_Element_Count :=
              Length (Static_Buffer.Buffer.all);
            Data_Processed  : Stream_Element_Count := Data_To_Process;
            Position        : Ada.Streams.Stream_Element_Offset := 0;
         begin
            while Data_To_Process > 0 loop
               PolyORB.Buffers.Partial_Extract_Data
                 (Static_Buffer.Buffer,
                  Data,
                  Data_Processed,
                  Use_Current => False,
                  At_Position => Position,
                  Partial => True);

               Insert_Raw_Data (Buffer, Data_Processed, Data);
               Data_To_Process := Data_To_Process - Data_Processed;
               Position := Position + Data_Processed;
            end loop;
         end;

      else
         pragma Debug (C, O ("Marshalling argument list"));
         Marshall_Argument_List
           (Sess.Implem, Buffer, Sess.Repr, R.Req.Args, PolyORB.Any.ARG_IN,
            Sess.Implem.Data_Alignment, Error);
         if Found (Error) then
            Replace_Marshal_5_To_Inv_Objref_2 (Error, Completed_No);
            --  An error in the marshalling of wchar data implies the
            --  server did not provide a valid codeset component. We
            --  convert this exception to Inv_ObjRef 2.

            Release (Header_Buffer);
            Release (Buffer);
            return;
         end if;
      end if;

      --  GIOP Header

      MCtx.Fragmented := False;
      MCtx.Message_Type := Request;
      MCtx.Message_Size :=
        Types.Unsigned_Long (Length (Buffer.all) - GIOP_Header_Size);

      Marshall_Global_GIOP_Header (Sess'Access, MCtx'Access, Header_Buffer);
      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

      --  Sending request

      Emit_Message (Sess.Implem, Sess'Access, MCtx'Access, Buffer, Error);
      pragma Debug (C, O ("Request sent, Id :" & R.Request_Id'Img
                       & ", size:" & MCtx.Message_Size'Img));

      Release (Buffer);
   end Send_Request;

   -------------------------------------------
   -- Negotiate_Code_Set_And_Update_Session --
   -------------------------------------------

   procedure Negotiate_Code_Set_And_Update_Session
     (Profile        : Binding_Data.Profile_Access;
      S              : access Session'Class;
      Error          : in out Errors.Error_Container)
   is
      Sess : GIOP_Session renames GIOP_Session (S.all);
      SCtx : GIOP_Session_Context_1_2
        renames GIOP_Session_Context_1_2 (Sess.SCtx.all);

   begin
      Sess.Mutex.Enter;
      if not SCtx.CSN_Complete then
         pragma Debug (C, O ("Negotiate_Code_Set_And_Update_Session"));

         declare
            use PolyORB.Binding_Data.GIOP;
            use PolyORB.GIOP_P.Tagged_Components;
            use PolyORB.GIOP_P.Tagged_Components.Code_Sets;

            TC : constant Tagged_Component_Access
              := Get_Component
              (GIOP_Profile_Type (Profile.all),
               Tag_Code_Sets);
         begin
            if TC = null then
               null;
            else
               SCtx.CS_Context := new QoS_GIOP_Code_Sets_Parameter;

               Negotiate_Code_Set
                 (Native_Char_Code_Set,
                  Conversion_Char_Code_Sets,
                  TC_Code_Sets (TC.all).For_Char_Data.Native_Code_Set,
                  TC_Code_Sets (TC.all).For_Char_Data.Conversion_Code_Sets,
                  Char_Data_Fallback_Code_Set,
                  SCtx.CS_Context.Char_Data,
                  Error);

               if Found (Error) then
                  Release (QoS_Parameter_Access (SCtx.CS_Context));
                  Sess.Mutex.Leave;
                  return;
               end if;

               Negotiate_Code_Set
                 (Native_Wchar_Code_Set,
                  Conversion_Wchar_Code_Sets,
                  TC_Code_Sets (TC.all).For_Wchar_Data.Native_Code_Set,
                  TC_Code_Sets (TC.all).For_Wchar_Data.Conversion_Code_Sets,
                  Wchar_Data_Fallback_Code_Set,
                  SCtx.CS_Context.Wchar_Data,
                  Error);

               if Found (Error) then
                  Release (QoS_Parameter_Access (SCtx.CS_Context));
                  Sess.Mutex.Leave;
                  return;
               end if;

               Set_Converters
                 (GIOP_1_2_CDR_Representation (Sess.Repr.all),
                  Get_Converter
                  (Native_Char_Code_Set,
                   SCtx.CS_Context.Char_Data),
                  Get_Converter
                  (Native_Wchar_Code_Set,
                   SCtx.CS_Context.Wchar_Data));
            end if;
         end;
         SCtx.CSN_Complete := True;
      end if;
      Sess.Mutex.Leave;
   end Negotiate_Code_Set_And_Update_Session;

   -------------------------
   -- Send_Cancel_Request --
   -------------------------

   overriding procedure Send_Cancel_Request
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      R      : Request_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.ORB;

      Sess  : GIOP_Session renames GIOP_Session (S.all);
      MCtx  : aliased GIOP_Message_Context_1_2;
      Error : Errors.Error_Container;
   begin
      if Sess.Role = Server then
         raise Bidirectionnal_GIOP_Not_Implemented;
      end if;

      MCtx.Fragmented := False;
      MCtx.Message_Type := Cancel_Request;
      Common_Send_Cancel_Request (Sess'Access, R, MCtx'Access, Error);
      if Found (Error) then
         Catch (Error);
         raise GIOP_Error;
      end if;
   end Send_Cancel_Request;

   ---------------------------------
   -- Unmarshalling / Marshalling --
   ---------------------------------

   ----------------------------
   -- Unmarshall_GIOP_Header --
   ----------------------------

   overriding procedure Unmarshall_GIOP_Header
     (Implem : access GIOP_Implem_1_2;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer : access Buffers.Buffer_Type)
   is
      use Octet_Flags;

      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      MCtx_1_2 : GIOP_Message_Context_1_2
                   renames GIOP_Message_Context_1_2 (MCtx.all);
      Flags   : Types.Octet;
   begin
      pragma Debug (C, O ("Unmarshall_GIOP_Header"));

      --  Flags

      Flags := Unmarshall (Buffer);
      pragma Debug (C, O ("Flags : " & Flags'Img));

      if Is_Set (Bit_Little_Endian, Flags) then
         MCtx_1_2.Message_Endianness := Little_Endian;
      else
         MCtx_1_2.Message_Endianness := Big_Endian;
      end if;
      pragma Assert (MCtx_1_2.Message_Endianness = Endianness (Buffer));

      pragma Debug (C, O ("Message Endianness : "
                       & MCtx.Message_Endianness'Img));

      MCtx_1_2.Fragmented := Is_Set (Bit_Fragment, Flags);
      pragma Debug (C, O ("Message Fragment   : " & MCtx_1_2.Fragmented'Img));

      --  Message type

      MCtx_1_2.Message_Type := Unmarshall (Buffer);
      pragma Debug
        (C, O ("Message Type       : " & MCtx_1_2.Message_Type'Img));

      --  Message size

      MCtx_1_2.Message_Size := Unmarshall (Buffer);
      pragma Debug (C, O ("Message Size       :" & MCtx_1_2.Message_Size'Img));

      if MCtx_1_2.Message_Type = Fragment then
         MCtx_1_2.Frag_State   := Req;
         MCtx_1_2.Frag_Size    := MCtx_1_2.Message_Size - Frag_Header_Size;
         MCtx_1_2.Message_Size := Frag_Header_Size;

      elsif MCtx_1_2.Fragmented then
         --  First fragment of a fragmented message
         MCtx_1_2.Frag_State   := First;
         MCtx_1_2.Frag_Size    := MCtx_1_2.Message_Size - Frag_Header_Size;
         MCtx_1_2.Frag_Type    := MCtx_1_2.Message_Type;
         MCtx_1_2.Message_Size := Frag_Header_Size;
         MCtx_1_2.Message_Type := Fragment;
      end if;
   end Unmarshall_GIOP_Header;

   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------

   overriding procedure Marshall_GIOP_Header
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer : access Buffers.Buffer_Type)
   is
      pragma Unreferenced (Implem, S);

      use Octet_Flags;

      MCtx_1_2 : GIOP_Message_Context_1_2
                   renames GIOP_Message_Context_1_2 (MCtx.all);
      Flags    : Types.Octet := 0;
   begin
      Set (Flags, Bit_Little_Endian, Endianness (Buffer) = Little_Endian);
      Set (Flags, Bit_Fragment, MCtx_1_2.Fragmented);

      Marshall (Buffer, Flags);
      Marshall (Buffer, MCtx_1_2.Message_Type);
      Marshall (Buffer, MCtx_1_2.Message_Size);
   end Marshall_GIOP_Header;

   --------------------------------
   -- Unmarshall_Request_Message --
   --------------------------------

   procedure Unmarshall_Request_Message
     (Buffer           : access Buffer_Type;
      MCtx             : access GIOP_Message_Context_1_2;
      Sync             :    out Sync_Scope;
      Target_Ref       :    out Target_Address_Access;
      Operation        :    out Types.String;
      Service_Contexts :    out QoS_GIOP_Service_Contexts_Parameter_Access)
   is
      Received_Flags : Types.Octet;
      Address_Disp   : Addressing_Disposition;
      Sink           : Types.Octet;
      pragma Unreferenced (Sink);

   begin
      pragma Debug (C, O ("Request_Id :" & MCtx.Request_Id'Img));

      --  Response flags

      Received_Flags := Unmarshall (Buffer);
      case Received_Flags is
         when 0 =>
            Sync := WITH_TRANSPORT;

            --  At this level, we cannot dissociate NONE from
            --  WITH_TRANSPORT. Besides, this makes no difference at
            --  this level. We assume WITH_TRANSPORT.

         when 1 =>
            Sync := WITH_SERVER;

         when 3 =>
            Sync := WITH_TARGET;

         when others =>
            raise GIOP_Error;
      end case;
      pragma Debug (C, O ("Sync       : " & Sync'Img));

      --  Reserved

      for J in 1 .. 3 loop
         Sink := Unmarshall (Buffer);

         --  Ignore unmarshalled value. Note that TAO may set these bytes to
         --  non-zero values.
      end loop;

      --  Target Reference

      Address_Disp := Unmarshall (Buffer);
      pragma Debug (C, O ("Addr_Type  : " & Address_Disp'Img));

      case Address_Disp is
         when Key_Addr  =>
            declare
               Obj : constant Stream_Element_Array :=  Unmarshall (Buffer);
            begin
               Target_Ref := new Target_Address'
                 (Address_Type => Key_Addr,
                  Object_Key   => new Object_Id'(Object_Id (Obj)));
            end;

         when Profile_Addr  =>
            declare
               use PolyORB.Binding_Data;
               use PolyORB.References.IOR;

               Pro : Binding_Data.Profile_Access;
            begin
               Pro := Unmarshall_Profile (Buffer);

               if Pro = null then
                  pragma Debug (C, O ("Incorrect profile"));
                  raise GIOP_Error;
               end if;

               Target_Ref := new Target_Address'
                 (Address_Type => Profile_Addr,
                  Profile      => Pro);
            end;

         when Reference_Addr  =>
            declare
               Ref : constant IOR_Addressing_Info_Access
                 := new IOR_Addressing_Info;
            begin
               Ref.Selected_Profile_Index := Unmarshall (Buffer);
               Ref.IOR := Unmarshall (Buffer);

               Target_Ref := new Target_Address'
                 (Address_Type => Reference_Addr,
                  Ref          => Ref);
            end;
      end case;

      --  Operation

      Operation := Types.String (Types.Identifier'(Unmarshall (Buffer)));
      pragma Debug (C, O ("Operation  : "
                       & Types.To_Standard_String (Operation)));

      --  Service context

      Unmarshall_Service_Context_List (Buffer, Service_Contexts);
   end Unmarshall_Request_Message;

   --------------------------------
   -- Marshall_GIOP_Header_Reply --
   --------------------------------

   overriding procedure Marshall_GIOP_Header_Reply
     (Implem  : access GIOP_Implem_1_2;
      S       : access Session'Class;
      R       : Request_Access;
      MCtx    : access GIOP_Message_Context'Class;
      Buffer  : access Buffers.Buffer_Type)
   is
      pragma Unreferenced (Implem, S);

      MCtx_1_2 : GIOP_Message_Context_1_2
                   renames GIOP_Message_Context_1_2 (MCtx.all);
   begin
      Marshall (Buffer, MCtx_1_2.Request_Id);
      Marshall (Buffer, MCtx_1_2.Reply_Status);

      Rebuild_Reply_Service_Contexts (R.all);
      Marshall_Service_Context_List
       (Buffer,
        QoS_GIOP_Service_Contexts_Parameter_Access
         (Extract_Reply_Parameter (GIOP_Service_Contexts, R.all)));
   end Marshall_GIOP_Header_Reply;

   -----------------------------
   -- Marshall_Locate_Request --
   -----------------------------

   procedure Marshall_Locate_Request
     (Buffer     :        Buffer_Access;
      Request_Id : Types.Unsigned_Long;
      Target_Ref : Target_Address)
   is
   begin

      --  Request id

      Marshall (Buffer, Request_Id);

      --  Target address

      Marshall (Buffer, Target_Ref.Address_Type);

      case Target_Ref.Address_Type is
         when Key_Addr =>
            Marshall
              (Buffer,
               Stream_Element_Array (Target_Ref.Object_Key.all));

         when Profile_Addr =>
            declare
               use PolyORB.References.IOR;

               Success : Boolean;
            begin
               Marshall (Buffer, Profile_Addr);
               Marshall_Profile (Buffer, Target_Ref.Profile, Success);
               if not Success then
                  pragma Debug (C, O ("Incorrect profile"));
                  raise GIOP_Error;
               end if;
            end;

         when Reference_Addr =>
            Marshall (Buffer, Target_Ref.Ref.Selected_Profile_Index);
            References.IOR.Marshall_IOR (Buffer, Target_Ref.Ref.IOR);
      end case;
   end Marshall_Locate_Request;

   ------------------------------
   -- Store_Reassembly_Context --
   ------------------------------

   procedure Store_Reassembly_Context
     (SCtx : access GIOP_Session_Context_1_2;
      MCtx : GIOP_Message_Context_Access)
   is
      use GIOP_Message_Context_Lists;
   begin
      Prepend (SCtx.Reassembly_Contexts, MCtx);
      GIOP_Message_Context_1_2 (MCtx.all).Frag_Position :=
        First (SCtx.Reassembly_Contexts);
   end Store_Reassembly_Context;

   ----------------------------
   -- Get_Reassembly_Context --
   ----------------------------

   function Get_Reassembly_Context
     (SCtx : access GIOP_Session_Context_1_2;
      Request_Id : Types.Unsigned_Long) return GIOP_Message_Context_Access
   is
      use GIOP_Message_Context_Lists;
      It : Iterator := First (SCtx.Reassembly_Contexts);
   begin
      while not Last (It) loop
         if Value (It).all.Request_Id = Request_Id then
            return Value (It).all;
         end if;
         Next (It);
      end loop;
      return null;
   end Get_Reassembly_Context;

   -------------------------------
   -- Remove_Reassembly_Context --
   -------------------------------

   procedure Remove_Reassembly_Context
     (SCtx : access GIOP_Session_Context_1_2;
      MCtx : in out GIOP_Message_Context_Access)
   is
      use GIOP_Message_Context_Lists;
   begin
      Remove (SCtx.Reassembly_Contexts,
              GIOP_Message_Context_1_2 (MCtx.all).Frag_Position);
      Free (MCtx);
   end Remove_Reassembly_Context;

   ----------------
   -- New_Implem --
   ----------------

   function New_Implem return GIOP_Implem_Access;

   function New_Implem return GIOP_Implem_Access is
   begin
      return new GIOP_Implem_1_2;
   end New_Implem;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Global_Register_GIOP_Version (GIOP_V1_2, New_Implem'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"protocols.giop.giop_1_2",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Protocols.GIOP.GIOP_1_2;
