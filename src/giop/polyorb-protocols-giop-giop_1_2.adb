------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . P R O T O C O L S . G I O P . G I O P _ 1 _ 2       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Binding_Data.GIOP;
with PolyORB.Binding_Data.Local;
with PolyORB.Buffers;
with PolyORB.Components;
with PolyORB.Exceptions;
with PolyORB.Filters;
with PolyORB.GIOP_P.Code_Sets.Converters;
with PolyORB.GIOP_P.Service_Contexts;
with PolyORB.GIOP_P.Tagged_Components.Code_Sets;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15
with PolyORB.Log;
with PolyORB.Objects;
with PolyORB.Obj_Adapters;
with PolyORB.Obj_Adapters.Group_Object_Adapter;
with PolyORB.ORB.Interface;
with PolyORB.Parameters;
with PolyORB.References.Binding;
with PolyORB.References.IOR;
with PolyORB.Representations.CDR.Common;
with PolyORB.Representations.CDR.GIOP_1_2;
with PolyORB.Request_QoS.Code_Sets;
with PolyORB.Request_QoS.Service_Contexts;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package body PolyORB.Protocols.GIOP.GIOP_1_2 is

   use PolyORB.Buffers;
   use PolyORB.Components;
   use PolyORB.Exceptions;
   use PolyORB.GIOP_P.Code_Sets;
   use PolyORB.GIOP_P.Code_Sets.Converters;
   use PolyORB.GIOP_P.Service_Contexts;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Representations.CDR.GIOP_1_2;
   use PolyORB.Request_QoS;
   use PolyORB.Request_QoS.Code_Sets;
   use PolyORB.Request_QoS.Service_Contexts;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.protocols.giop.giop_1_2");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Permitted_Sync_Scopes : constant PolyORB.Requests.Flags
     := Sync_None
     or Sync_With_Transport
     or Sync_With_Server
     or Sync_With_Target;

   procedure Free is new Ada.Unchecked_Deallocation
     (GIOP_Ctx_1_2, GIOP_Ctx_1_2_Access);

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

   procedure Marshall_Locate_Request
     (Buffer     : Buffer_Access;
      Request_Id : Types.Unsigned_Long;
      Target_Ref : Target_Address);

   procedure Unmarshall_Request_Message
     (Buffer           : access PolyORB.Buffers.Buffer_Type;
      Request_Id       : in out Types.Unsigned_Long;
      Sync             :    out Sync_Scope;
      Target_Ref       :    out Target_Address_Access;
      Operation        :    out Types.String;
      Service_Contexts :    out QoS_GIOP_Service_Contexts_Parameter_Access);

   -----------------------------------
   -- Internal function declaration --
   -----------------------------------

   procedure Process_Request (S : access GIOP_Session);

   procedure Process_Locate_Request (S : in out Session'Class);

   -----------------------
   -- Initialize_Implem --
   -----------------------

   procedure Initialize_Implem (Implem : access GIOP_Implem_1_2)
   is
      use PolyORB.Parameters;
      use PolyORB.Types;

      Max : constant Types.Unsigned_Long
        := Types.Unsigned_Long
        (Get_Conf
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

   procedure Initialize_Session
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      Sess : GIOP_Session renames GIOP_Session (S.all);
   begin
      Sess.Ctx  := new GIOP_Ctx_1_2;
      Sess.Repr := new GIOP_1_2_CDR_Representation;
      pragma Debug (O ("Initialize context for GIOP session 1.2"));
   end Initialize_Session;

   ----------------------
   -- Finalize_Session --
   ----------------------

   procedure Finalize_Session
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      Sess : GIOP_Session renames GIOP_Session (S.all);
      Ctx  : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);
   begin
      if Ctx.Frag_Buf /= null then
         Release (Ctx.Frag_Buf);
      end if;

      Release
        (QoS_Parameter_Access (GIOP_Ctx_1_2_Access (Sess.Ctx).CS_Context));
      Free (GIOP_Ctx_1_2_Access (Sess.Ctx));
      Release (GIOP_1_2_CDR_Representation (Sess.Repr.all));
      Free (GIOP_1_2_CDR_Representation_Access (Sess.Repr));
      pragma Debug (O ("Finalize context for GIOP session 1.2"));
   end Finalize_Session;

   ---------------------
   -- Process_Message --
   ---------------------

   procedure Process_Message
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class)
   is
      use PolyORB.ORB;
      use PolyORB.Types;

      Sess : GIOP_Session renames GIOP_Session (S.all);
      Ctx  : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);
   begin
      case Ctx.Message_Type is
         when Request =>
            if Sess.Role /= Server then
               raise Bidirectionnal_GIOP_Not_Implemented;
            end if;

            Process_Request (Sess'Access);

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

                  Request_Id := Ctx.Frag_Req_Id;
               end if;

               Reply_Status := Unmarshall (Sess.Buffer_In);
               Unmarshall_Service_Context_List
                 (Sess.Buffer_In, Service_Contexts);
               Common_Reply_Received
                 (Sess'Access, Request_Id, Reply_Status, Service_Contexts);
            end;

         when Close_Connection =>
            if Sess.Role /= Server then
               raise Bidirectionnal_GIOP_Not_Implemented;
            end if;

            Cancel_Pending_Request (Sess'Access);
            Expect_GIOP_Header (Sess'Access);

         when Fragment =>

            --  Fragmented packet

            case Ctx.Frag_State is
               when None =>
                  raise GIOP_Error;

               when First =>

                  --  First fragment: read the request_id to check it
                  --  in other fragments

                  Ctx.Frag_Req_Id := Unmarshall (Sess.Buffer_In);
                  Ctx.Frag_Size   := Ctx.Message_Size;
                  Ctx.Frag_Buf    := Sess.Buffer_In;
                  Sess.Buffer_In  := new Buffer_Type;
                  Ctx.Frag_State  := Req;
                  pragma Debug (O ("First fragment received"));
                  pragma Debug (O ("Request ID :" & Ctx.Frag_Req_Id'Img));
                  pragma Debug (O ("Size       :" & Ctx.Frag_Size'Img));
                  Expect_GIOP_Header (Sess'Access);

               when Req =>

                  --  Fragment, read Request ID
                  --  receive a fragmented packet
                  --  check if request id is ok
                  --  ask for the fragmented body, and place it in frag_buf

                  if Unmarshall (Sess.Buffer_In) /= Ctx.Frag_Req_Id then
                     raise GIOP_Error;
                  end if;
                  pragma Debug (O ("Header fragment received"));
                  pragma Debug (O ("Request ID :" & Ctx.Frag_Req_Id'Img));
                  pragma Debug (O ("Frag Size  :" & Ctx.Frag_Next'Img));

                  Ctx.Frag_State := Fragment;

                  if Ctx.Frag_Next > 0 then
                     Emit_No_Reply
                       (Port => Lower (S),
                        Msg  => GIOP_Data_Expected'
                        (In_Buf => Ctx.Frag_Buf,
                         Max    => Stream_Element_Count (Ctx.Frag_Next),
                         State  => Sess.State));
                  else
                     Process_Message (Implem, S);
                  end if;

               when Fragment =>

                  --  fragmented body is ok

                  pragma Debug (O ("Fragment received, size:"
                                   & Ctx.Frag_Size'Img));

                  --  increment frag_size

                  Ctx.Frag_Size := Ctx.Frag_Size + Ctx.Frag_Next;

                  --  check if it's the last fragment

                  if not Ctx.Fragmented then

                     --  last fragment

                     pragma Debug (O ("Last fragment, total size:"
                                      & Ctx.Frag_Size'Img));

                     --  release temp buffer

                     Release (Sess.Buffer_In);
                     Sess.Buffer_In := Ctx.Frag_Buf;
                     Ctx.Frag_State := None;
                     Ctx.Frag_Buf := null;

                     --  set correct message type and size

                     Ctx.Message_Type := Ctx.Frag_Type;
                     Ctx.Message_Size := Ctx.Frag_Size;

                     --  pass unfragmented message to process_message proc

                     Process_Message (Implem, S);
                  else

                     --  wait for next fragment

                     Ctx.Frag_State := Req;
                     Expect_GIOP_Header (Sess'Access);
                  end if;
            end case;

         when Locate_Reply =>
            if Sess.Role /= Client then
               raise Bidirectionnal_GIOP_Not_Implemented;
            end if;

            --  Exec request if request id is found in pending req list

            declare
               Request_Id   : Types.Unsigned_Long;
               Locate_Reply : Locate_Reply_Type;
            begin

               --  Request id has been read before in fragmenting packet

               if CDR_Position (Sess.Buffer_In) = GIOP_Header_Size then
                  Request_Id := Unmarshall (Sess.Buffer_In);
               else
                  Request_Id := Ctx.Frag_Req_Id;
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

         when others =>
            raise Program_Error;
      end case;
   end Process_Message;

   ---------------------
   -- Process_Request --
   ---------------------

   procedure Process_Request
     (S : access GIOP_Session)
   is
      use PolyORB.Annotations;
      use PolyORB.Any.NVList;
      use PolyORB.Binding_Data;
      use PolyORB.Binding_Data.Local;
      use PolyORB.Components;
      use PolyORB.Exceptions;
      use PolyORB.Obj_Adapters;
      use PolyORB.ORB;
      use PolyORB.ORB.Interface;
      use PolyORB.References;
      use PolyORB.Types;

      Ctx  : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (S.Ctx.all);

      ORB              : constant ORB_Access := ORB_Access (S.Server);
      Sync             : Sync_Scope;
      Target_Addr      : Target_Address_Access;
      Request_Id       : Unsigned_Long;
      Operation        : Types.String;
      Req_Flags        : Flags := 0;
      Args             : Any.NVList.Ref;
      Def_Args         : Component_Access;
      Target           : References.Ref;
      Req              : Request_Access;
      CSP              : QoS_GIOP_Code_Sets_Parameter_Access;
      Service_Contexts : QoS_GIOP_Service_Contexts_Parameter_Access;
      Error            : Exceptions.Error_Container;
      Result           : Any.NamedValue;
      --  Dummy NamedValue for Create_Request;
      --  the actual Result is set by the called method.
   begin
      if S.Role /= Server then
         raise Bidirectionnal_GIOP_Not_Implemented;
      end if;

      pragma Debug (O ("Request_Received: entering"));

      --  Set Request_Id if packet is fragmented

      Request_Id := Ctx.Frag_Req_Id;

      Unmarshall_Request_Message
        (S.Buffer_In,
         Request_Id,
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

            pragma Debug (O ("Object Key : "
                             & To_String (Target_Addr.Object_Key.all)));

            Args := Get_Empty_Arg_List
              (Object_Adapter (ORB),
               Target_Addr.Object_Key,
               To_Standard_String (Operation));

            if not Is_Nil (Args) then
               pragma Debug (O ("Immediate arguments unmarshalling"));
               S.State := Waiting_Unmarshalling;
               --  XXX change state name. We are not waiting for
               --  unmarshalling: we do it now. See next line.

               Handle_Unmarshall_Arguments (S, Args, Error);

               if Found (Error) then
                  Catch (Error);
                  raise Program_Error;
                  --  XXX We cannot silently ignore any error. For now,
                  --  we raise this exception. To be investigated.
               end if;

            else
               pragma Debug (O ("Unmarshalling of arguments deferred"));
               Def_Args := Component_Access (S);

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
            Create_Reference ((1 => Target_Addr.Profile), "", Target);

            Def_Args := Component_Access (S);
            --  XXX By default, we do deferred unmarshalling, we
            --  have no way to get servant signature.

         when Reference_Addr =>
            Target := References.Ref (Target_Addr.Ref.IOR);

            Def_Args := Component_Access (S);
            --  XXX By default, we do deferred unmarshalling, we
            --  have no way to get servant signature.
      end case;

      Create_Request
        (Target    => Target,
         Operation => To_Standard_String (Operation),
         Arg_List  => Args,
         Result    => Result,
         Deferred_Arguments_Session => Def_Args,
         Req       => Req,
         Req_Flags => Req_Flags,
         Dependent_Binding_Object =>
           Smart_Pointers.Entity_Ptr
         (S.Dependent_Binding_Object));

      Add_Request_QoS
        (Req,
         GIOP_Service_Contexts,
         QoS_Parameter_Access (Service_Contexts));
      Rebuild_Request_QoS_Parameters (Req);

      if not Ctx.CSN_Complete then
         CSP :=
           QoS_GIOP_Code_Sets_Parameter_Access
             (Extract_Request_Parameter (GIOP_Code_Sets, Req));
         Ctx.CS_Context   := null;
         Ctx.CSN_Complete := True;

         if CSP /= null then
            Ctx.CS_Context := new QoS_GIOP_Code_Sets_Parameter'(CSP.all);
            Set_Converters
              (GIOP_1_2_CDR_Representation (GIOP_Session (S.all).Repr.all),
               Get_Converter (Native_Char_Code_Set, CSP.Char_Data),
               Get_Converter (Native_Wchar_Code_Set, CSP.Wchar_Data));
         end if;
      end if;

      Set_Note
        (Req.Notepad,
         Request_Note'(Annotations.Note with Id => Request_Id));

      Queue_Request_To_Handler
        (ORB.Tasking_Policy, ORB,
         Queue_Request'
         (Request   => Req,
          Requestor => Component_Access (S)));

      Free (Target_Addr);
      pragma Debug (O ("Request queued."));
   end Process_Request;

   -------------------
   -- Process_Reply --
   -------------------

   procedure Process_Reply
     (Implem  : access GIOP_Implem_1_2;
      S       : access Session'Class;
      Request :        Requests.Request_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.ORB;

      Sess  : GIOP_Session renames GIOP_Session (S.all);
      Ctx   : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);
      Error : Exceptions.Error_Container;
   begin
      if Sess.Role = Client then
         raise Bidirectionnal_GIOP_Not_Implemented;
      end if;

      Ctx.Fragmented := False;
      Ctx.Message_Type := Reply;
      Common_Process_Reply
        (Sess'Access,
         Request,
         Ctx.Request_Id'Access,
         Ctx.Reply_Status'Access,
         Error);

      if Found (Error) then
         Request.Exception_Info := Error_To_Any (Error);
         Catch (Error);

         Common_Process_Reply
           (Sess'Access,
            Request,
            Ctx.Request_Id'Access,
            Ctx.Reply_Status'Access,
            Error);

         if Found (Error) then
            Catch (Error);
            raise GIOP_Error;
         end if;
      end if;
   end Process_Reply;

   ------------------
   -- Emit_Message --
   ------------------

   procedure Emit_Message
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      Buffer :        PolyORB.Buffers.Buffer_Access)
   is
      use PolyORB.Types;
      use PolyORB.Components;
      use PolyORB.Filters.Interface;
      use Octet_Flags;

      Sess : GIOP_Session renames GIOP_Session (S.all);
      Ctx  : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);

      Message_Size : Types.Unsigned_Long
        := Types.Unsigned_Long (Length (Buffer));
   begin
      if Message_Size > Implem.Max_GIOP_Message_Size then

         --  Message is too large, it must be fragmented.

         --  Message is divided into small slices. Each piece is
         --  copied in Out_Buf buffer, correct headers are added.

         declare
            Out_Buf        : Buffer_Access := new Buffer_Type;
            Flags          : Types.Octet;
            Message_Type   : Msg_Type;
            Message_Size2  : Types.Unsigned_Long;
            Emit_Size      : Types.Unsigned_Long;
            Request_Id     : Types.Unsigned_Long;
            Version        : GIOP_Version;
         begin
            pragma Debug (O ("Fragmenting message, size :"
                             & Message_Size'Img));

            Set_Endianness (Out_Buf, Endianness (Buffer.all));

            --  unmarshall headers of input buffer

            Rewind (Buffer);
            Unmarshall_Global_GIOP_Header (Buffer, Version);
            Flags := Unmarshall (Buffer);
            pragma Warnings (Off); --  WAG:3.15
            pragma Unreferenced (Flags);
            pragma Warnings (On); --  WAG:3.15

            Message_Type := Unmarshall (Buffer);

            --  check if fragmenting is allowed

            if False
              or else Message_Type = Cancel_Request
              or else Message_Type = Close_Connection
              or else Message_Type = Message_Error
              or else Message_Type = Fragment
            then
               raise GIOP_Error;
            end if;

            --  check if message_size correspond to buffer size

            Message_Size2 := Unmarshall (Buffer);
            pragma Assert (Message_Size2
                             + Types.Unsigned_Long (GIOP_Header_Size)
                           = Message_Size);

            --  read the request id
            --  request id is always the first field in all message type

            Request_Id := Unmarshall (Buffer);

            pragma Debug (O ("Request Id :" & Request_Id'Img));

            --  fill out_buf with headers

            Ctx.Message_Size := Implem.Max_Body;
            Ctx.Fragmented := True;
            Ctx.Message_Type := Message_Type;
            pragma Assert (Sess.Implem.Version = Version);
            Marshall_Global_GIOP_Header (Sess'Access, Out_Buf);

            --  marshall request id and first slice of data

            Marshall (Out_Buf, Request_Id);
            Copy (Buffer, Out_Buf,
                  Implem.Max_Body - Types.Unsigned_Long (Frag_Header_Size));
            Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => Out_Buf));
            Release_Contents (Out_Buf.all);
            pragma Debug (O ("First fragment sent, size :"
                             & Implem.Max_Body'Img));

            --  prepare for next slice

            Ctx.Message_Type := Fragment;
            Message_Size := Message_Size2 - Implem.Max_Body;

            --  loop to create others slices

            loop

               --  check if it's the last slice

               if Message_Size
                 > Implem.Max_Body - Types.Unsigned_Long (Frag_Header_Size)
               then
                  Emit_Size := Implem.Max_Body
                    - Types.Unsigned_Long (Frag_Header_Size);
                  Ctx.Fragmented := True;
               else
                  Emit_Size := Message_Size;
                  Ctx.Fragmented := False;
               end if;

               Ctx.Message_Size
                 := Emit_Size + Types.Unsigned_Long (Frag_Header_Size);

               --  fill out_buf with headers

               pragma Assert (Sess.Implem.Version = Version);
               Marshall_Global_GIOP_Header (Sess'Access, Out_Buf);
               Marshall (Out_Buf, Request_Id);

               --  if needed, copy data

               if Emit_Size > 0 then
                  Copy (Buffer, Out_Buf, Emit_Size);
               end if;

               pragma Debug (O ("Fragment sent, size :" & Emit_Size'Img));

               Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => Out_Buf));
               Release_Contents (Out_Buf.all);

               --  prepare for next slice

               Message_Size := Message_Size - Emit_Size;

               --  exit if it's last slice

               if not Ctx.Fragmented then
                  exit;
               end if;
            end loop;

            --  free buffer

            Release (Out_Buf);
         end;
      else
         pragma Debug (O ("Emit message, size :" & Message_Size'Img));
         Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => Buffer));
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
      use PolyORB.ORB;
      use PolyORB.References;

      Sess    : GIOP_Session renames GIOP_Session (S);
      Ctx     : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);
      Buffer  : Buffer_Access renames Sess.Buffer_In;

      Request_Id   : Types.Unsigned_Long;
      Target_Ref   : Target_Address_Access;
      Target       : References.Ref;
      Address_Disp : Addressing_Disposition;
      Result       : Locate_Reply_Type;

   begin
      if CDR_Position (Buffer) = GIOP_Header_Size then
         Request_Id := Unmarshall (Buffer);

      else
         --  Request Id, has been read before in fragmenting packet

         Request_Id := Ctx.Frag_Req_Id;
      end if;

      pragma Debug (O ("Locate_Request, Request_Id :" & Request_Id'Img));

      --  Target Ref

      Address_Disp := Unmarshall (Buffer);

      pragma Debug (O ("Addr_Type  : "
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
                  pragma Debug (O ("Incorrect profile"));
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

               Target := References.Ref (Ref.IOR);
            end;
      end case;

      --  Check if object is on this node

      declare
         ORB  : constant PolyORB.ORB.ORB_Access
           := PolyORB.ORB.ORB_Access (S.Server);

         Component : PolyORB.Components.Component_Access;
         Profile : PolyORB.Binding_Data.Profile_Access;

         Error : PolyORB.Exceptions.Error_Container;
      begin
         PolyORB.References.Binding.Bind
           (Target,
            ORB,
            Component,
            Profile,
            True,
            Error);

         if PolyORB.Exceptions.Found (Error) then
            if Error.Kind = ForwardRequest_E then
               Result := Object_Forward;
               Set
                 (Target,
                  PolyORB.Smart_Pointers.Entity_Of
                    (ForwardRequest_Members
                       (Error.Member.all).Forward_Reference));

            else
               Result := Unknown_Object;
            end if;

            PolyORB.Exceptions.Catch (Error);

         else
            Result := Object_Here;
         end if;

      end;
      pragma Debug (O ("Locate_Request: result is "
                       & Locate_Reply_Type'Image (Result)));

      Free (Target_Ref);

      --  XXX double check Target_Ref deallocation

      Ctx.Fragmented := False;
      Ctx.Message_Type := Locate_Reply;
      Common_Locate_Reply (Sess'Access, Request_Id, Result, Target);
      Expect_GIOP_Header (Sess'Access);
   end Process_Locate_Request;

   -------------------
   -- Locate_Object --
   -------------------

   procedure Locate_Object
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      R      : in     Pending_Request_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.ORB;
      use PolyORB.Binding_Data;
      use PolyORB.Types;

      Sess          : GIOP_Session renames GIOP_Session (S.all);
      Ctx           : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);
      Buffer        : Buffer_Access;
      Header_Buffer : Buffer_Access;
      Header_Space  : Reservation;
   begin
      if Sess.Role /= Client then
         raise Bidirectionnal_GIOP_Not_Implemented;
      end if;

      pragma Debug (O ("Send locate request to find target object"));
      pragma Debug (O ("Locate Request Id :" & R.Locate_Req_Id'Img));
      pragma Debug (O ("Request Id :" & R.Request_Id'Img));

      Buffer := new Buffer_Type;
      Header_Buffer := new Buffer_Type;
      Header_Space := Reserve (Buffer, GIOP_Header_Size);

      Marshall_Locate_Request
        (Buffer,
         R.Locate_Req_Id,
         Target_Address'
         (Address_Type => Key_Addr,
          Object_Key   => Get_Object_Key (R.Target_Profile.all)));

      Ctx.Fragmented := False;
      Ctx.Message_Type := Locate_Request;
      Ctx.Message_Size := Types.Unsigned_Long (Length (Buffer))
        - Types.Unsigned_Long (GIOP_Header_Size);
      Marshall_Global_GIOP_Header (Sess'Access, Header_Buffer);
      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
      Emit_Message (Sess.Implem, S, Buffer);
      Release (Buffer);
   end Locate_Object;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      R      : in     Pending_Request_Access;
      Error  : in out Exceptions.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.Requests.Unsigned_Long_Flags;
      use PolyORB.Types;

      Sess          : GIOP_Session renames GIOP_Session (S.all);
      Ctx           : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);
      Buffer        : Buffer_Access;
      Header_Buffer : Buffer_Access;
      Header_Space  : Reservation;

   begin
      pragma Debug (O ("Sending request , Id :" & R.Request_Id'Img));

      --  Process code sets negotiation once after setup connection

      if not Ctx.CSN_Complete then
         pragma Debug (O ("Negotiate code sets"));
         declare
            use PolyORB.Binding_Data.GIOP;
            use PolyORB.GIOP_P.Code_Sets;
            use PolyORB.GIOP_P.Tagged_Components;
            use PolyORB.GIOP_P.Tagged_Components.Code_Sets;

            TC : constant Tagged_Component_Access
              := Get_Component
                 (GIOP_Profile_Type (R.Target_Profile.all),
                  Tag_Code_Sets);
         begin
            if TC = null then
               pragma Debug (O ("No code sets tagged component in profile"));
               null;
            else
               Ctx.CS_Context := new QoS_GIOP_Code_Sets_Parameter;

               Negotiate_Code_Set
                 (Native_Char_Code_Set,
                  Conversion_Char_Code_Sets,
                  TC_Code_Sets (TC.all).For_Char_Data.Native_Code_Set,
                  TC_Code_Sets (TC.all).For_Char_Data.Conversion_Code_Sets,
                  Char_Data_Fallback_Code_Set,
                  Ctx.CS_Context.Char_Data,
                  Error);

               if Found (Error) then
                  Release (QoS_Parameter_Access (Ctx.CS_Context));
                  return;
               end if;

               pragma Debug
                 (O ("   TCS-C:"
                     & Code_Set_Id'Image (Ctx.CS_Context.Char_Data)));

               Negotiate_Code_Set
                 (Native_Wchar_Code_Set,
                  Conversion_Wchar_Code_Sets,
                  TC_Code_Sets (TC.all).For_Wchar_Data.Native_Code_Set,
                  TC_Code_Sets (TC.all).For_Wchar_Data.Conversion_Code_Sets,
                  Wchar_Data_Fallback_Code_Set,
                  Ctx.CS_Context.Wchar_Data,
                  Error);

               if Found (Error) then
                  Release (QoS_Parameter_Access (Ctx.CS_Context));
                  return;
               end if;

               pragma Debug
                 (O ("   TCS-W:"
                     & Code_Set_Id'Image (Ctx.CS_Context.Wchar_Data)));

               Set_Converters
                 (GIOP_1_2_CDR_Representation (Sess.Repr.all),
                  Get_Converter
                   (Native_Char_Code_Set,
                    Ctx.CS_Context.Char_Data),
                  Get_Converter
                   (Native_Wchar_Code_Set,
                    Ctx.CS_Context.Wchar_Data));
            end if;
         end;
         Ctx.CSN_Complete := True;
      end if;

      if Ctx.CS_Context /= null then
         Add_Request_QoS
           (R.Req,
            GIOP_Code_Sets,
            new QoS_GIOP_Code_Sets_Parameter'(Ctx.CS_Context.all));
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
         use PolyORB.Smart_Pointers;
         use PolyORB.Obj_Adapters.Group_Object_Adapter;
         use PolyORB.Binding_Data;

         OA_Entity : constant PolyORB.Smart_Pointers.Entity_Ptr
           := Get_OA (R.Target_Profile.all);
      begin
         if OA_Entity /= null
           and then OA_Entity.all in Group_Object_Adapter'Class
         then
            declare
               use PolyORB.References.IOR;

               Success : Boolean;
            begin
               Marshall (Buffer, Profile_Addr);
               Marshall_Profile (Buffer, R.Target_Profile, Success);
               if not Success then
                  pragma Debug (O ("Incorrect profile"));
                  raise GIOP_Error;
               end if;
            end;
         else
            declare
               Oid : constant Object_Id_Access
                 := Binding_Data.Get_Object_Key (R.Target_Profile.all);

            begin
               Marshall (Buffer, Key_Addr);
               Marshall
                 (Buffer,
                  Stream_Element_Array (Oid.all));
            end;

         end if;
      end;

      --  Operation

      pragma Debug (O ("Operation : " & R.Req.Operation.all));

      Marshall_Latin_1_String (Buffer, R.Req.Operation.all);

      --  Service context

      Rebuild_Request_Service_Contexts (R.Req);
      Marshall_Service_Context_List
        (Buffer,
         QoS_GIOP_Service_Contexts_Parameter_Access
           (Extract_Request_Parameter (GIOP_Service_Contexts, R.Req)));

      --  Arguments

      Marshall_Argument_List
        (Sess.Implem, Buffer, Sess.Repr.all, R.Req.Args, PolyORB.Any.ARG_IN,
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

      --  GIOP Header

      Ctx.Fragmented := False;
      Ctx.Message_Type := Request;
      Ctx.Message_Size := Types.Unsigned_Long (Length (Buffer))
        - Types.Unsigned_Long (GIOP_Header_Size);

      Marshall_Global_GIOP_Header (Sess'Access, Header_Buffer);
      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

      --  Sending request

      Emit_Message (Sess.Implem, Sess'Access, Buffer);
      pragma Debug (O ("Request sent, Id :" & R.Request_Id'Img));
      Release (Buffer);
   end Send_Request;

   ---------------------------
   -- Process_Abort_Request --
   ---------------------------

   procedure Process_Abort_Request
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      R      : in     Request_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.ORB;

      Sess          : GIOP_Session renames GIOP_Session (S.all);
      Ctx           : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);
   begin
      if Sess.Role = Server then
         raise Bidirectionnal_GIOP_Not_Implemented;
      end if;

      Ctx.Fragmented := False;
      Ctx.Message_Type := Cancel_Request;
      Common_Process_Abort_Request (Sess'Access, R);
   end Process_Abort_Request;

   ---------------------------------
   -- Unmarshalling / Marshalling --
   ---------------------------------

   ----------------------------
   -- Unmarshall_GIOP_Header --
   ----------------------------

   procedure Unmarshall_GIOP_Header
     (Implem  : access GIOP_Implem_1_2;
      S       : access Session'Class)
   is
      use Octet_Flags;
      use PolyORB.Types;

      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      Sess    : GIOP_Session renames GIOP_Session (S.all);
      Ctx     : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);
      Buffer  : Buffer_Access renames Sess.Buffer_In;
      Flags   : Types.Octet;
   begin
      --  Flags

      Flags := Unmarshall (Buffer);
      pragma Debug (O ("Flags : " & Flags'Img));

      if Is_Set (Bit_Little_Endian, Flags) then
         Ctx.Message_Endianness := Little_Endian;
      else
         Ctx.Message_Endianness := Big_Endian;
      end if;
      pragma Assert (Ctx.Message_Endianness = Endianness (Buffer.all));

      pragma Debug (O ("Message Endianness : "
                       & Ctx.Message_Endianness'Img));

      Ctx.Fragmented := Is_Set (Bit_Fragment, Flags);
      pragma Debug (O ("Message Fragment   : " & Ctx.Fragmented'Img));

      --  Message type

      Ctx.Message_Type := Unmarshall (Buffer);
      pragma Debug (O ("Message Type       : " & Ctx.Message_Type'Img));

      --  Message size

      Ctx.Message_Size := Unmarshall (Buffer);
      pragma Debug (O ("Message Size       :" & Ctx.Message_Size'Img));

      if Ctx.Message_Type = Fragment
        and then Ctx.Frag_State /= None
      then
         Ctx.Frag_Next
           := Ctx.Message_Size - Types.Unsigned_Long (Frag_Header_Size);

         Ctx.Message_Size := Types.Unsigned_Long (Frag_Header_Size);
      end if;

      if Ctx.Fragmented
        and then Ctx.Frag_State = None
      then
         Ctx.Frag_State   := First;
         Ctx.Frag_Type    := Ctx.Message_Type;
         Ctx.Message_Type := Fragment;
         pragma Debug (O ("Enter fragment mode"));
      end if;
   end Unmarshall_GIOP_Header;

   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------

   procedure Marshall_GIOP_Header
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      Buffer : access PolyORB.Buffers.Buffer_Type)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use Octet_Flags;

      Sess : GIOP_Session renames GIOP_Session (S.all);
      Ctx  : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);
      Flags : Types.Octet := 0;
   begin
      Set (Flags, Bit_Little_Endian, Endianness (Buffer.all) = Little_Endian);
      Set (Flags, Bit_Fragment, Ctx.Fragmented);

      Marshall (Buffer, Flags);
      Marshall (Buffer, Ctx.Message_Type);
      Marshall (Buffer, Ctx.Message_Size);
   end Marshall_GIOP_Header;

   --------------------------------
   -- Unmarshall_Request_Message --
   --------------------------------

   procedure Unmarshall_Request_Message
     (Buffer           : access Buffer_Type;
      Request_Id       : in out Types.Unsigned_Long;
      Sync             :    out Sync_Scope;
      Target_Ref       :    out Target_Address_Access;
      Operation        :    out Types.String;
      Service_Contexts :    out QoS_GIOP_Service_Contexts_Parameter_Access)
   is
      use PolyORB.Types;

      Received_Flags : Types.Octet;
      Address_Disp   : Addressing_Disposition;
      Sink           : Types.Octet;

   begin

      --  Request Id has been read before in fragmenting packet

      if CDR_Position (Buffer) = GIOP_Header_Size then
         Request_Id := Unmarshall (Buffer);
      end if;
      pragma Debug (O ("Request_Id :" & Request_Id'Img));

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
      pragma Debug (O ("Sync       : " & Sync'Img));

      --  Reserved

      for J in 1 .. 3 loop
         Sink := Unmarshall (Buffer);

         --  Ensure all bytes are equal to 0

         if Sink /= 0 then
            raise GIOP_Error;
         end if;
      end loop;

      --  Target Reference

      Address_Disp := Unmarshall (Buffer);
      pragma Debug (O ("Addr_Type  : " & Address_Disp'Img));

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
                  pragma Debug (O ("Incorrect profile"));
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
      pragma Debug (O ("Operation  : "
                       & Types.To_Standard_String (Operation)));

      --  Service context

      Unmarshall_Service_Context_List (Buffer, Service_Contexts);
   end Unmarshall_Request_Message;

   --------------------------------
   -- Marshall_GIOP_Header_Reply --
   --------------------------------

   procedure Marshall_GIOP_Header_Reply
     (Implem  : access GIOP_Implem_1_2;
      S       : access Session'Class;
      R       : Request_Access;
      Buffer  : access PolyORB.Buffers.Buffer_Type)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      Sess    : GIOP_Session renames GIOP_Session (S.all);
      Ctx     : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);
   begin
      Marshall (Buffer, Ctx.Request_Id);
      Marshall (Buffer, Ctx.Reply_Status);

      Rebuild_Reply_Service_Contexts (R);
      Marshall_Service_Context_List
       (Buffer,
        QoS_GIOP_Service_Contexts_Parameter_Access
         (Extract_Reply_Parameter (GIOP_Service_Contexts, R)));
   end Marshall_GIOP_Header_Reply;

   -----------------------------
   -- Marshall_Locate_Request --
   -----------------------------

   procedure Marshall_Locate_Request
     (Buffer     :        Buffer_Access;
      Request_Id : in     Types.Unsigned_Long;
      Target_Ref : in     Target_Address)
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
                  pragma Debug (O ("Incorrect profile"));
                  raise GIOP_Error;
               end if;
            end;

         when Reference_Addr =>
            Marshall (Buffer, Target_Ref.Ref.Selected_Profile_Index);
            References.IOR.Marshall_IOR (Buffer, Target_Ref.Ref.IOR);
      end case;
   end Marshall_Locate_Request;

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
      Global_Register_GIOP_Version
        (GIOP_Version'(Major => 1, Minor => 2), New_Implem'Access);
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
       Init      => Initialize'Access));
end PolyORB.Protocols.GIOP.GIOP_1_2;
