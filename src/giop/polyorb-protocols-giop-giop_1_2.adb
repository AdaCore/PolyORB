------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . P R O T O C O L S . G I O P . G I O P _ 1 _ 2       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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
with PolyORB.Log;
with PolyORB.Configuration;
with PolyORB.Buffers;
with PolyORB.Representations.CDR;
with PolyORB.Objects;
with PolyORB.Obj_Adapters;
with PolyORB.Binding_Data;
with PolyORB.Binding_Data.Local;
with PolyORB.Binding_Data.IIOP;
with PolyORB.ORB.Interface;
with PolyORB.Filters;
with PolyORB.Filters.Interface;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Utils.Strings;

package body PolyORB.Protocols.GIOP.GIOP_1_2 is

   use PolyORB.Buffers;
   use PolyORB.Representations.CDR;
   use PolyORB.Objects;

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.protocols.giop.giop_1_2");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Free is new Ada.Unchecked_Deallocation
     (GIOP_Ctx_1_2, GIOP_Ctx_1_2_Access);

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

   --  local helpers
   procedure Marshall_Locate_Request
     (Buffer     :        Buffer_Access;
      Request_Id : in     Types.Unsigned_Long;
      Target_Ref : in     Target_Address);

   procedure Unmarshall_Request_Message
     (Buffer     : access PolyORB.Buffers.Buffer_Type;
      Request_Id : in out Types.Unsigned_Long;
      Sync       :    out Sync_Scope;
      Target_Ref :    out Target_Address_Access;
      Operation  :    out Types.String);

   -----------------------------------
   -- Internal function declaration --
   -----------------------------------

   procedure Process_Request
     (S : access GIOP_Session);

   procedure Process_Locate_Request
     (S : in out Session'Class);

   ---------------
   -- Functions --
   ---------------

   procedure Initialize_Implem
     (Implem : access GIOP_Implem_1_2)
   is
      use PolyORB.Types;
      use PolyORB.Configuration;

      Max : constant Types.Unsigned_Long
        := Types.Unsigned_Long
        (Get_Conf
         ("giop",
          Get_Conf_Chain (Implem)
          & ".max_message_size",
          Default_Max_GIOP_Message_Size_1_2));
   begin
      Implem.Data_Alignment := Data_Alignment_1_2;
      Implem.Max_GIOP_Message_Size := Max - (Max mod 8);
      Implem.Max_Body := Implem.Max_GIOP_Message_Size
        - Types.Unsigned_Long (GIOP_Header_Size);
   end Initialize_Implem;

   procedure Initialize_Session
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      Sess : GIOP_Session renames GIOP_Session (S.all);
   begin
      Sess.Ctx := new GIOP_Ctx_1_2;
      pragma Debug (O ("Initialize context for GIOP session 1.2"));
   end Initialize_Session;

   procedure Finalize_Session
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      Sess    : GIOP_Session renames GIOP_Session (S.all);
      Ctx     : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);
   begin
      if Ctx.Frag_Buf /= null then
         Release (Ctx.Frag_Buf);
      end if;
      Free (GIOP_Ctx_1_2_Access (Sess.Ctx));
      pragma Debug (O ("Finalize context for GIOP session 1.2"));
   end Finalize_Session;

   ---------------------
   -- Process_Message --
   ---------------------

   procedure Process_Message
     (Implem     : access GIOP_Implem_1_2;
      S          : access Session'Class)
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
               Request_Id   : Types.Unsigned_Long;
               Reply_Status : Reply_Status_Type;
            begin
               --  Request Id has been read before in fragmenting packet
               if CDR_Position (Sess.Buffer_In) = GIOP_Header_Size then
                  Request_Id := Unmarshall (Sess.Buffer_In);
               else
                  Request_Id := Ctx.Frag_Req_Id;
               end if;
               Reply_Status := Unmarshall (Sess.Buffer_In);
               Unmarshall_Service_Context_List (Sess.Buffer_In);
               Common_Reply_Received (Sess'Access, Request_Id, Reply_Status);
            end;

         when Close_Connection =>
            if Sess.Role /= Server then
               raise Bidirectionnal_GIOP_Not_Implemented;
            end if;
            Cancel_Pending_Request (Sess'Access);
            Expect_GIOP_Header (Sess'Access);

         when Fragment =>
            --  Receive a fragmented packet
            case Ctx.Frag_State is
               when None =>
                  raise GIOP_Error;

               when First =>
                  --  First fragment
                  --  read the request_id to check it in other fragments
                  Ctx.Frag_Req_Id := Unmarshall (Sess.Buffer_In);
                  Ctx.Frag_Size := Ctx.Message_Size;
                  Ctx.Frag_Buf := Sess.Buffer_In;
                  Sess.Buffer_In := new Buffer_Type;
                  Ctx.Frag_State := Req;
                  pragma Debug (O ("First fragment received"));
                  pragma Debug (O ("Request ID :" & Ctx.Frag_Req_Id'Img));
                  pragma Debug (O ("Size       :" & Ctx.Frag_Size'Img));
                  Expect_GIOP_Header (Sess'Access);

               when Req =>
                  --  Fragment, read Request ID
                  --  receive a fragmented packet
                  --  check if request id is ok
                  --  ask for the fragmented body, anbd place it in frag_buf
                  if Unmarshall (Sess.Buffer_In) /= Ctx.Frag_Req_Id then
                     raise GIOP_Error;
                  end if;
                  pragma Debug (O ("Header fragment received"));
                  pragma Debug (O ("Request ID :" & Ctx.Frag_Req_Id'Img));
                  pragma Debug (O ("Frag Size  :" & Ctx.Frag_Next'Img));
                  Ctx.Frag_State := Fragment;
                  if Ctx.Frag_Next > 0 then
                     Filters.Interface.Expect_Data
                       (S,
                        Ctx.Frag_Buf,
                        Stream_Element_Count (Ctx.Frag_Next));
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
               --  Request Id has been read before in fragmenting packet
               if CDR_Position (Sess.Buffer_In) = GIOP_Header_Size then
                  Request_Id := Unmarshall (Sess.Buffer_In);
               else
                  Request_Id := Ctx.Frag_Req_Id;
               end if;
               Locate_Reply := Unmarshall (Sess.Buffer_In);
               Common_Process_Locate_Reply (Sess'Access,
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
            raise Not_Implemented;
      end case;
   end Process_Message;

   ---------------------
   -- Process_Request --
   ---------------------

   procedure Process_Request
     (S : access GIOP_Session)
   is
      use PolyORB.ORB;
      use PolyORB.ORB.Interface;
      use PolyORB.Components;
      use PolyORB.Binding_Data;
      use PolyORB.Binding_Data.Local;
      use PolyORB.Obj_Adapters;
      use PolyORB.Types;
      use PolyORB.Any.NVList;
      use PolyORB.References;
      use PolyORB.Annotations;

      Ctx  : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (S.Ctx.all);

      ORB         : ORB_Access;
      Object_Key  : Objects.Object_Id_Access;
      Sync        : Sync_Scope;
      Target_Addr : Target_Address_Access;
      Request_Id  : Unsigned_Long;
      Operation   : Types.String;
      Req_Flags   : Flags := 0;
      Args        : Any.NVList.Ref;
      Def_Args    : Component_Access;
      Target      : References.Ref;
      Req         : Request_Access;

      Result      : Any.NamedValue;
      --  Dummy NamedValue for Create_Request;
      --  the actual Result  is set by the called method.n
   begin
      if S.Role /= Server then
         raise Bidirectionnal_GIOP_Not_Implemented;
      end if;

      ORB := ORB_Access (S.Server);

      pragma Debug (O ("Request_Received: entering"));

      --  set Request_Id if packet is fragmented
      Request_Id := Ctx.Frag_Req_Id;
      Unmarshall_Request_Message
        (S.Buffer_In,
         Request_Id,
         Sync,
         Target_Addr,
         Operation);

      if Target_Addr.Address_Type = Key_Addr then
         Object_Key := Target_Addr.Object_Key;
      end if;

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

      pragma Debug (O ("Object Key : "
                       & To_String (Object_Key.all)));

      Args := Get_Empty_Arg_List
        (Object_Adapter (ORB),
         Object_Key,
         To_Standard_String (Operation));

      if not Is_Nil (Args) then
         pragma Debug (O ("Immediate arguments unmarshalling"));
         S.State := Waiting_Unmarshalling;
         --  XXX change state name. We are not waiting for
         --  unmarshalling: we do it now. See next line.

         Handle_Unmarshall_Arguments (S, Args);
      else
         pragma Debug (O ("Unmarshalling of arguments deffered"));
         S.State := Waiting_Unmarshalling;
         Def_Args := Component_Access (S);
      end if;

      case Target_Addr.Address_Type is
         when Key_Addr =>
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
            end;

         when Profile_Addr =>
            Create_Reference ((1 => Target_Addr.Profile), "", Target);

         when Reference_Addr =>
            Target := References.Ref (Target_Addr.Ref.IOR);
      end case;

      Create_Request
        (Target    => Target,
         Operation => To_Standard_String (Operation),
         Arg_List  => Args,
         Result    => Result,
         Deferred_Arguments_Session => Def_Args,
         Req       => Req,
         Req_Flags => Req_Flags);

      Set_Note
        (Req.Notepad,
         Request_Note'(Annotations.Note with Id => Request_Id));

      Queue_Request_To_Handler
        (ORB.Tasking_Policy, ORB,
         Queue_Request'
         (Request => Req,
          Requestor => Component_Access (S)));

      Free (Target_Addr);
      Free (Object_Key);
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

      Sess : GIOP_Session renames GIOP_Session (S.all);
      Ctx  : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);
   begin
      if Sess.Role = Client then
         raise Bidirectionnal_GIOP_Not_Implemented;
      end if;

      Ctx.Fragmented := False;
      Ctx.Message_Type := Reply;
      Common_Process_Reply
        (Sess'Access, Request, Ctx.Request_Id'Access, Ctx.Reply_Status'Access);
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

         --  Message too big, will be fragmented

         --  message will be cut in small slices
         --  each piece is copied in a new buffer, out_buf
         --  correct headers are added

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
              or Message_Type = Cancel_Request
              or Message_Type = Close_Connection
              or Message_Type = Message_Error
              or Message_Type = Fragment
            then
               raise GIOP_Error;
            end if;

            --  check if message_size correspond to buffer size
            Message_Size2 := Unmarshall (Buffer);
            pragma Assert (Message_Size2
                             + Types.Unsigned_Long (GIOP_Header_Size)
                             = Message_Size);

            --  read the request id
            --  request id is always the first fiels in all message type
            Request_Id := Unmarshall (Buffer);

            pragma Debug (O ("Request Id :"
                             & Request_Id'Img));

            --  fill out_buff with headers
            Marshall_Global_GIOP_Header (Version, Out_Buf);
            Ctx.Message_Size := Implem.Max_Body;
            Ctx.Fragmented := True;
            Ctx.Message_Type := Message_Type;
            Marshall_GIOP_Header (Implem, S, Out_Buf);


            --  marshall request id and first slice of data
            Marshall (Out_Buf, Request_Id);
            Copy (Buffer, Out_Buf,
                  Implem.Max_Body - Types.Unsigned_Long (Frag_Header_Size));
            pragma Debug (O ("First fragment sent, size :"
                             & Implem.Max_Body'Img));

            --  prepare for next slice
            Ctx.Message_Type := Fragment;
            Message_Size := Message_Size2 - Implem.Max_Body;

            --  loop to create others slices
            loop
               --  check if it's the last slice
               if Message_Size >
                 (Implem.Max_Body - Types.Unsigned_Long (Frag_Header_Size))
               then
                  Emit_Size := Implem.Max_Body
                    - Types.Unsigned_Long (Frag_Header_Size);
                  Ctx.Fragmented := True;
               else
                  Emit_Size := Message_Size;
                  Ctx.Fragmented := False;
               end if;

               Ctx.Message_Size := Emit_Size
                 + Types.Unsigned_Long (Frag_Header_Size);

               --  fill out_buf with headers
               Marshall_Global_GIOP_Header (Version, Out_Buf);
               Marshall_GIOP_Header (Implem, S, Out_Buf);
               Marshall (Out_Buf, Request_Id);

               --  if needed, copy data
               if Emit_Size > 0 then
                  Copy (Buffer, Out_Buf, Emit_Size);
               end if;

               pragma Debug (O ("Fragment sent, size :"
                                & Emit_Size'Img));


               --  prepare for next slice
               Message_Size := Message_Size - Emit_Size;

               --  exit if it's last slice
               if not Ctx.Fragmented then
                  exit;
               end if;
            end loop;

            --  send data and exit
            Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => Out_Buf));
            Release (Out_Buf);
         end;
      else
         pragma Debug (O ("Emit message, size :"
                          & Message_Size'Img));
         Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => Buffer));
      end if;
   end Emit_Message;

   ----------------------------
   -- Process Locate Request --
   ----------------------------

   procedure Process_Locate_Request
     (S : in out Session'Class)
   is
      Sess    : GIOP_Session renames GIOP_Session (S);
      Ctx     : GIOP_Ctx_1_2 renames GIOP_Ctx_1_2 (Sess.Ctx.all);
      Buffer  : Buffer_Access renames Sess.Buffer_In;

      Request_Id   : Types.Unsigned_Long;
      Target_Ref   : Target_Address_Access;
      Address_Disp : Addressing_Disposition;
      Result       : Locate_Reply_Type;
   begin
      --  Request Id, has been read before in fragmenting packet
      if CDR_Position (Buffer) = GIOP_Header_Size then
         Request_Id := Unmarshall (Buffer);
      else
         Request_Id := Ctx.Frag_Req_Id;
      end if;

      --  Target Ref
      Address_Disp := Unmarshall (Buffer);

      pragma Debug (O ("Addr_Type  : "
                       & Address_Disp'Img));

      case Address_Disp is
         when Key_Addr  =>
            null;
            declare
               Obj : constant Stream_Element_Array :=  Unmarshall (Buffer);

               pragma Warnings (Off);
               pragma Unreferenced (Obj);
               pragma Warnings (On);
            begin
               Target_Ref := new Target_Address'
                 (Address_Type => Key_Addr,
                  --  Object_Key => new Object_Id'(Object_Id (Obj)));
                  --  XXX line deactivated, because of memory leaks
                  --  need to be freed at the end of this procedure
                  Object_Key => null);
            end;

         when Profile_Addr  =>
            Target_Ref := new Target_Address'
              (Address_Type => Profile_Addr,
               Profile  =>  Binding_Data.IIOP.
               Unmarshall_IIOP_Profile_Body (Buffer));

         when Reference_Addr  =>
            declare
               Ref :
                 constant IOR_Addressing_Info_Access
                 := new IOR_Addressing_Info;
            begin
               Ref.Selected_Profile_Index := Unmarshall (Buffer);
               Ref.IOR := Unmarshall (Buffer);

               Target_Ref := new Target_Address'
                 (Address_Type => Reference_Addr,
                  Ref  => Ref);
            end;
      end case;

      pragma Debug (O ("Locate_Request, Request Id :"
                       & Request_Id'Img));

      Result := Object_Here;

      --  XXX need to be implemented

      Free (Target_Ref);
      Ctx.Fragmented := False;
      Ctx.Message_Type := Locate_Reply;
      Common_Locate_Reply (Sess'Access, Request_Id, Result);
      Expect_GIOP_Header (Sess'Access);
   end Process_Locate_Request;

   -------------------
   -- Locate Object --
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
          Object_Key =>
            Get_Object_Key (R.Target_Profile.all)));

      Marshall_Global_GIOP_Header (Sess'Access, Header_Buffer);
      Ctx.Fragmented := False;
      Ctx.Message_Type := Locate_Request;
      Ctx.Message_Size := Types.Unsigned_Long (Length (Buffer))
        - Types.Unsigned_Long (GIOP_Header_Size);
      Marshall_GIOP_Header (Sess.Implem, S, Header_Buffer);
      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
      Emit_Message (Sess.Implem, S, Buffer);
      Release (Buffer);
   end Locate_Object;

   ------------------
   -- Send Request --
   ------------------

   procedure Send_Request
     (Implem : access GIOP_Implem_1_2;
      S      : access Session'Class;
      R      : in     Pending_Request_Access)
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
      Sync          : Sync_Scope := NONE;
      Sink          : constant Types.Octet := 0;

      Oid           : constant Object_Id_Access
        := Binding_Data.Get_Object_Key (R.Target_Profile.all);
      Target_Ref    : constant Target_Address := Target_Address'
        (Address_Type => Key_Addr,
         Object_Key   => Oid);
   begin
      pragma Debug (O ("Sending request , Id :" & R.Request_Id'Img));

      if Is_Set (Sync_None, R.Req.Req_Flags) then
         Sync := NONE;
      elsif Is_Set (Sync_With_Transport, R.Req.Req_Flags) then
         Sync := WITH_TRANSPORT;
      elsif Is_Set (Sync_With_Server, R.Req.Req_Flags) then
         Sync := WITH_SERVER;
      elsif Is_Set (Sync_With_Target, R.Req.Req_Flags)
        or else Is_Set (Sync_Call_Back, R.Req.Req_Flags)
      then
         Sync := WITH_TARGET;
      end if;

      Buffer := new Buffer_Type;
      Header_Buffer := new Buffer_Type;
      Header_Space := Reserve (Buffer, GIOP_Header_Size);
      Marshall (Buffer, R.Request_Id);
      case Sync is
         when NONE | WITH_TRANSPORT =>
            Marshall (Buffer, Types.Octet (0));
         when WITH_SERVER =>
            Marshall (Buffer, Types.Octet (1));
         when WITH_TARGET =>
            Marshall (Buffer, Types.Octet (3));
      end case;

      for J in 1 .. 3 loop
         Marshall (Buffer, Sink);
      end loop;

      Marshall (Buffer, Key_Addr);
      Marshall
        (Buffer,
         Stream_Element_Array
         (Target_Ref.Object_Key.all));

      pragma Debug (O ("Operation : "
                       & To_Standard_String (R.Req.Operation)));

      Marshall (Buffer, R.Req.Operation);
      Marshall_Service_Context_List (Buffer);

      Marshall_Argument_List
        (Sess.Implem, Buffer, R.Req.Args, PolyORB.Any.ARG_IN,
         Sess.Implem.Data_Alignment);

      Marshall_Global_GIOP_Header (Sess'Access, Header_Buffer);
      Ctx.Fragmented := False;
      Ctx.Message_Type := Request;
      Ctx.Message_Size := Types.Unsigned_Long (Length (Buffer))
        - Types.Unsigned_Long (GIOP_Header_Size);
      Marshall_GIOP_Header (Sess.Implem, Sess'Access, Header_Buffer);
      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
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

   -----------------
   -- GIOP Header --
   -----------------

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
      Flags := Unmarshall (Buffer);
      pragma Debug (O ("Flags : "
                       & Flags'Img));

      if (Is_Set (Bit_Endianness, Flags)) then
         Ctx.Message_Endianness := Little_Endian;
      else
         Ctx.Message_Endianness := Big_Endian;
      end if;
      pragma Assert (Ctx.Message_Endianness = Endianness (Buffer.all));

      pragma Debug (O ("Message Endianness : "
                       & Ctx.Message_Endianness'Img));

      if (Is_Set (Bit_Fragment, Flags)) then
         Ctx.Fragmented := True;
      else
         Ctx.Fragmented := False;
      end if;
      pragma Debug (O ("Message Fragment   : "
                       & Ctx.Fragmented'Img));

      --  Extract type
      Ctx.Message_Type := Unmarshall (Buffer);
      pragma Debug (O ("Message Type       : "
                       & Ctx.Message_Type'Img));

      --  Extract size
      Ctx.Message_Size := Unmarshall (Buffer);
      pragma Debug (O ("Message Size       :"
                       & Ctx.Message_Size'Img));

      if Ctx.Message_Type = Fragment and Ctx.Frag_State /= None then
         Ctx.Frag_Next := Ctx.Message_Size
           - Types.Unsigned_Long (Frag_Header_Size);
         Ctx.Message_Size := Types.Unsigned_Long (Frag_Header_Size);
      end if;
      if Ctx.Fragmented and Ctx.Frag_State = None then
         Ctx.Frag_State := First;
         Ctx.Frag_Type := Ctx.Message_Type;
         Ctx.Message_Type := Fragment;
         pragma Debug (O ("Enter fragment mode"));
      end if;
   end Unmarshall_GIOP_Header;

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
      Set (Flags, Bit_Endianness, Ctx.Message_Endianness = Little_Endian);
      Set (Flags, Bit_Fragment, Ctx.Fragmented);

      Marshall (Buffer, Flags);
      Marshall (Buffer, Ctx.Message_Type);
      Marshall (Buffer, Ctx.Message_Size);
   end Marshall_GIOP_Header;

   ---------------------
   -- Request_Message --
   ---------------------

   procedure Unmarshall_Request_Message
     (Buffer     : access Buffer_Type;
      Request_Id : in out Types.Unsigned_Long;
      Sync       :    out Sync_Scope;
      Target_Ref :    out Target_Address_Access;
      Operation  :    out Types.String)
   is
      use Representations.CDR;
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
            --  XXX NONE = WITH_TRANSPORT
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
         if Sink /= 0 then
            --  Ensure all bytes are equal to 0
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
                  Object_Key => new Object_Id'(Object_Id (Obj)));
            end;

         when Profile_Addr  =>
            Target_Ref := new Target_Address'
              (Address_Type => Profile_Addr,
               Profile  =>  Binding_Data.IIOP.
               Unmarshall_IIOP_Profile_Body (Buffer));

         when Reference_Addr  =>
            declare
               Ref :
                 constant IOR_Addressing_Info_Access
                 := new IOR_Addressing_Info;
            begin
               Ref.Selected_Profile_Index := Unmarshall (Buffer);
               Ref.IOR := Unmarshall (Buffer);

               Target_Ref := new Target_Address'
                 (Address_Type => Reference_Addr,
                  Ref  => Ref);
            end;
      end case;

      --  Operation
      Operation :=  Unmarshall (Buffer);
      pragma Debug (O ("Operation  : "
                       & Types.To_Standard_String (Operation)));

      --  Service context
      Unmarshall_Service_Context_List (Buffer);
   end Unmarshall_Request_Message;

   -----------------------
   -- GIOP Header Reply --
   -----------------------

   procedure Marshall_GIOP_Header_Reply
     (Implem  : access GIOP_Implem_1_2;
      S       : access Session'Class;
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
      Marshall_Service_Context_List (Buffer);
   end Marshall_GIOP_Header_Reply;

   -----------------------------
   -- Marshall_Locate_Request --
   -----------------------------

   procedure Marshall_Locate_Request
     (Buffer     :        Buffer_Access;
      Request_Id : in     Types.Unsigned_Long;
      Target_Ref : in     Target_Address)
   is
      use Representations.CDR;
      use Binding_Data.IIOP;

   begin

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Marshalling the Target Address
      Marshall (Buffer, Target_Ref.Address_Type);

      case Target_Ref.Address_Type is
         when Key_Addr =>
            Marshall
              (Buffer,
               Stream_Element_Array
               (Target_Ref.Object_Key.all));

         when Profile_Addr =>
            Marshall_IIOP_Profile_Body
              (Buffer, Target_Ref.Profile);

         when Reference_Addr =>
            Marshall (Buffer, Target_Ref.Ref.Selected_Profile_Index);
            References.IOR.Marshall_IOR (Buffer, Target_Ref.Ref.IOR);
      end case;
   end Marshall_Locate_Request;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register_GIOP_Version (GIOP_V_1_2, new GIOP_Implem_1_2);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"protocols.giop.giop_1_2",
       Conflicts => Empty,
       Depends => +"protocols.giop",
       Provides => Empty,
       Init => Initialize'Access));
end PolyORB.Protocols.GIOP.GIOP_1_2;
