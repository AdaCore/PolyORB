------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . P R O T O C O L S . G I O P . G I O P _ 1 _ 1       --
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
with PolyORB.Binding_Data.Local;
with PolyORB.Buffers;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15
with PolyORB.Log;
with PolyORB.Objects;
with PolyORB.Obj_Adapters;
with PolyORB.ORB.Interface;
with PolyORB.Parameters;
with PolyORB.References;
with PolyORB.Representations.CDR;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package body PolyORB.Protocols.GIOP.GIOP_1_1 is

   use PolyORB.Buffers;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.Representations.CDR;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.protocols.giop.giop_1_1");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Free is new
     Ada.Unchecked_Deallocation (GIOP_Ctx_1_1, GIOP_Ctx_1_1_Access);

   Permitted_Sync_Scopes : constant PolyORB.Requests.Flags :=
     Sync_None or
     Sync_With_Transport or
     Sync_With_Target;

   --  Msg_Type

   function Unmarshall is new Generic_Unmarshall
     (Msg_Type, Types.Octet, Unmarshall);

   procedure Marshall is new Generic_Marshall
     (Msg_Type, Types.Octet, Marshall);

   --  local helpers

   procedure Marshall_Locate_Request
     (Buffer     :        Buffer_Access;
      Request_Id : in     Types.Unsigned_Long;
      Object_Key : in     PolyORB.Objects.Object_Id_Access);

   procedure Unmarshall_Request_Message
     (Buffer     : access PolyORB.Buffers.Buffer_Type;
      Request_Id :    out Types.Unsigned_Long;
      Resp_Exp   :    out Boolean;
      Object_Key :    out PolyORB.Objects.Object_Id_Access;
      Operation  :    out Types.String;
      Principal  :    out Types.String);

   -----------------------------------
   -- Internal function declaration --
   -----------------------------------

   procedure Process_Request
     (S : access GIOP_Session);

   procedure Process_Locate_Request
     (S : in out Session'Class);

   -----------------------
   -- Initialize_Implem --
   -----------------------

   procedure Initialize_Implem
     (Implem : access GIOP_Implem_1_1)
   is
      use PolyORB.Parameters;
      use PolyORB.Types;

      Max : constant Types.Unsigned_Long
        := Types.Unsigned_Long
        (Get_Conf
         (To_Standard_String (Implem.Section),
          Get_Conf_Chain (Implem)
          & ".max_message_size",
          Default_Max_GIOP_Message_Size_1_1));
   begin
      Implem.Data_Alignment := Data_Alignment_1_1;
      Implem.Max_GIOP_Message_Size := Max - (Max mod 8);
      Implem.Max_Body := Implem.Max_GIOP_Message_Size -
        Types.Unsigned_Long (GIOP_Header_Size);
      Implem.Permitted_Sync_Scopes := Permitted_Sync_Scopes;
   end Initialize_Implem;

   ------------------------
   -- Initialize_Session --
   ------------------------

   procedure Initialize_Session
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      Sess : GIOP_Session renames GIOP_Session (S.all);
   begin
      Sess.Ctx := new GIOP_Ctx_1_1;
      pragma Debug (O ("Initialize context for GIOP session 1.1"));
   end Initialize_Session;

   ----------------------
   -- Finalize_Session --
   ----------------------

   procedure Finalize_Session
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      Sess    : GIOP_Session renames GIOP_Session (S.all);
      Ctx     : GIOP_Ctx_1_1 renames GIOP_Ctx_1_1 (Sess.Ctx.all);
   begin
      if Ctx.Frag_Buf /= null then
         Release (Ctx.Frag_Buf);
      end if;
      Free (GIOP_Ctx_1_1_Access (Sess.Ctx));
      pragma Debug (O ("Finalize context for GIOP session 1.1"));
   end Finalize_Session;

   ---------------------
   -- Process_Message --
   ---------------------

   procedure Process_Message
     (Implem     : access GIOP_Implem_1_1;
      S          : access Session'Class)
   is
      use PolyORB.ORB;
      use PolyORB.Types;

      Sess : GIOP_Session renames GIOP_Session (S.all);
      Ctx  : GIOP_Ctx_1_1 renames GIOP_Ctx_1_1 (Sess.Ctx.all);
   begin
      case Ctx.Message_Type is
         when Request =>
            if Sess.Role /= Server then
               raise GIOP_Error;
            end if;
            Process_Request (Sess'Access);

         when Reply =>
            if Sess.Role /= Client then
               raise GIOP_Error;
            end if;
            Unmarshall_Service_Context_List (Sess.Buffer_In);
            declare
               Request_Id   : constant Types.Unsigned_Long :=
                 Unmarshall (Sess.Buffer_In);
               Reply_Status : constant Reply_Status_Type :=
                 Unmarshall (Sess.Buffer_In);
            begin
               Common_Reply_Received (Sess'Access, Request_Id, Reply_Status);
            end;

         when Close_Connection =>
            if Sess.Role /= Server then
               raise GIOP_Error;
            end if;
            Cancel_Pending_Request (Sess'Access);
            Expect_GIOP_Header (Sess'Access);

         when Fragment =>
            case Ctx.Frag_State is
               when None =>
                  raise GIOP_Error;

               when Fragment =>
                  declare
                     B_Acc : Buffer_Access;
                  begin
                     --  Fragment, read fragment content
                     pragma Debug (O ("Fragment received, size:"
                                   & Ctx.Frag_Size'Img));
                     Ctx.Frag_Size := Ctx.Frag_Size + Ctx.Frag_Next;
                     if not Ctx.Fragmented then
                        pragma Debug (O ("Last fragment, total size:"
                                         & Ctx.Frag_Size'Img));
                        Release (Ctx.Frag_Buf);
                        Ctx.Message_Type := Ctx.Frag_Type;
                        Ctx.Message_Size := Ctx.Frag_Size;
                        Ctx.Frag_State := None;
                        Ctx.Frag_Buf := null;
                        Process_Message (Implem, S);
                     else
                        B_Acc := Sess.Buffer_In;
                        Sess.Buffer_In := Ctx.Frag_Buf;
                        Ctx.Frag_Buf := B_Acc;
                        Expect_GIOP_Header (Sess'Access);
                     end if;
                  end;
            end case;

         when Locate_Reply =>
            if Sess.Role /= Client then
               raise GIOP_Error;
            end if;
            declare
               Request_Id : constant Types.Unsigned_Long
                 := Unmarshall (Sess.Buffer_In);
               Locate_Reply : constant Locate_Reply_Type
                 := Unmarshall (Sess.Buffer_In);
            begin
               --  Exec request if request id is found in pending req list
               Common_Process_Locate_Reply (Sess'Access,
                                            Request_Id,
                                            Locate_Reply);
            end;
         when Locate_Request =>
            if Sess.Role /= Server then
               raise GIOP_Error;
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
      use PolyORB.Annotations;
      use PolyORB.Any.NVList;
      use PolyORB.Binding_Data;
      use PolyORB.Binding_Data.Local;
      use PolyORB.Components;
      use PolyORB.Obj_Adapters;
      use PolyORB.ORB;
      use PolyORB.ORB.Interface;
      use PolyORB.References;
      use PolyORB.Types;

      ORB         : ORB_Access;
      Object_Key  : Objects.Object_Id_Access;
      Request_Id  : Unsigned_Long;
      Operation   : Types.String;
      Principal   : Types.String;
      Resp_Exp    : Boolean;
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
         raise GIOP_Error;
      end if;

      ORB := ORB_Access (S.Server);

      pragma Debug (O ("Request_Received: entering"));

      Unmarshall_Request_Message
        (S.Buffer_In,
         Request_Id,
         Resp_Exp,
         Object_Key,
         Operation,
         Principal);

      if Resp_Exp then
         Req_Flags := Sync_With_Target;
      else
         Req_Flags := Sync_None;
      end if;

      pragma Debug (O ("Object Key : "
                       & To_String (Object_Key.all)));

      Args := Get_Empty_Arg_List
        (Object_Adapter (ORB),
         Object_Key,
         To_Standard_String (Operation));

      if not Is_Nil (Args) then
         pragma Debug (O ("Immediate arguments unmarshalling"));
         Handle_Unmarshall_Arguments
           (S, Args);

      else
         pragma Debug (O ("Unmarshalling of arguments deferred"));
         S.State := Waiting_Unmarshalling;
         Def_Args := Component_Access (S);
      end if;

      declare
         Target_Profile : constant Binding_Data.Profile_Access
           := new Local_Profile_Type;
      begin
         Create_Local_Profile
           (Object_Key.all,
            Local_Profile_Type (Target_Profile.all));

         Create_Reference ((1 => Target_Profile), "", Target);
         --  Create a temporary, typeless reference for this object.
         --  If we wanted to have proper type information, we would
         --  have to resolve the (local) object id through the object
         --  adapter, and query the target object for its most derived
         --  type.
      end;

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

      Set_Note
        (Req.Notepad,
         Request_Note'(Annotations.Note with Id => Request_Id));

      Queue_Request_To_Handler
        (ORB.Tasking_Policy, ORB,
         Queue_Request'
         (Request => Req,
          Requestor => Component_Access (S)));

      Free (Object_Key);
      pragma Debug (O ("Request queued."));
   end Process_Request;

   -------------------
   -- Process_Reply --
   -------------------

   procedure Process_Reply
     (Implem  : access GIOP_Implem_1_1;
      S       : access Session'Class;
      Request :        Requests.Request_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.ORB;

      Sess : GIOP_Session renames GIOP_Session (S.all);
      Ctx  : GIOP_Ctx_1_1 renames GIOP_Ctx_1_1 (Sess.Ctx.all);
   begin
      if Sess.Role = Client then
         raise GIOP_Error;
      end if;

      Ctx.Message_Type := Reply;
      Common_Process_Reply
        (Sess'Access, Request, Ctx.Request_Id'Access, Ctx.Reply_Status'Access);
   end Process_Reply;

   ----------------------------
   -- Process_Locate_Request --
   ----------------------------

   procedure Process_Locate_Request
     (S : in out Session'Class)
   is
      Sess    : GIOP_Session renames GIOP_Session (S);
      Ctx     : GIOP_Ctx_1_1 renames GIOP_Ctx_1_1 (Sess.Ctx.all);
      Buffer  : Buffer_Access renames Sess.Buffer_In;

      Request_Id   : constant Types.Unsigned_Long := Unmarshall (Buffer);
      pragma Warnings (Off);
      Obj          : constant Stream_Element_Array :=  Unmarshall (Buffer);
      pragma Warnings (On);
      Result       : Locate_Reply_Type;
   begin
      Result := Object_Here;

      --  XXX need to be implemented

      Ctx.Message_Type := Locate_Reply;
      Common_Locate_Reply (Sess'Access, Request_Id, Result);
      Expect_GIOP_Header (Sess'Access);
   end Process_Locate_Request;

   -------------------
   -- Locate Object --
   -------------------

   procedure Locate_Object
     (Implem : access GIOP_Implem_1_1;
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
      Ctx           : GIOP_Ctx_1_1 renames GIOP_Ctx_1_1 (Sess.Ctx.all);
      Buffer        : Buffer_Access;
      Header_Buffer : Buffer_Access;
      Header_Space  : Reservation;
   begin
      if Sess.Role /= Client then
         raise GIOP_Error;
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
         Get_Object_Key (R.Target_Profile.all));

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
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class;
      R      : in     Pending_Request_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.Requests.Unsigned_Long_Flags;
      use PolyORB.Types;

      Sess          : GIOP_Session renames GIOP_Session (S.all);
      Ctx           : GIOP_Ctx_1_1 renames GIOP_Ctx_1_1 (Sess.Ctx.all);
      Buffer        : Buffer_Access;
      Header_Buffer : Buffer_Access;
      Header_Space  : Reservation;
      Resp_Exp      : constant Boolean :=
        Is_Set (Sync_With_Target, R.Req.Req_Flags)
        or Is_Set (Sync_Call_Back, R.Req.Req_Flags);
      Oid           : constant Object_Id_Access :=
        Binding_Data.Get_Object_Key (R.Target_Profile.all);
      Sink          : constant Types.Octet := 0;
   begin
      pragma Debug (O ("Sending request , Id :" & R.Request_Id'Img));

      Buffer := new Buffer_Type;
      Header_Buffer := new Buffer_Type;
      Header_Space := Reserve (Buffer, GIOP_Header_Size);
      Marshall_Service_Context_List (Buffer);
      Marshall (Buffer, R.Request_Id);
      Marshall (Buffer, Resp_Exp);
      for J in 1 .. 3 loop
         Marshall (Buffer, Sink);
      end loop;
      Marshall
        (Buffer,
         Stream_Element_Array
         (Oid.all));

      pragma Debug (O ("Operation : "
                       & To_Standard_String (R.Req.Operation)));

      Marshall (Buffer, R.Req.Operation);
      Marshall (Buffer, Nobody_Principal);

      Marshall_Argument_List
        (Sess.Implem, Buffer, R.Req.Args, PolyORB.Any.ARG_IN,
         Sess.Implem.Data_Alignment);

      Ctx.Fragmented := False;
      Ctx.Message_Type := Request;
      Ctx.Message_Size := Types.Unsigned_Long (Length (Buffer)) -
        Types.Unsigned_Long (GIOP_Header_Size);

      Marshall_Global_GIOP_Header (Sess'Access, Header_Buffer);
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
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class;
      R      : in     Request_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.ORB;

      Sess : GIOP_Session renames GIOP_Session (S.all);
      Ctx  : GIOP_Ctx_1_1 renames GIOP_Ctx_1_1 (Sess.Ctx.all);
   begin
      if Sess.Role = Server then
         raise GIOP_Error;
      end if;

      Ctx.Message_Type := Cancel_Request;
      Common_Process_Abort_Request (Sess'Access, R);
   end Process_Abort_Request;

   ----------------------------
   -- Marshall_Argument_List --
   ----------------------------

   procedure Marshall_Argument_List
     (Implem              : access GIOP_Implem_1_1;
      Buffer              :         PolyORB.Buffers.Buffer_Access;
      Args                : in out Any.NVList.Ref;
      Direction           :        Any.Flags;
      First_Arg_Alignment :        Buffers.Alignment_Type)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Types;

      It           : Iterator := First (List_Of (Args).all);
      Arg          : Element_Access;
      Message_Size : Types.Unsigned_Long;
   begin
      pragma Assert (Direction = ARG_IN or else Direction = ARG_OUT);

      while not Last (It) loop
         Arg := Value (It);
         if False
           or else Arg.Arg_Modes = Direction
           or else Arg.Arg_Modes = ARG_INOUT
         then
            pragma Debug (O ("Marshalling argument "
                             & Types.To_Standard_String (Arg.Name)
                               & " = " & Image (Arg.Argument)));
            if First (It) then
               Pad_Align (Buffer, First_Arg_Alignment);
            end if;
            Marshall (Buffer, Arg.all);
         end if;
         Next (It);
      end loop;
      Message_Size := Types.Unsigned_Long (Length (Buffer));
      if Message_Size > Implem.Max_GIOP_Message_Size then
         pragma Debug (O ("Fragment_Mode"));
         raise Not_Implemented;
      end if;
   end Marshall_Argument_List;

   ---------------------------------
   -- Unmarshalling / Marshalling --
   ---------------------------------

   ----------------------------
   -- Unmarshall_GIOP_Header --
   ----------------------------

   procedure Unmarshall_GIOP_Header
     (Implem  : access GIOP_Implem_1_1;
      S       : access Session'Class)
   is
      use Octet_Flags;
      use PolyORB.Types;

      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      Sess    : GIOP_Session renames GIOP_Session (S.all);
      Ctx     : GIOP_Ctx_1_1 renames GIOP_Ctx_1_1 (Sess.Ctx.all);
      Buffer  : Buffer_Access renames Sess.Buffer_In;
      Flags   : Types.Octet;
   begin
      Flags := Unmarshall (Buffer);

      if Is_Set (Bit_Little_Endian, Flags) then
         Ctx.Message_Endianness := Little_Endian;
      else
         Ctx.Message_Endianness := Big_Endian;
      end if;
      pragma Assert (Ctx.Message_Endianness = Endianness (Buffer.all));

      pragma Debug (O ("Message Endianness : "
                       & Ctx.Message_Endianness'Img));

      if Is_Set (Bit_Fragment, Flags) then
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
         declare
            B_Acc : Buffer_Access;
         begin
            B_Acc := Sess.Buffer_In;
            Sess.Buffer_In := Ctx.Frag_Buf;
            Ctx.Frag_Buf := B_Acc;
         end;
      end if;

      if Ctx.Fragmented and Ctx.Frag_State = None then
         Ctx.Frag_Buf := new Buffer_Type;
         Ctx.Frag_Size := 0;
         Ctx.Frag_State := Fragment;
         Ctx.Frag_Type := Ctx.Message_Type;
         Ctx.Message_Type := Fragment;
         pragma Debug (O ("Enter fragment mode"));
      end if;

   end Unmarshall_GIOP_Header;

   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------

   procedure Marshall_GIOP_Header
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class;
      Buffer : access PolyORB.Buffers.Buffer_Type)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use Octet_Flags;

      Sess : GIOP_Session renames GIOP_Session (S.all);
      Ctx  : GIOP_Ctx_1_1 renames GIOP_Ctx_1_1 (Sess.Ctx.all);
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
     (Buffer            : access PolyORB.Buffers.Buffer_Type;
      Request_Id        :    out Types.Unsigned_Long;
      Resp_Exp          :    out Types.Boolean;
      Object_Key        :    out PolyORB.Objects.Object_Id_Access;
      Operation         :    out Types.String;
      Principal         :    out Types.String)
   is
      use Representations.CDR;
      use PolyORB.Types;

      Sink : Types.Octet;
   begin
      --  Service context
      Unmarshall_Service_Context_List (Buffer);

      --  Request id
      Request_Id := Unmarshall (Buffer);

      pragma Debug (O ("Request_Id :"
                       & Request_Id'Img));

      --  Response flags
      Resp_Exp := Unmarshall (Buffer);

      --  Reserved
      for I in 1 .. 3 loop
         Sink :=  Unmarshall (Buffer);
         if Sink /= 0 then
            raise GIOP_Error;
         end if;
      end loop;

      declare
         Obj : constant Stream_Element_Array :=  Unmarshall (Buffer);
      begin
         Object_Key := new Object_Id'(Object_Id (Obj));
      end;

      --  Operation
      Operation :=  Unmarshall (Buffer);
      pragma Debug (O ("Operation  : "
                       & Types.To_Standard_String (Operation)));

      Principal := Unmarshall (Buffer);

   end Unmarshall_Request_Message;

   --------------------------------
   -- Marshall_GIOP_Header_Reply --
   --------------------------------

   procedure Marshall_GIOP_Header_Reply
     (Implem  : access GIOP_Implem_1_1;
      S       : access Session'Class;
      Buffer  : access PolyORB.Buffers.Buffer_Type)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      Sess    : GIOP_Session renames GIOP_Session (S.all);
      Ctx     : GIOP_Ctx_1_1 renames GIOP_Ctx_1_1 (Sess.Ctx.all);
   begin
      Marshall_Service_Context_List (Buffer);
      Marshall (Buffer, Ctx.Request_Id);
      Marshall (Buffer, Ctx.Reply_Status);
   end Marshall_GIOP_Header_Reply;

   -----------------------------
   -- Marshall_Locate_Request --
   -----------------------------

   procedure Marshall_Locate_Request
     (Buffer     :        Buffer_Access;
      Request_Id : in     Types.Unsigned_Long;
      Object_Key : in     PolyORB.Objects.Object_Id_Access)
   is
   begin
      Marshall (Buffer, Request_Id);
      Marshall (Buffer, Stream_Element_Array (Object_Key.all));
   end Marshall_Locate_Request;

   ----------------
   -- New_Implem --
   ----------------

   function New_Implem return GIOP_Implem_Access;

   function New_Implem return GIOP_Implem_Access is
   begin
      return new GIOP_Implem_1_1;
   end New_Implem;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Global_Register_GIOP_Version
        (GIOP_Version'(Major => 1, Minor => 1), New_Implem'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"protocols.giop.giop_1_1",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.Protocols.GIOP.GIOP_1_1;
