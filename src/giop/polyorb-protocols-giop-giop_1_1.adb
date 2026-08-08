------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . P R O T O C O L S . G I O P . G I O P _ 1 _ 1       --
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

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Binding_Data.Local;
with PolyORB.Buffers;
with PolyORB.GIOP_P.Service_Contexts;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Objects;
with PolyORB.Obj_Adapters;
with PolyORB.Protocols.GIOP.Common;
pragma Elaborate_All (PolyORB.Protocols.GIOP.Common);
with PolyORB.QoS.Service_Contexts;
with PolyORB.References;
with PolyORB.Representations.CDR.Common;
with PolyORB.Representations.CDR.GIOP_1_1;
with PolyORB.Representations.CDR.GIOP_Utils;
with PolyORB.Request_QoS;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package body PolyORB.Protocols.GIOP.GIOP_1_1 is

   use PolyORB.Buffers;
   use PolyORB.GIOP_P.Service_Contexts;
   use PolyORB.Log;
   use PolyORB.Objects;
   use PolyORB.Protocols.GIOP.Common;
   use PolyORB.QoS;
   use PolyORB.QoS.Service_Contexts;
   use PolyORB.Representations.CDR;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Representations.CDR.GIOP_1_1;
   use PolyORB.Representations.CDR.GIOP_Utils;
   use PolyORB.Request_QoS;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.protocols.giop.giop_1_1");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   procedure Free is
      new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => GIOP_1_1_CDR_Representation,


      Name   => GIOP_1_1_CDR_Representation_Access);

   Permitted_Sync_Scopes : constant PolyORB.Requests.Flags :=
     Sync_None or Sync_With_Transport or Sync_With_Target;

   --  Msg_Type

   function Unmarshall is new Generic_Unmarshall
     (Msg_Type, Types.Octet, Unmarshall);

   procedure Marshall is new Generic_Marshall
     (Msg_Type, Types.Octet, Marshall);

   --  local helpers

   procedure Marshall_Locate_Request
     (Buffer     :        Buffer_Access;
      Request_Id : Types.Unsigned_Long;
      Object_Key : PolyORB.Objects.Object_Id_Access);

   procedure Unmarshall_Request_Message
     (Buffer           : access Buffers.Buffer_Type;
      Request_Id       :    out Types.Unsigned_Long;
      Resp_Exp         :    out Boolean;
      Object_Key       :    out PolyORB.Objects.Object_Id_Access;
      Operation        :    out Types.String;
      Principal        :    out Types.String;
      Service_Contexts :    out QoS_GIOP_Service_Contexts_Parameter_Access);

   -----------------------------------
   -- Internal function declaration --
   -----------------------------------

   procedure Process_Request (S : in out GIOP_Session);

   procedure Process_Locate_Request (S : in out Session'Class);

   -----------------------
   -- Initialize_Implem --
   -----------------------

   overriding procedure Initialize_Implem (Implem : access GIOP_Implem_1_1) is
   begin
      Implem.Data_Alignment        := Data_Alignment_1_1;
      Implem.Permitted_Sync_Scopes := Permitted_Sync_Scopes;
   end Initialize_Implem;

   ------------------------
   -- Initialize_Session --
   ------------------------

   overriding procedure Initialize_Session
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      Sess : GIOP_Session renames GIOP_Session (S.all);
   begin
      Sess.MCtx := new GIOP_Message_Context_1_1;
      --  Sess.SCtx := new GIOP_Session_Context_1_1;
      --  There is no SCtx for GIOP 1.1

      Sess.Repr := new GIOP_1_1_CDR_Representation;
      pragma Debug (C, O ("Initialize context for GIOP session 1.1"));
   end Initialize_Session;

   ----------------------
   -- Finalize_Session --
   ----------------------

   overriding procedure Finalize_Session
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      Sess    : GIOP_Session renames GIOP_Session (S.all);
   begin
      Free (Sess.MCtx);
      --  Free (Sess.SCtx);
      --  There is no SCtx for GIOP 1.1

      Release (GIOP_1_1_CDR_Representation (Sess.Repr.all));
      Free (GIOP_1_1_CDR_Representation_Access (Sess.Repr));
      pragma Debug (C, O ("Finalize context for GIOP session 1.1"));
   end Finalize_Session;

   ---------------------
   -- Process_Message --
   ---------------------

   overriding procedure Process_Message
     (Implem     : access GIOP_Implem_1_1;
      S          : access Session'Class)
   is
      pragma Unreferenced (Implem);
      use PolyORB.ORB;

      Sess : GIOP_Session renames GIOP_Session (S.all);
      MCtx : GIOP_Message_Context_1_1
               renames GIOP_Message_Context_1_1 (Sess.MCtx.all);
   begin
      case MCtx.Message_Type is
         when Request =>
            if Sess.Role /= Server then
               raise GIOP_Error;
            end if;
            Process_Request (Sess);

         when Cancel_Request =>
            if Sess.Role /= Server then
               raise GIOP_Error;
            end if;
            Common_Process_Cancel_Request
              (Sess'Access, Request_Id => Unmarshall (Sess.Buffer_In));

         when Reply =>
            if Sess.Role /= Client then
               raise GIOP_Error;
            end if;

            declare
               Request_Id : Types.Unsigned_Long;
               Reply_Status : Reply_Status_Type;
               Service_Contexts : QoS_GIOP_Service_Contexts_Parameter_Access;

            begin
               Unmarshall_Service_Context_List
                 (Sess.Buffer_In, Service_Contexts);

               Request_Id := Unmarshall (Sess.Buffer_In);
               Reply_Status := Unmarshall (Sess.Buffer_In);

               Common_Reply_Received
                 (Sess'Access, Request_Id, Reply_Status, Service_Contexts);
            end;

         when Close_Connection =>
            if Sess.Role /= Server then
               raise GIOP_Error;
            end if;
            Expect_GIOP_Header (Sess'Access);

         when Fragment =>
            O ("GIOP 1.1 fragment discarded.", Warning);

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

      end case;
   end Process_Message;

   ---------------------
   -- Process_Request --
   ---------------------

   procedure Process_Request
     (S : in out GIOP_Session)
   is
      use PolyORB.Any.NVList;
      use PolyORB.Binding_Data;
      use PolyORB.Binding_Data.Local;
      use PolyORB.Components;
      use PolyORB.Errors;
      use PolyORB.Obj_Adapters;
      use PolyORB.ORB;
      use PolyORB.References;

      Object_Key       : Objects.Object_Id_Access;
      Request_Id       : Unsigned_Long;
      Operation        : Types.String;
      Principal        : Types.String;
      Resp_Exp         : Boolean;
      Req_Flags        : Flags := 0;
      Args             : Any.NVList.Ref;
      Def_Args         : Component_Access;
      Target           : References.Ref;
      Req              : Request_Access;
      Error            : Errors.Error_Container;
      Service_Contexts : QoS_GIOP_Service_Contexts_Parameter_Access;
      Result           : Any.NamedValue;
      --  Dummy NamedValue for Create_Request;
      --  the actual Result is set by the called method.
   begin
      if S.Role /= Server then
         raise GIOP_Error;
      end if;

      pragma Debug (C, O ("Request_Received: entering"));

      Unmarshall_Request_Message
        (S.Buffer_In,
         Request_Id,
         Resp_Exp,
         Object_Key,
         Operation,
         Principal,
         Service_Contexts);

      if Resp_Exp then
         Req_Flags := Sync_With_Target;
      else
         Req_Flags := Sync_With_Transport;
      end if;

      pragma Debug (C, O ("Object Key : "
                       & Oid_To_Hex_String (Object_Key.all)));

      Args := Get_Empty_Arg_List
        (Object_Adapter (ORB_Access (S.Server)),
         Object_Key,
         To_Standard_String (Operation));

      if not Is_Nil (Args) then
         pragma Debug (C, O ("Immediate arguments unmarshalling"));
         Handle_Unmarshall_Arguments (S'Unchecked_Access, Args, Error);

         if Found (Error) then
            Catch (Error);
            raise Program_Error;
            --  XXX We cannot silently ignore any error. For now,
            --  we raise this exception. To be investigated.
         end if;

      else
         pragma Debug (C, O ("Unmarshalling of arguments deferred"));
         S.State := Waiting_Unmarshalling;
         Def_Args := S'Unchecked_Access;
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
         Dependent_Binding_Object   =>
           Smart_Pointers.Entity_Ptr
             (S.Dependent_Binding_Object));

      Add_Request_QoS
        (Req.all,
         GIOP_Service_Contexts,
         QoS_Parameter_Access (Service_Contexts));
      Rebuild_Request_QoS_Parameters (Req.all);

      Queue_Request (S'Unchecked_Access, Req, Request_Id);
      Free (Object_Key);
      pragma Debug (C, O ("Request queued."));
   end Process_Request;

   ----------------
   -- Send_Reply --
   ----------------

   overriding procedure Send_Reply
     (Implem  : access GIOP_Implem_1_1;
      S       : access Session'Class;
      Request :        Requests.Request_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.ORB;
      use PolyORB.Errors;

      Sess  : GIOP_Session renames GIOP_Session (S.all);
      MCtx  : aliased GIOP_Message_Context_1_1;
      Error : Errors.Error_Container;
   begin
      if Sess.Role = Client then
         raise GIOP_Error;
      end if;

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
            raise GIOP_Error;
         end if;
      end if;
   end Send_Reply;

   ----------------------------
   -- Process_Locate_Request --
   ----------------------------

   procedure Process_Locate_Request
     (S : in out Session'Class)
   is
      use PolyORB.Errors;

      Sess    : GIOP_Session renames GIOP_Session (S);
      Buffer  : Buffer_Access renames Sess.Buffer_In;

      Request_Id   : constant Types.Unsigned_Long := Unmarshall (Buffer);
      pragma Warnings (Off);
      Obj          : constant Stream_Element_Array :=  Unmarshall (Buffer);
      pragma Warnings (On);
      Target       : References.Ref;
      Result       : Locate_Reply_Type;
      Error        : Errors.Error_Container;
      MCtx         : aliased GIOP_Message_Context_1_1;
   begin
      Result := Object_Here;

      --  XXX need to be implemented

      MCtx.Message_Type := Locate_Reply;
      MCtx.Request_Id   := Request_Id;
      Common_Locate_Reply (Sess'Access, MCtx'Access, Result, Target, Error);
      if Found (Error) then
         Catch (Error);
         raise GIOP_Error;
      end if;
      Expect_GIOP_Header (Sess'Access);
   end Process_Locate_Request;

   -------------------
   -- Locate Object --
   -------------------

   overriding procedure Locate_Object
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class;
      R      :        Pending_Request_Access;
      Error  : in out Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.ORB;
      use PolyORB.Binding_Data;

      Sess          : GIOP_Session renames GIOP_Session (S.all);
      MCtx          : aliased GIOP_Message_Context_1_1;
      Buffer        : Buffer_Access;
      Header_Buffer : Buffer_Access;
      Header_Space  : Reservation;
   begin
      if Sess.Role /= Client then
         raise GIOP_Error;
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
         Get_Object_Key (R.Target_Profile.all));

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
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class;
      R      : Pending_Request_Access;
      Error  : in out Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.Errors;
      use PolyORB.Requests.Unsigned_Long_Flags;

      Sess          : GIOP_Session renames GIOP_Session (S.all);
      MCtx          : aliased GIOP_Message_Context_1_1;
      Buffer        : Buffer_Access;
      Header_Buffer : Buffer_Access;
      Header_Space  : Reservation;
      Resp_Exp      : constant Boolean :=
        Is_Set (Sync_With_Target, R.Req.Req_Flags)
          or else Is_Set (Sync_Call_Back,   R.Req.Req_Flags);
      Oid           : constant Object_Id_Access :=
        Binding_Data.Get_Object_Key (R.Target_Profile.all);
   begin
      pragma Debug (C, O ("Sending request, Id :" & R.Request_Id'Img));

      Buffer := new Buffer_Type;
      Header_Buffer := new Buffer_Type;
      Header_Space := Reserve (Buffer, GIOP_Header_Size);

      Rebuild_Request_Service_Contexts (R.Req.all);
      Marshall_Service_Context_List
        (Buffer,
         QoS_GIOP_Service_Contexts_Parameter_Access
           (Extract_Request_Parameter (GIOP_Service_Contexts, R.Req.all)));
      Marshall (Buffer, R.Request_Id);
      Marshall (Buffer, Resp_Exp);
      for J in 1 .. 3 loop
         Marshall (Buffer, Types.Octet'(0));
      end loop;
      Marshall
        (Buffer,
         Stream_Element_Array
         (Oid.all));

      pragma Debug (C, O ("Operation : " & R.Req.Operation.all));

      Marshall_Latin_1_String (Buffer, R.Req.Operation.all);
      Marshall_Latin_1_String (Buffer, Nobody_Principal);

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

      MCtx.Message_Type := Request;
      MCtx.Message_Size :=
        Types.Unsigned_Long (Length (Buffer.all) - GIOP_Header_Size);

      Marshall_Global_GIOP_Header (Sess'Access, MCtx'Access, Header_Buffer);
      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
      Emit_Message (Sess.Implem, Sess'Access, MCtx'Access, Buffer, Error);
      pragma Debug (C, O ("Request sent, Id :" & R.Request_Id'Img
                       & ", size:" & MCtx.Message_Size'Img));

      Release (Buffer);
   end Send_Request;

   -------------------------
   -- Send_Cancel_Request --
   -------------------------

   overriding procedure Send_Cancel_Request
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class;
      R      : Request_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.Errors;
      use PolyORB.ORB;

      Sess  : GIOP_Session renames GIOP_Session (S.all);
      MCtx  : aliased GIOP_Message_Context_1_1;
      Error : Errors.Error_Container;
   begin
      if Sess.Role = Server then
         raise GIOP_Error;
      end if;

      MCtx.Message_Type := Cancel_Request;
      Common_Send_Cancel_Request (Sess'Access, R, MCtx'Access, Error);
      if Found (Error) then
         Catch (Error);
         raise GIOP_Error;
      end if;
   end Send_Cancel_Request;

   ----------------------------
   -- Marshall_Argument_List --
   ----------------------------

   overriding procedure Marshall_Argument_List
     (Implem              : access GIOP_Implem_1_1;
      Buffer              :        Buffers.Buffer_Access;
      Representation      : access CDR_Representation'Class;
      Args                : in out Any.NVList.Ref;
      Direction           :        Any.Flags;
      First_Arg_Alignment :        Buffers.Alignment_Type;
      Error               : in out Errors.Error_Container)
   is
      pragma Unreferenced (Implem);
      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Errors;

      It           : Iterator := First (List_Of (Args).all);
      First        : Boolean  := True;
      Arg          : Element_Access;
   begin
      pragma Assert (Direction = ARG_IN or else Direction = ARG_OUT);

      while not Last (It) loop
         Arg := Value (It);
         if False
           or else Arg.Arg_Modes = Direction
           or else Arg.Arg_Modes = ARG_INOUT
         then
            pragma Debug (C, O ("Marshalling argument "
                             & Types.To_Standard_String (Arg.Name)
                               & " = " & Image (Arg.Argument)));
            if First then
               Pad_Align (Buffer, First_Arg_Alignment);
               First := False;
            end if;

            Marshall (Buffer, Representation, Arg.all, Error);

            if Found (Error) then
               return;
            end if;
         end if;
         Next (It);
      end loop;
   end Marshall_Argument_List;

   ---------------------------------
   -- Unmarshalling / Marshalling --
   ---------------------------------

   ----------------------------
   -- Unmarshall_GIOP_Header --
   ----------------------------

   overriding procedure Unmarshall_GIOP_Header
     (Implem  : access GIOP_Implem_1_1;
      MCtx    : access GIOP_Message_Context'Class;
      Buffer  : access Buffers.Buffer_Type)
   is
      use Octet_Flags;

      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      MCtx_1_1 : GIOP_Message_Context_1_1
                   renames GIOP_Message_Context_1_1 (MCtx.all);
      Flags   : Types.Octet;
   begin
      Flags := Unmarshall (Buffer);

      if Is_Set (Bit_Little_Endian, Flags) then
         MCtx_1_1.Message_Endianness := Little_Endian;
      else
         MCtx_1_1.Message_Endianness := Big_Endian;
      end if;
      pragma Assert (MCtx_1_1.Message_Endianness = Endianness (Buffer));

      pragma Debug (C, O ("Message Endianness : "
                       & MCtx_1_1.Message_Endianness'Img));

      if Is_Set (Bit_Fragment, Flags) then
         O ("GIOP 1.1 fragmented message discarded", Warning);
      end if;

      --  Extract type

      MCtx_1_1.Message_Type := Unmarshall (Buffer);
      pragma Debug (C, O ("Message Type       : "
                       & MCtx_1_1.Message_Type'Img));

      --  Extract size

      MCtx_1_1.Message_Size := Unmarshall (Buffer);
      pragma Debug (C, O ("Message Size       :"
                       & MCtx_1_1.Message_Size'Img));
   end Unmarshall_GIOP_Header;

   --------------------------
   -- Marshall_GIOP_Header --
   --------------------------

   overriding procedure Marshall_GIOP_Header
     (Implem : access GIOP_Implem_1_1;
      S      : access Session'Class;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer : access Buffers.Buffer_Type)
   is
      pragma Unreferenced (Implem, S);

      use Octet_Flags;

      MCtx_1_1 : GIOP_Message_Context_1_1
                   renames GIOP_Message_Context_1_1 (MCtx.all);
      Flags : Types.Octet := 0;
   begin
      Set (Flags, Bit_Little_Endian, Endianness (Buffer) = Little_Endian);
      --  Set (Flags, Bit_Fragment, False);

      Marshall (Buffer, Flags);
      Marshall (Buffer, MCtx_1_1.Message_Type);
      Marshall (Buffer, MCtx_1_1.Message_Size);
   end Marshall_GIOP_Header;

   --------------------------------
   -- Unmarshall_Request_Message --
   --------------------------------

   procedure Unmarshall_Request_Message
     (Buffer           : access Buffers.Buffer_Type;
      Request_Id       :    out Types.Unsigned_Long;
      Resp_Exp         :    out Types.Boolean;
      Object_Key       :    out PolyORB.Objects.Object_Id_Access;
      Operation        :    out Types.String;
      Principal        :    out Types.String;
      Service_Contexts :    out QoS_GIOP_Service_Contexts_Parameter_Access)
   is
      Sink : Types.Octet;
   begin
      --  Service context

      Unmarshall_Service_Context_List (Buffer, Service_Contexts);

      --  Request id
      Request_Id := Unmarshall (Buffer);

      pragma Debug (C, O ("Request_Id :"
                       & Request_Id'Img));

      --  Response flags
      Resp_Exp := Unmarshall (Buffer);

      --  Reserved
      for I in 1 .. 3 loop
         Sink :=  Unmarshall (Buffer);
         pragma Debug (C and then Sink /= 0,
           O ("reserved byte in GIOP 1.1 header is non-zero"));
      end loop;

      declare
         Obj : constant Stream_Element_Array := Unmarshall (Buffer);
      begin
         Object_Key := new Object_Id'(Object_Id (Obj));
      end;

      --  Operation
      Operation := Types.String (Types.Identifier'(Unmarshall (Buffer)));
      pragma Debug (C, O ("Operation  : "
                       & Types.To_Standard_String (Operation)));

      Principal := Unmarshall_Latin_1_String (Buffer);

   end Unmarshall_Request_Message;

   --------------------------------
   -- Marshall_GIOP_Header_Reply --
   --------------------------------

   overriding procedure Marshall_GIOP_Header_Reply
     (Implem  : access GIOP_Implem_1_1;
      S       : access Session'Class;
      R       : Request_Access;
      MCtx    : access GIOP_Message_Context'Class;
      Buffer  : access Buffers.Buffer_Type)
   is
      pragma Unreferenced (Implem, S);

      MCtx_1_1 : GIOP_Message_Context_1_1
                   renames GIOP_Message_Context_1_1 (MCtx.all);
   begin
      Rebuild_Reply_Service_Contexts (R.all);
      Marshall_Service_Context_List
        (Buffer,
         QoS_GIOP_Service_Contexts_Parameter_Access
           (Extract_Reply_Parameter (GIOP_Service_Contexts, R.all)));
      Marshall (Buffer, MCtx_1_1.Request_Id);
      Marshall (Buffer, MCtx_1_1.Reply_Status);
   end Marshall_GIOP_Header_Reply;

   -----------------------------
   -- Marshall_Locate_Request --
   -----------------------------

   procedure Marshall_Locate_Request
     (Buffer     :        Buffer_Access;
      Request_Id : Types.Unsigned_Long;
      Object_Key : PolyORB.Objects.Object_Id_Access)
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
      Global_Register_GIOP_Version (GIOP_V1_1, New_Implem'Access);
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
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Protocols.GIOP.GIOP_1_1;
