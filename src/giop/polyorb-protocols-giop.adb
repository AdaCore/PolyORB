------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . P R O T O C O L S . G I O P                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with PolyORB.Annotations;
with PolyORB.Binding_Data.GIOP;
with PolyORB.Components;
with PolyORB.Errors.Helper;
with PolyORB.Protocols.GIOP.Common;
with PolyORB.GIOP_P.Exceptions;
with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.References.Binding;
with PolyORB.Representations.CDR.Common;
with PolyORB.Representations.CDR.GIOP_Utils;
with PolyORB.Servants.Iface;

package body PolyORB.Protocols.GIOP is

   use PolyORB.Annotations;
   use PolyORB.Buffers;
   use PolyORB.Components;
   use PolyORB.Protocols.GIOP.Common;
   use PolyORB.Log;
   use PolyORB.ORB;
   use PolyORB.Representations.CDR;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Representations.CDR.GIOP_Utils;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.giop");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   GIOP_Factories : array (GIOP_Version) of GIOP_Factory;
   --  It is assumed this array is written once at initialization
   --  time, read many during partition lifetime, no mutex is required.

   ------------
   -- Create --
   ------------

   procedure Create
     (Proto   : access GIOP_Protocol;
      Session :    out Filter_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Proto);
      pragma Warnings (On);
   begin
      Session := new GIOP_Session;
      pragma Debug (C, O ("Create GIOP Session"));
      Initialize (GIOP_Session (Session.all));
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Conf                  : access GIOP_Conf;
      Version               : GIOP_Version;
      Permitted_Sync_Scopes : PolyORB.Requests.Flags;
      Locate_Then_Request   : Boolean;
      Section               : String;
      Prefix                : String)
   is
      use PolyORB.Parameters;
      use PolyORB.Utils;

   begin
      pragma Debug (C, O ("Initialize parameters for GIOP Protocol"));
      pragma Debug (C, O ("Conf Section : " & Section));
      pragma Debug (C, O ("Conf Prefix : " & Prefix));

      pragma Debug (C, O ("Permitted sync scope" & Permitted_Sync_Scopes'Img));
      Conf.Permitted_Sync_Scopes := Permitted_Sync_Scopes;

      --  ??? The following assumes that the GIOP major version is always 1

      Conf.GIOP_Default_Version := To_GIOP_Version
        (Get_Conf
         (Section,
          Prefix & ".default_version.minor",
          Integer (To_Minor_GIOP (Version))));

      for J in GIOP_Version'Range loop
         if Get_Conf
           (Section,
            Prefix & ".1." &
            Trimmed_Image (Unsigned_Long_Long (To_Minor_GIOP (J)))
            & ".enable",
            True)
           and then GIOP_Factories (J) /= null
         then
            pragma Debug (C, O ("Enable GIOP Version : 1."
                             & Trimmed_Image (Unsigned_Long_Long
                                              (To_Minor_GIOP (J)))));

            Conf.GIOP_Implems (J) := GIOP_Factories (J).all;
            Conf.GIOP_Implems (J).Version := J;
            Conf.GIOP_Implems (J).Section := To_PolyORB_String (Section);
            Conf.GIOP_Implems (J).Prefix := To_PolyORB_String (Prefix);

            Initialize_Implem (Conf.GIOP_Implems (J));

            Conf.GIOP_Implems (J).Locate_Then_Request :=
              Get_Conf (Section,
                        Get_Conf_Chain (Conf.GIOP_Implems (J))
                        & ".locate_then_request",
                        Locate_Then_Request);
         end if;
      end loop;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (S : in out GIOP_Session) is
   begin
      pragma Debug (C, O ("Initializing GIOP session"));
      Tasking.Mutexes.Create (S.Mutex);
      S.Buffer_In  := new Buffer_Type;
   end Initialize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (S : in out GIOP_Session) is
   begin
      pragma Debug (C, O ("Destroying GIOP session"));
      pragma Assert (S.State = Not_Initialized);

      --  We assume that this session has already been disconnected.
      --  All pending requests have been flushed, and its state has been
      --  reset to Not_Initialized.

      Pend_Req_Tables.Deallocate (S.Pending_Reqs);
      Destroy (S.Mutex);

      if S.Buffer_In /= null then
         Release (S.Buffer_In);
      end if;

      if S.Implem /= null then
         Finalize_Session (S.Implem, S'Access);
      end if;

      Protocols.Destroy (Protocols.Session (S));
   end Destroy;

   ----------------------------
   -- Handle_Data_Indication --
   ----------------------------

   procedure Handle_Data_Indication
     (Sess        : access GIOP_Session;
      Data_Amount : Stream_Element_Count;
      Error       : in out Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Data_Amount);
      pragma Warnings (On);

      use Errors;

      Version : GIOP_Version;
   begin
      pragma Debug (C, O ("Received data in state " & Sess.State'Img));

      pragma Assert (Sess.State /= Not_Initialized);

      case Sess.State is

         when Expect_Header =>

            Unmarshall_Global_GIOP_Header (Sess, Sess.Buffer_In, Version);
            Unmarshall_GIOP_Header (Sess.Implem, Sess.MCtx, Sess.Buffer_In);

            Sess.State := Expect_Body;

            pragma Debug (C, O ("GIOP Header OK, ask for body, size :"
                             & Sess.MCtx.Message_Size'Img));

            if Sess.MCtx.Message_Size = 0 then
               Process_Message (Sess.Implem, Sess);

            else
               Emit_No_Reply
                 (Port => Lower (Sess),
                  Msg  => GIOP_Data_Expected'
                    (In_Buf => Sess.Buffer_In,
                     Max    => Stream_Element_Count
                       (Sess.MCtx.Message_Size),
                     State  => Sess.State));
            end if;

         when Expect_Body =>
            pragma Debug (C, O ("Received GIOP message body"));
            pragma Debug (Show (Sess.Buffer_In));
            Process_Message (Sess.Implem, Sess);

         when others =>
            Throw
              (Error,
               Comm_Failure_E,
               System_Exception_Members'(0, Completed_Maybe));
      end case;
   exception
      when others =>
         Throw
           (Error,
            Comm_Failure_E,
            System_Exception_Members'(0, Completed_Maybe));
   end Handle_Data_Indication;

   ---------------------------------
   -- Handle_Unmarshall_Arguments --
   ---------------------------------

   procedure Handle_Unmarshall_Arguments
     (Sess  : access GIOP_Session;
      Args  : in out Any.NVList.Ref;
      Error : in out Errors.Error_Container)
   is
      use PolyORB.Errors;

   begin
      pragma Debug (C, O ("Unmarshalling_Request_Arguments"));
      pragma Assert (Sess.State = Waiting_Unmarshalling);

      Unmarshall_Argument_List
        (Sess.Implem, Sess.Buffer_In, Sess.Repr, Args,
         PolyORB.Any.ARG_IN, Sess.Implem.Data_Alignment, Error);

      if Found (Error) then
         Replace_Marshal_5_To_Bad_Param_23 (Error, Completed_No);
         --  An error in the marshalling of wchar data implies the
         --  server did not provide a valid codeset service
         --  context. We convert this exception to Bad_Param 23.
      end if;

      Expect_GIOP_Header (Sess);
   end Handle_Unmarshall_Arguments;

   ------------------
   -- Handle_Flush --
   ------------------

   procedure Handle_Flush (Sess : access GIOP_Session)
     renames Expect_GIOP_Header;

   -------------------------------
   -- Handle_Connect_Indication --
   -------------------------------

   procedure Handle_Connect_Indication
     (Sess : access GIOP_Session) is
   begin
      pragma Debug (C, O ("Handle_Connect_Indication"));
      pragma Assert (Sess.State = Not_Initialized);

      Sess.Role := Server;
      Expect_GIOP_Header (Sess);
   end Handle_Connect_Indication;

   ---------------------------------
   -- Handle_Connect_Confirmation --
   ---------------------------------

   procedure Handle_Connect_Confirmation (Sess : access GIOP_Session) is
      use PolyORB.Binding_Data.GIOP;
      use PolyORB.Binding_Objects;
   begin
      pragma Debug (C, O ("Handle_Connect_Confirmation"));
      pragma Assert (Sess.State = Not_Initialized);

      Sess.Role := Client;

      if Sess.Implem = null then
         --  Initialize session with GIOP version specified by the profile
         --  used to create the session. As a client, we are allowed to use
         --  a lower protocol version than the one advertised by the server.

         Get_GIOP_Implem
           (Sess,
            Get_GIOP_Version
              (GIOP_Profile_Type'Class
                (Get_Profile (Sess.Dependent_Binding_Object).all)),
            Allow_Downgrade => True);
      end if;

      Expect_GIOP_Header (Sess);
   end Handle_Connect_Confirmation;

   -----------------------
   -- Handle_Disconnect --
   -----------------------

   procedure Handle_Disconnect
     (Sess : access GIOP_Session; Error : Errors.Error_Container)
   is
      use Pend_Req_Tables;
      P     : Pending_Request_Access;
      ORB   : constant ORB_Access := ORB_Access (Sess.Server);

   begin
      pragma Debug (C, O ("Handle_Disconnect"));

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
            Emit_No_Reply (Component_Access (ORB),
                           Servants.Iface.Executed_Request'(Req => P.Req));
            Free (P);
         end if;
      end loop;

      Sess.State := Not_Initialized;
   end Handle_Disconnect;

   --------------------
   -- Invoke_Request --
   --------------------

   procedure Invoke_Request
     (Sess : access GIOP_Session;
      R    :        Request_Access;
      Pro  : access Binding_Data.Profile_Type'Class)
   is
      use PolyORB.Binding_Data;
      use PolyORB.Errors;
      use Unsigned_Long_Flags;

      New_Pending_Req : Pending_Request_Access;
      Error           : Errors.Error_Container;
      Success         : Boolean;
   begin
      if (Sess.Conf.Permitted_Sync_Scopes and R.Req_Flags) = 0
        or else (Sess.Implem.Permitted_Sync_Scopes and R.Req_Flags) = 0
      then
         pragma Debug (C, O ("Requested sync scope not supported"));
         raise GIOP_Error;
      end if;

      New_Pending_Req := new Pending_Request;
      New_Pending_Req.Req := R;
      New_Pending_Req.Target_Profile := Profile_Access (Pro);

      Enter (Sess.Mutex);
      if Is_Set (Sync_None, R.Req_Flags)
        or else Is_Set (Sync_With_Transport, R.Req_Flags)
      then

         --  Oneway call: we won't see any reply for this request,
         --  therefore we need to destroy the pending request
         --  information now.

         New_Pending_Req.Request_Id := Get_Request_Id (Sess);
         Leave (Sess.Mutex);
         Send_Request (Sess.Implem, Sess, New_Pending_Req, Error);
         Free (New_Pending_Req);

         if Found (Error) then
            Set_Exception (R, Error);
            Catch (Error);

            --  Since this is a oneway called, this request will return to the
            --  caller immediately; no need to generate an Executed_Request
            --  message at this point (see PolyORB.Protocols.Invoke_Request).

         end if;

         return;
      end if;

      --  Two-way call: a reply is expected, we store the pending request

      if Sess.Implem.Locate_Then_Request then
         New_Pending_Req.Locate_Req_Id := Get_Request_Id (Sess);
         Add_Pending_Request (Sess, New_Pending_Req);
         Leave (Sess.Mutex);
         Locate_Object (Sess.Implem, Sess, New_Pending_Req, Error);
      else
         Add_Pending_Request (Sess, New_Pending_Req);
         Leave (Sess.Mutex);
         Send_Request (Sess.Implem, Sess, New_Pending_Req, Error);
      end if;

      if Found (Error) then
         Remove_Pending_Request (Sess, New_Pending_Req.Request_Id, Success);

         if not Success then
            raise GIOP_Error;
         end if;

         Set_Exception (R, Error);
         Catch (Error);

         declare
            ORB : constant ORB_Access := ORB_Access (Sess.Server);
         begin
            Emit_No_Reply
              (Component_Access (ORB),
               Servants.Iface.Executed_Request'(Req => R));
         end;
      end if;
   end Invoke_Request;

   -------------------
   -- Abort_Request --
   -------------------

   procedure Abort_Request
     (Sess : access GIOP_Session;
      R    :        Requests.Request_Access) is
   begin
      Process_Abort_Request (Sess.Implem, Sess, R);
   end Abort_Request;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (Sess : access GIOP_Session;
      R    :        Requests.Request_Access) is
   begin
      Send_Reply (Sess.Implem, Sess, R);
   end Send_Reply;

   ----------------------------
   -- Cancel_Pending_Request --
   ----------------------------

   procedure Cancel_Pending_Request
     (Sess : access GIOP_Session)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Sess);
      pragma Warnings (On);
   begin
      pragma Debug (C, O ("Cancelling pending request"));
      --  XXX Cancelling pending requests for a session before closing
      --  Not Implemented
      --  This function must stop request which are running
      --  in client AND server mode
      null;
   end Cancel_Pending_Request;

   -------------------
   -- Locate_Object --
   -------------------

   procedure Locate_Object
     (Sess  : access GIOP_Session;
      Profile : Binding_Data.Profile_Access;
      Error : in out Errors.Error_Container)
   is
      use PolyORB.Errors;
      use Unsigned_Long_Flags;

      New_Pending_Req : Pending_Request_Access;

   begin
      if not Sess.Implem.Locate_Then_Request then
         return;
      end if;

      New_Pending_Req := new Pending_Request;

      New_Pending_Req.Req := new PolyORB.Requests.Request;
      --  We build an empty request to store any exception sent back
      --  by the remote note.

      New_Pending_Req.Target_Profile := Profile;

      Enter (Sess.Mutex);
      New_Pending_Req.Locate_Req_Id := Get_Request_Id (Sess);
      Add_Pending_Request (Sess, New_Pending_Req);
      Leave (Sess.Mutex);

      Locate_Object (Sess.Implem, Sess, New_Pending_Req, Error);
   end Locate_Object;

   ------------------
   -- Emit Message --
   ------------------

   procedure Emit_Message
     (Implem : access GIOP_Implem;
      S      : access Session'Class;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer : Buffer_Access;
      Error  : in out Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem, MCtx);
      pragma Warnings (On);

      use PolyORB.Filters.Iface;

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

   -------------------------------------------------------------------------

   --  Local functions

   ------------------------
   -- Expect_GIOP_Header --
   ------------------------

   --  called to wait another GIOP message
   procedure Expect_GIOP_Header
     (Sess : access GIOP_Session) is
   begin

      --  Check if buffer has been totally read

      if Remaining (Sess.Buffer_In) /= 0 then
         pragma Debug (C, O ("Remaining data in buffer :"
                          & Remaining (Sess.Buffer_In)'Img
                          & " bytes"));
         null;
         --  It is not an error to leave data remaining in Buffer,
         --  e.g. in the case of an (unexpected) unknown user exception.
      end if;

      pragma Debug (C, O ("Waiting for next message"));

      Buffers.Release_Contents (Sess.Buffer_In.all);
      Sess.State := Expect_Header;
      Emit_No_Reply
        (Port => Lower (Sess),
         Msg  => GIOP_Data_Expected'
         (In_Buf => Sess.Buffer_In,
          Max    => GIOP_Header_Size,
          State  => Sess.State));
   end Expect_GIOP_Header;

   -----------------------------------
   -- Unmarshall_Global_GIOP_Header --
   -----------------------------------

   procedure Unmarshall_Global_GIOP_Header
     (Sess    : access GIOP_Session;
      Buffer  : access Buffer_Type;
      Version : out GIOP_Version)
   is
      use Octet_Flags;

      Message_Magic : Stream_Element_Array (Magic'Range);
      Flags         : Types.Octet;
      Version_Data : Types.Octet;
   begin
      --  Get Endianness
      --  This code works only if the endianness bit dont move
      --  in different giop version
      Flags := Types.Octet (Peek (Buffer, Flags_Index - 1));
      pragma Debug (C, O ("Flags : " & Flags'Img));

      if Is_Set (Bit_Little_Endian, Flags) then
         Set_Endianness (Buffer, Little_Endian);
      else
         Set_Endianness (Buffer, Big_Endian);
      end if;

      --  Beginning of GIOP message is byte-ordering independent

      --  Magic

      for J in Message_Magic'Range loop
         Message_Magic (J) :=
           Stream_Element (Types.Octet'(Unmarshall (Buffer)));
      end loop;

      if Message_Magic /= Magic then
         raise GIOP_Error;
      end if;

      --  Get GIOP Message version

      Version_Data := Unmarshall (Buffer);
      pragma Assert (Version_Data = 1);
      --  Major

      Version_Data := Unmarshall (Buffer);
      --  Minor

      Version := To_GIOP_Version (Integer (Version_Data));

      pragma Debug (C, O ("Received GIOP message, version: "
                       & GIOP_Version'Image (Version)));

      if Sess.Implem = null then
         Get_GIOP_Implem (Sess, Version);

      elsif Version /= Sess.Implem.Version then
         raise GIOP_Error;
      end if;
   end Unmarshall_Global_GIOP_Header;

   ---------------------------------
   -- Marshall_Global_GIOP_Header --
   ---------------------------------

   procedure Marshall_Global_GIOP_Header
     (Sess   : access GIOP_Session;
      MCtx   : access GIOP_Message_Context'Class;
      Buffer : access PolyORB.Buffers.Buffer_Type) is
   begin
      --  Magic

      for J in Magic'Range loop
         Marshall (Buffer, Types.Octet (Magic (J)));
      end loop;

      --  Version

      Marshall (Buffer, Types.Octet (1));
      Marshall (Buffer, To_Minor_GIOP (Sess.Implem.Version));

      --  Implem-specific data

      Marshall_GIOP_Header (Sess.Implem, Sess, MCtx, Buffer);
   end Marshall_Global_GIOP_Header;

   ------------------------------
   -- Unmarshall_Argument_List --
   ------------------------------

   procedure Unmarshall_Argument_List
     (Implem              : access GIOP_Implem;
      Buffer              :        Buffer_Access;
      Representation      : access CDR_Representation'Class;
      Args                : in out Any.NVList.Ref;
      Direction           :        Any.Flags;
      First_Arg_Alignment :        Buffers.Alignment_Type;
      Error               : in out Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Errors;

      It  : Iterator := First (List_Of (Args).all);
      Arg : Element_Access;
   begin
      pragma Assert (Direction = ARG_IN or else Direction = ARG_OUT);

      if not Last (It) then
         Align_Position (Buffer, First_Arg_Alignment);
      end if;

      while not Last (It) loop
         Arg := Value (It);
         if False
           or else Arg.Arg_Modes = Direction
           or else Arg.Arg_Modes = ARG_INOUT
         then
            Unmarshall_To_Any
              (Representation,
               Buffer,
               Get_Container (Arg.Argument).all,
               Error);
            if Found (Error) then
               return;
            end if;

         end if;
         Next (It);
      end loop;
   end Unmarshall_Argument_List;

   ----------------------------
   -- Marshall_Argument_List --
   ----------------------------

   procedure Marshall_Argument_List
     (Implem              : access GIOP_Implem;
      Buffer              :        Buffer_Access;
      Representation      : access CDR_Representation'Class;
      Args                : in out Any.NVList.Ref;
      Direction           :        Any.Flags;
      First_Arg_Alignment :        Buffers.Alignment_Type;
      Error               : in out Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;
      use PolyORB.Errors;

      It  : Iterator;
      Arg : Element_Access;

   begin
      if List_Of (Args) = null then

         --  Do not fail if there is no argument to marshall, for instance in
         --  the case of a simple acknowledgement (in Sync_With_Server mode)
         --  that is sent prior to reading arguments.

         return;
      end if;

      It := First (List_Of (Args).all);

      pragma Assert (Direction = ARG_IN or else Direction = ARG_OUT);

      if not Last (It) then
         Pad_Align (Buffer, First_Arg_Alignment);
      end if;

      while not Last (It) loop
         Arg := Value (It);

         if False
           or else Arg.Arg_Modes = Direction
           or else Arg.Arg_Modes = ARG_INOUT
         then
            pragma Debug (C, O ("Marshalling argument "
                             & Types.To_Standard_String (Arg.Name)
                             & " = " & Image (Arg.Argument)));

            Marshall (Buffer, Representation, Arg.all, Error);

            if Found (Error) then
               return;
            end if;
         end if;

         Next (It);
      end loop;
   end Marshall_Argument_List;

   ----------------------------------------
   -- Unmarshall_System_Exception_To_Any --
   ----------------------------------------

   procedure Unmarshall_System_Exception_To_Any
     (Buffer :        Buffer_Access;
      Repr   : access Representations.CDR.CDR_Representation'Class;
      Info   :    out Any.Any)
   is
      use PolyORB.Any;
      use PolyORB.Errors;
      use PolyORB.GIOP_P.Exceptions;

      Exception_Name : constant String
        := Extract_System_Exception_Name
             (To_Standard_String (Types.RepositoryId'(Unmarshall (Buffer))));
      Error : PolyORB.Errors.Error_Container;

   begin
      Info := Any.Get_Empty_Any
        (PolyORB.GIOP_P.Exceptions.System_Exception_TypeCode (Exception_Name));
      Unmarshall_To_Any (Repr, Buffer, Get_Container (Info).all, Error);

      if Found (Error) then
         Info := Helper.Error_To_Any (Error);
         Catch (Error);
      end if;
   end Unmarshall_System_Exception_To_Any;

   --  Version management

   ----------------------------------
   -- Global_Register_GIOP_Version --
   ----------------------------------

   procedure Global_Register_GIOP_Version
     (Version : GIOP_Version;
      Implem  : GIOP_Factory) is
   begin
      pragma Assert (Implem /= null);
      pragma Assert (GIOP_Factories (Version) = null);

      GIOP_Factories (Version) := Implem;
   end Global_Register_GIOP_Version;

   ---------------------
   -- Get_GIOP_Implem --
   ---------------------

   procedure Get_GIOP_Implem
     (Sess            : access GIOP_Session;
      Version         : GIOP_Version;
      Allow_Downgrade : Boolean := False)
   is
      use PolyORB.Utils;
      Use_Version : GIOP_Version := Version;
   begin
      pragma Debug (C, O ("Looking up implementation for "
                       & GIOP_Version'Image (Version)));

      loop
         Sess.Implem := Sess.Conf.GIOP_Implems (Use_Version);
         exit when Sess.Implem /= null
           or else not Allow_Downgrade
           or else Use_Version = Sess.Conf.GIOP_Implems'First;
         Use_Version := GIOP_Version'Pred (Use_Version);
      end loop;

      if Sess.Implem /= null then
         pragma Debug (C, O ("... using version " & Use_Version'Img));
         Initialize_Session (Sess.Implem, Sess);
      else
         Ada.Exceptions.Raise_Exception
           (GIOP_Error'Identity, "could not find a suitable GIOP version");
      end if;
   end Get_GIOP_Implem;

   -------------------------
   -- Get_Pending_Request --
   -------------------------

   procedure Get_Pending_Request
     (Sess    : access GIOP_Session;
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

   -----------------------------------
   -- Get_Pending_Request_By_Locate --
   -----------------------------------

   procedure Get_Pending_Request_By_Locate
     (Sess    : access GIOP_Session;
      Id      :        Types.Unsigned_Long;
      Req     :    out Pending_Request_Access;
      Success :    out Boolean)
   is
      use Pend_Req_Tables;
   begin
      pragma Debug (C, O ("Retrieving pending request with locate id"
                       & Types.Unsigned_Long'Image (Id)));

      Success := False;
      Enter (Sess.Mutex);

      for J in First (Sess.Pending_Reqs) .. Last (Sess.Pending_Reqs) loop
         if Sess.Pending_Reqs.Table (J) /= null
           and then Sess.Pending_Reqs.Table (J).Locate_Req_Id = Id
         then
            Req := Sess.Pending_Reqs.Table (J);
            Success := True;
            exit;
         end if;
      end loop;

      Leave (Sess.Mutex);
   end Get_Pending_Request_By_Locate;

   ----------------------------
   -- Remove_Pending_Request --
   ----------------------------

   procedure Remove_Pending_Request
     (Sess    : access GIOP_Session;
      Id      :        Types.Unsigned_Long;
      Success :    out Boolean)
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
            Sess.Pending_Reqs.Table (J) := null;
            Free (PRA);
            Success := True;
            exit;
         end if;
      end loop;

      Leave (Sess.Mutex);
   end Remove_Pending_Request;

   --------------------------------------
   -- Remove_Pending_Request_By_Locate --
   --------------------------------------

   procedure Remove_Pending_Request_By_Locate
     (Sess    : access GIOP_Session;
      Id      : Types.Unsigned_Long;
      Success :    out Boolean)
   is
      use Pend_Req_Tables;

      PRA : Pending_Request_Access;
   begin
      pragma Debug (C, O ("Removing pending request with locate id"
                       & Types.Unsigned_Long'Image (Id)));

      Success := False;
      Enter (Sess.Mutex);

      for J in First (Sess.Pending_Reqs) .. Last (Sess.Pending_Reqs) loop
         if Sess.Pending_Reqs.Table (J) /= null
           and then Sess.Pending_Reqs.Table (J).Locate_Req_Id = Id
         then
            PRA := Sess.Pending_Reqs.Table (J);
            Sess.Pending_Reqs.Table (J) := null;
            Free (PRA);
            Success := True;
            exit;
         end if;
      end loop;

      Leave (Sess.Mutex);
   end Remove_Pending_Request_By_Locate;

   -------------------------
   -- Add_Pending_Request --
   -------------------------

   procedure Add_Pending_Request
     (Sess     : access GIOP_Session;
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

   --------------------
   -- Get_Request_Id --
   --------------------

   function Get_Request_Id
     (Sess : access GIOP_Session) return Types.Unsigned_Long
   is
      R : constant Types.Unsigned_Long := Sess.Req_Index;
   begin
      Sess.Req_Index := Sess.Req_Index + 1;
      return R;
   end Get_Request_Id;

   --------------------
   -- Get_Conf_Chain --
   --------------------

   function Get_Conf_Chain
     (Implem : access GIOP_Implem'Class)
     return String
   is
      use PolyORB.Utils;

   begin
      return To_Standard_String (Implem.Prefix)
        & ".1."
        & Trimmed_Image (Unsigned_Long_Long (To_Minor_GIOP (Implem.Version)));
   end Get_Conf_Chain;

   ------------------------
   -- Get_Representation --
   ------------------------

   function Get_Representation
     (Sess : GIOP_Session)
     return PolyORB.Representations.CDR.CDR_Representation_Access
   is
   begin
      return Sess.Repr;
   end Get_Representation;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Sess : GIOP_Session)
     return Buffer_Access
   is
   begin
      return Sess.Buffer_In;
   end Get_Buffer;

end PolyORB.Protocols.GIOP;
