------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . P R O T O C O L S . G I O P                --
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

with Ada.Streams;

with PolyORB.Annotations;
with PolyORB.Binding_Data;
with PolyORB.Buffers;
with PolyORB.Components;
with PolyORB.GIOP_P.Exceptions;

with PolyORB.Log;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Configuration;
with PolyORB.Utils.Strings;
with PolyORB.ORB;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Representations.CDR;
with PolyORB.Types;
with PolyORB.References;

package body PolyORB.Protocols.GIOP is

   use PolyORB.Annotations;
   use PolyORB.Buffers;
   use PolyORB.Components;
   use PolyORB.Log;
   use PolyORB.ORB;
   use PolyORB.Representations.CDR;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.giop");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   type GIOP_Implem_Desc is record
      Func    : GIOP_Create_Implem_Func;
      Version : GIOP_Version;
   end record;

   GIOP_Implem_List : array (1 .. Max_GIOP_Implem) of
     GIOP_Implem_Desc;
   Nb_Implem : Natural range  0 .. Max_GIOP_Implem := 0;

   ------------
   -- Create --
   ------------

   procedure Create
     (Proto   : access GIOP_Protocol;
      Session : out   Filter_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Proto);
      pragma Warnings (On);

   begin
      Session := new GIOP_Session;
      Init_GIOP_Session (GIOP_Session (Session.all),
                         GIOP_Default_Version,
                         Default_Locate_Then_Request,
                         "giop",
                         "polyorb.protocols.giop");
      Set_Allocation_Class (Session.all, Dynamic);
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (S : in out GIOP_Session) is
   begin
      pragma Debug (O ("Initializing GIOP session"));

      Protocols.Initialize (Protocols.Session (S));
      S.Buffer_In  := new Buffer_Type;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (S : in out GIOP_Session)
   is
      use PolyORB.Binding_Data;

   begin
      pragma Debug (O ("Finalizing GIOP session"));
      if S.Buffer_In /= null then
         Release (S.Buffer_In);
      end if;

      if S.Implem /= null then
         Finalize_Session (S.Implem, S'Access);
      end if;

      Protocols.Finalize (Protocols.Session (S));
   end Finalize;

   ----------------------------
   -- Handle_Data_Indication --
   ----------------------------

   procedure Handle_Data_Indication
     (Sess        : access GIOP_Session;
      Data_Amount :        Stream_Element_Count)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Data_Amount);
      pragma Warnings (On);

      use ORB;
      use Pend_Req_Seq;

      Version : GIOP_Version;
   begin
      pragma Debug (O ("Received data in state " & Sess.State'Img));

      pragma Assert (Sess.State /= Not_Initialized);

      case Sess.State is

         when Expect_Header =>

            Unmarshall_Global_GIOP_Header (Sess.Buffer_In, Version);

            if Sess.Implem = null then
               Get_GIOP_Implem (Sess, Version);
            elsif Version /= Sess.Implem.Version then
               raise GIOP_Error;
            end if;

            Unmarshall_GIOP_Header (Sess.Implem, Sess);

            Sess.State := Expect_Body;

            pragma Debug (O ("GIOP Header OK, ask for body, size :"
                             & Sess.Ctx.Message_Size'Img));

            if Sess.Ctx.Message_Size = 0 then
               Process_Message (Sess.Implem, Sess);
            else
               Emit_No_Reply
                 (Port => Lower (Sess),
                  Msg  => GIOP_Data_Expected'
                  (In_Buf => Sess.Buffer_In,
                   Max =>  Stream_Element_Count (Sess.Ctx.Message_Size),
                   State => Sess.State));
            end if;

         when Expect_Body =>
            Process_Message (Sess.Implem, Sess);

         when others =>
            raise GIOP_Error;
      end case;
   end Handle_Data_Indication;

   ---------------------------------
   -- Handle_Unmarshall_Arguments --
   ---------------------------------

   procedure Handle_Unmarshall_Arguments
     (Sess : access GIOP_Session;
      Args : in out Any.NVList.Ref) is
   begin
      pragma Assert (Sess.State = Waiting_Unmarshalling);
      pragma Debug (O ("Unmarshalling request arguments"));

      Unmarshall_Argument_List
        (Sess.Implem, Sess.Buffer_In, Args,
         PolyORB.Any.ARG_IN, Sess.Implem.Data_Alignment);

      Expect_GIOP_Header (Sess);
   end Handle_Unmarshall_Arguments;

   -------------------------------
   -- Handle_Connect_Indication --
   -------------------------------

   procedure Handle_Connect_Indication
     (Sess : access GIOP_Session) is
   begin
      pragma Assert (Sess.State = Not_Initialized);
      pragma Debug (O ("Connexion from remote host"));

      Sess.Role := Server;
      Expect_GIOP_Header (Sess);
   end Handle_Connect_Indication;

   ---------------------------------
   -- Handle_Connect_Confirmation --
   ---------------------------------

   procedure Handle_Connect_Confirmation
     (Sess : access GIOP_Session) is
   begin
      pragma Assert (Sess.State = Not_Initialized);
      pragma Debug (O ("Connexion from neutral layer"));

      Sess.Role := Client;

      --  Init session with default GIOP version if implem is null
      if Sess.Implem = null then
         Get_GIOP_Implem (Sess, Sess.GIOP_Def_Ver);
      end if;

      Expect_GIOP_Header (Sess);
   end Handle_Connect_Confirmation;

   -----------------------
   -- Handle_Disconnect --
   -----------------------

   procedure Handle_Disconnect
     (Sess : access GIOP_Session) is
   begin
      pragma Debug (O ("Disconnected"));
      if Sess.Buffer_In /= null then
         Release (Sess.Buffer_In);
      end if;

      if Sess.Implem /= null then
         Finalize_Session (Sess.Implem, Sess);
         Sess.Implem := null;
      end if;

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
      use Unsigned_Long_Flags;

      Current_Req    : Pending_Request_Access := new Pending_Request;
      Binding_Object : Components.Component_Access;
      Profile        : Binding_Data.Profile_Access;
   begin
      References.Get_Binding_Info (R.Target, Binding_Object, Profile);
      Current_Req.Req := R;
      Current_Req.Target_Profile := Profile;

      if Profile = null then
         Current_Req.Target_Profile := Profile_Access (Pro);
         --  XXX this should *not* happen, it would be logical
         --  to Assesrt (Pro = Profile)!
         --  This work-around is currently necessary to pass
         --  all_types with a dynamic proxy. Apparently,
         --  Profile is left null for the target reference of
         --  a proxied request (on the client side of the proxy).
         --  Needs to be investigated.
      end if;

      if Is_Set (Sync_With_Transport, R.Req_Flags) then
         --  XXX avoiding memory leaks with one way calls
         Current_Req.Request_Id := Get_Request_Id (Sess);
         Send_Request (Sess.Implem, Sess, Current_Req);
         Free (Current_Req);
         return;
      end if;

      if Sess.Implem.Locate_Then_Request then
         Current_Req.Locate_Req_Id := Get_Request_Id (Sess);
         Add_Pending_Request (Sess, Current_Req);
         Locate_Object (Sess.Implem, Sess, Current_Req);
      else
         Add_Pending_Request (Sess, Current_Req);
         Send_Request (Sess.Implem, Sess, Current_Req);
      end if;
   end Invoke_Request;

   -------------------
   -- Abort_Request --
   -------------------

   procedure Abort_Request
     (Sess : access GIOP_Session;
      R    : Requests.Request_Access) is
   begin
      Process_Abort_Request (Sess.Implem, Sess, R);
   end Abort_Request;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (Sess : access GIOP_Session;
      R    : Requests.Request_Access) is
   begin
      Process_Reply (Sess.Implem, Sess, R);
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
      pragma Debug (O ("Cancelling pending request"));
      --  XXX Cancelling pending requests for a session before closing
      --  Not Implemented
      --  This function must stop request which are running
      --  in client AND server mode
      null;
   end Cancel_Pending_Request;

   ------------------
   -- Emit Message --
   ------------------

   procedure Emit_Message
     (Implem : access GIOP_Implem;
      S      : access Session'Class;
      Buffer :        Buffer_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.Filters.Interface;
      --  Default procedure, noimg to do
      --  This procedure is overriden for fragmenting management
   begin
      Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => Buffer));
   end Emit_Message;

   -------------------------------------------------------------------------

   --  Local functions

   ------------------------
   -- Expect_GIOP_Header --
   ------------------------

   --  called to wait another GIOP message
   procedure Expect_GIOP_Header
     (Sess : access GIOP_Session)
   is
   begin
      --  Check if buffer has been totally read
      if Remaining (Sess.Buffer_In) /= 0 then
         pragma Debug (O ("Remaining data in buffer :"
                          & Remaining (Sess.Buffer_In)'Img
                          & " bytes"));
         null;
         --  It is not an error to leave data remaining in Buffer,
         --  e.g. in the case of an (unexpected) unknown user exception.
      end if;

      pragma Debug (O ("Waiting for next message"));

      Buffers.Release_Contents (Sess.Buffer_In.all);
      Sess.State := Expect_Header;
      Emit_No_Reply
        (Port => Lower (Sess),
         Msg  => GIOP_Data_Expected'
         (In_Buf => Sess.Buffer_In,
          Max =>  GIOP_Header_Size,
          State => Sess.State));
   end Expect_GIOP_Header;

   ----------------------------
   -- Unmarshall_GIOP_Header --
   ----------------------------

   procedure Unmarshall_Global_GIOP_Header
     (Buffer  : access Buffer_Type;
      Version :    out GIOP_Version)
   is
      use Octet_Flags;

      Message_Magic : Stream_Element_Array (Magic'Range);
      Flags         : Types.Octet;
   begin
      --  Get Endianness
      --  This code works only if the endianness bit dont move
      --  in different giop version
      Flags := Types.Octet (Peek (Buffer, Flags_Index - 1));
      --  pragma Debug (O ("Flags : " & Flags'Img));

      if (Is_Set (Bit_Endianness, Flags)) then
         Set_Endianness (Buffer, Little_Endian);
      else
         Set_Endianness (Buffer, Big_Endian);
      end if;

      --  Begining of GIOP message is byte-ordering independent

      --  Magic
      for J in Message_Magic'Range loop
         Message_Magic (J) := Stream_Element
           (Types.Octet'(Unmarshall (Buffer)));
      end loop;

      if Message_Magic /= Magic then
         raise GIOP_Error;
      end if;

      --  Get GIOP Message version
      Version.Major := Unmarshall (Buffer);
      Version.Minor := Unmarshall (Buffer);

      pragma Debug (O ("Received GIOP message, version:"
                       & Version.Major'Img
                       & "."
                       & Version.Minor'Img));
   end Unmarshall_Global_GIOP_Header;

   ---------------------------------
   -- Marshall_Global_GIOP_Header --
   ---------------------------------

   procedure Marshall_Global_GIOP_Header
     (Sess   : access GIOP_Session;
      Buffer : access Buffer_Type) is
   begin
      Marshall_Global_GIOP_Header
        (Sess.Implem.Version, Buffer);
   end Marshall_Global_GIOP_Header;

   procedure Marshall_Global_GIOP_Header
     (Version :        GIOP_Version;
      Buffer  : access Buffer_Type)
   is
   begin
      --  Magic
      for J in Magic'Range loop
         Marshall (Buffer, Types.Octet (Magic (J)));
      end loop;

      --  Version
      Marshall (Buffer, Version.Major);
      Marshall (Buffer, Version.Minor);
   end Marshall_Global_GIOP_Header;

   ------------------
   -- Context_List --
   ------------------

   procedure Marshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type) is
   begin
      Marshall (Buffer, Types.Unsigned_Long (0));
   end Marshall_Service_Context_List;

   procedure Unmarshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type)
   is
      Nb : constant PolyORB.Types.Unsigned_Long := Unmarshall (Buffer);
   begin
      pragma Debug (O ("Unmarshall_Service_Context_List: enter, nb ="
                       & PolyORB.Types.Unsigned_Long'Image (Nb)));

      for J in 1 .. Nb loop
         declare
            Context_Id   : constant Types.Unsigned_Long := Unmarshall (Buffer);
            Context_Data : constant Encapsulation := Unmarshall (Buffer);
            pragma Warnings (Off);
            pragma Unreferenced (Context_Id, Context_Data);
            pragma Warnings (On);
         begin
            null;
         end;
      end loop;

      pragma Debug (O ("Unmarshall_Service_Context_List: leave"));
   end Unmarshall_Service_Context_List;

   -------------------
   -- Argument_List --
   -------------------

   procedure Unmarshall_Argument_List
     (Implem              : access GIOP_Implem;
      Buffer              :        Buffer_Access;
      Args                : in out Any.NVList.Ref;
      Direction           :        Any.Flags;
      First_Arg_Alignment :        Opaque.Alignment_Type)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      It  : Iterator := First (List_Of (Args).all);
      Arg : Element_Access;
   begin
      pragma Assert
        (Direction = ARG_IN or else Direction = ARG_OUT);

      while not Last (It) loop
         Arg := Value (It);
         if False
           or else Arg.Arg_Modes = Direction
           or else Arg.Arg_Modes = ARG_INOUT
         then
            if First (It) then
               Align_Position (Buffer, First_Arg_Alignment);
            end if;
            Unmarshall_To_Any (Buffer, Arg.Argument);
         end if;
         Next (It);
      end loop;
   end Unmarshall_Argument_List;

   procedure Marshall_Argument_List
     (Implem              : access GIOP_Implem;
      Buffer              :        Buffer_Access;
      Args                : in out Any.NVList.Ref;
      Direction           :        Any.Flags;
      First_Arg_Alignment :        Opaque.Alignment_Type)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Implem);
      pragma Warnings (On);

      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      It  : Iterator := First (List_Of (Args).all);
      Arg : Element_Access;
   begin
      pragma Assert
        (Direction = ARG_IN or Direction = ARG_OUT);

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
   end Marshall_Argument_List;

   ---------------
   -- Exception --
   ---------------

   procedure Unmarshall_System_Exception_To_Any
     (Buffer :     Buffer_Access;
      Info   : out Any.Any)
   is
      use PolyORB.GIOP_P.Exceptions;

      Exception_Name : constant String
        := Extract_System_Exception_Name (Unmarshall (Buffer));
   begin
      Info := Any.Get_Empty_Any (System_Exception_TypeCode (Exception_Name));
      Unmarshall_To_Any (Buffer, Info);
   end Unmarshall_System_Exception_To_Any;

   --------------------
   -- Select_Profile --
   --------------------

   function Select_Profile
     (Buffer  : access Buffer_Type)
     return PolyORB.Binding_Data.Profile_Access
   is
      use PolyORB.Binding_Data;
      use PolyORB.References;
      use PolyORB.References.IOR;

      New_Ref    : IOR.IOR_Type := Representations.CDR.Unmarshall (Buffer);
      Prof_Array : constant PolyORB.References.Profile_Array
        := Profiles_Of (New_Ref);
   begin
      pragma Debug (O ("Reply Message : Received Location_Forward"));

      for J in Prof_Array'Range loop
         if Get_Profile_Tag (Prof_Array (J).all) = Tag_Internet_IOP then
            return Prof_Array (J);
         end if;
      end loop;

      return null;
   end Select_Profile;

   --  Version managing

   -----------------------
   -- Init_GIOP_Session --
   -----------------------
   procedure Init_GIOP_Session
     (Sess                : in out GIOP_Session;
      Version             : GIOP_Version;
      Locate_Then_Request : Boolean;
      Section             : String;
      Prefix              : String)
   is
      use PolyORB.Configuration;
      use PolyORB.Utils;
   begin
      pragma Debug (O ("Initialize parameters for GIOP session"));
      pragma Debug (O ("Conf Section : " & Section));
      pragma Debug (O ("Conf Prefix : " & Prefix));
      Protocols.Initialize (Protocols.Session (Sess));

      Sess.GIOP_Def_Ver.Minor
        := Types.Octet
        (Get_Conf
         (Section,
          Prefix & ".default_version.minor",
          Integer (Version.Minor)));

      Sess.GIOP_Def_Ver.Major
        := Types.Octet
        (Get_Conf
         (Section,
          Prefix & ".default_version.major",
          Integer (Version.Major)));

      for J in 1 .. Nb_Implem loop
         if Get_Conf (Section,
                      Prefix
                      & "."
                      & Trimmed_Image
                      (Integer (GIOP_Implem_List (J).Version.Major))
                      & "."
                      & Trimmed_Image
                      (Integer (GIOP_Implem_List (J).Version.Minor))
                      & ".enable", True) then
            pragma Debug (O ("Enable GIOP Version : "
                             & GIOP_Implem_List (J).Version.Major'Img
                             & "."
                             & GIOP_Implem_List (J).Version.Minor'Img));
            Sess.Nb_Implem := Sess.Nb_Implem + 1;
            Sess.GIOP_Implem_List (Sess.Nb_Implem)
              := GIOP_Implem_List (J).Func.all;
            Sess.GIOP_Implem_List (Sess.Nb_Implem).Version
              := GIOP_Implem_List (J).Version;
            Sess.GIOP_Implem_List (Sess.Nb_Implem).Section
              := To_PolyORB_String (Section);
            Sess.GIOP_Implem_List (Sess.Nb_Implem).Prefix
              := To_PolyORB_String (Prefix);

            Initialize_Implem (Sess.GIOP_Implem_List (Sess.Nb_Implem));

            Sess.GIOP_Implem_List (Sess.Nb_Implem).Locate_Then_Request :=
              Get_Conf (Section,
                        Get_Conf_Chain (Sess.GIOP_Implem_List (Sess.Nb_Implem))
                        & ".locate_then_request",
                        Locate_Then_Request);
         end if;
      end loop;
   end Init_GIOP_Session;

   ----------------------------------
   -- Global_Register_GIOP_Version --
   ----------------------------------

   procedure Global_Register_GIOP_Version
     (Version : GIOP_Version;
      Implem  : GIOP_Create_Implem_Func) is
   begin
      if Implem /= null then
         Nb_Implem := Nb_Implem + 1;
         GIOP_Implem_List (Nb_Implem) :=
           GIOP_Implem_Desc'(Version => Version,
                             Func => Implem);
      end if;
   end Global_Register_GIOP_Version;

   ---------------------
   -- Get_GIOP_Implem --
   ---------------------

   procedure Get_GIOP_Implem
     (Sess    : access GIOP_Session;
      Version :        GIOP_Version) is
   begin
      for J in 1 .. Sess.Nb_Implem loop
         if Sess.GIOP_Implem_List (J).Version = Version then
            pragma Debug (O ("Binding session to version:"
                             & Version.Major'Img
                             & "."
                             & Version.Minor'Img));
            Sess.Implem := Sess.GIOP_Implem_List (J);
            Initialize_Session (Sess.Implem, Sess);
            return;
         end if;
      end loop;

      raise GIOP_Unknown_Version;
   end Get_GIOP_Implem;

   -------------------------
   -- Get_Pending_Request --
   -------------------------

   procedure Get_Pending_Request
     (Sess    : access GIOP_Session;
      Id      :        Types.Unsigned_Long;
      Req     :    out Pending_Request_Access;
      Success :    out Boolean)
   is
      Pending_Reqs : constant Pend_Req_Seq.Element_Array
        := Pend_Req_Seq.To_Element_Array (Sess.Pending_Reqs);
   begin
      pragma Debug (O ("Retrieving pending request with id"
                       & Id'Img));

      for J in Pending_Reqs'Range loop
         if Pending_Reqs (J).Request_Id = Id then
            Req := Pending_Reqs (J);
            Success := True;
            return;
         end if;
      end loop;

      Success := False;
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
      Pending_Reqs : constant Pend_Req_Seq.Element_Array
        := Pend_Req_Seq.To_Element_Array (Sess.Pending_Reqs);

   begin
      pragma Debug
        (O ("Retrieving pending request with locate id" & Id'Img));

      for J in Pending_Reqs'Range loop
         if Pending_Reqs (J).Locate_Req_Id = Id then
            Req := Pending_Reqs (J);
            Success := True;
            return;
         end if;
      end loop;

      Success := False;
   end Get_Pending_Request_By_Locate;

   -------------------------
   -- Add_Pending_Request --
   -------------------------

   procedure Add_Pending_Request
     (Sess     : access GIOP_Session;
      Pend_Req : in     Pending_Request_Access)
   is
      use Pend_Req_Seq;

      Request_Id : constant Types.Unsigned_Long := Get_Request_Id (Sess);
   begin
      pragma Debug (O ("Adding pending request with id"
                       & Request_Id'Img));
      Set_Note
        (Pend_Req.Req.Notepad, Request_Note'
           (Annotations.Note with Id => Request_Id));
      Pend_Req.Request_Id := Request_Id;
      Append (Sess.Pending_Reqs, Pend_Req);
   end Add_Pending_Request;

   --------------------
   -- Get_Request_Id --
   --------------------

   function Get_Request_Id
     (Sess : access GIOP_Session)
     return Types.Unsigned_Long
   is
      R : Types.Unsigned_Long;
   begin
      --  XXX should we protect Sess.Req_Index against concurrent accesses ?
      R := Sess.Req_Index;
      Sess.Req_Index := Sess.Req_Index + 1;
      return R;
   end Get_Request_Id;

   --------------------------
   -- Free Pending Request --
   --------------------------

   procedure Free_Pending_Request
     (Sess    : access GIOP_Session;
      Id      :        Types.Unsigned_Long)
   is
      Pending_Reqs : Pend_Req_Seq.Element_Array
        := Pend_Req_Seq.To_Element_Array (Sess.Pending_Reqs);
   begin
      for J in Pending_Reqs'Range loop
         if Pending_Reqs (J).Request_Id = Id then
            pragma Debug (O ("Free request with id :" & Id'Img));
            Free (Pending_Reqs (J));
            Pend_Req_Seq.Delete (Sess.Pending_Reqs, J, 1);
            return;
         end if;
      end loop;
      raise GIOP_Error;
   end Free_Pending_Request;

   --------------------
   -- Get_Conf_Chain --
   --------------------

   function Get_Conf_Chain
     (Implem : access GIOP_Implem'Class)
     return String
   is
      use PolyORB.Utils;
   begin
      return
        To_Standard_String (Implem.Prefix)
        & "."
        & Trimmed_Image
        (Integer (Implem.Version.Major))
        & "."
        & Trimmed_Image
        (Integer (Implem.Version.Minor));
   end Get_Conf_Chain;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      null;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;
begin
   Register_Module
     (Module_Info'
      (Name => +"protocols.giop",
       Conflicts => Empty,
       Depends => Empty,
       Provides => Empty,
       Init => Initialize'Access));
end PolyORB.Protocols.GIOP;
