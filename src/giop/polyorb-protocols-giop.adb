------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . P R O T O C O L S . G I O P                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Streams;

with PolyORB.Any;
with PolyORB.Annotations;
with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;
with PolyORB.Binding_Data;
with PolyORB.Binding_Data.IIOP;
with PolyORB.Binding_Data.Local;
with PolyORB.Buffers;
with PolyORB.Components;
with PolyORB.Exceptions;
with PolyORB.Filters;
with PolyORB.Filters.Interface;
with PolyORB.GIOP_P.Exceptions;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.Objects.Interface;
with PolyORB.Opaque;
with PolyORB.ORB;
with PolyORB.ORB.Interface;
with PolyORB.Protocols;
with PolyORB.Protocols.GIOP.GIOP_1_0;
with PolyORB.Protocols.GIOP.GIOP_1_1;
with PolyORB.Protocols.GIOP.GIOP_1_2;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Representations;
with PolyORB.Representations.CDR;
with PolyORB.Requests;
with PolyORB.Tasking.Soft_Links;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package body PolyORB.Protocols.GIOP is

   use Ada.Streams;

   use PolyORB.Annotations;
   use PolyORB.Any.NVList;
   use PolyORB.Binding_Data;
   use PolyORB.Binding_Data.IIOP;
   use PolyORB.Buffers;
   use PolyORB.Components;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.ORB;
   use PolyORB.ORB.Interface;
   use PolyORB.Protocols;
   use PolyORB.Requests;
   use PolyORB.Representations;
   use PolyORB.Representations.CDR;
   use PolyORB.Tasking.Soft_Links;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols.giop");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --  Global data

   --  Each GIOP request has a unique Request_Id, allocated
   --  using a global counter.

   Current_Request_Id : Types.Unsigned_Long := 1;
   Counter_Lock : Mutex_Access;
   --  Global variable Current_Request_Id must be accessed
   --  only with Counter_Lock locked.

   --  A note can be attached to a PolyORB request to augment
   --  it with personality-specific information. The GIOP stack
   --  uses such a note to associate the Request with its
   --  Request_Id.

   type Request_Note is new Note with record
     Id : Types.Unsigned_Long;
   end record;

   --  Subprograms related to the global Request_Id counter.
   --  (actually used only on the GIOP client side).

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Create (Counter_Lock);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      Destroy (Counter_Lock);
   end Finalize;

   ---------------------
   -- Gest_Request_Id --
   ---------------------

   function Get_Request_Id return Types.Unsigned_Long
   is
      R : Types.Unsigned_Long;
   begin
      Enter (Counter_Lock);
      R := Current_Request_Id;
      Current_Request_Id := Current_Request_Id + 1;
      Leave (Counter_Lock);
      return R;
   end Get_Request_Id;

   --  Conversion tables for various fields of GIOP messages.

   MsgType_To_Octet :
     constant array (Msg_Type'Range) of Types.Octet
     := (Request          => 0,
         Reply            => 1,
         Cancel_Request   => 2,
         Locate_Request   => 3,
         Locate_Reply     => 4,
         Close_Connection => 5,
         Message_Error    => 6,
         Fragment         => 7);

   ReplyStatusType_To_Unsigned_Long :
     constant array (Reply_Status_Type'Range) of Types.Unsigned_Long
     := (No_Exception     => 0,
         User_Exception   => 1,
         System_Exception => 2,
         Location_Forward => 3,
         Location_Forward_Perm => 4,
         Needs_Addressing_Mode => 5);

   LocateStatusType_To_Unsigned_Long :
     constant array (Locate_Status_Type'Range) of Types.Unsigned_Long
     := (Unknown_Object => 0,
         Object_Here    => 1,
         Object_Forward => 2,
         Object_Forward_Perm => 3,
         Loc_System_Exception => 4,
         Loc_Needs_Addressing_Mode => 5);

   Octet_To_MsgType :
     constant array (Types.Octet range 0 .. 7) of Msg_Type
     := (0 => Request,
         1 => Reply,
         2 => Cancel_Request,
         3 => Locate_Request,
         4 => Locate_Reply,
         5 => Close_Connection,
         6 => Message_Error,
         7 => Fragment);

   Unsigned_Long_To_ReplyStatusType :
     constant array (Types.Unsigned_Long range 0 .. 5) of Reply_Status_Type
     := (0 => No_Exception,
         1 => User_Exception,
         2 => System_Exception,
         3 => Location_Forward,
         4 => Location_Forward_Perm,
         5 => Needs_Addressing_Mode);

   Unsigned_Long_To_LocateStatusType :
     constant array (Types.Unsigned_Long range 0 .. 5) of Locate_Status_Type
     := (0 => Unknown_Object,
         1 => Object_Here,
         2 => Object_Forward,
         3 => Object_Forward_Perm,
         4 => Loc_System_Exception,
         5 => Loc_Needs_Addressing_Mode);

   ---------------
   -- To_Buffer --
   ---------------

   --  Utility function for testing.

   procedure To_Buffer
     (S      : access GIOP_Session;
      Octets : access Encapsulation)
   is
      use PolyORB.Representations.CDR;
      use PolyORB.Opaque;

      Endianness1 : Endianness_Type;
      Z : constant Zone_Access
        := Zone_Access'(Octets.all'Unchecked_Access);
      --  Bypass runtime accessibility check.
   begin

      if PolyORB.Types.Boolean'Val
        (PolyORB.Types.Octet (Octets (Octets'First)))
      then
         Endianness1 := Little_Endian;
      else
         Endianness1 := Big_Endian;
      end if;

      Initialize_Buffer
        (Buffer               => S.Buffer_In,
         Size                 => Octets'Length - 1,
         Data                 => To_Opaque_Pointer (Z),
         Endianness           => Endianness1,
         Initial_CDR_Position => 0);

      Show (S.Buffer_In.all);

   end To_Buffer;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Msg_Type) is
   begin
      Marshall (Buffer, MsgType_To_Octet (Value));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Reply_Status_Type) is
   begin
      Marshall (Buffer, ReplyStatusType_To_Unsigned_Long (Value));
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Locate_Status_Type) is
   begin
      Marshall (Buffer, LocateStatusType_To_Unsigned_Long (Value));
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Msg_Type is
   begin
      return Octet_To_MsgType (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Reply_Status_Type is
   begin
      return Unsigned_Long_To_ReplyStatusType (Unmarshall (Buffer));
   end Unmarshall;

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Locate_Status_Type is
   begin
      return Unsigned_Long_To_LocateStatusType (Unmarshall (Buffer));
   end Unmarshall;

   ----------
   -- Spec --
   ----------

   procedure Unmarshall_Locate_Request
     (Buffer        : access Buffer_Type;
      Request_Id    : out Types.Unsigned_Long;
      Object_Key    : out Objects.Object_Id);
   pragma Warnings (Off);
   pragma Unreferenced (Unmarshall_Locate_Request);
   pragma Warnings (On);

   procedure Request_Received
     (Ses : access GIOP_Session);

   procedure Reply_Received
     (Ses : access GIOP_Session);

   procedure Locate_Request_Receive
     (Ses : access GIOP_Session);
   pragma Warnings (Off);
   pragma Unreferenced (Locate_Request_Receive);
   --  XXX Should be referenced in receive dispatch procedure!
   pragma Warnings (On);

   procedure Marshall_Argument_List
     (Ses : access GIOP_Session;
      Args : in out Any.NVList.Ref;
      Direction : Any.Flags);
   --  Internal subprogram: Marshall arguments from Args
   --  into Ses.
   --  Direction may be ARG_IN or ARG_OUT. Only NamedValues
   --  with Arg_Modes equal to either ARG_INOUT or Direction
   --  will be considered.

   procedure Unmarshall_Argument_List
     (Ses : access GIOP_Session;
      Args : in out Any.NVList.Ref;
      Direction : Any.Flags);
   --  Internal subprogram: set the values of arguments in
   --  Args by unmarshalling them from Ses.
   --  Direction may be ARG_IN or ARG_OUT. Only NamedValues
   --  with Arg_Modes equal to either ARG_INOUT or Direction
   --  will be considered.

   -----------------------------
   -- Marshall_Cancel_Request --
   -----------------------------

   procedure Marshall_Cancel_Request
     (Buffer     : access Buffer_Type;
      Request_Id : in Types.Unsigned_Long) is
   begin

      --  Request id
      Marshall (Buffer, Request_Id);

   end Marshall_Cancel_Request;

   -----------------------------
   -- Marshall_Locate_Request --
   -----------------------------

   procedure Marshall_Locate_Request
     (Buffer           : access Buffer_Type;
      Request_Id       : in Types.Unsigned_Long;
      Object_Key       : access Objects.Object_Id)
   is
      use Representations.CDR;
   begin

      --  Request id
      Marshall (Buffer, Request_Id);

      --  Object Key
      Marshall (Buffer, Stream_Element_Array (Object_Key.all));

   end  Marshall_Locate_Request;

   ---------------------------
   -- Marshall_Locate_Reply --
   ---------------------------

   procedure  Marshall_Locate_Reply
     (Buffer         : access Buffer_Type;
      Request_Id     : in Types.Unsigned_Long;
      Locate_Status  : in Locate_Status_Type) is
   begin
      --  Request id
      Marshall (Buffer, Request_Id);

      --  Locate Status
      Marshall (Buffer, Locate_Status);
   end  Marshall_Locate_Reply;

   ----------------------------
   -- Unarmshall_GIOP_Header --
   ----------------------------

   procedure Unmarshall_GIOP_Header
     (Ses                   : access GIOP_Session;
      Message_Type          : out Msg_Type;
      Message_Size          : out Types.Unsigned_Long;
      Fragment_Next         : out Types.Boolean;
      Success               : out Boolean)
   is
      Buffer : Buffer_Access renames Ses.Buffer_In;

      Stream_Header          : constant Stream_Element_Array
        := To_Stream_Element_Array (Buffer);
      Message_Magic          : Stream_Element_Array (Magic'Range);
      Message_Major_Version  : Octet;
      Message_Minor_Version  : Octet;
      Message_Endianness     : Endianness_Type;
      Endianness             : Endianness_Type;
      Flags                  : Types.Octet;

   begin

      Success := False;

      if Types.Boolean'Val
           (Types.Octet (Stream_Header
                         (Stream_Header'First
                          + Byte_Order_Offset)) and 1) then
         Message_Endianness := Little_Endian;
      else
         Message_Endianness := Big_Endian;
      end if;
      pragma Debug (O ("Message_Endianness = " & Message_Endianness'Img));
      Set_Endianness (Buffer, Message_Endianness);

      --  Magic
      for I in Message_Magic'Range loop
         Message_Magic (I) := Stream_Element
           (Types.Octet'(Unmarshall (Buffer)));
      end loop;

      if Message_Magic /= Magic then
         pragma Debug (O ("Unmarshall_GIOP_Header: Bad magic!"));
         return;
      end if;

      --  Test if the GIOP version of the Message received is supported
      Message_Major_Version := Unmarshall (Buffer);
      Message_Minor_Version := Unmarshall (Buffer);

      if not (Message_Major_Version =  Ses.Major_Version)
        or else (Ses.Minor_Version < Message_Minor_Version)
      then
         pragma Debug
           (O ("Unmarshall_GIOP_Header: GIOP version not supported"));
         return;
      end if;

      Flags := Unmarshall (Buffer);
      if Is_Set (Flags, Endianness_Bit) then
         Endianness := Little_Endian;
      else
         Endianness := Big_Endian;
      end if;

      pragma Debug (O ("Flags =" & Flags'Img));

      pragma Assert (Message_Endianness = Endianness);

      if Message_Minor_Version > 0 then
         Fragment_Next := Is_Set (Flags, Fragment_Bit);
      end if;

      --  Message type
      Message_Type := Unmarshall (Buffer);

      --  Message size
      Message_Size := Unmarshall (Buffer);

      pragma Debug (O ("Size:" & Message_Size'Img));

      Ses.Major_Version := Message_Major_Version;
      Ses.Minor_Version := Message_Minor_Version;
      --  XXX questionable.

      pragma Debug (O ("Version:" & Ses.Major_Version'Img
                       & "." & Ses.Minor_Version'Img));

      Success := True;

      --  At this point, do /not/ Release_Contents on
      --  Buffer_In: we need to keep the current CDR position
      --  value so the remainder of the message is correctly
      --  aligned.
   end Unmarshall_GIOP_Header;

   -------------------------------
   -- Unmarshall_Locate_Message --
   -------------------------------

   procedure Unmarshall_Locate_Request
     (Buffer        : access Buffer_Type;
      Request_Id    : out Types.Unsigned_Long;
      Object_Key    : out Objects.Object_Id) is
   begin
      --  Request id
      Request_Id := Unmarshall (Buffer);

      --  Object key
      Object_Key := Objects.Object_Id
        (Stream_Element_Array'(Unmarshall (Buffer)));
   end Unmarshall_Locate_Request;

   -----------------------------
   -- Unmarshall_Locate_Reply --
   -----------------------------

   procedure Unmarshall_Locate_Reply
     (Buffer        : access Buffer_Type;
      Request_Id    : out Types.Unsigned_Long;
      Locate_Status : out Locate_Status_Type) is
   begin
      --  Request id
      Request_Id := Unmarshall (Buffer);

      --  Reply Status
      Locate_Status := Unmarshall (Buffer);
   end Unmarshall_Locate_Reply;

   ---------------------
   -- Request_Message --
   ---------------------

   procedure Request_Message
     (Ses                    : access GIOP_Session;
      Pend_Req               : access Pending_Request;
      Response_Expected      : in Boolean;
      Fragment_Next          : out Boolean;
      Sync_Type              : in Sync_Scope)
   is
      use Internals;
      use Internals.NV_Sequence;
      use PolyORB.Objects;

      Header_Space  : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      Header_Buffer : Buffer_Access := new Buffer_Type;
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;

   begin
      Get_Note (Pend_Req.Req.Notepad, N);
      Fragment_Next := False;

      --  Major version 1 is assumed.

      case Ses.Minor_Version is
         when 0 =>
            --  GIOP 1.0

            GIOP.GIOP_1_0.Marshall_Request_Message
              (Ses.Buffer_Out, Request_Id,
               Pend_Req.Target_Profile, Response_Expected,
               To_Standard_String (Pend_Req.Req.Operation));

         when 1 =>
            --  GIOP 1.1

            GIOP.GIOP_1_1.Marshall_Request_Message
              (Ses.Buffer_Out, Request_Id,
               Pend_Req.Target_Profile, Response_Expected,
               To_Standard_String (Pend_Req.Req.Operation));

         when 2 =>
            --  GIOP 1.2

            pragma Debug
              (O (Image (IIOP_Profile_Type
                         (Pend_Req.Target_Profile.all))));

            declare
               Oid : constant Object_Id_Access
                 := Binding_Data.Get_Object_Key
                 (Pend_Req.Target_Profile.all);
            begin
               GIOP.GIOP_1_2.Marshall_Request_Message
                 (Ses.Buffer_Out,
                  Request_Id,
                  Target_Address'
                  (Address_Type => Key_Addr,
                   Object_Key   => Oid),
                  Sync_Type,
                  To_Standard_String (Pend_Req.Req.Operation));
            end;
         when others =>
            Release (Header_Buffer);
            raise GIOP_Error;
            --  An invalid value for the Minor_Version:
            --  should not happen.
      end case;

      Marshall_Argument_List
        (Ses, Pend_Req.Req.Args, PolyORB.Any.ARG_IN);

      if  Length (Ses.Buffer_Out) > Maximum_Message_Size then
         Fragment_Next := True;
      end if;

      case Ses.Minor_Version is
         when 0 =>
            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Request,  Length (Ses.Buffer_Out));

         when 1 =>
            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Request, Length (Ses.Buffer_Out),
               Fragment_Next);

         when 2 =>
            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Request, Length (Ses.Buffer_Out),
               Fragment_Next);
         when others =>
            Release (Header_Buffer);
            raise GIOP_Error;
      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

      pragma Debug (Show (Ses.Buffer_Out.all));
   end Request_Message;

   ------------------------
   -- No_Exception_Reply --
   ------------------------

   procedure No_Exception_Reply
     (Ses           : access GIOP_Session;
      Request       :        Requests.Request_Access;
      Fragment_Next :    out Boolean)
   is
      Header_Buffer : Buffer_Access := new Buffer_Type;
      Header_Space : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;
   begin
      Get_Note (Request.Notepad, N);

      Fragment_Next := False;

      case Ses.Minor_Version is
         when 0 =>
            GIOP.GIOP_1_0.Marshall_No_Exception
              (Ses.Buffer_Out, Request_Id);

         when 1 =>
            GIOP.GIOP_1_1.Marshall_No_Exception
              (Ses.Buffer_Out, Request_Id);

         when 2 =>
            GIOP.GIOP_1_2.Marshall_No_Exception
              (Ses.Buffer_Out, Request_Id);

         when others =>
            Release (Header_Buffer);
            raise GIOP_Error;
      end case;

      --  Marshall the reply Body

      if Ses.Minor_Version >= 2 then
         Pad_Align (Ses.Buffer_Out, 8);
         --  For GIOP 1.2 and higher, reply bodies are
         --  aligned on an 8-byte boundary.
      end if;

      Marshall_From_Any (Ses.Buffer_Out, Request.Result.Argument);

      Marshall_Argument_List
        (Ses, Request.Args, PolyORB.Any.ARG_OUT);

      if Length (Ses.Buffer_Out)  > Maximum_Message_Size then
         Fragment_Next := True;
      end if;

      case Ses.Minor_Version is
         when 0 =>
            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply,
               Length (Ses.Buffer_Out));

         when 1 =>
            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply,
               Length (Ses.Buffer_Out),
               Fragment_Next);

         when 2 =>
            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply,
               Length (Ses.Buffer_Out),
               Fragment_Next);

         when others =>
            Release (Header_Buffer);
            raise GIOP_Error;

      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
   end No_Exception_Reply;

   ---------------------
   -- Exception_Reply --
   ---------------------

   procedure Exception_Reply
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access;
      Exception_Type  : in Reply_Status_Type;
      Occurence       : in Any.Any;
      Fragment_Next   : out Boolean)
   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;
      Header_Space : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;

      CORBA_Occurence : PolyORB.Any.Any
        := PolyORB.GIOP_P.Exceptions.To_CORBA_Exception (Occurence);
   begin

      Get_Note (Request.Notepad, N);

      pragma Assert (Exception_Type in User_Exception  .. System_Exception);

      case Ses.Minor_Version is
         when 0 =>
            GIOP.GIOP_1_0.Marshall_Exception
              (Ses.Buffer_Out, Request_Id,
               Exception_Type, CORBA_Occurence);

            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length (Ses.Buffer_Out));

         when 1 =>
            GIOP.GIOP_1_1.Marshall_Exception
              (Ses.Buffer_Out,
               Request_Id,
               Exception_Type,
               CORBA_Occurence);

            if Length (Ses.Buffer_Out) > Maximum_Message_Size then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length (Ses.Buffer_Out),
               Fragment_Next);

         when 2 =>
            GIOP.GIOP_1_2.Marshall_Exception
              (Ses.Buffer_Out,
               Request_Id,
               Exception_Type,
               CORBA_Occurence);

            if  Length (Ses.Buffer_Out) > Maximum_Message_Size then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length (Ses.Buffer_Out),
               Fragment_Next);

         when others =>
            Release (Header_Buffer);
            raise GIOP_Error;
      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

   end Exception_Reply;

   ----------------------------
   -- Location_Forward_Reply --
   ----------------------------

   procedure Location_Forward_Reply
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access;
      Forward_Ref     : in PolyORB.References.IOR.IOR_Type;
      Fragment_Next   : out Boolean)

   is
      Header_Buffer : Buffer_Access := new Buffer_Type;
      Header_Space  : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;
   begin

      Get_Note (Request.Notepad, N);
      Fragment_Next := False;

      case Ses.Minor_Version is
         when 0 =>
            GIOP.GIOP_1_0.Marshall_Location_Forward
              (Ses.Buffer_Out, Request_Id,
               Forward_Ref);

            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply,
               Length (Ses.Buffer_Out));

         when 1 =>
            GIOP.GIOP_1_1.Marshall_Location_Forward
              (Ses.Buffer_Out, Request_Id,
               Forward_Ref);

            if  Length (Ses.Buffer_Out) >
              Maximum_Message_Size
            then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length (Ses.Buffer_Out),
               Fragment_Next);

         when 2 =>
            GIOP.GIOP_1_2.Marshall_Location_Forward
              (Ses.Buffer_Out, Request_Id,
               GIOP.Location_Forward,
               Forward_Ref);

            if  Length (Ses.Buffer_Out) >
              Maximum_Message_Size
            then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Reply, Length (Ses.Buffer_Out),
               Fragment_Next);

         when others =>
            Release (Header_Buffer);
            raise GIOP_Error;
      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
   end Location_Forward_Reply;

   -----------------------------------
   -- Needs_Addressing_Mode_Message --
   -----------------------------------

   procedure Needs_Addressing_Mode_Message
     (Ses             : access GIOP_Session;
      Request          : Requests.Request_Access;
      Address_Type    : in Addressing_Disposition)

   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;
      Header_Space  : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;
   begin

      Get_Note (Request.Notepad, N);

      if Ses.Minor_Version /=  2 then
         Release (Header_Buffer);
         raise GIOP_Error;
      end if;

      GIOP.GIOP_1_2.Marshall_Needs_Addressing_Mode
        (Ses.Buffer_Out, Request_Id, Address_Type);

      GIOP.GIOP_1_2.Marshall_GIOP_Header
        (Header_Buffer,
         GIOP.Reply, Length (Ses.Buffer_Out),
         False);

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
   end Needs_Addressing_Mode_Message;

   ----------------------------
   -- Cancel_Request_Message --
   ----------------------------

   procedure Cancel_Request_Message
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access)
   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;
      Header_Space  : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;

   begin

      Get_Note (Request.Notepad, N);

      case Ses.Minor_Version is
         when 0 =>
            GIOP.Marshall_Cancel_Request
              (Ses.Buffer_Out, Request_Id);
            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Cancel_Request, Length (Ses.Buffer_Out));

         when 1 =>
            GIOP.Marshall_Cancel_Request
              (Ses.Buffer_Out, Request_Id);
            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Cancel_Request, Length (Ses.Buffer_Out),
               False);

         when 2 =>
            GIOP.Marshall_Cancel_Request
              (Ses.Buffer_Out, Request_Id);
            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Cancel_Request, Length (Ses.Buffer_Out),
               False);

         when others =>
            Release (Header_Buffer);
            raise GIOP_Error;
      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
   end Cancel_Request_Message;

   ----------------------------
   -- Locate_Request_Message --
   ----------------------------

   procedure Locate_Request_Message
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access;
      Object_Key      : access Objects.Object_Id;
      Fragment_Next   : out Boolean)
   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;
      Header_Space  : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;

   begin
      Get_Note (Request.Notepad, N);

      Fragment_Next := False;

      case Ses.Minor_Version is
         when 0 =>
            GIOP.Marshall_Locate_Request
              (Ses.Buffer_Out, Request_Id,
               Object_Key);

            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Request,
               Length (Ses.Buffer_Out));

         when 1 =>
            GIOP.Marshall_Locate_Request
              (Ses.Buffer_Out, Request_Id,
               Object_Key);

            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Request,
               Length (Ses.Buffer_Out),
               False);

         when 2 =>
            GIOP.GIOP_1_2.Marshall_Locate_Request
              (Ses.Buffer_Out, Request_Id,
               Target_Address'
               (Address_Type => Key_Addr,
                Object_Key =>   Object_Key.all'Unchecked_Access));

            if  Length (Ses.Buffer_Out) > Maximum_Message_Size then
               Fragment_Next := True;
            end if;

            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Request,
               Length (Ses.Buffer_Out),
               False);

         when others =>
            Release (Header_Buffer);
            raise GIOP_Error;
      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);

   end Locate_Request_Message;

   --------------------------
   -- Locate_Reply_Message --
   --------------------------

   procedure Locate_Reply_Message
     (Ses             : access GIOP_Session;
      Request         : Requests.Request_Access;
      Locate_Status   : in Locate_Status_Type)
   is
      Header_Buffer :  Buffer_Access := new Buffer_Type;
      Header_Space  : constant Reservation
        := Reserve (Ses.Buffer_Out, Message_Header_Size);
      N : Request_Note;
      Request_Id : Types.Unsigned_Long renames N.Id;

   begin

      Get_Note (Request.Notepad, N);

      if Ses.Minor_Version < 2
        and then
        (False
         or else Locate_Status = Object_Forward_Perm
         or else Locate_Status = Loc_System_Exception
         or else Locate_Status = Loc_Needs_Addressing_Mode)
      then
         Release (Header_Buffer);
         raise GIOP_Error;
      end if;

      GIOP.Marshall_Locate_Reply
        (Ses.Buffer_Out, Request_Id, Locate_Status);

      case Ses.Minor_Version is
         when 0 =>
            GIOP.GIOP_1_0.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Reply, Length (Ses.Buffer_Out));

         when 1 =>
            GIOP.GIOP_1_1.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Reply,
               Length (Ses.Buffer_Out), False);

         when 2 =>
            GIOP.GIOP_1_2.Marshall_GIOP_Header
              (Header_Buffer,
               GIOP.Locate_Reply,
               Length (Ses.Buffer_Out), False);
         when others =>
            Release (Header_Buffer);
            raise GIOP_Error;
      end case;

      Copy_Data (Header_Buffer.all, Header_Space);
      Release (Header_Buffer);
   end Locate_Reply_Message;

   --------------------
   -- Select_Profile --
   --------------------

   function Select_Profile
     (Buffer  : access Buffer_Type) return
      Profile_Access;

   function Select_Profile
     (Buffer  : access Buffer_Type) return
      Profile_Access
   is
      use PolyORB.References;
      use PolyORB.References.IOR;

      New_Ref    : IOR.IOR_Type := Representations.CDR.Unmarshall (Buffer);
      Prof_Array : constant PolyORB.References.Profile_Array
        := Profiles_Of (New_Ref);

   begin
      pragma Debug (O ("Reply Message : Received Location_Forward"));
      for I in Prof_Array'Range loop
         if Prof_Array (I).all in Binding_Data.IIOP.IIOP_Profile_Type then
            return Prof_Array (I);
         end if;
      end loop;
      return null;
   end Select_Profile;

   -------------------
   -- Store_Profile --
   -------------------

   procedure Store_Profile
     (Ses      :  access GIOP_Session;
      Profile  :  Profile_Access)
   is
   begin
      pragma Assert (Ses.Current_Profile = null);
      --  Else we would leak memory.

      Ses.Current_Profile := Profile;
   end Store_Profile;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (S : in out GIOP_Session) is
   begin
      Protocols.Initialize (Protocols.Session (S));
      S.Buffer_In  := new Buffer_Type;
      S.Buffer_Out := new Buffer_Type;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (S : in out GIOP_Session) is
   begin
      pragma Debug (O ("Finalizing GIOP session"));
      Protocols.Finalize (Protocols.Session (S));
      if S.Current_Profile /= null then
         pragma Debug (O ("... destroying server profile."));
         Destroy_Profile (S.Current_Profile);
         --  XXX currently this is correct because a /copy/
         --  of the profile is made in Bind_Non_Local_Profile.
      end if;
      if S.Buffer_In /= null then
         Release (S.Buffer_In);
      end if;
      if S.Buffer_Out /= null then
         Release (S.Buffer_Out);
      end if;
   end Finalize;

   --------------------
   -- Expect_Message --
   --------------------

   procedure Expect_Message (S : access GIOP_Session);
   --  Prepare S to receive next GIOP message.
   --  This must be called once when a session is established
   --  (in Handle_Connect_Indication for a server session,
   --  in Handle_Connect_Confirmation for a client session),
   --  and then exactly once after a message has been received.
   --  This must not be called after sending a message (because
   --  message sends and receives can be interleaved in an
   --  arbitrary way, and Expect_Message must not be called
   --  twice in a row).

   procedure Expect_Message (S : access GIOP_Session) is
   begin
      Buffers.Release_Contents (S.Buffer_In.all);
      Expect_Data (S, S.Buffer_In, Message_Header_Size);
      S.State := Expect_Header;
   end Expect_Message;

   -------------------
   -- Store_Request --
   -------------------

   procedure Store_Request
     (Ses     :  access GIOP_Session;
      R       :  Requests.Request_Access;
      Profile :  Profile_Access;
      Pending :  out Pending_Request)
   is
      use Pend_Req_Seq;
   begin
      Set_Note
        (R.Notepad, Request_Note'
         (Annotations.Note with Id => Get_Request_Id));
      Pending  := Pending_Request'
        (Req => R, Target_Profile => Profile);
      Append (Ses.Pending_Rq, Pending);
   end Store_Request;

   -----------------
   -- Set_Version --
   -----------------

   procedure Set_Version
     (S             : access GIOP_Session;
      Major_Version :        Types.Octet;
      Minor_Version :        Types.Octet) is
   begin
      S.Major_Version := Major_Version;
      S.Minor_Version := Minor_Version;
   end Set_Version;

   ----------------------------
   -- Marshall_Argument_List --
   ----------------------------

   procedure Marshall_Argument_List
     (Ses : access GIOP_Session;
      Args : in out Any.NVList.Ref;
      Direction : Any.Flags)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Sequence;

      List : constant NV_Sequence_Access := List_Of (Args);
      Arg  : PolyORB.Any.NamedValue;
   begin
      pragma Assert
        (Direction = ARG_IN or else Direction = ARG_OUT);

      for I in 1 ..  Get_Count (Args) loop
         Arg := NV_Sequence.Element_Of (List.all, Positive (I));
         if False
           or else Arg.Arg_Modes = Direction
           or else Arg.Arg_Modes = ARG_INOUT
         then
            pragma Debug (O ("Marshalling argument "
              & Types.To_Standard_String (Arg.Name)
              & " = " & Image (Arg.Argument)));
            Marshall (Ses.Buffer_Out, Arg);
         end if;
      end loop;
   end Marshall_Argument_List;

   ------------------------------
   -- Unmarshall_Argument_List --
   ------------------------------

   procedure Unmarshall_Argument_List
     (Ses : access GIOP_Session;
      Args : in out Any.NVList.Ref;
      Direction : Any.Flags)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Sequence;

      List     : constant NV_Sequence_Access := List_Of (Args);
      Temp_Arg : Any.NamedValue;
   begin
      pragma Assert
        (Direction = ARG_IN or else Direction = ARG_OUT);

      for I in 1 .. Get_Count (Args) loop
         Temp_Arg :=  NV_Sequence.Element_Of
           (List.all, Positive (I));
         if False
           or else Temp_Arg.Arg_Modes = Direction
           or else Temp_Arg.Arg_Modes = ARG_INOUT
         then
            Unmarshall_To_Any (Ses.Buffer_In, Temp_Arg.Argument);
         end if;
         Copy_Any_Value
           (NV_Sequence.Element_Of (List.all, Positive (I)).Argument,
            Temp_Arg.Argument);
         --          NV_Sequence.Replace_Element
         --            (List.all, Positive (I), Temp_Arg);

         --  XXX This is very inefficient because of multiple
         --    copies of an Any. The Unmarshall_To_Any should
         --    be done in-place on the actual Element_Array
         --    deep within List...
      end loop;
   end Unmarshall_Argument_List;

   ---------------------------------
   -- Handle_Unmarshall_Arguments --
   ---------------------------------

   procedure Handle_Unmarshall_Arguments
     (Ses : access GIOP_Session;
      Args : in out Any.NVList.Ref)
   is
   begin
      pragma Assert (Ses.State = Arguments_Ready);
      Unmarshall_Argument_List (Ses, Args, PolyORB.Any.ARG_IN);
      Expect_Message (Ses);
   end Handle_Unmarshall_Arguments;

   ----------------------
   -- Request_Received --
   ----------------------

   procedure Request_Received
     (Ses : access GIOP_Session)
   is
      use Binding_Data.IIOP;
      use Binding_Data.Local;
      use References;

      use PolyORB.Objects;

      use Req_Seq;

      Request_Id        :  Types.Unsigned_Long;
      Response_Expected :  Boolean;
      Sync_Type         :  Sync_Scope := NONE;
      Object_Key        :  Objects.Object_Id_Access;
      Operation         :  Types.String;

      Req       : Request_Access;
      Req_Flags : Flags := 0;
      Args      : Any.NVList.Ref;

      Result : Any.NamedValue;
      --  Dummy NamedValue for Create_Request; the actual Result
      --  is set by the called method.

      Target : References.Ref;
      Target_Ref : Target_Address_Access;

      Deferred_Arguments_Session : Component_Access;
      ORB : constant ORB_Access := ORB_Access (Ses.Server);

   begin
      pragma Debug (O ("Request_Received: entering"));

      case Ses.Minor_Version is
         when 0 =>
            GIOP.GIOP_1_0.Unmarshall_Request_Message
              (Ses.Buffer_In,
               Request_Id,
               Response_Expected,
               Object_Key,
               Operation);

            if not Response_Expected then
               Req_Flags := Sync_With_Target;
            end if;

         when 1 =>
            GIOP.GIOP_1_1.Unmarshall_Request_Message
              (Ses.Buffer_In,
               Request_Id,
               Response_Expected,
               Object_Key,
               Operation);

            if not Response_Expected then
               Req_Flags := Sync_With_Target;
            end if;

         when 2 =>
            GIOP.GIOP_1_2.Unmarshall_Request_Message
              (Ses.Buffer_In,
               Request_Id,
               Response_Expected,
               Sync_Type,
               Target_Ref,
               Operation);

            if Target_Ref.Address_Type = Key_Addr then
               Object_Key := Target_Ref.Object_Key;
            end if;

            if not Response_Expected then
               case Sync_Type is
                  when WITH_TRANSPORT => Req_Flags := Sync_With_Transport;
                  when WITH_SERVER    => Req_Flags := Sync_With_Server;
                  when WITH_TARGET    => Req_Flags := Sync_With_Target;
                  when others         => null;
               end case;
            end if;

         when others =>
            raise GIOP_Error;
      end case;

      pragma Debug (O ("Request_Received: Unmarshalled request header"));

      Args := Obj_Adapters.Get_Empty_Arg_List
        (Object_Adapter (ORB),
         Object_Key,
         To_Standard_String (Operation));

      Ses.State := Arguments_Ready;

      if not Is_Nil (Args) then
         --  The signature of the method is known: unmarshall
         --  the arguments right now.
         Handle_Unmarshall_Arguments (Ses, Args);
      else
         --  Unable to obtain the list of arguments at this point.
         --  Defer the unmarshalling until the Servant has a chance
         --  to provide its own arg list.
         Deferred_Arguments_Session
           := Components.Component_Access (Ses);
      end if;

      if Ses.Minor_Version = 2
        and then Target_Ref.Address_Type /= Key_Addr
      then
         if Target_Ref.Address_Type = Profile_Addr then
            Create_Reference ((1 => Target_Ref.Profile), "", Target);
            --  Create a temporary, typeless reference for this object.
            --  If we wanted to have proper type information, we would
            --  have to resolve the (local) object id through the object
            --  adapter, and query the target object for its most derived
            --  type.
         else
            Target := References.Ref (Target_Ref.Ref.IOR);
         end if;
         Free (Target_Ref);
         --  XXX Corollary: when Target_Ref.Address_Type = Profile_Addr,
         --  the profile must not be destroyed when Target_Ref.all
         --  is finalised, else Target will reference a free'd profile.
      else
         declare
            Target_Profile : constant Binding_Data.Profile_Access
              := new Local_Profile_Type;
            --  Should be free'd when the Target_Reference
            --  is finalized.

         begin
            Create_Local_Profile
              (Object_Key.all, Local_Profile_Type (Target_Profile.all));
            Create_Reference ((1 => Target_Profile), "", Target);
            --  Create a temporary, typeless reference for this object.
            --  (see comment above).
            Free (Target_Ref);
         end;
      end if;

      Create_Request
        (Target    => Target,
         Operation => To_Standard_String (Operation),
         Arg_List  => Args,
         Result    => Result,
         Deferred_Arguments_Session => Deferred_Arguments_Session,
         Req       => Req,
         Req_Flags => Req_Flags);

      Set_Note
        (Req.Notepad, Request_Note'(Annotations.Note with Id => Request_Id));

      PolyORB.ORB.Queue_Request_To_Handler
        (ORB.Tasking_Policy, ORB,
         Queue_Request'
         (Request => Req,
          Requestor => Component_Access (Ses)));
      Free (Object_Key);
   end Request_Received;

   ----------------------------------------
   -- Unmarshall_System_Exception_To_Any --
   ----------------------------------------

   procedure Unmarshall_System_Exception_To_Any
     (Buffer : Buffer_Access;
      Info   : out Any.Any);

   procedure Unmarshall_System_Exception_To_Any
     (Buffer : Buffer_Access;
      Info   : out Any.Any)
   is
   begin
      Info := Any.Get_Empty_Any
        (Exceptions.System_Exception_TypeCode (Unmarshall (Buffer)));
      Unmarshall_To_Any
        (Buffer, Info);
   end Unmarshall_System_Exception_To_Any;

   --------------------
   -- Reply_Received --
   --------------------

   procedure Reply_Received
     (Ses : access GIOP_Session)
   is
      use References.IOR;
      use Binding_Data.IIOP;
      use Pend_Req_Seq;

      Reply_Status  : Reply_Status_Type;
      Request_Id    : Types.Unsigned_Long;
      Current_Req   : Pending_Request;
      N             : Request_Note;
      ORB           : constant ORB_Access
        := ORB_Access (Ses.Server);

   begin
      case Ses.Minor_Version is
         when  0 =>

            GIOP.GIOP_1_0.Unmarshall_Reply_Message
              (Ses.Buffer_In,
               Request_Id,
               Reply_Status);

            if Reply_Status = Location_Forward_Perm
              or else Reply_Status = Needs_Addressing_Mode then
               raise GIOP_Error;
            end if;

         when 1 =>
            GIOP.GIOP_1_1.Unmarshall_Reply_Message
              (Ses.Buffer_In,
               Request_Id,
               Reply_Status);

            if Reply_Status = Location_Forward_Perm
              or else Reply_Status = Needs_Addressing_Mode
            then
               raise GIOP_Error;
            end if;

         when  2 =>
            GIOP.GIOP_1_2.Unmarshall_Reply_Message
              (Ses.Buffer_In,
               Request_Id,
               Reply_Status);

         when others =>
            raise GIOP_Error;
      end case;

      declare
         Pending_Reqs : constant Pend_Req_Seq.Element_Array
           := Pend_Req_Seq.To_Element_Array (Ses.Pending_Rq);
      begin
         for J in Pending_Reqs'Range loop
            Get_Note (Pending_Reqs (J).Req.Notepad, N);
            if N.Id  = Request_Id then
               Delete (Ses.Pending_Rq, J, 1);
               Current_Req := Pending_Reqs (J);
               exit;
            end if;
         end loop;

         if Current_Req.Req = null then
            raise GIOP_Error;
         end if;
      end;

      pragma Debug (O ("Received reply with status "
                       & Reply_Status_Type'Image (Reply_Status)));

      case Reply_Status is

         when No_Exception =>

            --  Unmarshall reply body.

            if Ses.Minor_Version >= 2 then
               Align_Position (Ses.Buffer_In, 8);
               --  For GIOP 1.2 and higher, reply bodies are
               --  aligned on an 8-byte boundary.
            end if;

            Unmarshall_To_Any
              (Ses.Buffer_In, Current_Req.Req.Result.Argument);

            Unmarshall_Argument_List
              (Ses, Current_Req.Req.Args, PolyORB.Any.ARG_OUT);

            Emit_No_Reply
              (Current_Req.Req.Requesting_Component,
               Objects.Interface.Executed_Request'
               (Req => Current_Req.Req));

         when User_Exception =>
            declare
               RepositoryId : constant PolyORB.Types.String
                 := Unmarshall (Ses.Buffer_In);
               Except_Index : constant PolyORB.Types.Unsigned_Long
                 := Any.ExceptionList.Search_Exception_Id
                 (Current_Req.Req.Exc_List, RepositoryId);
            begin
               if Except_Index = 0 then
                  --  Current_Req.Req.Exception_Info
                  --    := To_Any (UNKNOWN);
                  raise Not_Implemented;
               else
                  Current_Req.Req.Exception_Info
                    := PolyORB.Any.Get_Empty_Any
                    (Any.ExceptionList.Item
                     (Current_Req.Req.Exc_List, Except_Index));
                  Unmarshall_To_Any
                    (Ses.Buffer_In,
                     Current_Req.Req.Exception_Info);
                  pragma Debug
                    (O ("Exception: "
                        & Any.Image (Current_Req.Req.Exception_Info)));
               end if;
               Emit_No_Reply
                 (Component_Access (ORB),
                  Objects.Interface.Executed_Request'
                  (Req => Current_Req.Req));
            end;

         when Needs_Addressing_Mode =>
--             Current_Req.Req.Exception_Info
--               := To_Any (Not_Implemented);

--             Emit_No_Reply
--               (Component_Access (ORB),
--                Objects.Interface.Executed_Request'
--                (Req => Current_Req.Req));
            raise Not_Implemented;

         when System_Exception =>
            Unmarshall_System_Exception_To_Any
              (Ses.Buffer_In, Current_Req.Req.Exception_Info);
            Emit_No_Reply
              (Component_Access (ORB),
               Objects.Interface.Executed_Request'
               (Req => Current_Req.Req));

         when Location_Forward | Location_Forward_Perm =>

            declare
               New_Ses : Session_Access;
               Prof    : Profile_Access;

            begin
               Prof := Select_Profile (Ses.Buffer_In);
               New_Ses := Session_Access
                 (Binding_Data.IIOP.Bind_Profile
                    (IIOP_Profile_Type (Prof.all),
                     Component_Access (ORB)));

               --  Release the previous session buffers
               Release (Ses.Buffer_In);
               Release (Ses.Buffer_Out);

               Store_Request (Ses, Current_Req.Req, Prof, Current_Req);

               GIOP.Invoke_Request
                 (GIOP_Session (New_Ses.all)'Access,
                  Current_Req.Req,
                  Prof);
            end;
      end case;
   end Reply_Received;

   ----------------------------
   -- Locate_Request_Receive --
   ----------------------------

   procedure Locate_Request_Receive
     (Ses : access GIOP_Session)
   is
--      Request_Id    : Types.Unsigned_Long;
--       Object_Key    : Objects.Object_Id_Access := null;
--       Target_Ref    : Target_Address_Access := null;
   begin
      --   Processing of Incoming Locate Request not yet implemented
      raise Not_Implemented;

--       if Ses.Minor_Version /= 2 then
--          GIOP.Unmarshall_Locate_Request
--            (Ses.Buffer_In,
--             Request_Id,
--             Object_Key.all);
--       else
--          GIOP.GIOP_1_2.Unmarshall_Locate_Request
--            (Ses.Buffer_In,
--             Request_Id,
--             Target_Ref.all);
--       end if;

   end Locate_Request_Receive;

   -------------------------
   -- Visible subprograms --
   -------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (Proto   : access GIOP_Protocol;
      Session : out Filter_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Proto);
      pragma Warnings (On);
   begin
      Session := new GIOP_Session;
      Set_Allocation_Class (Session.all, Dynamic);
   end Create;

   --------------------
   -- Invoke_Request --
   --------------------

   procedure Invoke_Request
     (S   : access GIOP_Session;
      R   : Requests.Request_Access;
      Pro : access Binding_Data.Profile_Type'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Pro);
      pragma Warnings (On);

      use Buffers;
      use Binding_Data.IIOP;
      use PolyORB.Filters.Interface;
      use PolyORB.Objects;
      use PolyORB.Requests;
      use Pend_Req_Seq;

      Fragment_Next  : Boolean := False;
      Current_Req    : aliased Pending_Request;
   begin

      if S.Role  = Server then
         raise GIOP_Error;
      end if;

      Release_Contents (S.Buffer_Out.all);
      Store_Request (S, R, S.Current_Profile, Current_Req);

      --  fragmentation not yet implemented
      --  Message_Size:= Length (Buf1);

      --  if Message_Size > Maximum_Body_Size then
      --     Buf2 :=
      --  end if;

      --  XXX
      --  The following block (force Locate_Request before each
      --  invocation) is disabled because:
      --    * it is not known for sure that it is correct and
      --      desirable;
      --    * Locate_Request handling is not implemented by
      --      the Server part of this GIOP stack.

      if False and then S.Object_Found = False then
         if S.Nbr_Tries <= Max_Nb_Tries then
            declare
               Oid : constant Object_Id_Access := Get_Object_Key
                   (Current_Req.Target_Profile.all);
            begin
               Locate_Request_Message
                 (S, Current_Req.Req, Oid, Fragment_Next);
               S.Nbr_Tries := S.Nbr_Tries + 1;
               pragma Debug (O ("Locate Request Message"));
            end;
         else
            pragma Debug (O ("Number of tries exceeded"));

            return;
         end if;
      else
         if Is_Set (Sync_None, R.Req_Flags) then
            Request_Message (S, Current_Req'Access,
                             False, Fragment_Next, NONE);

            Delete (S.Pending_Rq, Length (S.Pending_Rq), 1);
            --  XXX Is it safe to destroy the pending request at this stage ?
            --  Is anyone still referencing it at this point ?

         elsif Is_Set (Sync_With_Transport, R.Req_Flags) then
            Request_Message (S, Current_Req'Access,
                             False, Fragment_Next, WITH_TRANSPORT);

            Delete (S.Pending_Rq, Length (S.Pending_Rq), 1);
            --  XXX Is it safe to destroy the pending request at this stage ?
            --  Is anyone still referencing it at this point ?

         elsif Is_Set (Sync_With_Server, R.Req_Flags) then
            Request_Message (S, Current_Req'Access,
                             False, Fragment_Next, WITH_SERVER);

         elsif Is_Set (Sync_With_Target, R.Req_Flags) or
           Is_Set (Sync_Call_Back, R.Req_Flags) then
            Request_Message (S, Current_Req'Access,
                             True, Fragment_Next, WITH_TARGET);
         end if;

         S.Object_Found := True;
         S.Nbr_Tries := 0;
      end if;

      --  Send the data to lower layers
      Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => S.Buffer_Out));
   end Invoke_Request;

   -------------------
   -- Abort_Request --
   -------------------

   procedure Abort_Request
     (S : access GIOP_Session;
      R : Requests.Request_Access)
   is
      use PolyORB.Filters.Interface;
      use Pend_Req_Seq;
      Current_Req   : aliased Pending_Request;
      Pending_Note  : Request_Note;
      Current_Note  : Request_Note;
      Pending : constant Pend_Req_Seq.Element_Array
        := Pend_Req_Seq.To_Element_Array (S.Pending_Rq);

   begin

      if S.Role  = Server then
         raise GIOP_Error;
      end if;

      Get_Note (R.Notepad, Current_Note);

      for I in Pending'Range loop
         Get_Note (Pending (I).Req.Notepad, Pending_Note);
         if Pending_Note.Id = Current_Note.Id then
            Current_Req := Pending (I);
            Delete (S.Pending_Rq, I - Pending'First + 1, 1);
            exit;
         end if;
         raise GIOP_Error;
      end loop;

      Release_Contents (S.Buffer_Out.all);
      Cancel_Request_Message (S, Current_Req.Req);

      --  Sending the message
      Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => S.Buffer_Out));

   end Abort_Request;

   ----------------
   -- Send Reply --
   ----------------

   procedure Send_Reply
     (S : access GIOP_Session;
      R :        Requests.Request_Access)
   is
      use Buffers;
      use Representations.CDR;
      use PolyORB.Filters.Interface;
      use Req_Seq;
      Fragment_Next : Boolean := False;
   begin

      if S.Role  = Client then
         raise GIOP_Error;
      end if;

      Release_Contents (S.Buffer_Out.all);

      if PolyORB.Any.Is_Empty (R.Exception_Info) then
         pragma Debug (O ("Send_Reply: No exception."));
         No_Exception_Reply (S, R, Fragment_Next);
      else
         declare
            use PolyORB.Any;

            TC : constant TypeCode.Object
              := Get_Type (R.Exception_Info);
            EId : constant String
              := To_Standard_String (TypeCode.Id (TC));
            EType : Reply_Status_Type;
         begin
            if EId'Length > PolyORB.Exceptions.PolyORB_Root'Length
              and then EId
              (EId'First .. EId'First
               + PolyORB.Exceptions.PolyORB_Prefix'Length - 1)
              = PolyORB.Exceptions.PolyORB_Prefix
            then
               EType := System_Exception;
            else
               EType := User_Exception;
            end if;
            pragma Debug
              (O ("Send_Reply: " & Reply_Status_Type'Image (EType)));
            pragma Debug (O (EId));
            pragma Debug (O (EId
              (EId'First .. EId'First
               + PolyORB.Exceptions.PolyORB_Prefix'Length - 1)));
            pragma Debug (O (PolyORB.Exceptions.PolyORB_Prefix));
            Exception_Reply
              (S, R, EType, R.Exception_Info, Fragment_Next);
         end;
      end if;

      pragma Debug (O ("Sending reply"));

      --  Send the message down to lower layers.
      Emit_No_Reply (Lower (S), Data_Out'(Out_Buf => S.Buffer_Out));

      pragma Debug (O ("After Sending Reply"));
   end Send_Reply;

   -------------------------------
   -- Handle_Connect_Indication --
   -------------------------------

   procedure Handle_Connect_Indication (S : access GIOP_Session) is
   begin
      S.Role := Server;
      Expect_Message (S);
   end Handle_Connect_Indication;

   ---------------------------------
   -- Handle_Connect_Confirmation --
   ---------------------------------

   procedure Handle_Connect_Confirmation (S : access GIOP_Session) is
   begin
      S.Role := Client;
      Expect_Message (S);
   end Handle_Connect_Confirmation;

   ----------------------------
   -- Handle_Data_Indication --
   ----------------------------

   procedure Handle_Data_Indication
     (S           : access GIOP_Session;
      Data_Amount :        Stream_Element_Count)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Data_Amount);
      pragma Warnings (On);

      use Binding_Data.IIOP;
      use Objects;
      use ORB;
      use References;
      use References.IOR;
      use Pend_Req_Seq;

      Mess_Type     : Msg_Type;
      Mess_Size     : Types.Unsigned_Long;
      Fragment_Next : Boolean;
      Success       : Boolean;
      ORB           : constant ORB_Access := ORB_Access (S.Server);

   begin
      pragma Debug (O ("Received data on transport endpoint..."));
      pragma Debug (Buffers.Show (S.Buffer_In.all));

      if S.State = Expect_Header then
         Unmarshall_GIOP_Header
           (S, Mess_Type, Mess_Size,
            Fragment_Next, Success);

         if not Success then
            raise GIOP_Error;
         end if;

         pragma Debug
           (O ("Got GIOP header of type "
               & Mess_Type'Img
               & "," & Mess_Size'Img & " bytes expected."));
         S.Mess_Type_Received  := Mess_Type;
         S.State := Expect_Body;
         Expect_Data (S, S.Buffer_In, Stream_Element_Count (Mess_Size));
         return;
      else
         pragma Assert (S.State = Expect_Body);
         --  When receiving a Data_Indication, the session must
         --  be in one of the states where it expects data.
         null;
      end if;

      pragma Debug (O ("Processing message body of type "
                       & S.Mess_Type_Received'Img));

      case S.Mess_Type_Received  is

         when Request =>
            if S.Role = Server then
               Request_Received (S);
            else
               raise GIOP_Error;
            end if;

         when Reply =>
            if S.Role = Client then
               Reply_Received (S);
            else
               raise GIOP_Error;
            end if;

         when Cancel_Request =>
            if S.Role = Client then
               raise Not_Implemented;
            else
               raise GIOP_Error;
            end if;

         when Locate_Request =>
            if S.Role = Server then
               --  not yet implemented
               raise Not_Implemented;
            else
               raise GIOP_Error;
            end if;

         when Locate_Reply =>
            if S.Role = Client  then
               declare
                  Req_Id        : Types.Unsigned_Long;
                  Locate_Status : Locate_Status_Type;
               begin
                  Unmarshall_Locate_Reply
                    (S.Buffer_In, Req_Id, Locate_Status);
                  case Locate_Status is
                     when Object_Here =>

                        declare
                           Current_Req : Pending_Request;
                           Current_Note : Request_Note;
                           Pending : constant Pend_Req_Seq.Element_Array
                             := Pend_Req_Seq.To_Element_Array (S.Pending_Rq);
                        begin
                           S.Object_Found := False;
                           for I in Pending'Range loop
                              Current_Req := Pending (I);
                              Get_Note (Current_Req.Req.Notepad, Current_Note);
                              S.Object_Found := (Current_Note.Id = Req_Id);
                              exit when S.Object_Found;
                           end loop;

                           if not S.Object_Found then
                              raise GIOP_Error;
                           end if;

                           Invoke_Request
                             (S,
                              Current_Req.Req,
                              Current_Req.Target_Profile);
                        end;

                     when Unknown_Object =>
                        pragma Debug (O ("Object not found"));
                        Release (S.Buffer_In);
                        Release (S.Buffer_Out);
                        return;

                     when Object_Forward | Object_Forward_Perm =>
                        declare
                           New_Ses     : Session_Access;
                           Current_Req : Pending_Request;
                           Current_Note : Request_Note;
                        begin

                           for I in 1 .. Length (S.Pending_Rq) loop
                              Current_Req := Element_Of (S.Pending_Rq, I);
                              Get_Note (Current_Req.Req.Notepad, Current_Note);
                              if Current_Note.Id = Req_Id then
                                 Current_Req.Target_Profile :=
                                    Select_Profile (S.Buffer_In);
                                 Replace_Element (S.Pending_Rq,
                                     I, Current_Req);
                              end if;
                              raise GIOP_Error;
                           end loop;

                           New_Ses := Session_Access
                             (Binding_Data.IIOP.Bind_Profile
                                (IIOP_Profile_Type
                                   (Current_Req.Target_Profile.all),
                                 Component_Access (ORB)));

                           --  Release the previous session buffers.

                           Release (S.Buffer_In);
                           Release (S.Buffer_Out);
                           GIOP.Invoke_Request
                             (GIOP_Session (New_Ses.all)'Access,
                              Current_Req.Req,
                              Current_Req.Target_Profile);
                        end;

                     when Loc_Needs_Addressing_Mode =>
                        raise Not_Implemented;

                     when Loc_System_Exception =>
                        raise Not_Implemented;
                  end case;
               end;
            else
               raise GIOP_Error;
            end if;

         when Close_Connection =>
            if S.Role = Server or else S.Minor_Version = 2 then
               raise Program_Error;
            else
               raise Not_Implemented;
            end if;

         when Message_Error =>
            raise GIOP_Error;

         when Fragment =>
            raise Not_Implemented;
      end case;

      if S.State = Expect_Body then

         --  The expected message body has now been received
         --  and processed: prepare to receive next message.

         Expect_Message (S);
      end if;
   end Handle_Data_Indication;

   -----------------------
   -- Handle_Disconnect --
   -----------------------

   procedure Handle_Disconnect (S : access GIOP_Session) is
   begin
      Release (S.Buffer_In);
      Release (S.Buffer_Out);
   end Handle_Disconnect;

   ---------
   -- Set --
   ---------

   procedure Set
     (Bit_Field : in out Types.Octet;
      Bit_Order : Bit_Order_Type;
      Bit_Value : Boolean) is
   begin
      if Bit_Value then
         Bit_Field := Bit_Field or (2 ** Bit_Order);
      else
         Bit_Field := Bit_Field and not (2 ** Bit_Order);
      end if;
   end Set;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Bit_Field : Types.Octet;
      Bit_Order : Bit_Order_Type)
     return Boolean is
   begin
      return (Bit_Field and (2 ** Bit_Order)) /= 0;
   end Is_Set;

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
